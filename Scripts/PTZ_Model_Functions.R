#Functions needed to set up analysis - based on work of Dr. Justin Kenney
# Load necessary libraries
library(slider) # For sliding window
library(dplyr) # For bind_cols
library(randomForest)
library(zoo)
library(zipR) # For zip function
library(signal) #For sgolayfilt
library(fields) #for rdist
library(pracma) #for linearproj

#Function Distance 
pairwise_distances <- function(df, normalize=TRUE){
  x.indices <- grep('x',colnames(df))
  y.indices <- grep('y',colnames(df))
  
  #Get all pairwise distances
  df.distance <- apply(df, 1, function(x) {out <- rdist(matrix(x, ncol=2, byrow=TRUE)); out <- t(out[lower.tri(out)]); out})
  df.distance <- t(df.distance)
  
  #Add in column names
  l.colnames.short <- colnames(df)[x.indices]
  l.colnames.short <- gsub('x_dp_', '', l.colnames.short) #Remove x_dp_ 
  colname.combos <- combn(1:(ncol(df)/2), m=2)
  l.dist.colnames <- apply(colname.combos, 2, function(x) {paste('dist',l.colnames.short[x[1]], l.colnames.short[x[2]], sep='.')})
  colnames(df.distance) <- l.dist.colnames
  
  if(normalize == TRUE){
    df.distance <- df.distance / median(df.distance[,'dist.Head.Tail3'])
  }
  
  return(df.distance)
  
}

all_angle_combinations <- function(df){
  #Get x and y indices
  x.indices <- grep('x',colnames(df))
  y.indices <- grep('y',colnames(df))
  
  n.points <- ncol(df)/2
  n.combos <- combn(c(1:n.points), m=3) #Get all combos of three
  
  df.angles <- apply(df, 1, function(x) {out <- combo_angles_between_points(n.combos, matrix(x, ncol=2, byrow=TRUE)); out})
  
  if (dim(n.combos)[2] > 1) {
    df.angles <- t(df.angles)
  } else {
    df.angles <- data.frame(df.angles)
  }
  
  l.colnames.short <- colnames(df)[x.indices]
  l.colnames.short <- gsub('x_dp_', '', l.colnames.short)
  l.angle.colnames <- apply(n.combos, 2, function(x) {paste('angle',l.colnames.short[x[1]], l.colnames.short[x[2]],l.colnames.short[x[3]], sep='.')})
  colnames(df.angles) <- l.angle.colnames
  
  return(df.angles)
}

angle_three_points <- function(P1, P2, P3){
  #P2 is the vertex for the points
  
  angle <- atan2(P1[2] - P2[2], P1[1] - P2[1]) - atan2(P3[2] - P2[2], P3[1] - P2[1])
  
  if(is.na(angle)){
    angle <- 0
  } else if (angle < 0){
    angle <- angle + 2*pi
  }
  
  #Turn into degrees from radians:
  angle <- angle * 180/pi
  
  return(angle)
}

combo_angles_between_points <- function(column.indices, df){
  
  angles <- apply(column.indices, 2, function(x) {angle_three_points(df[x[1],], df[x[2],], df[x[3],])})
  return(angles)
}

project_point_df <- function(df, col.basis.start, col.basis.end){

  #Get direction of first basis vector for each frame
  df.basis <- df[,col.basis.end] - df[,col.basis.start]
  colnames(df.basis) <- c('x.basis','y.basis')
  
  #Generate the projection, which is the difference from a frame and the previous frame
  df.to.proj <- data.frame(diff(as.matrix(df), lag=1, difference=1))
  
  #Combine basis and vectors to be projected
  df.basis.to.proj <- data.frame(df.basis[2:nrow(df.basis),],df.to.proj)
  
  #Project vectors onto basis
  df.proj <- apply(df.basis.to.proj, 1, function(x) {c(linearproj(make_basis_matrix(x[1:2]), matrix(x[3:length(x)],2,(length(x)-2)/2))$P)})
  df.proj <- data.frame(t(df.proj))
  
  #Update column names
  proj.colnames <- colnames(df)
  proj.colnames <- lapply(proj.colnames, function(x) {paste(x,'proj', sep='.')})
  colnames(df.proj) <- proj.colnames
  
  return(df.proj)
}


make_basis_matrix <- function(basis.vector){
  m.basis <- matrix(c(basis.vector, rev(basis.vector) * c(1,-1)),2,2)
  return(m.basis)
}

get_k_nearest_neighbors <- function(df.targets, l.inputs, k){
  #Get the indices of the k nearest neighbors between a long list of targets (df.targets) and a single input (l.inputs)
  
  #Calculate euclidean distances between single list of numbers and all numbers in df.targets
  dist.list <- rowSums(sweep(df.targets, 2, l.inputs, function(x,y) {(x-y)^2}))
  #Get the ids of the k nearest nodes
  l.neighbor.ids <- sort(dist.list, index.return=TRUE)$ix[1:k]
  
  return(l.neighbor.ids)
  
}


Mode <- function(x) {
  #Based on https://stackoverflow.com/questions/2547402/how-to-find-the-statistical-mode
  ux <- unique(x)
  tab <- tabulate(match(x,ux))
  max.tab <- which.max(tab)
  mode <- ux[max.tab]
  mode.proportion <- tab[max.tab] / sum(tab)
  
  return(list(mode=mode, mode.proportion=mode.proportion))
}


getmode <- function(v) {
  #Just the mode only
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

