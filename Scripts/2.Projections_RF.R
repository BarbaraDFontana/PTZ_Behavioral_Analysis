#Get libraries and functions
source('E:/Barbara/Postdoc_Denis(2024-2025)/Data_Analysis/PTZ_Model_Functions.R') 

# Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

set.seed(14)

# Getting RDS file containing training info
l.df.input.1 <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Labelled_Data_RF.rds')

# Make sure the columns are with correct names
l.columns.data <- c('X', 'Head_x', 'Head_y', 'Trunk_x', 'Trunk_y', 'Tail_x', 'Tail_y', 'Z_x', 'Z_y', 'behavior.label')
l.df.input.1 <- lapply(l.df.input.1, function(x) `colnames<-`(x, l.columns.data))

# Make sure columns 2 to 9 are numeric 
l.df.input.1 <- lapply(l.df.input.1, function(x) {x[2:9] <- lapply(x[2:9], as.numeric); x})

# Smooth the traces to remove the 'jiggle' from DLC from column 2 to 9
l.df.input.1 <- lapply(l.df.input.1, function(x) {as.data.frame(apply(x[2:9], 2, sgolayfilt, n=5)); x})

# Convert pixel values to cm - from column 2 to 9
cm = 22.5
l.df.input.1 <- mapply(function(x, y) {x[2:9] <- x[2:9] * y; x}, l.df.input.1, cm, SIMPLIFY = FALSE)

# Remove column 1.frames and 10.behavior label to run the postural analysis
l.df.input.2 <- lapply(l.df.input.1, function(x) {as.data.frame(x[-c(1, 10)])})


# Now start creating list of dataframes with postural information
l.df.posture.info <- lapply(l.df.input.2, pairwise_distances, normalize=FALSE)
l.df.posture.info <- mapply(function(x, y) {out <- cbind(x, all_angle_combinations(y)); out}, l.df.posture.info, l.df.input.2, SIMPLIFY=FALSE)
l.df.posture.info <- mapply(function(x, y) {out <- cbind(x[1:(nrow(x)-1),], project_point_df(y, col.basis.start=c('Trunk_x', 'Trunk_y'), col.basis.end=c('Head_x','Head_y'))); out},
                            l.df.posture.info, l.df.input.2, SIMPLIFY=FALSE) 
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$angular.velocity <- c(0, diff(x$angle.Head_x.Trunk_x.Tail_x)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$angular.acceleration <- c(0, diff(x$angular.velocity)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Head_x_acc <- c(0, diff(x$Head_x.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Head_y_acc <- c(0, diff(x$Head_y.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Trunk_x_acc <- c(0, diff(x$Trunk_x.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Trunk_y_acc <- c(0, diff(x$Trunk_y.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Tail_x_acc <- c(0, diff(x$Tail_x.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Tail_y_acc <- c(0, diff(x$Tail_y.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Z_x_acc <- c(0, diff(x$Z_x.proj)); x})
l.df.posture.info <- lapply(l.df.posture.info, function(x) {x$Z_y_acc <- c(0, diff(x$Z_y.proj)); x})

# Bring back the behavior label and X
l.df.input.3 <- lapply(l.df.input.1, function(x) {head(x, -1)})
l.df.input <- mapply(function(df, m) {df$behavior.label <- m$behavior.label; df},
                     l.df.posture.info, l.df.input.3, SIMPLIFY=FALSE)
l.df.input <- mapply(function(df, m) {df$X <- m$X; df},
                     l.df.input, l.df.input.3, SIMPLIFY=FALSE)

# Organize order of columns
l.df.input <- lapply(l.df.input, function(x) {x[c(30, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,22,23,24,25,26,27,28,29)]})

# Create dataframes w/ the sliding window of 30 frames
n.frame.after <- 8
n.frame.before <- 7

# Make dataframes
l.df.sliding.window <- lapply(l.df.input, function(x) {out <- data.frame(frame=x$X); out})


# Count how many times animals were displaying angles below 90 or 270 between head, trunk and tail
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$n.turn.angle <- as.numeric(slide(df.input$angle.Head_x.Trunk_x.Tail_x, function(y) {sum(y < 90, y > 270)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# Adding in angular information
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.turn.angle <- as.numeric(slide(df.input$angle.Head_x.Trunk_x.Tail_x, function(y) {mean(abs(180-y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.turn.angle <- as.numeric(slide(df.input$angle.Head_x.Trunk_x.Tail_x, function(y) {mean(180-y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.ang.vel <- as.numeric(slide(df.input$angular.velocity, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.ang.vel <- as.numeric(slide(df.input$angular.velocity, function(y) {mean(y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

# Add in distance between point information
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.head.trunk <- as.numeric(slide(df.input$dist.Head_x.Trunk_x, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.head.tail <- as.numeric(slide(df.input$dist.Head_x.Tail_x, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.trunk.tail <- as.numeric(slide(df.input$dist.Trunk_x.Tail_x, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$dist.trunk.Z <- as.numeric(slide(df.input$dist.Trunk_x.Z_x, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Projections - tail, trunk, and head relative to fish heading
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.vel.x <- as.numeric(slide(df.input$Tail_x.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.vel.x <- as.numeric(slide(df.input$Tail_x.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.vel.y <- as.numeric(slide(df.input$Tail_y.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.vel.y <- as.numeric(slide(df.input$Tail_y.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.vel.x <- as.numeric(slide(df.input$Trunk_x.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.vel.x <- as.numeric(slide(df.input$Trunk_x.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.vel.y <- as.numeric(slide(df.input$Trunk_y.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.vel.y <- as.numeric(slide(df.input$Trunk_y.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.vel.x <- as.numeric(slide(df.input$Head_x.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.vel.x <- as.numeric(slide(df.input$Head_x.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.vel.y <- as.numeric(slide(df.input$Head_y.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.vel.y <- as.numeric(slide(df.input$Head_y.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.Z.vel.x <- as.numeric(slide(df.input$Z_x.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.Z.vel.x <- as.numeric(slide(df.input$Z_x.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.Z.vel.y <- as.numeric(slide(df.input$Z_y.proj, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.Z.vel.y <- as.numeric(slide(df.input$Z_y.proj, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

#Acceleration for the angles, head, trunk,tail and Z
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.acc.x <- as.numeric(slide(df.input$Tail_x_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.acc.x <- as.numeric(slide(df.input$Tail_x_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.tail.acc.y <- as.numeric(slide(df.input$Tail_y_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.tail.acc.y <- as.numeric(slide(df.input$Tail_y_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.acc.x <- as.numeric(slide(df.input$Trunk_x_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.acc.x <- as.numeric(slide(df.input$Trunk_x_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.trunk.acc.y <- as.numeric(slide(df.input$Trunk_y_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.trunk.acc.y <- as.numeric(slide(df.input$Trunk_y_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.acc.x <- as.numeric(slide(df.input$Head_x_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.acc.x <- as.numeric(slide(df.input$Head_x_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.head.acc.y <- as.numeric(slide(df.input$Head_y_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.head.acc.y <- as.numeric(slide(df.input$Head_y_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.z.acc.x <- as.numeric(slide(df.input$Z_x_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.z.acc.x <- as.numeric(slide(df.input$Z_x_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.z.acc.y <- as.numeric(slide(df.input$Z_y_acc, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.z.acc.y <- as.numeric(slide(df.input$Z_y_acc, mean, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$abs.ang.acc <- as.numeric(slide(df.input$angular.acceleration, function(y) {mean(abs(y))}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$net.ang.acc <- as.numeric(slide(df.input$angular.acceleration, function(y) {mean(y)}, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)

#Add in mode of behavioral label
l.df.sliding.window <- mapply(function(df.slide, df.input) {df.slide$behavior.label <- as.character(slide(df.input$behavior.label, getmode, .before=n.frame.before, .after=n.frame.after)); df.slide},
                              l.df.sliding.window, l.df.input, SIMPLIFY=FALSE)


#Now put all the data together and remove the NA frames to make a dataframe for training the model
df.training.data.labels <- bind_rows(l.df.sliding.window)

#Remove NA columns (only for training)
df.training.data.labels <- na.omit(df.training.data.labels)
data.frame(table(df.training.data.labels$behavior.label))

#Save file in RDS format
saveRDS(df.training.data.labels, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Training_Data.30.06.rds')
df.training.data.labels <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Training_Data.30.06.rds')

#Separate depending on behavior label
dfNormal.Turn <- df.training.data.labels[which(df.training.data.labels$behavior.label == "NT"),]
dfStraight.Swimming <- df.training.data.labels[which(df.training.data.labels$behavior.label == "ST"),]
df.Freezing <- df.training.data.labels[which(df.training.data.labels$behavior.label == "IM"),]
df.Clonic <- df.training.data.labels[which(df.training.data.labels$behavior.label == "CL"),]
df.Hypo <- df.training.data.labels[which(df.training.data.labels$behavior.label == "HYPO"),]
df.Abnormal <- df.training.data.labels[which(df.training.data.labels$behavior.label == "HYPE"),]


#Splitting dataframes - Normal Turn and Straight Swimming 80% for training 20% testing
data_set_size1= floor(nrow(dfStraight.Swimming)*0.80)
index.ST <- sample(1:nrow(dfStraight.Swimming), size = data_set_size1)
df.ST.Training80 <- dfStraight.Swimming[index.ST,]
df.ST.Testing20 <- dfStraight.Swimming[-index.ST,]

data_set_size1= floor(nrow(dfNormal.Turn)*0.80)
index.Normal.Turn <- sample(1:nrow(dfNormal.Turn), size = data_set_size1)
df.Normal.Turn.Training80 <- dfNormal.Turn[index.Normal.Turn,]
df.Normal.Turn.Testing20 <- dfNormal.Turn[-index.Normal.Turn,]

#Splitting dataframes - Freezing 80% for training 20% testing
data_set_size2= floor(nrow(df.Freezing)*0.80)
index.Freezing <- sample(1:nrow(df.Freezing), size = data_set_size2)
df.Freezing.Training80 <- df.Freezing[index.Freezing,]
df.Freezing.Testing20 <- df.Freezing[-index.Freezing,]

#Splitting dataframes - Clonic 80% for training 20% testing
data_set_size5= floor(nrow(df.Clonic)*0.80)
index.Clonic <- sample(1:nrow(df.Clonic), size = data_set_size5)
df.Clonic.Training80 <- df.Clonic[index.Clonic,]
df.Clonic.Testing20 <- df.Clonic[-index.Clonic,]

#Splitting dataframes - Hypo 80% for training 20% testing
data_set_size5= floor(nrow(df.Hypo)*0.80)
index.Hypo <- sample(1:nrow(df.Hypo), size = data_set_size5)
df.Hypo.Training80 <- df.Hypo[index.Hypo,]
df.Hypo.Testing20 <- df.Hypo[-index.Hypo,]

#Splitting dataframes - Abnormal 80% for training 20% testing
data_set_size5= floor(nrow(df.Abnormal)*0.80)
index.Abnormal <- sample(1:nrow(df.Abnormal), size = data_set_size5)
df.Abnormal.Training80 <- df.Abnormal[index.Abnormal,]
df.Abnormal.Testing20 <- df.Abnormal[-index.Abnormal,]


#Now lets combine the files and create the rds for training the model (max 1:1)
df.Training.Final <- rbind(df.Normal.Turn.Training80, df.ST.Training80, df.Freezing.Training80, df.Hypo.Training80[sample(nrow(df.Hypo.Training80), 10000),], df.Clonic.Training80, df.Abnormal.Training80[sample(nrow(df.Abnormal.Training80), 10000),])

#Data for testing
df.Testing.Final <- rbind(df.Normal.Turn.Testing20, df.ST.Testing20, df.Freezing.Testing20, df.Abnormal.Testing20, df.Clonic.Testing20, df.Hypo.Testing20)

#Count to check amount of behaviors labelled 
data.frame(table(df.Training.Final$behavior.label))

#Save file in RDS format - using 7.5k max
saveRDS(df.Training.Final, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_training_15.rds')
saveRDS(df.Testing.Final, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_testing_15.rds')
