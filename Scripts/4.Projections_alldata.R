#Get libraries and functions
source('E:/Barbara/Postdoc_Denis(2024-2025)/Data_Analysis/PTZ_Model_Functions.R') 

#Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

set.seed(14)

#Putting it together for all the files
l.filenames <- Sys.glob("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/Correct_Format/*.csv")
l.df.input.1 <- lapply(l.filenames, read.csv)

#Make sure the columns are with correct name
l.columns.data <- c('X', 'Head_x', 'Head_y', 'Trunk_x', 'Trunk_y', 'Tail_x', 'Tail_y', 'Z_x', 'Z_y', 'behavior.label')
l.df.input.1 <- lapply(l.df.input.1, function(x) `colnames<-`(x, l.columns.data))

#Smooth the traces to remove the 'jiggle' from DLC from column 2 to 9
l.df.input.1 <- lapply(l.df.input.1, function(x) {as.data.frame(apply(x[2:9], 2, sgolayfilt, n=5));x})

#Convert pixel values to cm - from column 2 to 9
cm = 22.5
l.df.input.1 <- mapply(function(x, y) {x[2:9] <- x[2:9] *y;x}, l.df.input.1, cm, SIMPLIFY = FALSE)

#remove column 1.frames and 8.behavior label to run the postural analysis
l.df.input.2 <- lapply(l.df.input.1, function(x) {as.data.frame(x[-c(1,10)])})


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


#Save as RDS
saveRDS(l.df.sliding.window, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/l.df.all_PTZ_15.rds')

