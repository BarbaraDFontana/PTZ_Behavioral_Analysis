#For training a Random Forest Machine learning classifier
########
library(randomForest)
library(ggplot2)
library(randomForestExplainer)


#Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

set.seed(14)

#Bring in the data
df.training.allB <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_training_15.rds')

#Make the behavior label a factor
df.training.allB$behavior.label <- as.factor(df.training.allB$behavior.label)
df.training.allB <- df.training.allB[-c(1)]  #Remove the frame column as you do not want that in the training
data.frame(table(df.training.allB$behavior.label))

#model test - trying mtry from 1 to X (depending on mtry number)
oob.values <- vector(length=10)
for(i in 1:15) {
  temp.model <- randomForest(behavior.label ~ ., data=df.training.allB, mtry=i, ntree=2000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

which(oob.values == min(oob.values)) #decide Mtry

MTRY = 12  #Choose mtry based on the last code  
rf.model <- randomForest(behavior.label ~ ., data=df.training.allB, ntree=2000, mtry=MTRY, importance=TRUE)
saveRDS(rf.model, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/rf.model.2000.12mtry_15frames.rds')




#RUN FROM HERE FOR PREDICTIONS (Testing Dataset)
rf.final <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/rf.model.2000.12mtry_15frames.rds')

#Data untouched so far, testing to see errors in the predictions
testing_data <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_testing_15.rds')
result <- data.frame(testing_data$behavior.label, predict(rf.final, testing_data[,2:44], type = 'response'))

#Create a table then a graph for analyzing the Random Forest prediction errors - Better Graphic for Representation of the Confusion Matrix
tab <- table(result[,1], result[,2])
tab <- tab / rowSums(tab)
tab <- as.data.frame(tab, stringsAsFactors = TRUE)
tab$Var2 <- factor(tab$Var2, rev(levels(tab$Var2)))
ggplot(tab, aes(Var1, Var2)) +
  geom_tile(aes(fill = Freq * 100), color = "black", size = 0.5) +
  geom_text(aes(label = paste0(round(Freq * 100, 2), "%")), size = 6) +
  scale_fill_gradient(name = "Percentage", limits = c(0, 100),
                      breaks = c(0, 25, 50, 75, 100),
                      labels = c("0", "25", "50", "75", "100"),
                      low = "white", high = "#4059ad") +
  labs(x = NULL, y = NULL, title = "Confusion matrix of predicted behaviors") +
  scale_x_discrete(labels = c("Burst.Swimming" = "Burst Swimming", 
                              "Erratic.Movement" = "Erratic Movement", 
                              "Normal.Turn" = "Normal Turns", 
                              "Straight.Swimming" = "Straight Swimming")) +
  scale_y_discrete(labels = c("Burst.Swimming" = "Burst Swimming", 
                              "Erratic.Movement" = "Erratic Movement", 
                              "Normal.Turn" = "Normal Turns", 
                              "Straight.Swimming" = "Straight Swimming")) +
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
ggsave(filename = "D:/Script/Matrix.pdf", plot = last_plot(), width = 35, height = 25, units = "cm")

#See the Variable Importance
varImpPlot(rf.final, bg='#3690C0', n.var=min(10), main='Variable Importance')




