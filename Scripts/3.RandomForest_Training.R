# Load necessary libraries
library(randomForest)
library(ggplot2)
library(randomForestExplainer)

# Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

set.seed(14)

# Bring in the data
df.training.allB <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_training_15.rds')

# Make the behavior label a factor
df.training.allB$behavior.label <- as.factor(df.training.allB$behavior.label)
df.training.allB <- df.training.allB[-c(1)]  # Remove the frame column as you do not want that in the training
data.frame(table(df.training.allB$behavior.label))

# Model test - trying mtry from 15 to 20
mtry_values <- 1:20
oob.values <- vector(length=length(mtry_values))

for(i in seq_along(mtry_values)) {
  mtry <- mtry_values[i]
  temp.model <- randomForest(behavior.label ~ ., data=df.training.allB, mtry=mtry, ntree=1000)
  oob.values[i] <- temp.model$err.rate[nrow(temp.model$err.rate),1]
}

# Print out-of-bag error values
print(oob.values)

MTRY = 18  #Choose mtry based on the last code  
rf.model <- randomForest(behavior.label ~ ., data=df.training.allB, ntree=1000, mtry=MTRY, importance=TRUE)
saveRDS(rf.model, 'E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/rf.model.1000.18mtry_15frames.rds')


#RUN FROM HERE FOR PREDICTIONS (Testing Dataset)
rf.final <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/rf.model.1000.18mtry_15frames.rds')

#Data untouched so far, testing to see errors in the predictions
testing_data <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/df_testing_15.rds')
result <- data.frame(testing_data$behavior.label, predict(rf.final, testing_data[,2:44], type = 'response'))

# Create a table of results
tab <- table(result[,1], result[,2])
tab <- tab / rowSums(tab)  # Convert counts to proportions
tab <- as.data.frame(tab, stringsAsFactors = TRUE)

# Define the order of the factors
order <- c("IM", "HYPE", "CL", "TO", "NT", "SW")

# Convert Var1 and Var2 to factors with the specified order
tab$Var1 <- factor(tab$Var1, levels = order)
tab$Var2 <- factor(tab$Var2, levels = rev(order))

# Plot the confusion matrix
ggplot(tab, aes(Var1, Var2)) +
  geom_tile(aes(fill = Freq * 100), color = "black", size = 0.5) +
  geom_text(aes(label = paste0(round(Freq * 100, 2), "%")), size = 6) +
  scale_fill_gradient(name = "Percentage", limits = c(0, 100),
                      breaks = c(0, 25, 50, 75, 100),
                      labels = c("0", "25", "50", "75", "100"),
                      low = "white", high = "#4059ad") +
  theme_void() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16))
ggsave(filename = "E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Matrix_mtry18.pdf", plot = last_plot(), width = 35, height = 25, units = "cm")

#See the Variable Importance
varImpPlot(rf.final, bg='#3690C0', n.var=min(10), main='Variable Importance')




