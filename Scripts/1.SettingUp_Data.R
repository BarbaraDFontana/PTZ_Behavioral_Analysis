library(dplyr)
library(zoo)
library(readxl)

# Set directory that the current file is in as the working directory
current_path <- rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path))

# Load CSV file names and clean them
l.full.filenames <- Sys.glob("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Videos/CSV_Model1/*.csv")
l.filenames.clean <- list.files('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Videos/CSV_Model1/', pattern='*.csv')
l.filenames.clean <- sub("(.*)D.*", "\\1", substr(l.filenames.clean, 1, 12))

# Bring in the data
l.df.zfish.input <- lapply(l.full.filenames, read.csv, header=FALSE)

# Pull out and pretty up the column names
l.colnames <- lapply(l.df.zfish.input, function(x) apply(x, 2, function(y) paste(y[2], y[3], sep='_')))
l.df.zfish.input <- mapply(function(x, y) {colnames(x) <- y; x}, l.df.zfish.input, l.colnames, SIMPLIFY = FALSE)

# Change column names from 'Truck' to 'Trunk'
l.df.zfish.input <- lapply(l.df.zfish.input, function(x) {
  colnames(x) <- gsub("Truck", "Trunk", colnames(x))
  x
})

# Make data numeric, remove a few rows, remove likelihood columns, and clean up NA values
l.df.zfish.input <- lapply(l.df.zfish.input, function(x) {
  out <- as.data.frame(apply(x[5:nrow(x),], 2, as.numeric))
  out <- select(out, -contains('likelihood'))
  out <- as.data.frame(apply(out, 2, na.approx, na.rm=TRUE))
  out
})

# Add in column for labelling of behavior
l.df.zfish.behavior <- lapply(l.df.zfish.input, function(x) {
  x$behavior.label <- NA
  x
})

# Remove any frames after 18000 to keep videos at 10 min
l.df.zfish.behavior <- lapply(l.df.zfish.behavior, function(x) head(x, 18000))

# Write everything to a CSV file for labelling
mapply(function(filename, data) {
  write.csv(data, paste0('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/Correct_Format/', filename, '.csv'), row.names=FALSE)
}, l.filenames.clean, l.df.zfish.behavior)
