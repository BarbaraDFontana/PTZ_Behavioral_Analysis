#Analyzing the data for distance and individual behaviors
library(randomForest)
library(dplyr)
library(tidyr)
library(slider) #For sliding window
library(readxl)
library(janitor)

set.seed(14)

#Get 'Mode' Function
source('E:/Barbara/Postdoc_Denis(2024-2025)/Data_Analysis/NTT_analysis_functions.R')

#Set directory that the current file is in as the working directory
current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))

#Bring the random forest model
rf.final <- readRDS("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/rf.model.2000.12mtry_15frames.rds")

#Bring the final organized data
l.filenames <- Sys.glob("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/Correct_Format/*.csv")
l.filenames.clean <- list.files('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/Correct_Format/', pattern='*.csv')
#Bring ID info file
id.table <- read_excel("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/ID_PTZ.xlsx")

# Use lapply to remove the .csv extension
l.filenames.id <- lapply(l.filenames.clean, function(x) {x <- sub("\\.csv$", "", x)})

#Bring Sliding Window Data
l.sliding.window <- readRDS('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/l.df.all_PTZ_15.rds')

#Analyze Data - Predict behavior using the model
l.result <- lapply(l.sliding.window, function(x) {data.frame(predict(rf.final, x[,2:44], type = 'Response', na.roughfix=TRUE))})

#Create sliding window for results (to avoid random errors in the middle of a behavior)
col.behavior <- paste0('behavior.label')
l.result <- lapply(l.result, setNames, col.behavior)
l.result1 <- lapply(l.result, function(x) {as.data.frame(x); x})


# Adding the column posture to the behavioral data
l.posture <- Sys.glob("E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/Posture/*.csv")
l.df.posture <- lapply(l.posture, read.csv)

# Remove the first row from each data frame in l.df.posture
l.df.posture <- lapply(l.df.posture, function(x) {as.data.frame(x[-1, ])})
l.df.posture <- lapply(l.df.posture, function(x) {
  colnames(x)[1] <- "Posture"
  return(x)
})

# Extract the abs.trunk.vel.x column from l.sliding.window
abs_trunk_vel_x_list <- lapply(l.sliding.window, function(df) df$abs.trunk.vel.x)

# Combine the data frames from l.df.posture, l.result1, and the extracted column
l.df.combined <- mapply(function(posture_df, result_df, vel_x) {
  # Create a data frame from vel_x
  vel_x_df <- data.frame(abs.trunk.vel.x = vel_x)
  
  # Combine the data frames
  combined_df <- bind_cols(posture_df, result_df, vel_x_df)
  return(combined_df)
}, l.df.posture, l.result1, abs_trunk_vel_x_list, SIMPLIFY = FALSE)

# Apply condition so TO is only when posture is lost
l.df.combined <- lapply(l.df.combined, function(df) {
  df <- df %>%
    mutate(
      behavior = ifelse(Posture == 1 & as.character(behavior.label) %in% c("HYPO", "IMO"), "TO", as.character(behavior.label)),
      behavior = ifelse(abs.trunk.vel.x < 0.75 & as.character(behavior.label) == "CL", "HYPE", behavior)
    )
  return(df)
})

# Get mode in case there are some flicking on the posture tracking
l.df.combined <- lapply(l.df.combined, function(x) {x$behavior <- as.character(unlist(slide(x$behavior, getmode, .before=7, .after=7))); x})

# Count the frequency of each behavior in n or %
l.result.percentage <- lapply(l.df.combined, function(x) {
  if (length(x$behavior) == 0) {
    return("No data available")
  } else {
    return(prop.table(table(x$behavior)) * 100)
  }
})

l.result.percentage.labeled <- mapply(function(df, m) {df$ID <- m; df},
                                      l.result.percentage, l.filenames.id, SIMPLIFY=FALSE)
l.result.percentage.labeled <- lapply(l.result.percentage.labeled, function(x) {as.data.frame(x)})


#Apply the function and set the data together
df.results.labeled <- dplyr::bind_rows(lapply(l.result.percentage.labeled,function(x) as.data.frame((x),stringsAsFactors=FALSE)))
df.results.labeled[is.na(df.results.labeled)] <- 0  #change NA for 0
df.results.labeled <- dplyr::inner_join(df.results.labeled, id.table, by='ID')
df.final <- df.results.labeled

#Create a column for normal behavior (active swimming and percentage active for burst and EM)
df.final$Normal.Swimming <- df.final$NT + df.final$ST


# Function to calculate episodes and latency and then add them to final table with the percentages
library(data.table)

calculate_episodes_latency <- function(df, min_frames = 30) {
  setDT(df)  # Convert df to data.table
  
  # Check if the 'animal' column is present
  if ("animal" %in% colnames(df)) {
    # Compute run-length encoding within data.table grouped by animal and behavior
    df[, rle_id := rleid(animal, behavior)]
    
    # Calculate the lengths of each episode
    episode_lengths <- df[, .(lengths = .N, start = .I[1]), by = .(animal, behavior, rle_id)]
    
    # Filter episodes that meet the min_frames criteria
    valid_episodes <- episode_lengths[lengths >= min_frames]
    
    # Calculate latency and number of episodes for each behavior
    results <- valid_episodes[, .(
      Episodes = .N,
      Latency = min(start) / 30  # Get the earliest start time of valid episodes and convert to seconds
    ), by = .(animal, behavior)]
    
    # Aggregate results by behavior across all animals
    final_results <- results[, .(
      Episodes = sum(Episodes),
      Latency = mean(Latency)  # Calculate the average latency across all animals
    ), by = behavior]
  } else {
    # Compute run-length encoding within data.table grouped by behavior only
    df[, rle_id := rleid(behavior)]
    
    # Calculate the lengths of each episode
    episode_lengths <- df[, .(lengths = .N, start = .I[1]), by = .(behavior, rle_id)]
    
    # Filter episodes that meet the min_frames criteria
    valid_episodes <- episode_lengths[lengths >= min_frames]
    
    # Calculate latency and number of episodes for each behavior
    final_results <- valid_episodes[, .(
      Episodes = .N,
      Latency = min(start) / 30  # Get the earliest start time of valid episodes and convert to seconds
    ), by = behavior]
  }
  
  # Convert to data.frame
  result_df <- as.data.frame(final_results)
  
  return(result_df)
}

# Apply the function to all combined data frames
l.episodes.latency <- lapply(l.df.combined, calculate_episodes_latency)

# Combine results with IDs
l.episodes.latency.labeled <- mapply(function(df, id) {df$ID <- id; df}, l.episodes.latency, l.filenames.id, SIMPLIFY = FALSE)
df.episodes.latency.final <- bind_rows(l.episodes.latency.labeled)

# Pivot the data frame to wide format
df_wide <- df.episodes.latency.final %>%
  pivot_wider(names_from = behavior, values_from = c(Episodes, Latency), names_glue = "{behavior}_{.value}")

#Replace NA for 0 
df_wide <- df_wide %>%
  mutate(across(everything(), ~replace_na(.x, 0)))

# Merge with the final data frame
df.final <- df.final %>%
  left_join(df_wide, by = c("ID"))

# Create a column for normal behavior (active swimming and percentage active for burst and EM)
df.final$Normal.Swimming <- df.final$NT + df.final$ST

#Because animals that are with no latency (0 value found) do not reach the behaviors so is the max time the latency
columns_to_update <- c("HYPO_Latency", "HYPE_Latency", "CL_Latency", "TO_Latency", 'IM_Latency')
df.final[, columns_to_update] <- lapply(df.final[, columns_to_update], function(x) ifelse(x <= 1, 600, x))

# Write CSV file for video
mapply(function(x, y) {write.csv(y, paste('E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Data/RF_Results/', x, '', sep=''))}, l.filenames.clean, l.df.combined)

# Save Final Dataset
write.csv(df.final, "E:/Barbara/Postdoc_Denis(2024-2025)/PTZ_Projeto/Results_PTZ_Predictions_15frames.csv")

