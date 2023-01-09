# Preprocessing Function
preprocess_raw <- function(file) {
  data <- jsonlite::fromJSON(file, flatten=TRUE)
  
  if (!"final_results" %in% data$screen) {
    print(paste0("Warning: Incomplete data for ", file))
    return(data.frame())
  }
  
  # Info
  info <- data[data$screen == "browser_info" & !is.na(data$screen), ]
  
  # Filter out practice trials
  if ("practice_debrief" %in% data$screen) {
    data <- data[which(data$screen == "practice_debrief"):nrow(data), ]
  }
  
  
  # Trial data
  trials <- data[data$screen == "Trial"  & !is.na(data$screen), ]
  
  # Unnest responses
  trials <- tidyr::unnest(data=trials, cols= "response")
  
  df <- data.frame(
   Subject_ID = ifelse(is.null(trials$subject_id), NA, paste0('sub-', trials$subject_id)),
    Participant = trials$participant_id,
    Date = ifelse(is.null(info$date), NA, info$date),
    Time = ifelse(is.null(info$time), NA, info$time),
    Duration = as.numeric(data[data$screen == "final_results" & !is.na(data$screen), "time_elapsed"]) / 1000 / 60,
    Screen_Resolution = paste0(trials$screen_width, "x", trials$screen_height),
    Screen_Size = (as.numeric(trials$screen_width) / 1000) * (as.numeric(trials$screen_height) / 1000),
    Screen_Refresh = trials$vsync_rate,
    Browser = trials$browser,
    Browser_Version = trials$browser_version,
    Device = ifelse(trials$mobile == TRUE, "Mobile", "Desktop"),
    Device_OS = trials$os,
    Illusion_Type = trials$type,
    Block = trials$block_number,
    Block_Order = as.numeric(trials$block_number),
    Trial = as.numeric(trials$trial_number),
    Stimulus = gsub(".png", "", gsub("stimuli/", "", trials$stimulus)),
    Illusion_Strength = as.numeric(trials$illusion_strength),
   # Illusion_Effect = ifelse(sign(as.numeric(trials$illusion_strength)) == -1, "Congruent", ifelse(sign(as.numeric(trials$illusion_strength)) == 0, "Null", "Incongruent")),
    Illusion_Direction = ifelse(sign(as.numeric(trials$illusion_strength)) == -1, "Congruent", "Incongruent"),
    Illusion_Side = as.factor(sign(as.numeric(trials$illusion_difference))),
    Illusion_Difference = abs(as.numeric(trials$illusion_difference)),
    Illusion_Difference_Category = ifelse(abs(as.numeric(trials$illusion_difference)) > mean(abs(as.numeric(trials$illusion_difference))), 'Hard', 'Easy'),
    Illusion_Strength_Category = ifelse(abs(as.numeric(trials$illusion_strength)) > mean(abs(as.numeric(trials$illusion_strength))), 'Strong', 'Mild'),
    Answer = trials$response,
    Error = as.integer(!as.logical(trials$correct)),
    ISI = as.numeric(data[data$screen == "fixation" & !is.na(data$screen), "trial_duration"]),
    RT = as.numeric(trials$rt)
  )
  
  # Format names
  df$Illusion_Type <- ifelse(df$Illusion_Type == "MullerLyer", "Müller-Lyer", df$Illusion_Type)
  df$Illusion_Type <- ifelse(df$Illusion_Type == "VerticalHorizontal", "Vertical-Horizontal", df$Illusion_Type)
  
  df

}

path<- "C:/Users/anshu/Dropbox/Studies/Data_IllusionGameEEG/Pilot/beh/"

participants <- list.files(path)

df <- data.frame()
for (ppt in participants) {
  if (ppt %in% c('sub-114', 'sub-117', 'sub-120')){
    print(paste0('WARNING: NO FILE FOUND for ', ppt))
  }
  else{
  raw<- list.files(paste0(path, ppt))
  raw<-raw[grep(".json", raw, fixed=T)]
  file_path = paste0(path, paste0(ppt, "/"))
  #print(paste0(file_path,raw))
  df <- rbind(df, preprocess_raw(file = paste0(file_path, raw)))
  }
}

write.csv(df, "C:/Users/anshu/Documents/GitHub/IllusionGameEEG/data/data_beh.csv", row.names = FALSE)





