# Preprocessing Function
preprocess_raw <- function(file) {
  data <- read.csv(file)
  
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
    Experiment_Duration = as.numeric(data[data$screen == "final_results" & !is.na(data$screen), "time_elapsed"]) / 1000 / 60,
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
    File = gsub(".png", "", gsub("stimuli/", "", trials$stimulus)),
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


path <- "C:/Users/olive/Box/IllusionGameEEG/ig_beh/"

participants <- list.files(path)
participants <- participants[grepl(".csv", participants, fixed = TRUE)]

df <- data.frame()
for (ppt in participants) {
  # Skip iterations for missing/excluded files
  if (grepl("sub-117|sub-139|sub-146", ppt)) {
    print(paste0('WARNING: NO FILE FOUND for ', ppt))
    next
  } else {
    file_path <- paste0(path, ppt)
    df <- rbind(df, preprocess_raw(file = file_path))
  }
}

# Manually fix for ppts without subject id 
df$Subject_ID[df$Participant=='qh07hl'] <- 'sub-110'
df$Subject_ID[df$Participant=='ld8429'] <- 'sub-111'
df$Subject_ID[df$Participant=='wo5rw2'] <- 'sub-113'
df$Subject_ID[df$Participant =='yuqm8t'] <- 'sub-114'
#df$Subject_ID[df$Participant =='khrd26'] <- 'sub-117'
df$Subject_ID[df$Participant =='6su1qc']<- 'sub-120'
df$Subject_ID[df$Participant =='ltqvm8']<- 'sub-119'
df$Subject_ID[df$Participant =='8pgdpu']<- 'sub-121'
df$Subject_ID[df$Participant =='aqm2s1']<- 'sub-134'
df$Subject_ID[df$Participant =='00fl9o']<- 'sub-116'
df$Subject_ID[df$Participant =='1tavm8']<- 'sub-119'
df$Subject_ID[df$Participant=='khrdz6'] <- 'sub-123'


# fix event number 
df$Trial_Order = df$Trial -18
df<- df|> dplyr::mutate(Event_Number = ifelse(df$Block==1, df$Trial_Order, df$Trial))
df$Trial<- NULL
df$Trial_Order<- NULL



# Save Data

# Participant-level data: one row per participant, session/device-level variables
participant_vars <- c("Subject_ID", "Participant", "Date", "Time", "Experiment_Duration",
                      "Screen_Resolution", "Screen_Size", "Browser", "Browser_Version",
                      "Device", "Device_OS")
df_participants <- unique(df[, participant_vars])


illusion_vars <- c("Subject_ID", "Participant", "Illusion_Type", "Illusion_Difference",
                   "Illusion_Side", "Illusion_Strength", "File", "Block", "Event_Number",
                   "ISI", "RT", "Answer", "Error")
df_illusion <- df[, illusion_vars]

write.csv(df_participants, "C:/Users/olive/Documents/rawdata_beh_participants.csv", row.names = FALSE)
write.csv(df_illusion, "C:/Users/olive/Documents/rawdata_beh_illusion.csv", row.names = FALSE)