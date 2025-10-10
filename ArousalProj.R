library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# how many subjects - plots
# how many including different working sleep scores
# any subjects with wide range of runs that will have enough data 
# how many minutes ana's data
# match demographic data



###################### TASK 1: Make the data frame for FD Nums ###########################
# grab a list of all subjects
file_list <- read.csv("/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/file_list.csv")
sleep_scores <- read.csv("/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/inetworks-sleep-data.csv")
# 
# # ---- Function to extract metadata and FD frames ----
# extract_ffd_info <- function(fpath) {
#   fname <- basename(fpath)
#   parts <- strsplit(fname, "_")[[1]]
#   
#   # Safely extract parts
#   subject <- sub("sub-", "", parts[grep("^sub-", parts)])
#   session <- sub("ses-", "", parts[grep("^ses-", parts)])
#   task    <- sub("task-", "", parts[grep("^task-", parts)])
#   
#   run <- if (any(grepl("^run-", parts))) {
#     sub("run-", "", parts[grep("^run-", parts)])
#   } else {
#     NA
#   }
#   
#   # ---- Read frame numbers, remove header and invalid lines ----
#   lines <- readLines(fpath, warn = FALSE)
#   lines <- trimws(lines)
#   lines <- lines[lines != ""]                   # remove blank lines
#   lines <- lines[grepl("^[0-9eE.+-]+$", lines)] # keep only numeric-like lines
#   
#   if (length(lines) == 0) {
#     warning(paste("No numeric data found in file:", fname))
#     FD_frames <- NA
#   } else {
#     FD_frames <- paste(lines, collapse = ",")
#   }
  
#   # Return one-row dataframe
#   data.frame(
#     subject = ifelse(length(subject) > 0, subject, NA),
#     session = ifelse(length(session) > 0, session, NA),
#     run     = ifelse(length(run) > 0, run, NA),
#     task    = ifelse(length(task) > 0, task, NA),
#     file    = fname,
#     path    = fpath,
#     FD_frames = FD_frames,
#     stringsAsFactors = FALSE
#   )
# }
# 
# # ---- Apply to all files ----
# ffd_files <- do.call(rbind, lapply(file_list, extract_ffd_info))
# 
# # ---- Keep only rest task ----
# ffd_files <- ffd_files %>%
#   filter(task == "rest")
# 
# # ---- Expand FD_frames into one row per run ----
# ffd_files <- ffd_files %>%
#   mutate(FD_frames = strsplit(FD_frames, ",")) %>%
#   unnest(FD_frames) %>%
#   mutate(
#     FD_frames = trimws(FD_frames),
#     FD = suppressWarnings(as.numeric(FD_frames))
#   ) %>%
#   filter(!is.na(FD)) %>% # drop rows that couldn't convert to numeric
#   group_by(subject, session, task) %>%
#   mutate(run = row_number()) %>%
#   ungroup() %>%
#   select(-FD_frames) # remove redundant column

# 
# # Merge sleep.score from sleep_scores into file_list by subject, session, task, run
# inet_data <- file_list %>%
#   left_join(
#     sleep_scores %>%
#       select(subject, session, task, run, sleep.score),
#     by = c("subject", "session", "task", "run")
#   )

### PLOTTING # OF SLEEPY SUBJECTS

# Load in data
# inet_data_duplicate = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/Arousal Project Gratton Lab/inetworks-sleep-data.csv")
# inet_data_duplicate = read.csv("/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/inetworks-sleep-data.csv")  # alt path

inet_data <- unique(inet_data)
# sleep_scores <- unique(sleep_scores) #workspace var

# pm_data = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/Arousal Project Gratton Lab/PM-sleep-data.csv")
pm_data = read.csv("/Users/grattonlab/Desktop/Praise_Learning/Arousal-Project/PM-sleep-data.csv") # alt path
# life_data = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/arousal proj/lifespan-sleep-data.csv")

inet_data$FD <- as.numeric(inet_data$FD)
# sleep_scores$FD.hold <- as.numeric(sleep_scores$FD.hold)
pm_data$FD.hold <- as.numeric(pm_data$FD.hold)
# life_data$FD.hold <- as.numeric(life_data$FD.hold)

# Get subjects without sleep_score.
inet_data_Na_sleep_subs <- inet_data %>%
  group_by(subject) %>%
  summarise(
    n_Na = sum(is.na(sleep.score))
  ) %>%
  ungroup()

# # Filter out eyes_closed or not_rs
# inet_data_filtered <- inet_data[!(inet_data$category %in% c("eyes_closed", "not_rs")), ]
# life_data_filtered <- life_data[!(life_data$category %in% c("eyes_closed", "not_rs")), ]


INET_TR = 1.1
INET_FRAMES = 450
# LS_TR = 1.1
# LS_FRAMES = 270 

INET_MIN_PER_RUN = (INET_TR * INET_FRAMES) / 60 # Find the real time by frames x TR.
# LS_MIN_PER_RUN = (LS_TR * LS_FRAMES) / 60

# inet_data$time = inet_data$FD.hold * INET_MIN_PER_RUN # Multiply total minutes by proportion of good frames.
inet_data$time = (inet_data$FD * INET_TR) / 60
# life_data$time = life_data$FD.hold * LS_MIN_PER_RUN

MIN_MINUTES = 20

#Find the total time each subject has without grouping by sleepiness.
# inet_total_time <- inet_data %>%
#   group_by(subject) %>%
#   summarise(total_time = sum(get("time"), na.rm = TRUE)) 
# 
# life_total_time <- life_data %>%
#   group_by(subject) %>%
#   summarise(total_time = sum(get("time"), na.rm = TRUE)) 



# Filter out subjects who do not have FDCalc and therefore have no time.
# inet_usable_subjects <- filter(inet_total_time, total_time > 0)
# life_usable_subjects <- filter(life_total_time, total_time > 0)

# Same thing but doesn't group by subject and keeps dataframe structure.
inet_usable_subjects <- inet_data %>%
  group_by(subject) %>%
  filter(sum(time, na.rm = TRUE) >= 20) %>%
  ungroup()

# life_usable_subjects <- life_data %>%
#   group_by(subject) %>%
#   filter(sum(time, na.rm = TRUE) >= 20) %>%
#   ungroup()




# Define function to calculate number of sleepy subjects with total time > 20 minutes, depending on what sleepy sleep score is.
count_sleepy_subjects <- function(data, sleepy_sleep_score_min, min_minutes = 20) { # Takes in INET or LS data sets.
  data_filtered <- data %>% filter(sleep.score >= sleepy_sleep_score_min)

  total_time_per_subject <- data_filtered %>% # Calculates only for people whose sleep scores are considered "sleepy"
    group_by(subject) %>%
    summarise(total_time = sum(get("time"), na.rm = TRUE)) # Calculates total time for the "sleepy" runs of one subject
  
  # Count subjects with total time > min_minutes
  count <- total_time_per_subject %>%
    filter(total_time > min_minutes) %>%
    summarise(n = n_distinct(subject)) %>%
    pull(n)
  
  return(count)
}

count_awake_subjects <- function(data, awake_sleep_score_max, max_minutes = 20) { # Takes in INET or LS data sets.
  data_filtered <- data %>% filter(sleep.score < awake_sleep_score_max)
  
  total_time_per_subject <- data_filtered %>% # Calculates only for people whose sleep scores are considered "sleepy"
    group_by(subject) %>%
    summarise(total_time = sum(get("time"), na.rm = TRUE)) # Calculates total time for the "sleepy" runs of one subject
  
  # Count subjects with total time > min_minutes
  count <- total_time_per_subject %>%
    filter(total_time > max_minutes) %>%
    summarise(n = n_distinct(subject)) %>%
    pull(n)
  
  return(count)
}

# Count sleepy minutes
count_sleepy_subjects <- function(data, sleepy_sleep_score_min, min_minutes = 20) { # Takes in INET or LS data sets.
  data_filtered <- data %>% filter(sleep.score >= sleepy_sleep_score_min)
  
  total_time_per_subject <- data_filtered %>% # Calculates only for people whose sleep scores are considered "sleepy"
    group_by(subject) %>%
    summarise(total_time = sum(get("time"), na.rm = TRUE)) # Calculates total time for the "sleepy" runs of one subject
  
  # Count subjects with total time > min_minutes
  count <- total_time_per_subject %>%
    filter(total_time > min_minutes) %>%
    summarise(n = n_distinct(subject)) %>%
    pull(n)
  
  return(count)
}


# Create the vector for sleep scores.
sleep_scores <- 1:7
sleep_scores_pm <- 1:5

inet_counts_sleepy <- sapply(sleep_scores, function(score) count_sleepy_subjects(inet_data, score)) 
# life_counts_sleepy <- sapply(sleep_scores, function(score) count_sleepy_subjects(life_data, score))
inet_counts_awake <- sapply(sleep_scores, function(score) count_awake_subjects(inet_data, score)) 
# life_counts_awake <- sapply(sleep_scores, function(score) count_awake_subjects(life_data, score))
pm_sessions_sleepy <- pm_data[pm_data$sleep.score >= 4,]
pm_sessions_awake <- pm_data[pm_data$sleep.score <= 2,]

inet_plot_sleepy_data <- data.frame(sleep_score = sleep_scores, subject_count = inet_counts_sleepy) # Plots number of sleepy subjects with total time > 40 minutes.
# life_plot_sleepy_data <- data.frame(sleep_score = sleep_scores, subject_count = life_counts_sleepy)

inet_plot_awake_data <- data.frame(sleep_score = sleep_scores, subject_count = inet_counts_awake) # Plots number of awake subjects with total time > 40 minutes.
# life_plot_awake_data <- data.frame(sleep_score = sleep_scores, subject_count = life_counts_awake)





# Plot for number of INET sleepy subjects with total time > 40 minutes
p1 <- ggplot(inet_plot_sleepy_data, aes(x = sleep_score, y = subject_count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = sleep_scores) +
  ggtitle("iNET: # of 'Sleepy' Subjects") +
  xlab("Working Min Sleep Score") +
  ylab("Number of 'Sleepy' Subjects")

# # Plot for number of LS sleepy subjects with total time > 40 minutes
# p2 <- ggplot(life_plot_sleepy_data, aes(x = sleep_score, y = subject_count)) +
#   geom_line(color = "red") +
#   geom_point(color = "red") +
#   scale_x_continuous(breaks = sleep_scores) +
#   ggtitle("LS: # of 'Sleepy' Subjects") +
#   xlab("Working Min Sleep Score") +
#   ylab("Number of 'Sleepy' Subjects")

p3 <- ggplot(inet_plot_awake_data, aes(x = sleep_score, y = subject_count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = sleep_scores) +
  ggtitle("iNET: # of 'Awake' Subjects") +
  xlab("Working Max Sleep Score") +
  ylab("Number of 'Awake' Subjects")
# 
# p4 <- ggplot(life_plot_awake_data, aes(x = sleep_score, y = subject_count)) +
#   geom_line(color = "red") +
#   geom_point(color = "red") +
#   scale_x_continuous(breaks = sleep_scores) +
#   ggtitle("LS: # of 'Awake' Subjects") +
#   xlab("Working Max Sleep Score") +
#   ylab("Number of 'Awake' Subjects")



# Arrange the INET plots in a 1x2 grid
grid.arrange(p1, p3, nrow = 1, ncol = 2)
grid.arrange(p2, p4, nrow = 1, ncol = 2)





count_both_subjects <- function(data, sleepy_min = 6, awake_max = 3, min_minutes = 40) {
  # Sleepy totals
  sleepy_totals <- data %>%
    filter(sleep.score >= sleepy_min) %>%
    group_by(subject) %>%
    summarise(sleepy_time = sum(time, na.rm = TRUE), .groups = "drop") %>%
    filter(sleepy_time >= min_minutes)
  
  # Awake totals
  awake_totals <- data %>%
    filter(sleep.score < awake_max) %>%
    group_by(subject) %>%
    summarise(awake_time = sum(time, na.rm = TRUE), .groups = "drop") %>%
    filter(awake_time >= min_minutes)
  
  # Merge sleepy and awake
  both <- inner_join(sleepy_totals, awake_totals, by = "subject")
  
  # Return both subject count + times
  return(list(
    n_both_subjects = nrow(both),
    sleepy_awake_times = both   # dataframe with subject, sleepy_time, awake_time
  ))
}

# count_both_subjects(life_data)
results <- count_both_subjects(inet_data)
minutes_for_sleepy_awake <- results$sleepy_awake_times
results <- count_both_subjects(pm_data, sleepy_min = 5, awake_max = 2)
minutes_for_sleepy_awake <- rbind(minutes_for_sleepy_awake, results$sleepy_awake_times)

