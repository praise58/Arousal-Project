library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)

# how many subjects - plots
# how many including different working sleep scores
# any subjects with wide range of runs that will have enough data 
# how many minutes ana's data
# match demographic data



###################### TASK 1: Data frame for FD Nums ###########################
# grab a list of all excel files
file_list <- list.files(path = "/Volumes/illinois-las-psych-gratton/iNetworks/Nifti", pattern = "_desc-framenums_fFD\\.txt$", recursive = TRUE, full.names = TRUE)

# initialize blank df
# create an empty data frame
ffd_files <- data.frame(
  subject = character(),
  session = character(),
  run     = character(),
  task    = character(),
  file    = character(),
  frames  = character(),
  stringsAsFactors = FALSE
)

# iterate through files
for (i in seq_along(ffd_files)) {
  
  fname <- basename(ffd_files[i])          # just the file name
  parts <- strsplit(fname, "_")[[1]]       # split at underscores
  
  # extract elements
  subject <- sub("sub-", "", parts[1])
  session <- sub("ses-", "", parts[2])
  task    <- sub("task-", "", parts[3])
  
  # optional: if run number exists in name
  run <- ifelse(any(grepl("^run-", parts)), 
                sub("run-", "", parts[grepl("^run-", parts)]), 
                NA)
  
  # read in frame numbers (assume text file with numbers inside)
  frames <- paste(readLines(ffd_files[i]), collapse = ",")
  
  # add to dataframe
  ffd_df <- rbind(ffd_df, data.frame(
    subject = subject,
    session = session,
    run     = run,
    task    = task,
    file    = fname,
    frames  = frames,
    stringsAsFactors = FALSE
  ))
}




### PLOTTING # OF SLEEPY SUBJECTS

# Load in data
inet_data_duplicate = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/Arousal Project Gratton Lab/inetworks-sleep-data.csv")
inet_data <- unique(inet_data)
pm_data = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/Arousal Project Gratton Lab/PM-sleep-data.csv")
# life_data = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/arousal proj/lifespan-sleep-data.csv")

inet_data$FD.hold <- as.numeric(inet_data$FD.hold)
pm_data$FD.hold <- as.numeric(pm_data$FD.hold)
# life_data$FD.hold <- as.numeric(life_data$FD.hold)



# # Filter out eyes_closed or not_rs
# inet_data_filtered <- inet_data[!(inet_data$category %in% c("eyes_closed", "not_rs")), ]
# life_data_filtered <- life_data[!(life_data$category %in% c("eyes_closed", "not_rs")), ]


INET_TR = 1.1
INET_FRAMES = 450
# LS_TR = 1.1
# LS_FRAMES = 270 

INET_MIN_PER_RUN = (INET_TR * INET_FRAMES) / 60 # Find the real time by frames x TR.
# LS_MIN_PER_RUN = (LS_TR * LS_FRAMES) / 60

inet_data$time = inet_data$FD.hold * INET_MIN_PER_RUN # Multiply total minutes by proportion of good frames.
# life_data$time = life_data$FD.hold * LS_MIN_PER_RUN

MIN_MINUTES = 20

inet_total_time <- inet_data %>%
  group_by(subject) %>%
  summarise(total_time = sum(get("time"), na.rm = TRUE)) 
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

