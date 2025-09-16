library(readxl)
library(tidyr)
library(dplyr)
library(ggplot2)
library(gridExtra)


### PLOTTING # OF SLEEPY/AWAKE SUBJECTS

# Load in data
inet_data = read.csv("C:/Users/tempu/Downloads/research/labs/gratton/arousal proj/inetworks-sleep-data.csv")

# Turn FD percent retained to numeric
inet_data$FD.hold <- as.numeric(inet_data$FD.hold)

# # Filter out eyes_closed or not_rs
# inet_data_filtered <- inet_data[!(inet_data$category %in% c("eyes_closed", "not_rs")), ]
# life_data_filtered <- life_data[!(life_data$category %in% c("eyes_closed", "not_rs")), ]

#Compute the total number of "good" minutes per run
INET_TR = 1.1
INET_FRAMES = 450

INET_MIN_PER_RUN = (INET_TR * INET_FRAMES) / 60 # Find the real time by frames x TR.

inet_data$time = inet_data$FD.hold * INET_MIN_PER_RUN # Multiply total minutes by proportion of good frames.

# Discard any subjects that, even including all sleepy states, do not meet the minutes threshold
MIN_MINUTES = 20

# Compute total number of minutes by subject for inet_data
inet_total_time <- inet_data %>%
  group_by(subject) %>%
  summarise(total_time = sum(get("time"), na.rm = TRUE)) 

# Filter out subjects who do not have FDCalc and therefore have no time.
inet_usable_subjects <- filter(inet_total_time, total_time > 0)

# Define function to calculate number of sleepy subjects with total time > 40 minutes, depending on what sleepy sleep score is.
count_sleepy_subjects <- function(data, sleepy_sleep_score_min, min_minutes = 20) { # Takes in INET or LS data sets.
  data_filtered <- data %>% filter(sleep.score > sleepy_sleep_score_min)
  
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

# Define function to calculate number of awake subjects with total time > 40 minutes, depending on what awake sleep score is.
count_awake_subjects <- function(data, awake_sleep_score_max, max_minutes = 20) { # Takes in INET or LS data sets.
  data_filtered <- data %>% filter(sleep.score <= awake_sleep_score_max)
  
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

# Create the vector for sleep scores.
sleep_scores <- 1:7

# Apply function for each sleep score for inet_data
inet_counts_sleepy <- sapply(sleep_scores, function(score) count_sleepy_subjects(inet_data, score)) 
inet_counts_awake <- sapply(sleep_scores, function(score) count_awake_subjects(inet_data, score)) 
# sapply() for vectors,
# function(score) to assign a variable to each element in the vector. (In this case, "score" stands for each score in the vector "sleep_scores".)
# Pass original INET dataframe into the function that counts total minutes of sleepy runs per subject.

# Create dataframes for plotting
inet_plot_sleepy_data <- data.frame(sleep_score = sleep_scores, subject_count = inet_counts_sleepy) # Plots number of sleepy subjects with total time > 40 minutes.
inet_plot_awake_data <- data.frame(sleep_score = sleep_scores, subject_count = inet_counts_awake) # Plots number of awake subjects with total time > 40 minutes.


# Plot for number of INET sleepy subjects with total time > 40 minutes
p1 <- ggplot(inet_plot_sleepy_data, aes(x = sleep_score, y = subject_count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = sleep_scores) +
  ggtitle("iNET: # of 'Sleepy' Subjects") +
  xlab("Working Min Sleep Score") +
  ylab("Number of 'Sleepy' Subjects")


# Plot for number of INET awake subjects with total time > 40 minutes
p2 <- ggplot(inet_plot_awake_data, aes(x = sleep_score, y = subject_count)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  scale_x_continuous(breaks = sleep_scores) +
  ggtitle("iNET: # of 'Awake' Subjects") +
  xlab("Working Max Sleep Score") +
  ylab("Number of 'Awake' Subjects")


# Arrange the plots in a 1x2 grid
grid.arrange(p1, p2, nrow = 1, ncol = 2)


# Counting number of subjects with scores >= 5 and <= 3
subject_count_sleepy_awake <- inet_data %>%
  group_by(subject) %>%
  summarise(
    has_sleepy = any(sleep.score >= 5, na.rm = TRUE),
    has_awake  = any(sleep.score <= 3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(has_sleepy & has_awake)

# Count how many such subjects
n_both <- nrow(subject_count_sleepy_awake)

