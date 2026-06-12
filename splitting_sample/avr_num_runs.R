## find average number of resting state runs per participant.

library(readxl)

files <- list.files("C:\\Users\\tempu\\Downloads\\research\\labs\\gratton\\Arousal-Project\\splitting_sample\\QC_Completed_Templates", pattern = "\\.xlsx$", full.names = TRUE)

extract_rest_runs <- function(file) {
  tryCatch({
    df <- read_excel(file, col_names = FALSE)
    trigger_row <- which(df[[3]] == "# Rest runs")
    
    if (length(trigger_row) == 0) {
      message("'# Rest runs' not found in: ", basename(file))
      return(rep(NA, 4))
    }
    
    as.numeric(df[[3]][(trigger_row + 1):(trigger_row + 4)])
  }, error = function(e) {
    message("Failed to read: ", basename(file))
    rep(NA, 4)
  })
}

all_rest_runs <- lapply(files, function(f) suppressMessages(extract_rest_runs(f))) |> unlist()

all_rest_runs <- all_rest_runs[!is.na(all_rest_runs)]
  
mean(all_rest_runs) * 4
