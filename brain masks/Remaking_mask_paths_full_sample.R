# Required package
if (!requireNamespace("dplyr", quietly = TRUE)) install.packages("dplyr")
library(dplyr)

all_sample = rbind(inet_awake_runs, inet_sleepy_runs)
all_sample = unique(all_sample)

# Helper: ensure subject looks like INET### or keep '1' if that special case is needed
canon_subject <- function(subj) {
  s <- toupper(as.character(subj))
  s <- gsub("[^A-Z0-9]", "", s)            # remove punctuation
  if (s == "") return(NA_character_)
  # if purely numeric and equals "1", keep "1"
  if (grepl("^[0-9]+$", s)) {
    if (s == "1") return("1")
    return(sprintf("INET%03d", as.integer(s)))
  }
  # if already starts with INET, zero-pad digits after it
  if (grepl("^INET[0-9]+$", s)) {
    num <- sub("^INET", "", s)
    return(sprintf("INET%03d", as.integer(num)))
  }
  # fallback: return cleaned string
  s
}

# Main transformation for INET masks
all_sample_masks <- all_sample %>%
  mutate(
    subj_can = vapply(subject, canon_subject, FUN.VALUE = character(1)),
    ses_str  = as.character(session),
    run_num  = as.integer(run),
    run_pad  = sprintf("%02d", run_num),
    # folder path (remove trailing slash if any, but template has ending slash)
    path = paste0(
      "/Volumes/illinois-las-psych-gratton/iNetworks/Nifti/derivatives/",
      "preproc_fmriprep-24.1.1/sub-", subj_can, "/ses-", ses_str, "/func/"
    ),
    name = paste0(
      "sub-", subj_can,
      "_ses-", ses_str,
      "_task-rest_run-", run_pad,
      "_space-MNI152NLin6Asym_res-2_desc-brain_mask.nii.gz"
    )
  ) %>%
  select(path, name)

# Same for PM masks
pm_sample_paths <- pm_sample %>%
  mutate(
    # ensure path is character (not factor)
    fullpath = as.character(path),
    # folder path (dirname) and ensure it ends with a single slash
    folder = dirname(fullpath) %>% as.character(),
    folder = ifelse(substring(folder, nchar(folder)) == "/", folder, paste0(folder, "/")),
    # filename only
    name = basename(fullpath)
  ) %>%
  # choose output columns: folder -> path, name
  transmute(path = folder, name = name)


all_sample_paths = rbind(pm_sample_paths, INET_sample_masks)
# Optionally write to CSV
# write.csv(out, "files_from_df.csv", row.names = FALSE)
