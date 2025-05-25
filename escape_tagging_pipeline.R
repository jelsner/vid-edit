# escape_tagging_pipeline.R

# Load required packages
library(jsonlite)
library(dplyr)
library(purrr)

# ---- USER INPUTS ----
video_file <- "vids/IMG_2360.MOV"
clip_duration <- 20             # seconds (total clip length)
pre_buffer <- 5                 # seconds before detected word
output_dir <- "clips"           # folder to save the clips

# ---- STEP 1: Transcribe using Whisper CLI ----
cat("Transcribing video...\n")
system(sprintf("/Users/jameselsner/opt/anaconda3/bin/whisper %s --output_format json --output_dir .", shQuote(video_file)))

# ---- STEP 2: Read the transcript JSON ----
json_file <- paste0(tools::file_path_sans_ext(basename(video_file)), ".json")
transcript <- fromJSON(json_file)$segments

# ---- STEP 3: Search for Keywords ----
keywords <- c("ready two one throw", "ready two one go")

tagged_events <- transcript %>%
  filter(grepl(paste0("\\b(", paste(keywords, collapse = "|"), ")\\b"), tolower(text))) %>%
  mutate(
    tag = case_when(
      grepl("ready two one throw", text, ignore.case = TRUE) ~ "Service",
      grepl("ready two one go", text, ignore.case = TRUE) ~ "Service",
      TRUE ~ "Other"
    )
  )

# ---- STEP 4: Create Clips with ffmpeg ----
if (!dir.exists(output_dir)) dir.create(output_dir)

cat("Creating clips...\n")
walk2(tagged_events$start, seq_len(nrow(tagged_events)), function(start_time, i) {
  ss <- max(start_time - pre_buffer, 0)
  output_clip <- file.path(
    output_dir,
    sprintf("clip_%02d_%s.mp4", i, gsub(" ", "_", tagged_events$tag[i]))
  )
  cmd <- sprintf(
    "ffmpeg -y -i %s -ss %.2f -t %.2f -c copy %s",
    shQuote(video_file), ss, clip_duration, shQuote(output_clip)
  )
  system(cmd)
})

# ---- DONE ----
cat("All done! Extracted", nrow(tagged_events), "clips.\n")
