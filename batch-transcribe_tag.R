# Required libraries
library(parallel)
library(dplyr)
library(reticulate)

# -------------------------------
# CONFIGURATION
# -------------------------------
audio_file <- "auds/Camera1.m4a"   # audio file
chunk_length <- 60                  # 1 minutes in seconds
output_dir <- "auds/clips2"
model_size <- "base"
python_path <- "~/whisper-arm64-env/bin/python"  # update as needed


# -------------------------------
# SET UP PYTHON ENV
# -------------------------------
use_python(path.expand(python_path), required = TRUE)
fasterwhisper <- import("faster_whisper", convert = TRUE)

# -------------------------------
# FUNCTION: Transcribe using faster-whisper
# -------------------------------
transcribe_clip <- function(clip_path, model_size = "base", device = "cpu", compute_type = "int8") {
  WhisperModel <- fasterwhisper$WhisperModel
  model <- WhisperModel(model_size, device = device, compute_type = compute_type)
  result <- model$transcribe(clip_path)
  segments <- reticulate::iterate(result[[1]])
  
  df <- tibble(
    file = basename(clip_path),
    start = sapply(segments, function(s) s$start),
    end   = sapply(segments, function(s) s$end),
    text  = sapply(segments, function(s) s$text)
  )
  return(df)
}

# -------------------------------
# STEP 1: Split audio using ffmpeg
# -------------------------------
cat("Splitting audio into chunks...\n")
split_cmd <- sprintf(
  "ffmpeg -i %s -f segment -segment_time %d -c copy %s/part_%%03d.m4a",
  shQuote(audio_file), chunk_length, "auds/clips2"
)
system(split_cmd)

# -------------------------------
# STEP 2: Transcribe each chunk
# -------------------------------
chunk_files <- list.files("auds/clips2", pattern = "part_\\d+\\.m4a$", full.names = TRUE)

start_time <- Sys.time()
cat("Transcribing clips...\n")
transcripts <- lapply(chunk_files, transcribe_clip)
Sys.time() - start_time

# -------------------------------
# STEP 3: Combine all transcripts
# -------------------------------
all_transcripts <- bind_rows(transcripts)

# Add global start times (in case we want to clip across full video)
all_transcripts <- all_transcripts %>%
  mutate(
    clip_index = as.integer(gsub("[^0-9]", "", tools::file_path_sans_ext(file))),
    global_start = start + clip_index * chunk_length,
    global_end   = end + clip_index * chunk_length
  )

# -------------------------------
# STEP 4: Add tags from volleys, game starts, and game ends
# -------------------------------
# Assume 'all_transcripts' already exists and has global_start, global_end, text columns
# Normalize text for easier matching
all_transcripts <- all_transcripts %>%
  mutate(text_lower = tolower(text))

# Identify volley start points
volleys <- all_transcripts %>%
  filter(
    grepl("ready", text_lower) |
      grepl("(two|2).*one.*throw", text_lower) |
      grepl("(two|2).*1.*throw", text_lower) |
      grepl("1.*throw", text_lower)
  ) %>%
  arrange(global_start) %>%
  mutate(tag = "volley_start")

# Tag "game start" and "game end" indicators
starts <- all_transcripts %>%
  filter(grepl("\\bgame\\b", text_lower)) %>%
  mutate(tag = "game_start")

ends <- all_transcripts %>%
  filter(grepl("\\bcut\\b", text_lower)) %>%
  mutate(tag = "game_end")

# Combine all tagged events
tags <- bind_rows(volleys, starts, ends) %>%
  arrange(global_start)

# Create volley clip ranges: start at volley_start, end 13 sec before the next volley_start
volley_clips <- tags %>%
  filter(tag == "volley_start") %>%
  mutate(
    clip_start = global_start,
    clip_end = lead(global_start) - 11  # end 11 sec before next volley
  ) %>%
  filter(!is.na(clip_end) & clip_end > clip_start)

# -------------------------------
# Step 5: Cut clips with ffmpeg
# -------------------------------

video_file <- "vids/Camera.mp4" # full resolution vid file
clip_folder <- "vids/volley_clips2"
if (!dir.exists(clip_folder)) dir.create(clip_folder)

cat("\nCutting volley clips...\n")
for (i in seq_len(nrow(volley_clips))) {
  ss <- volley_clips$clip_start[i]
  dur <- volley_clips$clip_end[i] - volley_clips$clip_start[i]
  out_file <- file.path(clip_folder, sprintf("volley_clip_%03d.mp4", i))
  cmd <- sprintf("ffmpeg -y -ss %.2f -i %s -t %.2f -c copy %s",
                 ss, shQuote(video_file), dur, shQuote(out_file))
  system(cmd)
}

cat("\n✅ Done: Volley clips saved to:", clip_folder, "\n")
