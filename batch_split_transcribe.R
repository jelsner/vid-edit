# Required libraries
library(parallel)
library(dplyr)
library(reticulate)

# -------------------------------
# CONFIGURATION
# -------------------------------
video_file <- "vids/IMG_2360.MOV"   # replace with your file path
chunk_length <- 600                  # 10 minutes in seconds
output_dir <- "vids/clips"
model_size <- "base"
python_path <- "~/whisper-arm64-env/bin/python"  # update as needed
final_output <- "vids/final_compilation.mp4"


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
# STEP 1: Split video using ffmpeg
# -------------------------------
if (!dir.exists("vids/output_dir")) dir.create("vids/output_dir")

cat("Splitting video into chunks...\n")
split_cmd <- sprintf(
  "ffmpeg -i %s -f segment -segment_time %d -c copy %s/part_%%03d.mov",
  shQuote(video_file), chunk_length, "vids/output_dir"
)
system(split_cmd)

# -------------------------------
# STEP 2: Transcribe each chunk
# -------------------------------
chunk_files <- list.files("vids/output_dir", pattern = "part_\\d+\\.mov$", full.names = TRUE)

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
# STEP 4: Tag "Ready, two, one, throw" sequences
# -------------------------------
point_starts <- all_transcripts %>%
  mutate(text = tolower(text)) %>%
  filter(
    grepl("ready", text) |
      grepl("(two|2).*one.*throw", text) |
      grepl("(two|2).*1.*throw", text) |
      grepl("1.*throw", text)
  ) %>%
  arrange(global_start)

# Define clip windows from each point start to 13 sec before the next point start
clip_windows <- point_starts %>%
  mutate(
    next_start = lead(global_start),
    clip_end = ifelse(!is.na(next_start), next_start - 13, global_end + 5),
    clip_start = global_start
  ) %>%
  filter(clip_end > clip_start)

# -------------------------------
# STEP 5: Save clip windows for video extraction
# -------------------------------
write.csv(clip_windows, "point_clip_windows.csv", row.names = FALSE)

# -------------------------------
# STEP 6: Cut clips and concatenate
# -------------------------------
clip_folder <- file.path(output_dir, "point_clips")
if (!dir.exists(clip_folder)) dir.create(clip_folder)

clip_list_file <- file.path(output_dir, "clip_list.txt")
clip_paths <- c()

cat("\nExtracting clips...\n")
for (i in seq_len(nrow(clip_windows))) {
  out_path <- file.path(clip_folder, sprintf("point_clip_%03d.mp4", i))
  ss <- clip_windows$clip_start[i]
  dur <- clip_windows$clip_end[i] - clip_windows$clip_start[i]
  cmd <- sprintf("ffmpeg -ss %.2f -i %s -t %.2f -c copy %s",
                 ss, shQuote(video_file), dur, shQuote(out_path))
  system(cmd)
  clip_paths <- c(clip_paths, normalizePath(out_path))
}

# Write all clip paths to list file
cat(sprintf("file '%s'\n", clip_paths), file = clip_list_file, sep = "", append = FALSE)

# Concatenate all point clips
cat("\nConcatenating final video...\n")
concat_cmd <- sprintf("ffmpeg -f concat -safe 0 -i %s -c copy %s",
                      shQuote(clip_list_file), shQuote(final_output))
system(concat_cmd)

cat("\n✅ Done: Final video saved as:", final_output, "\n")

