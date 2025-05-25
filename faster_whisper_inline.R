library(reticulate)
library(dplyr)

# ---- CONFIGURATION ----
use_python("~/whisper-arm64-env/bin/python", required = TRUE)

# ---- IMPORT PYTHON MODULES ----

fasterwhisper <- import_from_path("faster_whisper", path = ".", convert = TRUE)

# Define a wrapper function to transcribe
transcribe_audio <- function(audio_path, model_size = "base", device = "cpu", compute_type = "int8") {
  # Load the Whisper model
  WhisperModel <- fasterwhisper$WhisperModel
  model <- WhisperModel(model_size, device = device, compute_type = compute_type)
  
  # Transcribe and extract segments
  result <- model$transcribe(audio_path)
  segments <- reticulate::iterate(result[[1]])
  
  # Convert segments to an R data frame
  df <- tibble::tibble(
    start = sapply(segments, function(s) s$start),
    end   = sapply(segments, function(s) s$end),
    text  = sapply(segments, function(s) s$text)
  )
  
  return(df)
}

# ---- RUN ON A FILE ----
start_time <- Sys.time()
audio_file <- "vids/clips/part_011.mov"  # Replace with your actual file path
transcript <- transcribe_audio(audio_file)
Sys.time() - start_time

# ---- OPTIONAL: FILTER BY KEYWORDS ----
keywords <- c("throw", "ready", "tip", "point", "off", "angle")
tagged <- transcript %>%
  filter(grepl(paste(keywords, collapse = "|"), text, ignore.case = TRUE))

# Preview
print(tagged)
