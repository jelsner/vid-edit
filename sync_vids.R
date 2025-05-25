library(tuneR)
library(signal)

# ---- CONFIGURATION ----
video1 <- "vids/Phone.MOV"
video2 <- "vids/Camera.mp4"
audio1 <- "cam1.wav"
audio2 <- "cam2.wav"
synced_video2 <- "camera2_synced.mov"
ffmpeg <- "ffmpeg"  # or full path to ffmpeg

# ---- STEP 1: Extract audio from both videos ----
system(sprintf("%s -i %s -vn -ac 1 -ar 16000 -acodec pcm_s16le %s", ffmpeg, shQuote(video1), audio1))
system(sprintf("%s -i %s -vn -ac 1 -ar 16000 -acodec pcm_s16le %s", ffmpeg, shQuote(video2), audio2))

# ---- STEP 2: Load WAV files and normalize ----
wav1 <- readWave(audio1)
wav2 <- readWave(audio2)

a1 <- wav1@left / 2^15
a2 <- wav2@left / 2^15

# Downsample for speed if needed
a1 <- a1[seq(1, length(a1), by = 4)]
a2 <- a2[seq(1, length(a2), by = 4)]

# ---- STEP 3: Compute cross-correlation ----
# Set time window for lag search
max_offset_sec <- 3 * 60  # 3 minutes
effective_sample_rate <- 16000 / 4  # adjust if downsample factor changes
lag_max_samples <- max_offset_sec * effective_sample_rate

# FFT-based cross-correlation
xc <- xcorr(a1, a2, max.lag = lag_max_samples)

# Compute lag at maximum correlation
lags <- seq(-lag_max_samples, lag_max_samples)
max_lag <- lags[which.max(xc)]
time_offset_sec <- max_lag / effective_sample_rate

cat(sprintf("\n📏 Estimated offset (FFT): %.2f seconds\n", time_offset_sec))

# ---- STEP 4: Sync video2 using ffmpeg ----
# If cam2 lags behind cam1, we trim cam2
if (time_offset_sec > 0) {
  system(sprintf("%s -ss %.2f -i %s -c copy %s", ffmpeg, time_offset_sec, shQuote(video2), synced_video2))
} else {
  # If cam2 is early, pad it with silence (not exact frame sync but aligns audio)
  system(sprintf("%s -itsoffset %.2f -i %s -c copy %s", ffmpeg, abs(time_offset_sec), shQuote(video2), synced_video2))
}

cat("\n✅ Synced video written to:", synced_video2, "\n")
