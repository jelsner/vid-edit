library(tuneR)
library(signal)

# ---- CONFIGURATION ----
video1 <- "vids/Phone_Clipped.mp4"
video2 <- "vids/Camera_Clipped.mp4"
audio1 <- "auds/cam1.wav"
audio2 <- "auds/cam2.wav"
synced_video2 <- "vids/camera2_synced.mov"
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
# Use base R cross-correlation instead of FFT method
# Set time window for lag search
max_offset_sec <- .5 * 60  # 1/2 minute
samp_rate <- 16000
downsample_factor <- 4
effective_sample_rate <- samp_rate / downsample_factor
lag_max_samples <- max_offset_sec * effective_sample_rate

# Use ccf() for cross-correlation
ccf_result <- ccf(a1, a2, lag.max = lag_max_samples, plot = FALSE, na.action = na.pass)
lags_full <- ccf_result$lag
xc_full <- ccf_result$acf
time_lags_full <- lags_full / effective_sample_rate

# Extract zoomed window
plot_window <- abs(time_lags_full) < 10
plot(
  time_lags_full[plot_window],
  xc_full[plot_window],
  type = "l",
  xlab = "Lag (seconds)",
  ylab = "Cross-correlation",
  main = "Zoomed Cross-Correlation: ±10 sec"
)

# Identify best match within 10 second window
search_window <- which(abs(time_lags_full) <= 10)
max_lag <- lags_full[search_window][which.max(xc_full[search_window])]
time_offset_sec <- max_lag / effective_sample_rate
abline(v = time_offset_sec, col = "red", lty = 2)
legend("topright", legend = sprintf("Estimated: %.2f sec", time_offset_sec), col = "red", lty = 2, bty = "n")

# ---- STEP 4: Sync video2 using ffmpeg ----
# If cam2 lags behind cam1, we trim cam2
if (time_offset_sec < 0) {
  system(sprintf("%s -ss %.2f -i %s -c copy %s", ffmpeg, abs(time_offset_sec), shQuote(video2), synced_video2))
} else {
  # If cam2 is early, pad it with silence (not exact frame sync but aligns audio)
  system(sprintf("%s -itsoffset %.2f -i %s -c copy %s", ffmpeg, -time_offset_sec, shQuote(video2), synced_video2))
}

cat("\n✅ Synced video written to:", synced_video2, "\n")
