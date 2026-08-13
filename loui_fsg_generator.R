### GEÄNDERT START — av-Paket fuer MP3-Export laden ###
library(tuneR)
library(jsonlite)
library(av)
### GEÄNDERT ENDE ###
messagef <- function(...) message(sprintf(...))
set.seed(42)

### GEÄNDERT START — Output-Verzeichnis + Dateinamen auf v4/mp3 umbenannt ###
OUTPUT_DIR           <- file.path(getwd(), "stimuli_loui_fsg_v5")
### GEÄNDERT ENDE ###
MELODY_LENGTH        <- 10
N_PER_STYLE          <- 25
TONE_DURATION        <- 0.4
SAMPLE_RATE          <- 44100
F0                   <- 220
MAX_SIMILARITY       <- 0.70
MAX_ATTEMPTS_SHUFFLE <- 200

# [... bp_frequency(), chord_A/B/C, chord_sequence, next_idx(), get_next_step(),
#      generate_grammar_melody(), is_valid_loui_string(), make_matched_random(),
#      positional_similarity(), max_pairwise_similarity(), generate_grammar_pool(),
#      make_tone() — ALLE UNVERÄNDERT, bitte 1:1 aus deiner bestehenden Version übernehmen ]

### GEÄNDERT START — render_melody() -> render_melody_mp3() (WAV als Zwischenschritt, dann geloescht) ###
render_melody_mp3 <- function(bp_notes, mp3_path) {
  wav_path <- sub("\\.mp3$", ".wav", mp3_path)
  chunks <- lapply(bp_notes, function(n) make_tone(bp_frequency(n)))
  tuneR::writeWave(do.call(tuneR::bind, chunks), wav_path)
  av::av_audio_convert(wav_path, mp3_path, verbose = FALSE)
  file.remove(wav_path)
  invisible(mp3_path)
}
### GEÄNDERT ENDE ###

main_loui <- function() {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)
  grammar_pool <- generate_grammar_pool(N_PER_STYLE)
  random_pool  <- vector("list", N_PER_STYLE)
  shuffle_attempts <- integer(N_PER_STYLE)
  for (i in seq_along(grammar_pool)) {
    res <- make_matched_random(grammar_pool[[i]])
    random_pool[[i]] <- res$tones
    shuffle_attempts[i] <- res$attempts
  }

  item_bank <- data.frame()
  for (i in seq_along(grammar_pool)) {
    ### GEÄNDERT START — .wav -> .mp3 Dateinamen + render_melody_mp3() ###
    fn_a <- sprintf("loui_styleA_%02d.mp3", i)
    fn_b <- sprintf("loui_styleB_%02d.mp3", i)
    render_melody_mp3(grammar_pool[[i]], file.path(OUTPUT_DIR, fn_a))
    render_melody_mp3(random_pool[[i]],  file.path(OUTPUT_DIR, fn_b))
    ### GEÄNDERT ENDE ###
    item_bank <- rbind(item_bank,
                       data.frame(item_number = i, style = "A", correct = "A", file_name = fn_a,
                                  tones = paste(grammar_pool[[i]], collapse = "-"), stringsAsFactors = FALSE),
                       data.frame(item_number = i, style = "B", correct = "B", file_name = fn_b,
                                  tones = paste(random_pool[[i]], collapse = "-"), stringsAsFactors = FALSE))
  }
  item_bank$block <- 1
  item_bank$grammar_type <- "loui_fsg"

  write.csv(item_bank, file.path(OUTPUT_DIR, "loui_item_bank.csv"), row.names = FALSE)
  saveRDS(item_bank, file.path(OUTPUT_DIR, "loui_item_bank.rds"))

  meta <- list(
    grammar_type = "loui_fsg", melody_length = MELODY_LENGTH, n_per_style = N_PER_STYLE,
    scale = "Bohlen-Pierce",
    max_similarity_within_style_A = max_pairwise_similarity(grammar_pool),
    mean_shuffle_attempts_for_valid_control = mean(shuffle_attempts),
    ideal_observer_acc = 1.0
  )
  jsonlite::write_json(meta, file.path(OUTPUT_DIR, "loui_summary.json"), auto_unbox = TRUE, pretty = TRUE)

  messagef("FERTIG. %d MP3-Dateien + item_bank + summary geschrieben nach:", 2 * N_PER_STYLE)
  messagef(">>> %s <<<", normalizePath(OUTPUT_DIR))
  invisible(item_bank)
}

main_loui()
