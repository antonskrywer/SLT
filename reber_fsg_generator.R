# ============================================================
# SLT – Stimulus-Generator "Reber-FSG" (Version 4 — BUGFIX + MP3)
# ============================================================
# GEÄNDERT ggü. v3:
#  1) BUGFIX: generate_grammar_pool() (Rejection-Sampling) ersetzt durch
#     exhaustive Enumeration ALLER gueltigen Reber-Strings der Laenge L
#     (nur 36 bei L=10) + exakte Maximum-Independent-Set-Auswahl der
#     paarweise unaehnlichsten Strings. Rejection-Sampling konnte die
#     mathematische Obergrenze (18 bei sim_limit=0.70) nicht zuverlaessig
#     erreichen und brach bei N_PER_STYLE=25 still bei ~14 ab.
#  2) MP3-Export via av::av_audio_convert() statt reinem WAV-Output.
#
# WICHTIG: N_PER_STYLE ist bei diesem Automaten und MELODY_LENGTH=10
# durch nur 36 existierende gueltige Strings hart nach oben begrenzt.
# Bei sim_limit=0.70 sind maximal 18 paarweise hinreichend unaehnliche
# Strings erreichbar (exakt berechnet, keine Heuristik). Siehe Kommentar
# bei N_PER_STYLE unten, falls eine andere Kombination gewuenscht ist.
# ============================================================
library(tuneR)
library(jsonlite)
library(av)      # NEU: fuer MP3-Export, install.packages("av") falls noetig
messagef <- function(...) message(sprintf(...))
set.seed(2026)

# WICHTIG: Passe diesen Pfad ggf. an dein System an.
# setwd("C:/Users/anton/Nextcloud/Promotion/Tests/SLT_analysis_and_publications")
OUTPUT_DIR           <- file.path(getwd(), "stimuli_reber_fsg_v4")
MELODY_LENGTH        <- 10

### GEÄNDERT START — N_PER_STYLE auf mathematisch erreichbaren Wert gesetzt ###
# Bei MELODY_LENGTH=10 existieren nur 36 gueltige Reber-Strings insgesamt.
# Bei MAX_SIMILARITY=0.70 sind davon exakt max. 18 paarweise unaehnlich.
# => N_PER_STYLE=18 ist die groesste bei diesem sim_limit konsistent mit
#    Markov- (v4) und Loui-Generator erreichbare Anzahl.
# Alternative (lockerer): N_PER_STYLE=20, MAX_SIMILARITY=0.75 (siehe Diagnose-Chat)
N_PER_STYLE          <- 18
MAX_SIMILARITY       <- 0.70
### GEÄNDERT ENDE ###

TONE_DURATION        <- 0.4
SAMPLE_RATE          <- 44100
MAX_ATTEMPTS_SHUFFLE <- 200

alphabet  <- c("A", "B", "C", "D", "E")
midi_pool <- c(A = 61, B = 63, C = 65, D = 68, E = 70)

transitions <- list(
  S0 = list(list(to = "S1",     tone = "A"), list(to = "S3",     tone = "D")),
  S1 = list(list(to = "S1",     tone = "B"), list(to = "S2",     tone = "D")),
  S2 = list(list(to = "ACCEPT", tone = "B"), list(to = "S3",     tone = "E")),
  S3 = list(list(to = "S1",     tone = "C"), list(to = "S4",     tone = "C")),
  S4 = list(list(to = "S4",     tone = "E"), list(to = "ACCEPT", tone = "A"))
)
states <- names(transitions)

### GEÄNDERT START — exhaustive Enumeration statt Zaehl-Tabelle + Zufallssampling ###
#' Enumeriert ALLE grammatikalisch gueltigen Strings der Laenge L via DFS.
#' Ersetzt count_paths()/sample_grammar_path() als Quelle des Grammar-Pools:
#' der Zustandsraum ist bei L=10 klein genug (36 Strings), um ihn komplett
#' zu durchsuchen statt zu samplen - dadurch gibt es KEIN "still abbrechen".
enumerate_all_grammar_strings <- function(L) {
  results <- list()
  dfs <- function(state, path) {
    if (length(path) == L) {
      if (state == "ACCEPT") results[[length(results) + 1]] <<- path
      return(invisible(NULL))
    }
    if (state == "ACCEPT") return(invisible(NULL))
    for (tr in transitions[[state]]) {
      dfs(tr$to, c(path, tr$tone))
    }
  }
  dfs("S0", character(0))
  results
}
### GEÄNDERT ENDE ###

is_valid_grammar_string <- function(tones) {
  s <- "S0"
  for (t in tones) {
    match_tr <- Filter(function(tr) tr$tone == t, transitions[[s]])
    if (length(match_tr) == 0) return(FALSE)
    s <- match_tr[[1]]$to
  }
  s == "ACCEPT"
}

make_matched_random <- function(gram_tones, max_attempts = MAX_ATTEMPTS_SHUFFLE) {
  for (i in 1:max_attempts) {
    cand <- sample(gram_tones)
    if (!is_valid_grammar_string(cand)) return(list(tones = cand, attempts = i))
  }
  warning("Reber: keine ungueltige Permutation gefunden - letzte Kandidatin wird verwendet")
  list(tones = cand, attempts = max_attempts)
}

positional_similarity <- function(a, b) mean(a == b)
max_pairwise_similarity <- function(lst) {
  n <- length(lst); if (n < 2) return(0)
  best <- 0
  for (i in 1:(n - 1)) for (j in (i + 1):n) {
    s <- positional_similarity(lst[[i]], lst[[j]]); if (s > best) best <- s
  }
  best
}

### GEÄNDERT START — exakte Maximum-Independent-Set-Auswahl statt Rejection-Sampling ###
#' Findet exakt (Branch-and-Bound, nicht heuristisch) die groesste Teilmenge
#' von `strings`, die paarweise ALLE unterhalb von sim_limit liegen.
#' Bei nur 36 Kandidaten ist das in Millisekunden erledigt.
max_independent_subset <- function(strings, sim_limit) {
  ns <- length(strings)
  sim_mat <- matrix(0, ns, ns)
  for (i in 1:ns) for (j in 1:ns) {
    if (i != j) sim_mat[i, j] <- positional_similarity(strings[[i]], strings[[j]])
  }
  conflict <- sim_mat >= sim_limit

  order_idx <- order(-rowSums(conflict))  # hoher Konfliktgrad zuerst -> besseres Pruning
  best <- integer(0)

  bb <- function(candidates, chosen) {
    if (length(chosen) + length(candidates) <= length(best)) return(invisible(NULL))
    if (length(candidates) == 0) {
      if (length(chosen) > length(best)) best <<- chosen
      return(invisible(NULL))
    }
    v <- candidates[1]
    rest <- candidates[-1]
    new_candidates <- rest[!conflict[v, rest]]
    bb(new_candidates, c(chosen, v))
    bb(rest, chosen)
  }

  bb(order_idx, integer(0))
  best
}

#' Waehlt n paarweise unaehnliche Strings. Hard-Fail-Guard statt stillem
#' Abbruch, falls n bei diesem sim_limit nicht erreichbar ist.
select_diverse_subset <- function(strings, n, sim_limit) {
  max_idx <- max_independent_subset(strings, sim_limit)
  stopifnot(
    "n uebersteigt die bei diesem sim_limit maximal erreichbare Anzahl paarweise unaehnlicher Strings - n reduzieren oder sim_limit erhoehen (siehe Diagnose-Kommentar am Skriptanfang)" =
      n <= length(max_idx)
  )
  chosen_idx <- if (length(max_idx) > n) sample(max_idx, n) else max_idx
  strings[chosen_idx]
}
### GEÄNDERT ENDE ###

midi_to_hz <- function(m) 440 * 2^((m - 69) / 12)

make_tone <- function(freq, duration = TONE_DURATION, sr = SAMPLE_RATE) {
  t <- seq(0, duration, length.out = round(duration * sr))
  w <- sin(2 * pi * freq * t)
  fade_n <- round(0.005 * sr)
  w[1:fade_n] <- w[1:fade_n] * seq(0, 1, length.out = fade_n)
  w[(length(w) - fade_n + 1):length(w)] <- w[(length(w) - fade_n + 1):length(w)] * seq(1, 0, length.out = fade_n)
  tuneR::Wave(left = round(w * 32767), samp.rate = sr, bit = 16)
}

render_melody <- function(tones, filepath) {
  chunks <- lapply(tones, function(t) make_tone(midi_to_hz(midi_pool[[t]])))
  tuneR::writeWave(do.call(tuneR::bind, chunks), filepath)
}

### GEÄNDERT START — WAV -> MP3 Konvertierung via av-Paket, WAV wird danach geloescht ###
#' Rendert eine Melodie direkt als MP3 (WAV nur als kurzlebige Zwischendatei).
render_melody_mp3 <- function(tones, mp3_path) {
  wav_path <- sub("\\.mp3$", ".wav", mp3_path)
  render_melody(tones, wav_path)
  av::av_audio_convert(wav_path, mp3_path, verbose = FALSE)
  file.remove(wav_path)
  invisible(mp3_path)
}
### GEÄNDERT ENDE ###

main_reber <- function() {
  dir.create(OUTPUT_DIR, showWarnings = FALSE, recursive = TRUE)

  ### GEÄNDERT START — Pool-Erzeugung komplett ersetzt ###
  all_strings <- enumerate_all_grammar_strings(MELODY_LENGTH)
  messagef("Reber-Automat: %d grammatikalisch gueltige Strings der Laenge %d gefunden",
           length(all_strings), MELODY_LENGTH)
  stopifnot(
    "Automat liefert keine gueltigen Strings - Transitions pruefen" =
      length(all_strings) > 0
  )

  grammar_pool <- select_diverse_subset(all_strings, N_PER_STYLE, MAX_SIMILARITY)
  messagef("Grammar-Pool: %d Strings ausgewaehlt (max. paarweise Similarity = %.3f)",
           length(grammar_pool), max_pairwise_similarity(grammar_pool))
  ### GEÄNDERT ENDE ###

  random_pool  <- vector("list", N_PER_STYLE)
  shuffle_attempts <- integer(N_PER_STYLE)
  for (i in seq_along(grammar_pool)) {
    res <- make_matched_random(grammar_pool[[i]])
    random_pool[[i]] <- res$tones
    shuffle_attempts[i] <- res$attempts
  }

  item_bank <- data.frame()
  for (i in seq_along(grammar_pool)) {
    fn_a <- sprintf("reber_styleA_%02d.mp3", i)
    fn_b <- sprintf("reber_styleB_%02d.mp3", i)
    render_melody_mp3(grammar_pool[[i]], file.path(OUTPUT_DIR, fn_a))
    render_melody_mp3(random_pool[[i]],  file.path(OUTPUT_DIR, fn_b))
    item_bank <- rbind(item_bank,
                       data.frame(item_number = i, style = "A", correct = "A", file_name = fn_a,
                                  tones = paste(grammar_pool[[i]], collapse = "-"), stringsAsFactors = FALSE),
                       data.frame(item_number = i, style = "B", correct = "B", file_name = fn_b,
                                  tones = paste(random_pool[[i]], collapse = "-"), stringsAsFactors = FALSE))
  }
  item_bank$block <- 1
  item_bank$grammar_type <- "reber_fsg"

  write.csv(item_bank, file.path(OUTPUT_DIR, "reber_item_bank.csv"), row.names = FALSE)
  saveRDS(item_bank, file.path(OUTPUT_DIR, "reber_item_bank.rds"))

  meta <- list(
    grammar_type = "reber_fsg", melody_length = MELODY_LENGTH, n_per_style = N_PER_STYLE,
    n_total_valid_strings_at_length = length(all_strings),
    max_similarity_within_style_A = max_pairwise_similarity(grammar_pool),
    similarity_threshold_used = MAX_SIMILARITY,
    mean_shuffle_attempts_for_valid_control = mean(shuffle_attempts),
    ideal_observer_acc = 1.0
  )
  jsonlite::write_json(meta, file.path(OUTPUT_DIR, "reber_summary.json"), auto_unbox = TRUE, pretty = TRUE)

  messagef("FERTIG. %d MP3-Dateien + item_bank + summary geschrieben nach:", 2 * N_PER_STYLE)
  messagef(">>> %s <<<", normalizePath(OUTPUT_DIR))
  invisible(item_bank)
}

main_reber()
