# =============================================================================
# compile_item_bank6.R
# Baut die Paketdaten SLT6_items fuer SLT version 6 aus den CSVs, die in
# SLT_analysis_and_publications/slt6/ erzeugt wurden.
#
# Aufruf (im Paket-Wurzelordner SLT/):
#   Rscript data_raw/compile_item_bank6.R
#
# Ergebnis: data/SLT6_items.rda (Liste)
#   items        34 Items x 2 Sprachen, alle 2AFC (slt6_items.csv)
#   item_audio   Datei + Einsatzzeiten der Optionen je Item (slt6_item_audio.csv)
#   stream       Expositionsstrom je Sprache (slt6_stream.csv)
#   oddball_key  Oddball-Plan je Sprache, 48 je Sprache (seit 23.10.) (slt6_oddball_key.csv)
#   languages    Tonsprachen L1/L2 (slt6_languages.csv)
#   practice     Uebungsstrom fuer die Oddball-Taste (hier erzeugt, s. unten)
#   tones        16 Tonnamen (Dateinamen der Einzeltoene)
# =============================================================================

src <- file.path("..", "SLT_analysis_and_publications", "slt6")
rd  <- function(f) utils::read.csv(file.path(src, f), stringsAsFactors = FALSE,
                                   colClasses = c(tp_group = "character"))

items       <- rd("slt6_items.csv")
item_audio  <- rd("slt6_item_audio.csv")
stream      <- rd("slt6_stream.csv")
oddball_key <- rd("slt6_oddball_key.csv")
languages   <- rd("slt6_languages.csv")
tones       <- c("C3", "D3", "E3", "F#3", "G#3", "A#3", "C4", "D4", "E4", "F#4",
                 "G#4", "A#4", "C5", "D5", "E5", "F#5")

# Uebungsstrom (Plan Abschnitt 5): 3 Phrasen a 12 Toene aus allen 16 Toenen,
# ohne ein Tonpaar, das in L1 oder L2 innerhalb eines Tonworts vorkommt
# (verraet nichts ueber die Sprachen), 3 Oddballs.
set.seed(20261021)
within <- unique(unlist(lapply(split(languages, paste(languages$language, languages$triplet_id)), function(t) {
  t <- t[order(t$position), ]
  c(paste(t$tone[1], t$tone[2]), paste(t$tone[2], t$tone[3]))
})))
phrase <- function() repeat {
  s <- sample(tones, 12, replace = TRUE)
  if (all(head(s, -1) != tail(s, -1)) && !any(paste(head(s, -1), tail(s, -1)) %in% within)) return(s)
}
prac <- do.call(rbind, lapply(1:3, function(p) {
  s <- phrase()
  data.frame(phrase = p, tone_index = (p - 1) * 12 + 1:12, tone = s,
             onset_ms = (p - 1) * (12 * 350 + 700) + (0:11) * 350)
}))
prac$odd <- prac$tone_index %in% c(6, 12 + 9, 24 + 4)

stopifnot(nrow(items) == 68, nrow(item_audio) == 68, nrow(stream) == 2304,   # 34 Items x 2 (seit 21.10.)
          nrow(oddball_key) == 96, nrow(languages) == 48, sum(prac$odd) == 3)

SLT6_items <- list(items = items, item_audio = item_audio, stream = stream,
                   oddball_key = oddball_key, languages = languages,
                   practice = prac, tones = tones)
usethis::use_data(SLT6_items, overwrite = TRUE)
