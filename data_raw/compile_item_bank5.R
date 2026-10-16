# =============================================================================
# compile_item_bank5.R
# Baut die Paketdaten SLT5_items fuer SLT version 5 aus den CSVs, die in
# SLT_analysis_and_publications/slt5_pilot/ erzeugt wurden.
#
# Aufruf (im Paket-Wurzelordner SLT/):
#   Rscript data_raw/compile_item_bank5.R
#
# Ergebnis: data/SLT5_items.rda (exportierte Paketdaten, NICHT sysdata.rda)
# =============================================================================

src <- file.path("..", "SLT_analysis_and_publications", "slt5_pilot")
rd  <- function(f) utils::read.csv(file.path(src, f), stringsAsFactors = FALSE)

anota <- rd("slt5_items_anota.csv")
anota$file_name <- file.path("anota", anota$slt5_item_id)

probe <- rd("slt5_items_probe.csv")
probe$file_name <- file.path("probe", probe$slt5_probe_id)

practice <- rd("slt5_practice.csv")
practice$file_name <- file.path("practice", practice$practice_id)

pitch_check <- rd("slt5_pitch_check.csv")
pitch_check$file_name <- file.path("pitch_check", pitch_check$pitch_id)

oddball_key <- rd("slt5_oddball_key.csv")

exposure <- expand.grid(block = 1:4, grammar = c("G1", "G2"), difficulty = c("normal", "easy"),
                        stringsAsFactors = FALSE)
exposure$file_name <- with(exposure, file.path("exposure", sprintf("expo_%s_%s_b%d", difficulty, grammar, block)))
exposure <- merge(exposure, oddball_key, by = "block")
exposure <- exposure[order(exposure$difficulty, exposure$grammar, exposure$block), ]
rownames(exposure) <- NULL

stopifnot(nrow(anota) == 48, nrow(probe) == 30, nrow(practice) == 3,
          nrow(pitch_check) == 3, nrow(exposure) == 16)

SLT5_items <- list(anota = anota, probe = probe, practice = practice,
                   pitch_check = pitch_check, exposure = exposure)
usethis::use_data(SLT5_items, overwrite = TRUE)
