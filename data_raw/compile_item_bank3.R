# data-raw/compile_item_bank3.R
# ------------------------------------------------------------------
# Kombiniert Reber- und Loui-FSG Item-Banken zu SLT_item_bank3,
# analog zu SLT_item_bank / SLT_item_bank2.
#
# WICHTIG: Reber (36 Items, 18/Style) und Loui (50 Items, 25/Style)
# haben durch die Automaten-Struktur UNTERSCHIEDLICHE Poolgroessen -
# das ist gewollt (siehe Diagnose zum Reber-Generator) und wird NICHT
# hier hartkodiert geprueft, sondern in SLT.R/main_test3.R automatisch
# aus dieser Tabelle ermittelt. Hier wird nur noch die interne
# Konsistenz (A- und B-Anzahl gleich, je Grammatik) validiert.
# ------------------------------------------------------------------
library(dplyr)

# Pfade ggf. anpassen (Ausgabeverzeichnisse der Generator-Skripte)
reber_path <- file.path(getwd(), "stimuli_reber_fsg_v4", "reber_item_bank.rds")
loui_path  <- file.path(getwd(), "stimuli_loui_fsg_v5",  "loui_item_bank.rds")

stopifnot(
  "reber_item_bank.rds nicht gefunden - Pfad pruefen" = file.exists(reber_path),
  "loui_item_bank.rds nicht gefunden - Pfad pruefen"  = file.exists(loui_path)
)

reber_item_bank <- readRDS(reber_path)
loui_item_bank  <- readRDS(loui_path)

required_cols <- c("item_number", "style", "correct", "file_name",
                   "tones", "block", "grammar_type")

stopifnot(
  "reber_item_bank fehlen Pflichtspalten" = all(required_cols %in% names(reber_item_bank)),
  "loui_item_bank fehlen Pflichtspalten"  = all(required_cols %in% names(loui_item_bank))
)

# Dateiendung entfernen: SLT_item2() haengt ".mp3" selbst via sprintf() an
strip_ext <- function(x) tools::file_path_sans_ext(x)

reber_item_bank <- reber_item_bank %>% mutate(file_name = strip_ext(file_name))
loui_item_bank  <- loui_item_bank  %>% mutate(file_name = strip_ext(file_name))

SLT_item_bank3 <- bind_rows(reber_item_bank, loui_item_bank) %>%
  select(all_of(required_cols))

### GEÄNDERT START — Hard-Fail-Guards ohne hartkodierte 25/25-Annahme ###
check_counts <- SLT_item_bank3 %>% count(grammar_type, style)

# Pro Grammatik muss Style A und Style B gleich gross sein (egal wie gross
# der Pool insgesamt ist - Reber und Loui duerfen sich unterscheiden)
counts_by_type <- split(check_counts, check_counts$grammar_type)
style_balanced <- vapply(counts_by_type, function(df) length(unique(df$n)) == 1, logical(1))

stopifnot(
  "Innerhalb einer Grammatik muessen Style A und Style B gleich viele Items haben" =
    all(style_balanced),
  "grammar_type-Werte unerwartet" =
    setequal(unique(SLT_item_bank3$grammar_type), c("reber_fsg", "loui_fsg")),
  "file_name nicht eindeutig innerhalb einer Grammatik" =
    !any(duplicated(SLT_item_bank3[, c("grammar_type", "file_name")]))
)
### GEÄNDERT ENDE ###

usethis::use_data(SLT_item_bank3, overwrite = TRUE)

message("SLT_item_bank3 erstellt: ", nrow(SLT_item_bank3), " Items insgesamt")
totals <- SLT_item_bank3 %>% count(grammar_type)
print(totals)
print(check_counts)
