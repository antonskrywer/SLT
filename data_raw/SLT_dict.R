#SLT_dict_raw <- readRDS("data_raw/SLT_dict.RDS")
SLT_dict_raw <- readxl::read_xlsx("data_raw/SLT_dict.xlsx")
#names(SLT_dict_raw) <- c("key", "DE", "EN")
SLT_dict_raw <- SLT_dict_raw[,c("key", "EN", "DE","DE_F")]
# Texte fuer version 5 (Pilot 2026) liegen separat in SLT5_dict.csv (UTF-8),
# damit sie unabhaengig von der xlsx gegengelesen und versioniert werden koennen
SLT5_dict_raw <- utils::read.csv("data_raw/SLT5_dict.csv", stringsAsFactors = FALSE,
                                 fileEncoding = "UTF-8", check.names = FALSE)
stopifnot(!any(SLT5_dict_raw$key %in% SLT_dict_raw$key))
SLT_dict_raw <- rbind(as.data.frame(SLT_dict_raw), SLT5_dict_raw[, c("key", "EN", "DE", "DE_F")])
SLT_dict <- psychTestR::i18n_dict$new(SLT_dict_raw)
usethis::use_data(SLT_dict, overwrite = TRUE)
