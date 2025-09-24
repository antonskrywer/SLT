#SLT_dict_raw <- readRDS("data_raw/SLT_dict.RDS")
SLT_dict_raw <- readxl::read_xlsx("data_raw/SLT_dict.xlsx")
#names(SLT_dict_raw) <- c("key", "DE", "EN")
SLT_dict_raw <- SLT_dict_raw[,c("key", "EN", "DE","DE_F")]
SLT_dict <- psychTestR::i18n_dict$new(SLT_dict_raw)
usethis::use_data(SLT_dict, overwrite = TRUE)

