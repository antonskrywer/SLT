 SLT_item_bank <- readr::read_csv2("data_raw/SLT_item_bank.csv")
 usethis::use_data(SLT_item_bank, overwrite = TRUE)
 SLT_item_bank2 <- readr::read_csv2("data_raw/SLT_item_bank_v2.csv")
 usethis::use_data(SLT_item_bank2, overwrite = TRUE)

make_item_bank_v2 <- function(stim_dir = "data_raw/stimuli"){
  get_item_difficulty <- function(num_tones, prob){
    easy <- num_tones == 3 & prob >= 70 | num_tones == 4 & prob >= 80
    medium <- num_tones == 3 & prob < 70 | num_tones == 4 & prob < 80  | num_tones == 5 & prob < 80
    hard <- num_tones ==6 | num_tones == 5 & prob >= 80
    diff <- 3 * as.integer(hard) + 2 * as.integer(medium) + as.integer(easy)
    c("easy", "medium", "hard")[diff]
  }
  fnames <- list.files(stim_dir) %>%
    tools::file_path_sans_ext()
  item_bank <- fnames %>%
    str_split_fixed("_", 6) %>%
    as.data.frame() %>%
    as_tibble() %>%
    select(V2:V6) %>%
    set_names(c("block", "num_tones", "prob", "style", "running_no")) %>%
    mutate(block = as.integer(block),
           num_tones = str_extract(num_tones, "[0-9]+") %>% as.integer(),
           prob = str_extract(prob, "[0-9]+") %>% as.integer(),
           style = str_extract(style, "[AB]"),
           running_no = as.integer(running_no))
  item_bank <- item_bank %>%
    mutate(file_name = basename(fnames),
           difficulty = get_item_difficulty(num_tones, prob))
  item_bank
}

flatten_stimuli <- function(stim_dir = "data_raw/melodies_midi_v4/", outdir = "data_raw/stimuli"){
  blocks <- list.files(stim_dir, full.names = F)
  blocks <- blocks[stringr::str_detect(blocks, "block")]
  for(bl in blocks){
    print(cat("Copying block ", bl))
    mp3 <- list.files(sprintf("%s/%s/mp3", stim_dir, bl), pattern = "*mp3", full.names = T)
    new_files <- sprintf("%s/%s_%s", outdir, bl, basename(mp3))
    file.copy(mp3, new_files)
  }
}

