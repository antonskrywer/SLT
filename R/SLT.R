null_or_else <- function(x, default) {
  if (is.null(x)) default else x
}

#' SLT
#'
#' This function defines a SLT  module for incorporation into a
#' psychTestR timeline.
#' @param num_items (Integer scalar) Number of items in the test.
#'   Fuer version 3/4 wird dieser Wert immer auf 50 gesetzt (25 pro
#'   Grammatik-Typ); ein abweichender Wert erzeugt eine Warnung.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param label (Character scalar) Label to give the SLT results in the output file.
#' @param version (Integer scalar) 1 = Markov (Block-Design), 2 = Markov
#'   gamifiziert (Easy/Medium/Hard-Bloecke), 3 = Reber-FSG (deterministisch),
#'   4 = Loui-FSG (deterministisch, Bohlen-Pierce)
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param autoplay (Scalar boolean) Indicates whether you want to have autoplay for item pages (instruction pages always not-autoplay)
#' @param composer_pairs (List) Composer-Namenspaare pro Block. Bei
#'   version 3/4 wird automatisch EIN Paar (Noa/Sam) verwendet, falls
#'   NULL uebergeben wird (Default).
#' @export

SLT <- function(num_items = 20L,
                num_blocks = 3L,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                label = "SLT",
                version = 1,
                feedback = SLT_feedback_with_score(dict = SLT::SLT_dict),
                dict = SLT::SLT_dict,
                autoplay = TRUE,
                composer_pairs = NULL,
                ...
) {
  ### GEÄNDERT START — audio_dir je Version (inkl. v3 Reber / v4 Loui) ###
  audio_dir <- switch(
    as.character(version),
    "1" = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT/",
    "2" = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT2/",
    "3" = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT3/",
    "4" = "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT4/",
    stop(sprintf("SLT: unbekannte version = %s (erlaubt: 1, 2, 3, 4)", version))
  )
  ### GEÄNDERT ENDE ###

  ### GEÄNDERT START — grammar_type einmalig ermitteln (fuer instructions() und main_test3()) ###
  grammar_type <- switch(
    as.character(version),
    "3" = "reber_fsg",
    "4" = "loui_fsg",
    NULL
  )
  ### GEÄNDERT ENDE ###
  ### GEÄNDERT START — num_items fuer v3/v4 automatisch aus Item-Bank ermittelt ###
  if (version %in% c(3, 4)) {
    if (!missing(num_items)) {
      warning(sprintf(
        paste0("SLT version %s administriert immer den vollstaendigen Item-Pool ",
               "der jeweiligen Grammatik (Anzahl wird automatisch aus ",
               "SLT_item_bank3 ermittelt: Reber = 36, Loui = 50). ",
               "num_items = %s wird ignoriert."),
        version, num_items
      ))
    }
    num_items <- NULL  # main_test3() ermittelt die tatsaechliche Anzahl selbst
  }
  ### GEÄNDERT ENDE ###

  ### GEÄNDERT START — Default-Composer-Paare abhaengig von version ###
  if (is.null(composer_pairs)) {
    composer_pairs <- if (version %in% c(3, 4)) {
      list(c(A = "Noa", B = "Sam"))
    } else {
      list(
        c(A = "Noa",  B = "Taylor"),
        c(A = "Alex", B = "Luca"),
        c(A = "Kai",  B = "Mika")
      )
    }
  }
  ### GEÄNDERT ENDE ###

  ### GEÄNDERT START — num_items darf NULL sein (wird bei v3/4 automatisch ermittelt) ###
  stopifnot(purrr::is_scalar_character(label),
            is.null(num_items) ||
              purrr::is_scalar_integer(num_items) ||
              purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  ### GEÄNDERT ENDE ###
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (with_welcome) SLT_welcome_page(),
    ### GEÄNDERT START — version/grammar_type an instructions() uebergeben ###
    if (take_training) psychTestR::new_timeline(
      instructions(audio_dir, version = version, grammar_type = grammar_type),
      dict = dict),
    ### GEÄNDERT ENDE ###
    if (version == 1) psychTestR::new_timeline(
      main_test(label = label,
                num_items = num_items,
                num_blocks = num_blocks,
                audio_dir = audio_dir,
                dict = dict,
                autoplay = autoplay
      ),
      dict = dict),
    if (version == 2) psychTestR::new_timeline(
      main_test2(label = label,
                 num_items = num_items,
                 audio_dir = audio_dir,
                 dict = dict,
                 n_start = null_or_else(list(...)$n_start, 6),
                 min_each = null_or_else(list(...)$min_each, 2),
                 composer_pairs = composer_pairs,
                 autoplay = autoplay
      ),
      dict = dict),
    ### GEÄNDERT START — version 3 (Reber-FSG) / version 4 (Loui-FSG) ###
    if (version %in% c(3, 4)) psychTestR::new_timeline(
      main_test3(label = label,
                 grammar_type = grammar_type,
                 num_items = num_items,
                 audio_dir = audio_dir,
                 dict = dict,
                 n_start = null_or_else(list(...)$n_start, 6),
                 min_each = null_or_else(list(...)$min_each, 2),
                 composer_pairs = composer_pairs,
                 autoplay = autoplay
      ),
      dict = dict),
    ### GEÄNDERT ENDE ###
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    psychTestR::code_block(function(state, ...){
      res <- psychTestR::get_results(state, complete = T, add_session_info = T) %>% as.list()
      #browser()
    }),
    if (with_finish) SLT_finished_page(),
    psychTestR::end_module())
}
