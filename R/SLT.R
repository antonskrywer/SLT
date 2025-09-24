#printf   <- function(...) print(sprintf(...))
#messagef <- function(...) message(sprintf(...))
#' SLT
#'
#' This function defines a SLT  module for incorporation into a
#' psychTestR timeline.
#' Use this function if you want to include the SLT in a
#' battery of other tests, or if you want to add custom psychTestR
#' pages to your test timeline.
#'
#' For demoing the SLT, consider using \code{\link{SLT_demo}()}.
#' For a standalone implementation of the SLT,
#' consider using \code{\link{SLT_standalone}()}.
#' @param num_items (Integer scalar) Number of items in the test.
#' @param with_welcome (Scalar boolean) Indicates, if a welcome page shall be displayed. Defaults to TRUE
#' @param take_training (Logical scalar) Whether to include the training phase. Defaults to FALSE
#' @param with_finish (Scalar boolean) Indicates, if a finish (not final!) page shall be displayed. Defaults to TRUE
#' @param label (Character scalar) Label to give the SLT results in the output file.
#' @param feedback (Function) Defines the feedback to give the participant
#' at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @param autoplay (Scalar boolean) Indicates whether you want to have autoplay for item pages (instruction pages always not-autoplay)
#' @export

SLT <- function(num_items = 20,
                num_blocks = 3,
                with_welcome = TRUE,
                take_training = FALSE,
                with_finish = TRUE,
                label = "SLT",
                feedback = SLT_feedback_with_score(),
                dict = SLT::SLT_dict,
                autoplay = TRUE
          ) {
  audio_dir <- "https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT/"

  stopifnot(purrr::is_scalar_character(label),
            purrr::is_scalar_integer(num_items) || purrr::is_scalar_double(num_items),
            purrr::is_scalar_character(audio_dir),
            psychTestR::is.timeline(feedback) ||
              is.list(feedback) ||
              psychTestR::is.test_element(feedback) ||
              is.null(feedback))
  audio_dir <- gsub("/$", "", audio_dir)

  psychTestR::join(
    psychTestR::begin_module(label),
    if (take_training) psychTestR::new_timeline(instructions(audio_dir),
                                                dict = dict),
    if (with_welcome) SLT_welcome_page(),
    psychTestR::new_timeline(
      main_test(label = label,
                num_items = num_items,
                num_blocks = num_blocks,
                audio_dir = audio_dir,
                dict = dict,
                autoplay = autoplay
                ),
                dict = dict),
    scoring(),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    feedback,
    if(with_finish) SLT_finished_page(),
    psychTestR::end_module())
}

# with_welcome
# with_finish
# take_training
