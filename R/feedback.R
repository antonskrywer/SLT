#' SLT feedback (with score)
#'
#' Here the participant is given textual feedback at the end of the test.
#' @param dict The psychTestR dictionary used for internationalisation.
#' @export
#' @examples
#' \dontrun{
#' SLT_demo(feedback = SLT_feedback_with_score())}

SLT_feedback_with_score <- function(dict = SLT::SLT_dict) {
  psychTestR::new_timeline(
      psychTestR::reactive_page(function(state, ...) {
        results <- psychTestR::get_results(state = state,
                                           complete = TRUE,
                                           add_session_info = FALSE) %>%
          as.list() %>%
          pluck("SLT")


        #browser()
        results <- results$items %>%
          group_by(block_no) %>%
          summarise(sum          = sum(correct),
                    num_questions = n(),
                    percent      = round(100 * sum(correct) / n()))
        text_finish2 <- NULL
        text_finish3 <- NULL
        #browser()
        text_finish1 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK1",
                                         html = TRUE,
                                         sub = list(number_correct_1 = results$sum[[1]],
                                                    total_1          = results$num_questions[[1]],
                                                    percent_1        = results$percent[[1]]))
        if(nrow(results) > 1) {text_finish2 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK2",
                                                                html = TRUE,
                                                                sub = list(number_correct_2 = results$sum[[2]],
                                                                           total_2          = results$num_questions[[2]],
                                                                           percent_2        = results$percent[[2]]))}
        if(nrow(results) > 2) {text_finish3 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK3",
                                                                html = TRUE,
                                                                sub = list(number_correct_3 = results$sum[[3]],
                                                                           total_3          = results$num_questions[[3]],
                                                                           percent_3        = results$percent[[3]]))}
          psychTestR::one_button_page(
          body= shiny::div(
            shiny::p(text_finish1, shiny::tags$br(), text_finish2, shiny::tags$br(), text_finish3)
          ),
          button_text = psychTestR::i18n("CONTINUE")
        )
      }), dict = dict)
}

