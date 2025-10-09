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
        #browser()
        results <- psychTestR::get_results(state = state,
                                           complete = TRUE,
                                           add_session_info = FALSE) %>%
          as.list() %>%
          pluck("SLT")
        results <- results[str_detect(names(results), "q[0-9]+")] %>% bind_rows()
        #browser()
        # return(
        #   psychTestR::one_button_page(
        #     body= "TEST",
        #     button_text = psychTestR::i18n("CONTINUE")
        #   )
        # )
        results <- results %>%
          mutate(block = substr(label, 2,2)) %>%
          group_by(block) %>%
          summarise(sum = sum(correct),
                    num_questions = n())
        #sum_score <- sum(purrr::map_lgl(result"s[[1]], function(x) x$correct))
        #num_question <- length(results[[1]])
        #messagef("Sum scores: %d, total items: %d", sum_score, num_question)
        #browser()
        text_finish2 <- NULL
        text_finish3 <- NULL
        #browser()
        text_finish1 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK1",
                                          html = TRUE,
                                          sub = list(number_correct_1 = results$sum[[1]]))
        if(nrow(results) > 1) {text_finish2 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK2",
                                         html = TRUE,
                                         sub = list(number_correct_2 = results$sum[[2]]))}
        if(nrow(results) > 2) {text_finish3 <- psychTestR::i18n("FINAL_FEEDBACK_BLOCK3",
                                         html = TRUE,
                                         sub = list(number_correct_3 = results$sum[[3]]))}
          psychTestR::one_button_page(
          body= shiny::div(
            shiny::p(text_finish1, shiny::tags$br(), text_finish2, shiny::tags$br(), text_finish3)
          ),
          button_text = psychTestR::i18n("CONTINUE")
        )
      }), dict = dict)
}

