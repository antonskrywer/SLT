scoring <- function(label = "SLT"){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_results(state = state,
                                       complete = FALSE,
                                       add_session_info = FALSE) %>% as.list()

    sum_score <- sum(purrr::map_lgl(results[[label]], function(x) x$correct))
    num_question <- length(results[[label]])
    perc_correct <- sum_score/num_question
    psychTestR::save_result(place = state,
                 label = "score",
                 value = perc_correct)
    psychTestR::save_result(place = state,
                             label = "num_questions",
                             value = num_question)

  })
}

main_test <- function(label,
                      num_items,
                      num_blocks,
                      audio_dir,
                      dict = SLT::SLT_dict,
                      autoplay = TRUE, ...) {
  elts <- c()
  item_bank <- SLT::SLT_item_bank
  for (j in 1:num_blocks) {
    item_sequence <- sample(1:max(item_bank$item_number), num_items)
    for (i in 1:length(item_sequence)) {
      item <- SLT::SLT_item_bank %>%
        filter(block == j, item_number == item_sequence[i])
      messagef("Adding item %d from block %d", item_sequence[i], j)

      item_page <- SLT_item( label = sprintf("q%d_%d", item$block[1], item$item_number[1]),
                             correct_answer = item$correct[1],
                             prompt = get_prompt(i, num_items),
                             audio_file = item$audio_file[1],
                             audio_dir = audio_dir,
                             save_answer = TRUE, autoplay = autoplay )
      #browser()
      elts <- c(elts, item_page, item_feedback_page())
    }
    if (j != num_blocks) {
    elts <- c(elts, break_page(block = j))
    }
  }
  elts <- c(elts, SLT_feedback_with_score())
}

item_page <- function(item_number, item_id, num_items, audio_dir, autoplay,
                      dict = SLT::SLT_dict) {
  item <- SLT::SLT_item_bank %>% filter(item_number == item_id) %>% as.data.frame()
  emotion <- psychTestR::i18n(item[1,]$emotion_i18)
  SLT_item(label = item_id,
           correct_answer = item$correct[1],
           prompt = get_prompt(item_number, num_items),
           audio_file = item$audio_file[1],
           audio_dir = audio_dir,
           autoplay = autoplay,
           save_answer = TRUE)
  #psychTestR::audio_NAFC_page(label = sprintf("%s-%s", item_number, num_items_in_test),
  #                promp = get_prompt(item_number, num_items_in_test, emotion),
  #                choices = c("1", "2"),
  #                url = file.path(audio_dir, item$audio_file[1]))
} # Is this function used anywhere? NR

get_prompt <- function(item_number, num_items,
                       dict = SLT::SLT_dict) {
  shiny::div(
    shiny::h4(
      psychTestR::i18n(
        "PROGRESS_TEXT",
        sub = list(num_question = item_number,
                   test_length = if (is.null(num_items))
                     "?" else
                       num_items)),
      style  = "text_align:left"
    ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:justify")
    )
}

SLT_welcome_page <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("WELCOME")),
      shiny::div(psychTestR::i18n("INTRO_TEXT"),
               style = "margin-left:20%;margin-right:20%;width:60%;display:block;text-align:justify")
    ),
    button_text = psychTestR::i18n("CONTINUE")
  ), dict = dict)
}

SLT_finished_page <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        psychTestR::i18n("SUCCESS"),
                         style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

SLT_final_page <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::final_page(
      body = shiny::div(
        shiny::h4(psychTestR::i18n("THANKS")),
        shiny::div(psychTestR::i18n("SUCCESS"),
                   style = "margin-left:0%;display:block"),
        button_text = psychTestR::i18n("CONTINUE")
      )
    ), dict = dict)
}

break_page <- function(dict = SLT::SLT_dict, block){
  if (block >= 3) {
    return(NULL)
  }
    psychTestR::one_button_page(
      body =  shiny::div(
        psychTestR::i18n(sprintf("BREAK_PAGE%d", block)),
        style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    )
}

correct_a <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        psychTestR::i18n("CORRECT_A"),
        style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

correct_b <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        psychTestR::i18n("CORRECT_B"),
        style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
false_a <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        psychTestR::i18n("FALSE_A"),
        style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}

false_b <- function(dict = SLT::SLT_dict){
  psychTestR::new_timeline(
    psychTestR::one_button_page(
      body =  shiny::div(
        psychTestR::i18n("FALSE_B"),
        style = "margin-left:0%;display:block"),
      button_text = psychTestR::i18n("CONTINUE")
    ), dict = dict)
}
