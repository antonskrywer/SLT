scoring <- function(label = "SLT"){
  psychTestR::code_block(function(state,...){
    #browser()
    results <- psychTestR::get_global("results", state)
    sum_score <- sum(results$correct)
    num_question <- nrow(results)
    perc_correct <- sum_score/num_question
    #browser()
    psychTestR::save_result(place = state,
                            label = "items",
                            value = results)
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
    #item_sequence <- sample(1:max(item_bank$item_number), num_items)
    item_sequence <- 1:num_items
    block_elts <- c()
    for (i in 1:length(item_sequence)) {
      item <- SLT::SLT_item_bank %>%
        filter(block == j, item_number == item_sequence[i])
      messagef("Adding item %d from block %d", item_sequence[i], j)
      item_page <- SLT_item( label = sprintf("q%d_%d", item$block[1], item$item_number[1]),
                             correct_answer = item$correct[1],
                             prompt = get_prompt(i, num_items),
                             audio_file = item$audio_file[1],
                             audio_dir = audio_dir,
                             save_answer = TRUE, autoplay = autoplay)
      block_elts <- c(block_elts, item_page, item_feedback_page())
    }

    elts <- c(elts, psychTestR::order_at_run_time(logic = block_elts,
                                                  label = sprintf("block%d", j),
                                                  get_order = function(...){
                                                    order <- rep(0, 2 * num_items)
                                                    item_pages <- 2* (sample(1:num_items) - 1) + 1
                                                    feedback_pages <- item_pages + 1
                                                    order[2 * (1:num_items - 1) + 1] <- item_pages
                                                    order[2 * (1:num_items - 1) + 2] <- feedback_pages
                                                    print(order)
                                                    order

                                                  }))
    if (j != num_blocks) {
      elts <- c(elts, break_page(block = j))
    }
  }
  elts
}

main_test2 <- function(label,
                       num_items,
                       audio_dir,
                       dict = SLT::SLT_dict,
                       n_start = 6,
                       min_each = 2,
                       composer_pairs = list(        # NEU — Default-Werte
                         c(A = "Noa",  B = "Sam"),
                         c(A = "Alex", B = "Robin"),
                         c(A = "Kai",  B = "Mika")
                       ),
                       autoplay = TRUE, ...) {
  elts <- psychTestR::code_block(function(state, ...) {
    psychTestR::set_global("block", 0, state)
    psychTestR::set_global("results", data.frame(), state)
    psychTestR::set_global("composer_pairs", composer_pairs, state)  # NEU
  })

  item_bank <- SLT::SLT_item_bank2
  num_blocks <- 3
  difficulties <- c("easy", "medium", "hard")
  for (j in 1:num_blocks) {
    #item_sequence <- sample(1:max(item_bank$item_number), num_items)

    block_elts <- psychTestR::code_block(function(state, ...) {
      #browser()
      block <- psychTestR::get_global("block", state)
      seq_df <- get_items(
        difficulty =  difficulties[block + 1],
        num_items = num_items,
        n_start   = n_start,
        min_each  = min_each
      )

      psychTestR::set_global("items", seq_df, state)
      psychTestR::set_global("counter", 1, state)
      psychTestR::set_global("block", block + 1, state)
    })
    if (j == 1) {                          # NEU
      block_elts <- c(block_intro_page(),  # NEU
                      block_elts)          # NEU
    }                                      # NEU

    for (i in 1:num_items) {
      messagef("Adding item %d from block %d", i, j)
      item_page <- SLT_item2(audio_dir = audio_dir,
                             save_answer = TRUE,
                             autoplay = autoplay)
      block_elts <- c(block_elts, item_page, item_feedback_page())
    }
    elts <- c(elts, block_elts)
    if (j != num_blocks) {
      elts <- c(elts, break_page(block = j))
    }
  }
  elts
}



get_prompt <- function(item_number, num_items,
                       dict = SLT::SLT_dict) {
  shiny::div(
    # shiny::h4(
    #   # psychTestR::i18n(
    #   #   "PROGRESS_TEXT",
    #   #   sub = list(num_question = item_number,
    #     #            test_length = if (is.null(num_items))
    #     #              "?" else
    #     #                num_items)),
    #   style  = "text_align:left"
    # ),
    shiny::p(
      psychTestR::i18n("ITEM_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:center")
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
block_intro_page <- function(dict = SLT::SLT_dict) {
  psychTestR::reactive_page(function(state, ...) {
    composer_pairs <- psychTestR::get_global("composer_pairs", state)
    pair           <- composer_pairs[[1]]   # immer Block 1
    psychTestR::one_button_page(
      body = shiny::div(
        psychTestR::i18n(
          "BLOCK1_INTRO",
          sub = list(composer_a = pair[["A"]],
                     composer_b = pair[["B"]])
        ),
        style = "margin-left:20%;margin-right:20%;width:60%;
                 display:block;text-align:justify"
      ),
      button_text = psychTestR::i18n("CONTINUE")
    )
  })
}
break_page <- function(dict = SLT::SLT_dict, block) {
  if (block >= 3) {
    return(NULL)
  }
  psychTestR::reactive_page(function(state, ...) {
    composer_pairs <- psychTestR::get_global("composer_pairs", state)
    next_pair      <- composer_pairs[[block + 1]]   # Namen für den nächsten Block
    name_a <- next_pair[["A"]]
    name_b <- next_pair[["B"]]
    psychTestR::one_button_page(
      body = shiny::div(
        psychTestR::i18n(
          sprintf("BREAK_PAGE%d", block),
          sub = list(composer_a = name_a, composer_b = name_b)
        ),
        style = "margin-left:0%;display:block;text-align:justify;width:60%"
      ),
      button_text = psychTestR::i18n("CONTINUE")
    )
  })
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
get_items <- function(difficulty = c("easy", "medium", "hard"),
                      num_items = 24,
                      n_start   = 6,
                      min_each  = 2,
                      seed      = NULL) {

  if (!is.null(seed)) set.seed(seed)
  difficulty <- match.arg(difficulty)
  block_id <- SLT::SLT_item_bank2 %>% filter(difficulty == !!difficulty) %>% pull(block) %>% sample(1)
  pool <- SLT::SLT_item_bank2 %>% filter(block == block_id)
  if(num_items == 2){
    itemA <- pool %>% filter(style == "A") %>% dplyr::sample_n(1)
    itemB <- pool %>% filter(style == "B") %>% dplyr::sample_n(1)
    return(bind_rows(itemA, itemB) %>%
             mutate(idx = "start", seq_id = sample(1:nrow(.))) %>%
             dplyr::sample_n(2)
           )
  }

  if(num_items < n_start){
    stop(sprintf("Num items [%d] must be at least n_start [%d]", num_items, n_start))
  }
  n_per_style <- floor(num_items / 2)  # 12 Items pro Style
  max_offset <- n_start - 2 * min_each

  offsetA <- sample(0:max_offset, 1)
  offsetB <- max_offset - offsetA


  if((n_per_style - min_each  - offsetB) <0 || (n_per_style - min_each  - offsetA  +  num_items %% 2) < 0){
    browser()
  }
  idxA <- c(rep("start", min_each + offsetA), rep("tail", n_per_style - min_each  - offsetA  +  num_items %% 2))
  idxB <- c(rep("start", min_each  + offsetB), rep("tail", max(0, n_per_style - min_each  - offsetB)))
  idx <- c(idxA, idxB, rep("unused", nrow(pool) - length(idxA) - length(idxB))) %>% sample()

  pool <- pool %>% mutate(idx = idx) %>% filter(idx != "unused")

  start <- pool[pool$idx == "start",]
  tail <- pool[pool$idx == "tail",]

  #browser()
  if(nrow(tail) == 0){
    start
  }
  else{
    bind_rows(start[sample(1:nrow(start)),],
            tail[sample(1:nrow(tail)),]) %>%
      dplyr::slice(1:num_items) %>% mutate(seq_id = 1:nrow(.))
  }
}
