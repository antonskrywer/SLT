media_js <- list(
  media_not_played = "var media_played = false;",
  media_played = "media_played = true;",
  play_media = "document.getElementById('media').play();",
  show_media   = paste0("if (!media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='inherit'};"),
  hide_media   = paste0("if (media_played) ",
                        "{document.getElementById('media')",
                        ".style.visibility='hidden'};"),
  show_media_btn = paste0("if (!media_played) ",
                          "{document.getElementById('btn_play_media')",
                          ".style.visibility='inherit'};"),
  hide_media_btn = paste0("document.getElementById('btn_play_media')",
                          ".style.visibility='hidden';"),
  show_responses = "document.getElementById('response_ui').style.visibility = 'inherit';"
)

media_mobile_play_button <- shiny::tags$p(
  shiny::tags$button(shiny::tags$span("\u25B6"),
                     type = "button",
                     id = "btn_play_media",
                     style = "visibility: hidden",
                     onclick = media_js$play_media)
)

get_audio_ui <- function(url,
                         type = tools::file_ext(url),
                         autoplay = TRUE,
                         width = 0,
                         wait = TRUE,
                         loop = FALSE) {
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type),
            purrr::is_scalar_logical(wait),
            purrr::is_scalar_logical(loop))
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    script,
    src,
    id = "media",
    preload = "auto",
    autoplay = if(autoplay) "autoplay",
    width = width,
    loop = if (loop) "loop",
    oncanplaythrough = media_js$show_media_btn,
    onplay = paste0(media_js$media_played, media_js$hide_media_btn),
    onended = if (wait) media_js$show_responses else "null"
  )
  shiny::tags$div(audio, media_mobile_play_button)
}

get_audio_element <- function(url,
                              type = tools::file_ext(url),
                              wait = F,
                              autoplay = FALSE,
                              width = 200,
                              height = 50,
                              id = "media") {
  stopifnot(purrr::is_scalar_character(url),
            purrr::is_scalar_character(type)
  )
  src    <- shiny::tags$source(src = url, type = paste0("audio/", type))
  script <- shiny::tags$script(shiny::HTML(media_js$media_not_played))
  audio  <- shiny::tags$audio(
    src,
    script,
    id = id,
    preload = "auto",
    controls = "controls",
    controlslist = "nodownload noremoteplayback",
    autoplay = if(autoplay) "autoplay",
    width = width,
    height = height,
    onplay = paste0(media_js$media_played, media_js$hide_media),
    onended = if (wait) paste0(media_js$show_responses, media_js$hide_media) else "null"
  )
  audio
}

### GEÄNDERT START — Verweilzeit-/Entscheidungszeit-Erfassung (neu) ###

#' JS-Snippet: erfasst Audio-Start, Audio-Ende (=Antwort-Buttons sichtbar)
#' und Klick-Zeitpunkt als Millisekunden-Timestamps in Shiny-Inputs.
#' Wird als zusaetzliches Element in die Page-UI eingehaengt (kein Eingriff
#' in get_audio_ui()/media_js noetig - additiv, unabhaengig testbar).
#'
#' Der Klick-Listener nutzt die CAPTURE-Phase (letztes Argument TRUE bei
#' addEventListener), damit unser Timestamp VOR psychTestR's eigener
#' Klick-Verarbeitung (die input$last_btn_pressed setzt) erfasst wird.
rt_capture_script <- function() {
  shiny::tags$script(shiny::HTML("
    (function() {
      var mediaEl = document.getElementById('media');
      if (mediaEl) {
        mediaEl.addEventListener('play', function() {
          Shiny.setInputValue('audio_start_time', Date.now(), {priority: 'event'});
        });
        mediaEl.addEventListener('ended', function() {
          Shiny.setInputValue('response_visible_time', Date.now(), {priority: 'event'});
        });
      }
      var respEl = document.getElementById('response_ui');
      if (respEl) {
        respEl.addEventListener('click', function(e) {
          Shiny.setInputValue('answer_click_time', Date.now(), {priority: 'event'});
        }, true);
      }
    })();
  "))
}

#' Berechnet rt_decision_ms (Audio-Ende -> Klick) und rt_total_ms
#' (Audio-Start -> Klick) aus den von rt_capture_script() gesetzten Inputs.
#' Gibt NA_real_ zurueck, falls ein Timestamp fehlt (defensiver Guard,
#' z.B. falls JS aus irgendeinem Grund nicht gefeuert hat).
compute_rt <- function(input) {
  audio_start  <- input$audio_start_time
  resp_visible <- input$response_visible_time
  click_time   <- input$answer_click_time
  list(
    rt_decision_ms = if (!is.null(resp_visible) && !is.null(click_time))
      as.numeric(click_time - resp_visible) else NA_real_,
    rt_total_ms = if (!is.null(audio_start) && !is.null(click_time))
      as.numeric(click_time - audio_start) else NA_real_
  )
}

#' Verallgemeinerte Audio-NAFC-Page mit RT-Erfassung.
#' Ersetzt audio_NAFC_page_flex() (v1) UND psychTestR::audio_NAFC_page()
#' (v2/v3/v4) - beide boten keinen Zugriff auf die fuer RT noetigen
#' JS-Timestamps, hier haben wir volle Kontrolle ueber UI und get_answer.
#'
#' @param get_answer Funktion(input, ...) - MUSS compute_rt(input) selbst
#'   aufrufen und die RT-Werte in den Rueckgabewert einbauen (siehe
#'   SLT_item()/SLT_item2() unten als Beispiele)
audio_NAFC_page_rt <- function(label,
                               prompt,
                               audio_url,
                               choices,
                               labels = choices,
                               get_answer,
                               save_answer = TRUE,
                               on_complete = NULL,
                               autoplay = TRUE,
                               admin_ui = NULL) {
  stopifnot(purrr::is_scalar_character(label))
  audio_ui <- get_audio_ui(audio_url, autoplay = autoplay, wait = TRUE, loop = FALSE, width = 200)
  ui <- shiny::div(
    tagify(prompt),
    audio_ui,
    rt_capture_script(),
    psychTestR::make_ui_NAFC(choices,
                             labels = labels,
                             hide = TRUE,
                             arrange_vertically = FALSE,
                             id = "response_ui")
  )

  validate <- function(answer, ...) !is.null(answer)

  psychTestR::page(ui = ui, label = label,
                   get_answer = get_answer, save_answer = save_answer,
                   validate = validate, on_complete = on_complete,
                   final = FALSE,
                   admin_ui = admin_ui)
}
### GEÄNDERT ENDE ###

### GEÄNDERT START — SLT_item() (v1) nutzt jetzt audio_NAFC_page_rt() ###
SLT_item <- function(label = "",
                     emotion,
                     audio_file,
                     correct_answer,
                     prompt = "",
                     audio_dir = "",
                     save_answer = TRUE,
                     on_complete = NULL,
                     get_answer = NULL,
                     autoplay = TRUE
){
  page_prompt <- shiny::div(prompt)
  choices <- c("1", "2")
  audio_url <- file.path(audio_dir, audio_file)

  get_answer_rt <- function(input, ...) {
    answer <- as.numeric(gsub("answer", "", input$last_btn_pressed))
    item_bank <- SLT::SLT_item_bank %>% mutate(label = sprintf("q%d_%d", block, item_number))
    correct <- item_bank[item_bank$label == label,]$correct == answer
    rt <- compute_rt(input)
    tibble(answer = answer,
           label = label,
           correct = correct,
           rt_decision_ms = rt$rt_decision_ms,
           rt_total_ms    = rt$rt_total_ms)
  }

  audio_NAFC_page_rt(label = label,
                     prompt = page_prompt,
                     audio_url = audio_url,
                     choices = choices,
                     labels = choices,
                     get_answer = get_answer_rt,
                     save_answer = save_answer,
                     autoplay = autoplay,
                     on_complete = on_complete
  )
}
### GEÄNDERT ENDE ###

### GEÄNDERT START — SLT_item2() (v2/v3/v4) nutzt jetzt audio_NAFC_page_rt()
### statt psychTestR::audio_NAFC_page() (war noetig, um RT ueberhaupt zu
### erfassen - siehe Erklaerung im Chat) ###
SLT_item2 <- function(audio_dir = "",
                      save_answer = TRUE,
                      autoplay = TRUE){
  psychTestR::reactive_page(function(state, ...) {
    counter    <- psychTestR::get_global("counter", state)
    cur_block  <- psychTestR::get_global("block", state)
    seq_df     <- psychTestR::get_global("items", state)
    item       <- seq_df[counter, ]

    composer_pairs <- psychTestR::get_global("composer_pairs", state)
    pair           <- composer_pairs[[cur_block]]

    stim_url <- sprintf("%s/%s.mp3", audio_dir, item$file_name)
    label    <- sprintf("q%d_%d", cur_block, counter)
    messagef("[%s] counter: %s", label, counter)

    get_answer_rt <- function(input, ...) {
      answer <- gsub("answer", "", input$last_btn_pressed)  # "A" oder "B"
      rt <- compute_rt(input)
      list(answer = answer,
           rt_decision_ms = rt$rt_decision_ms,
           rt_total_ms    = rt$rt_total_ms)
    }

    audio_NAFC_page_rt(
      label   = label,
      prompt  = get_prompt(counter, num_items),
      audio_url = stim_url,
      choices = c("A", "B"),
      labels  = c(pair[["A"]], pair[["B"]]),
      get_answer = get_answer_rt,
      save_answer = save_answer,
      autoplay = autoplay,
      on_complete = function(answer, state, ...) {
        ans_value      <- answer$answer
        correct        <- as.integer(ans_value == item$style)
        rt_decision_ms <- answer$rt_decision_ms
        rt_total_ms    <- answer$rt_total_ms
        results        <- psychTestR::get_global("results", state)
        new_row        <- item %>%
          mutate(seq_id   = counter,
                 block_no = cur_block,
                 answer   = ans_value,
                 correct  = correct,
                 rt_decision_ms = rt_decision_ms,
                 rt_total_ms    = rt_total_ms)
        psychTestR::set_global("counter", counter + 1, state)
        psychTestR::set_global("results", bind_rows(results, new_row), state)
      }
    )
  })
}
### GEÄNDERT ENDE ###

item_feedback_page <- function() {
  psychTestR::reactive_page(function(answer, state, ...) {
    results        <- psychTestR::get_global("results", state)
    last           <- results %>% dplyr::slice(nrow(results))
    cur_block      <- psychTestR::get_global("block", state)
    composer_pairs <- psychTestR::get_global("composer_pairs", state)
    pair           <- composer_pairs[[cur_block]]

    if (last$correct == TRUE) {
      composer_name <- pair[[last$answer]]
      prompt <- psychTestR::i18n("CORRECT_COMPOSER",
                                 sub = list(composer = composer_name))
    } else {
      correct_style <- last$style
      composer_name <- pair[[correct_style]]
      prompt <- psychTestR::i18n("FALSE_COMPOSER",
                                 sub = list(composer = composer_name))
    }
    psychTestR::one_button_page(
      body        = prompt,
      button_text = psychTestR::i18n("CONTINUE")
    )
  })
}
