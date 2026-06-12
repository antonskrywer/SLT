# info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
#   psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE),
#                                          style = style),
#                               button_text = psychTestR::i18n("CONTINUE"))
# }

# ── Hilfsfunktion: einfache Infoseite ──────────────────────────────────────
info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  psychTestR::one_button_page(
    shiny::div(psychTestR::i18n(id, html = TRUE), style = style),
    button_text = psychTestR::i18n("CONTINUE")
  )
}

demo_trial_page <- function(audio_url, correct_style) {

  # 1) Item-Page: neutrale Button-Labels
  item <- psychTestR::audio_NAFC_page(
    label   = paste0("demo_", tolower(correct_style)),
    prompt  = shiny::p(
      psychTestR::i18n("DEMO_TRIAL_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:center"
    ),
    url     = audio_url,
    choices = c("A", "B"),
    labels  = c(psychTestR::i18n("COMPOSER_A"),
                psychTestR::i18n("COMPOSER_B")),
    save_answer = FALSE
  )

  # 2) Feedback-Page: nur Richtig/Falsch
  feedback <- psychTestR::reactive_page(function(answer, ...) {
    if (is.null(answer)) answer <- ""
    correct <- (answer == correct_style)
    key <- if (correct) "DEMO_CORRECT" else "DEMO_FALSE"
    psychTestR::one_button_page(
      body        = psychTestR::i18n(key),
      button_text = psychTestR::i18n("CONTINUE")
    )
  })

  list(item, feedback)
}

# ── Haupt-Instruktionsfunktion ─────────────────────────────────────────────
instructions <- function(audio_dir) {

  demo_A_url <- paste0(audio_dir, "/block_07_tc4_p80_styleA_01.mp3")
  demo_B_url <- paste0(audio_dir, "/block_07_tc4_p80_styleB_01.mp3")

  psychTestR::join(
    # 1. Intro
    info_page("DEMO_INTRO"),

    # 2. Demo-Trial A + Feedback
    demo_trial_page(audio_url = demo_A_url, correct_style = "A"),

    # 3. Demo-Trial B + Feedback
    demo_trial_page(audio_url = demo_B_url, correct_style = "B"),

    # 4. Outro
    info_page("DEMO_OUTRO")
  )
}
