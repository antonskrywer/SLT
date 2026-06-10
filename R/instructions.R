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

# ── Demo-Trial-Page: identisch zur Haupttest-Page ──────────────────────────
# audio_url    : vollständige URL zur MP3
# correct_style: "A" oder "B" (ground truth für Feedback)
demo_trial_page <- function(audio_url, correct_style) {

  # 1) Item-Page: audio_NAFC_page mit gleichen Choices wie Haupttest
  item <- psychTestR::audio_NAFC_page(
    label   = paste0("demo_", tolower(correct_style)),
    prompt  = shiny::p(
      psychTestR::i18n("DEMO_TRIAL_INSTRUCTION"),
      style = "margin-left:20%;margin-right:20%;text-align:center"
    ),
    url     = audio_url,
    choices = c("A", "B"),
    save_answer = FALSE          # Demo-Antworten nicht in Ergebnisse
  )

  # 2) Feedback-Page: liest Antwort aus input, vergleicht mit correct_style
  feedback <- psychTestR::reactive_page(function(answer, ...) {
    if (is.null(answer)) answer <- ""
    correct <- (answer == correct_style)
    composer <- if (correct) answer else correct_style
    key <- if (correct) "CORRECT_A" else "FALSE_A"
    psychTestR::one_button_page(
      body = psychTestR::i18n(key, sub = list(composer = composer)),
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
