# info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
#   psychTestR::one_button_page(shiny::div(psychTestR::i18n(id, html = TRUE),
#                                          style = style),
#                               button_text = psychTestR::i18n("CONTINUE"))
# }
# ── Hilfsfunktion: einfache Infoseite (existierend, unverändert) ────────────
info_page <- function(id, style = "text-align:justify; margin-left:20%;margin-right:20%") {
  psychTestR::one_button_page(
    shiny::div(psychTestR::i18n(id, html = TRUE), style = style),
    button_text = psychTestR::i18n("CONTINUE")
  )
}

# ── Demo-Item-Seite mit Audio (nicht-autoplay) ───────────────────────────────
demo_item_page <- function(audio_dir, style_label, i18n_key) {
  audio_url <- file.path(audio_dir, sprintf("demo_%s.mp3", style_label))
  audio     <- get_audio_element(url = audio_url, autoplay = FALSE)
  body <- shiny::div(
    shiny::div(
      psychTestR::i18n(i18n_key),
      style = "text-align:justify; margin-left:20%; margin-right:20%; margin-bottom:20px"
    ),
    shiny::p(audio)
  )
  psychTestR::one_button_page(body = body,
                              button_text = psychTestR::i18n("CONTINUE"))
}

# ── Demo-Feedback-Seite (fest: "Group A" war korrekt / "Group B" war korrekt)
demo_feedback_page <- function(correct_group) {
  # correct_group: "A" oder "B"
  key <- if (correct_group == "A") "CORRECT_A" else "CORRECT_B"
  psychTestR::one_button_page(
    body = psychTestR::i18n(key, sub = list(composer = correct_group)),
    button_text = psychTestR::i18n("CONTINUE")
  )
}

# ── Haupt-Instruktionsfunktion (nur für version == 2) ───────────────────────
instructions <- function(audio_dir) {
  psychTestR::join(
    # 1. Erklärungsseite: Aufgabe beschreiben
    info_page("DEMO_INTRO"),

    # 2. Demo-Item Group A + Feedback
    demo_item_page(audio_dir, style_label = "A", i18n_key = "DEMO_ITEM_A"),
    demo_feedback_page(correct_group = "A"),

    # 3. Demo-Item Group B + Feedback
    demo_item_page(audio_dir, style_label = "B", i18n_key = "DEMO_ITEM_B"),
    demo_feedback_page(correct_group = "B"),

    # 4. Übergangsseite zum Haupttest
    info_page("DEMO_OUTRO")
  )
}
