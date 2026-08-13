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

### GEÄNDERT START — get_demo_urls(): Audioquelle abhaengig von version ###
#' Ermittelt die Audio-URLs fuer die zwei Demo-Trials (Style A / Style B).
#'
#' version 1/2 (Markov, Block-Design): unveraendert fest block_07_tc4_p80
#' (mittlere Schwierigkeit, bewaehrter Demo-Kandidat).
#'
#' version 3/4 (Reber-FSG / Loui-FSG): je EIN zufaelliges Item pro Style
#' aus SLT_item_bank3 gezogen (per grammar_type gefiltert). Dadurch keine
#' hartkodierten Dateinamen noetig, und die Demo zeigt bei jedem Testlauf
#' ein anderes Beispielpaar aus dem jeweiligen Pool.
#'
#' @param audio_dir Basis-URL der Audiodateien
#' @param version SLT-version (1-4)
#' @param grammar_type "reber_fsg"/"loui_fsg", nur fuer version 3/4 noetig
get_demo_urls <- function(audio_dir, version = 1, grammar_type = NULL) {
  if (version %in% c(3, 4)) {
    stopifnot(
      "grammar_type muss fuer version 3/4 gesetzt sein" = !is.null(grammar_type)
    )
    pool <- SLT::SLT_item_bank3[SLT::SLT_item_bank3$grammar_type == grammar_type, ]
    stopifnot(
      "Kein Item-Pool fuer diesen grammar_type in SLT_item_bank3 gefunden" =
        nrow(pool) > 0
    )
    poolA <- pool[pool$style == "A", ]
    poolB <- pool[pool$style == "B", ]
    item_A <- poolA[sample(nrow(poolA), 1), ]
    item_B <- poolB[sample(nrow(poolB), 1), ]
    list(
      A = sprintf("%s/%s.mp3", audio_dir, item_A$file_name),
      B = sprintf("%s/%s.mp3", audio_dir, item_B$file_name)
    )
  } else {
    list(
      A = paste0(audio_dir, "/block_07_tc4_p80_styleA_01.mp3"),
      B = paste0(audio_dir, "/block_07_tc4_p80_styleB_01.mp3")
    )
  }
}
### GEÄNDERT ENDE ###

### GEÄNDERT START — instructions() nimmt version/grammar_type entgegen ###
# ── Haupt-Instruktionsfunktion ─────────────────────────────────────────────
instructions <- function(audio_dir, version = 1, grammar_type = NULL) {

  demo_urls <- get_demo_urls(audio_dir, version = version, grammar_type = grammar_type)

  psychTestR::join(
    # 1. Intro
    info_page("DEMO_INTRO"),

    # 2. Demo-Trial A + Feedback
    demo_trial_page(audio_url = demo_urls$A, correct_style = "A"),

    # 3. Demo-Trial B + Feedback
    demo_trial_page(audio_url = demo_urls$B, correct_style = "B"),

    # 4. Outro
    info_page("DEMO_OUTRO")
  )
}
### GEÄNDERT ENDE ###
