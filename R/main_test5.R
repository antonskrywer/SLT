# Zellen fuer SLT version 5 (2 Schwierigkeiten x 2 Grammatiken)
SLT5_cells <- data.frame(
  cell       = 1:4,
  difficulty = c("normal", "normal", "easy", "easy"),
  grammar    = c("G1", "G2", "G1", "G2"),
  stringsAsFactors = FALSE
)

# Zaehlt begonnene Sessions je Zelle im Ergebnisordner
# 
# Liest alle Ergebnisdateien in \code{results_dir} und zaehlt, wie oft jede
# Zelle bereits zugeteilt wurde. Pilot-Sessions (Admin-Testlaeufe,
# Dateiname enthaelt "pilot=true") werden nicht gezaehlt. Unlesbare Dateien
# werden uebersprungen (Warnung im Log), damit eine defekte Datei nie den
# Teststart blockiert.
# 
# @param results_dir psychTestR-Ergebnisordner (\code{opt$results_dir})
# @param label Modul-Label, unter dem die Zelle gespeichert wurde
# @return Integer-Vektor der Laenge 4 (Anzahl je Zelle 1-4)
SLT5_count_cells <- function(results_dir, label = "SLT") {
  counts <- integer(4)
  files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
  files <- files[!grepl("pilot=true", basename(files), fixed = TRUE)]
  for (f in files) {
    cell <- tryCatch({
      res <- as.list(readRDS(f))
      res[[label]]$cell
    }, error = function(e) {
      messagef("SLT5_count_cells: Datei nicht lesbar (%s): %s", basename(f), conditionMessage(e))
      NULL
    })
    if (length(cell) == 1 && !is.na(cell) && cell %in% 1:4) counts[cell] <- counts[cell] + 1L
  }
  counts
}

# Waehlt die am schwaechsten besetzte Zelle (Gleichstand: Zufall)
SLT5_pick_cell <- function(counts) {
  cand <- which(counts == min(counts))
  if (length(cand) == 1) cand else sample(cand, 1)
}

# Zuteilung auf eine der 4 Zellen und sofortiges Zwischenspeichern
# 
# Muss innerhalb eines \code{code_block()} laufen (pro Session, nicht beim
# App-Start). Speichert cell/difficulty/grammar als Ergebnis und als
# Globals; das anschliessende \code{elt_save_results_to_disk(complete =
# FALSE)} sorgt dafuer, dass auch abgebrochene Sessions mitgezaehlt werden.
SLT5_assign_cell <- function(label = "SLT") {
  psychTestR::code_block(function(state, opt, ...) {
    counts <- SLT5_count_cells(opt$results_dir, label = label)
    cell   <- SLT5_pick_cell(counts)
    row    <- SLT5_cells[SLT5_cells$cell == cell, ]
    messagef("SLT5: Zellenstand %s -> Zelle %d (%s/%s)",
             paste(counts, collapse = "/"), cell, row$difficulty, row$grammar)
    psychTestR::set_global("slt5_cell", row, state)
    psychTestR::save_result(state, label = "cell", value = cell)
    psychTestR::save_result(state, label = "difficulty", value = row$difficulty)
    psychTestR::save_result(state, label = "grammar", value = row$grammar)
  })
}

# --- Begruessung + HALT (laufen in SLT() VOR begin_module(label)) -----------

# Begruessung und Ueberblick
SLT5_welcome_pages <- function() {
  psychTestR::join(
    psychTestR::one_button_page(
      body = shiny::div(shiny::h4(psychTestR::i18n("SLT5_WELCOME_TITLE")),
                        shiny::p(psychTestR::i18n("SLT5_WELCOME"))),
      button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::one_button_page(
      body = shiny::p(psychTestR::i18n("SLT5_OVERVIEW")),
      button_text = psychTestR::i18n("CONTINUE"))
  )
}

# HALT-Kopfhoerer-Check (Entscheidungen 2026-10-15, Anton): ohne Frequenztest,
# angenommene Kopfhoerer-Quote 70 % (Prolific mit Kopfhoerer-Pflicht), bei
# Nichtbestehen sofortiger Abbruch.
# Laeuft VOR der Zellenzuteilung -> Ausgeschlossene belegen keine Zelle.
#
# HALT_stop_page() (HALT 0.12.4) speichert die Ergebnisse nicht
# (elt_save_results_to_disk() steckt wirkungslos in einem code_block) und
# bietet keine Prolific-Rueckleitung. Daher:
# - Geraete-Ausschluss in HALT aus (devices_exclude = FALSE); danach
#   entscheidet SLT5_halt_passed() anhand von HALT.device_screening$correct.
# - Die uebrigen HALT-Abbrueche (Links/rechts-Bild, Stereo-Kanaele, 5x
#   Lautstaerke nicht eingestellt) laesst auto_config() bei screening_parts =
#   TRUE nicht abschalten (lr_*_exclude wird auf TRUE zurueckgesetzt). Fuer sie
#   wird HALT_stop_page() beim Aufbau durch unsere speichernde Stopp-Seite
#   ersetzt (Entscheidung Anton 2026-10-16, nur fuer HALT 0.12.4).
SLT5_halt <- function(label = "HALT", dict = SLT::SLT_dict, screenout_code = NULL) {
  # HALT ist nur Suggests (Erhebung laeuft ohne HALT, Entscheidung 19.10.)
  if (!requireNamespace("HALT", quietly = TRUE)) {
    stop("SLT5: with_halt = TRUE braucht das Paket HALT ",
         "(remotes::install_github(\"KilianSander/HALT\")) - oder with_halt = FALSE setzen.",
         call. = FALSE)
  }
  halt <- SLT5_with_halt_stop_page(
    # HALT ruft HALT_stop_page(dict) bzw. (dict = dict) mit HALT_dict auf -> ignorieren
    function(...) SLT5_halt_internal_stop_page(label, dict, screenout_code),
    HALT::HALT(label = label,
               config = HALT::auto_config(frequency_check = FALSE, baserate_hp = 0.7,
                                          devices_exclude = FALSE),
               dict = HALT::HALT_dict))
  psychTestR::join(
    halt,
    psychTestR::conditional(
      test = function(state, ...) !SLT5_halt_passed(state, label),
      logic = SLT5_halt_stop_page(dict, screenout_code, reason = "device"))
  )
}

# Wertet `expr` (Aufbau der HALT-Timeline) aus, waehrend HALT:::HALT_stop_page
# durch `stop_page` ersetzt ist; danach wird das Original wiederhergestellt.
# HALT ruft HALT_stop_page() nur beim Aufbau auf, nicht zur Laufzeit.
SLT5_with_halt_stop_page <- function(stop_page, expr) {
  if (utils::packageVersion("HALT") != "0.12.4") {
    warning("SLT5: HALT ", utils::packageVersion("HALT"), " statt 0.12.4 installiert - ",
            "HALT-eigene Abbrueche speichern evtl. nicht. Ersatz der Stopp-Seite uebersprungen.",
            call. = FALSE)
    return(expr)
  }
  orig <- get("HALT_stop_page", envir = asNamespace("HALT"))
  utils::assignInNamespace("HALT_stop_page", stop_page, ns = "HALT")
  on.exit(utils::assignInNamespace("HALT_stop_page", orig, ns = "HALT"), add = TRUE)
  force(expr)
}

# Ersatz fuer HALT_stop_page(): laeuft innerhalb des HALT-Moduls. Grund des
# Abbruchs aus HALTs lokalen Antworten bestimmen und speichern, HALT-Modul
# verlassen, dann unsere Stopp-Seite (speichert, Screen-out-Link).
SLT5_halt_internal_stop_page <- function(label, dict, screenout_code) {
  psychTestR::join(
    psychTestR::new_timeline(psychTestR::join(
      psychTestR::code_block(function(state, ...) {
        po4 <- psychTestR::get_local("po4", state)
        po5 <- psychTestR::get_local("po5", state)
        reason <- if (identical(po4, "right")) "lr_image"
                  else if (!is.null(po5) && !identical(po5, "stereo channels correct")) "lr_audio"
                  else "volume"
        psychTestR::set_global("slt5_halt_stop_reason", reason, state)
      }),
      psychTestR::end_module()), dict = dict),
    SLT5_halt_stop_page(dict, screenout_code)
  )
}

# TRUE, wenn HALT das Geraet als Kopfhoerer eingestuft hat (oder kein
# Screening-Ergebnis vorliegt, z. B. bei abgeschalteten Screening-Teilen)
SLT5_halt_passed <- function(state, label = "HALT") {
  res <- as.list(psychTestR::get_results(state, complete = FALSE))
  ds  <- res[[label]]$device_screening
  if (is.null(ds)) return(TRUE)
  isTRUE(if (is.list(ds) || is.data.frame(ds)) ds$correct else ds)
}

# Stopp-Seite fuer HALT-Ausschluss: speichert zuerst, dann Endseite.
# `reason` = Abbruchgrund; NULL = aus Global slt5_halt_stop_reason
SLT5_halt_stop_page <- function(dict = SLT::SLT_dict, screenout_code = NULL, reason = NULL) {
  psychTestR::join(
    # new_timeline mit dict, sonst setzt psychTestR die Sprachen dieses
    # Zweigs auf "en" und de/de_f werden fuer den ganzen Test abgelehnt
    psychTestR::new_timeline(SLT5_save_prolific_ids_halt(reason), dict = dict),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::new_timeline(
      psychTestR::final_page(shiny::div(
        shiny::h4(psychTestR::i18n("SLT5_HALT_STOP_TITLE")),
        shiny::p(psychTestR::i18n("SLT5_HALT_STOP")),
        SLT5_prolific_link(screenout_code))),
      dict = dict)
  )
}

# Prolific-ID auch bei HALT-Ausschluss speichern (eigenes Modul, da ausserhalb
# des SLT-Moduls)
SLT5_save_prolific_ids_halt <- function(reason = NULL) {
  psychTestR::join(psychTestR::begin_module("SLT5_screenout"),
                   SLT5_save_prolific_ids(),
                   psychTestR::code_block(function(state, ...) {
                     psychTestR::save_result(state, "halt_excluded", TRUE)
                     r <- if (is.null(reason)) psychTestR::get_global("slt5_halt_stop_reason", state) else reason
                     psychTestR::save_result(state, "halt_stop_reason", if (is.null(r)) NA_character_ else r)
                   }),
                   psychTestR::end_module())
}

# --- Screening nach der Zuteilung ----------------------------------------------

# Lautstaerke einstellen mit Klavier-Kalibrierton (Einzeltoene, keine Melodie)
SLT5_volume_page <- function(audio_dir) {
  psychTestR::one_button_page(
    body = shiny::div(
      shiny::h4(psychTestR::i18n("SLT5_VOLUME_TITLE")),
      shiny::p(psychTestR::i18n("SLT5_VOLUME")),
      get_audio_element(file.path(audio_dir, "misc", "volume_calibration.mp3"))),
    button_text = psychTestR::i18n("CONTINUE"))
}

# Tonhoehen-Check: 3 Aufgaben (Ganzton, hoeher/tiefer). Speichert pro Aufgabe
# Antwort, Korrektheit und RT sowie den Gesamtscore (Kriterium 3/3 erst in der
# Auswertung, kein Abbruch im Test).
SLT5_pitch_check <- function(audio_dir, autoplay = TRUE) {
  items <- SLT::SLT5_items$pitch_check
  pages <- lapply(seq_len(nrow(items)), function(i) {
    item <- items[i, ]
    audio_NAFC_page_rt(
      label     = item$pitch_id,
      prompt    = shiny::p(psychTestR::i18n("SLT5_PITCH_PROMPT")),
      audio_url = sprintf("%s/%s.mp3", audio_dir, item$file_name),
      choices   = c("hoeher", "tiefer"),
      labels    = list(psychTestR::i18n("SLT5_HIGHER"), psychTestR::i18n("SLT5_LOWER")),
      get_answer = function(input, ...) {
        rt <- compute_rt(input)
        list(answer = gsub("answer", "", input$last_btn_pressed),
             rt_decision_ms = rt$rt_decision_ms, rt_total_ms = rt$rt_total_ms)
      },
      save_answer = FALSE,
      autoplay  = autoplay,
      on_complete = function(answer, state, ...) {
        res <- psychTestR::get_global("slt5_pitch", state)
        row <- data.frame(pitch_id = item$pitch_id, answer = answer$answer,
                          correct = as.integer(answer$answer == item$correct),
                          rt_decision_ms = answer$rt_decision_ms, rt_total_ms = answer$rt_total_ms)
        psychTestR::set_global("slt5_pitch", rbind(res, row), state)
      })
  })
  psychTestR::join(
    psychTestR::code_block(function(state, ...) psychTestR::set_global("slt5_pitch", NULL, state)),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PITCH_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    pages,
    psychTestR::code_block(function(state, ...) {
      res <- psychTestR::get_global("slt5_pitch", state)
      psychTestR::save_result(state, label = "pitch_check", value = res)
      psychTestR::save_result(state, label = "pitch_score", value = sum(res$correct))
      messagef("SLT5: Tonhoehen-Check %d/%d", sum(res$correct), nrow(res))
    })
  )
}

# --- Exposition ----------------------------------------------------------------

# Instruktion inkl. Oddball-Hoerbeispiel (mit Bedienelementen, beliebig oft)
SLT5_expo_instructions <- function(audio_dir) {
  psychTestR::join(
    psychTestR::one_button_page(
      body = shiny::p(psychTestR::i18n("SLT5_EXPO_INTRO")),
      button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::p(psychTestR::i18n("SLT5_EXPO_ODDBALL")),
        shiny::p(psychTestR::i18n("SLT5_EXPO_EXAMPLE")),
        get_audio_element(file.path(audio_dir, "misc", "oddball_example.mp3"))),
      button_text = psychTestR::i18n("CONTINUE"))
  )
}

# Ein Expositionsblock: Audio startet automatisch, ist nicht steuerbar und die
# Seite geht nach dem Ende von selbst weiter. Laedt das Audio nicht (z. B.
# Netzwerkfehler), erscheint ein Weiter-Knopf, damit niemand haengen bleibt;
# das wird als audio_error gespeichert.
SLT5_expo_block_page <- function(block, audio_dir) {
  psychTestR::reactive_page(function(state, ...) {
    cell  <- psychTestR::get_global("slt5_cell", state)
    expo  <- SLT::SLT5_items$exposure
    item  <- expo[expo$difficulty == cell$difficulty & expo$grammar == cell$grammar & expo$block == block, ]
    url   <- sprintf("%s/%s.mp3", audio_dir, item$file_name)
    js <- shiny::HTML("
      (function() {
        var a = document.getElementById('expo_audio');
        var started = false, pauses = 0;
        a.addEventListener('play', function() {
          if (!started) { started = true; Shiny.setInputValue('expo_start', Date.now()); }
        });
        // Pause (z. B. Medientaste am Kopfhoerer) -> sofort weiterspielen und zaehlen
        a.addEventListener('pause', function() {
          if (!a.ended) { pauses += 1; Shiny.setInputValue('expo_pauses', pauses); a.play(); }
        });
        a.addEventListener('ended', function() {
          Shiny.setInputValue('expo_end', Date.now());
          Shiny.setInputValue('expo_error', false);
          trigger_button('next');
        });
        a.addEventListener('error', function() {
          Shiny.setInputValue('expo_error', true);
          document.getElementById('next').style.display = 'inline-block';
        });
        var p = a.play();
        if (p !== undefined) p.catch(function() {
          document.getElementById('expo_play').style.display = 'inline-block';
        });
      })();")
    ui <- shiny::div(
      shiny::p(psychTestR::i18n("SLT5_EXPO_BLOCK", sub = list(n = block))),
      shiny::tags$audio(shiny::tags$source(src = url, type = "audio/mp3"),
                        id = "expo_audio", preload = "auto"),
      # Fallback, falls der Browser Autoplay blockiert
      shiny::tags$button(shiny::HTML("&#9654;"), id = "expo_play", type = "button",
                         style = "display:none",
                         onclick = "document.getElementById('expo_audio').play(); this.style.display='none';"),
      psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE"), style = "display:none"),
      shiny::tags$script(js))
    psychTestR::page(
      ui = ui, label = sprintf("expo_block%d", block), save_answer = FALSE,
      get_answer = function(input, ...) {
        list(start = null_or_else(input$expo_start, NA_real_),
             end   = null_or_else(input$expo_end, NA_real_),
             pauses = null_or_else(input$expo_pauses, 0L),
             audio_error = isTRUE(input$expo_error))
      },
      on_complete = function(answer, state, ...) {
        res <- psychTestR::get_global("slt5_expo", state)
        row <- data.frame(block = block, file = item$file_name,
                          n_oddballs = item$n_oddballs,
                          listen_ms = answer$end - answer$start,
                          pauses = answer$pauses,
                          audio_error = answer$audio_error,
                          oddball_answer = NA_integer_)
        psychTestR::set_global("slt5_expo", rbind(res, row), state)
      })
  })
}

# Zaehlfrage nach jedem Block (Zahlenfeld, nur ganze Zahlen 0-99)
SLT5_oddball_question <- function(block) {
  psychTestR::text_input_page(
    label = sprintf("oddball_count%d", block),
    prompt = shiny::div(shiny::p(psychTestR::i18n("SLT5_ODDBALL_QUESTION")),
                        shiny::p(psychTestR::i18n("SLT5_ODDBALL_HINT"), style = "font-size:small")),
    one_line = TRUE, save_answer = FALSE, width = "100px",
    button_text = psychTestR::i18n("CONTINUE"),
    validate = function(answer, ...) {
      if (grepl("^[0-9]{1,2}$", trimws(answer))) TRUE else psychTestR::i18n("SLT5_ERR_NUMBER")
    },
    on_complete = function(answer, state, ...) {
      res <- psychTestR::get_global("slt5_expo", state)
      res$oddball_answer[res$block == block] <- as.integer(trimws(answer))
      psychTestR::set_global("slt5_expo", res, state)
    })
}

# Ganze Expositionsphase: Instruktion, 4 x (Block, Zaehlfrage, Pause), Speichern
SLT5_exposure <- function(audio_dir) {
  blocks <- lapply(1:4, function(b) psychTestR::join(
    SLT5_expo_block_page(b, audio_dir),
    SLT5_oddball_question(b),
    if (b < 4) psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_BREAK")),
                                           button_text = psychTestR::i18n("CONTINUE"))
  ))
  psychTestR::join(
    psychTestR::code_block(function(state, ...) psychTestR::set_global("slt5_expo", NULL, state)),
    SLT5_expo_instructions(audio_dir),
    do.call(psychTestR::join, blocks),
    psychTestR::code_block(function(state, ...) {
      res <- psychTestR::get_global("slt5_expo", state)
      res$oddball_error <- res$oddball_answer - res$n_oddballs
      psychTestR::save_result(state, label = "exposure", value = res)
      messagef("SLT5: Oddball-Abweichungen %s", paste(res$oddball_error, collapse = "/"))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )
}

# --- Bewertungsseiten (A-not-A und Probeton) -----------------------------------

# 6-stufige Bewertung; Knoepfe erscheinen erst nach Audio-Ende (audio_NAFC_page_rt)
SLT5_rating_page <- function(label, prompt, audio_url, label_prefix, autoplay, on_complete) {
  audio_NAFC_page_rt(
    label     = label,
    prompt    = prompt,
    audio_url = audio_url,
    choices   = as.character(1:6),
    labels    = lapply(1:6, function(k) psychTestR::i18n(sprintf("%s_%d", label_prefix, k))),
    get_answer = function(input, ...) {
      rt <- compute_rt(input)
      list(rating = as.integer(gsub("answer", "", input$last_btn_pressed)),
           rt_decision_ms = rt$rt_decision_ms, rt_total_ms = rt$rt_total_ms)
    },
    save_answer = FALSE,
    autoplay  = autoplay,
    on_complete = on_complete)
}

# Uebungsseiten (Ergebnis wird mitgespeichert, aber nicht ausgewertet)
SLT5_practice_pages <- function(type, prompt_key, label_prefix, audio_dir, autoplay) {
  items <- SLT::SLT5_items$practice
  items <- items[items$type == type, ]
  lapply(seq_len(nrow(items)), function(i) {
    item <- items[i, ]
    SLT5_rating_page(
      label = item$practice_id,
      prompt = shiny::div(shiny::p(psychTestR::i18n("SLT5_PRACTICE_LABEL"), style = "font-size:small"),
                          shiny::p(psychTestR::i18n(prompt_key))),
      audio_url = sprintf("%s/%s.mp3", audio_dir, item$file_name),
      label_prefix = label_prefix, autoplay = autoplay,
      on_complete = function(answer, state, ...) {
        psychTestR::save_result(state, label = item$practice_id, value = answer$rating)
      })
  })
}

# A-not-A: Instruktion, 2 Uebungen, 48 Items in zufaelliger Reihenfolge.
# is_grammatical = Item stammt aus der Grammatik, die die Person gehoert hat.
SLT5_anota <- function(audio_dir, autoplay = TRUE) {
  items <- SLT::SLT5_items$anota
  n <- nrow(items)
  pages <- lapply(seq_len(n), function(pos) {
    psychTestR::reactive_page(function(state, ...) {
      ord  <- psychTestR::get_global("slt5_anota_order", state)
      item <- items[ord[pos], ]
      SLT5_rating_page(
        label = sprintf("anota_pos%02d", pos),
        prompt = shiny::div(
          shiny::p(psychTestR::i18n("SLT5_PROGRESS", sub = list(num = pos, total = n)), style = "font-size:small"),
          shiny::p(psychTestR::i18n("SLT5_ANOTA_PROMPT"))),
        audio_url = sprintf("%s/%s.mp3", audio_dir, item$file_name),
        label_prefix = "SLT5_RATING", autoplay = autoplay,
        on_complete = function(answer, state, ...) {
          cell <- psychTestR::get_global("slt5_cell", state)
          res  <- psychTestR::get_global("slt5_anota", state)
          row  <- data.frame(position = pos, item_id = item$slt5_item_id,
                             source_grammar = item$source_grammar, E_stratum = item$E_stratum,
                             is_grammatical = as.integer(item$source_grammar == cell$grammar),
                             rating = answer$rating,
                             rt_decision_ms = answer$rt_decision_ms, rt_total_ms = answer$rt_total_ms)
          psychTestR::set_global("slt5_anota", rbind(res, row), state)
        })
    })
  })
  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("slt5_anota", NULL, state)
      psychTestR::set_global("slt5_anota_order", sample.int(n), state)
    }),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_ANOTA_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PRACTICE_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    SLT5_practice_pages("anota", "SLT5_ANOTA_PROMPT", "SLT5_RATING", audio_dir, autoplay),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PRACTICE_END")),
                                button_text = psychTestR::i18n("CONTINUE")),
    pages,
    psychTestR::code_block(function(state, ...) {
      psychTestR::save_result(state, label = "anota", value = psychTestR::get_global("slt5_anota", state))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )
}

# Probeton: Instruktion, 1 Uebung, 30 Aufgaben in zufaelliger Reihenfolge.
# Gespeichert wird u. a. die TP des Probetons unter der eigenen Grammatik und
# Schwierigkeit (tp_own, level_own: 1 = staerkster Uebergang ... 4, 0 = illegal).
SLT5_probe <- function(audio_dir, autoplay = TRUE) {
  items <- SLT::SLT5_items$probe
  n <- nrow(items)
  pages <- lapply(seq_len(n), function(pos) {
    psychTestR::reactive_page(function(state, ...) {
      ord  <- psychTestR::get_global("slt5_probe_order", state)
      item <- items[ord[pos], ]
      SLT5_rating_page(
        label = sprintf("probe_pos%02d", pos),
        prompt = shiny::div(
          shiny::p(psychTestR::i18n("SLT5_PROBE_PROGRESS", sub = list(num = pos, total = n)), style = "font-size:small"),
          shiny::p(psychTestR::i18n("SLT5_PROBE_PROMPT"))),
        audio_url = sprintf("%s/%s.mp3", audio_dir, item$file_name),
        label_prefix = "SLT5_PROBE", autoplay = autoplay,
        on_complete = function(answer, state, ...) {
          cell  <- psychTestR::get_global("slt5_cell", state)
          other <- if (cell$grammar == "G1") "G2" else "G1"
          row <- data.frame(position = pos, probe_id = item$slt5_probe_id,
                            tp_own   = item[[sprintf("tp_%s_%s", cell$grammar, cell$difficulty)]],
                            tp_other = item[[sprintf("tp_%s_%s", other, cell$difficulty)]],
                            level_own = item[[sprintf("level_%s", cell$grammar)]],
                            rating = answer$rating,
                            rt_decision_ms = answer$rt_decision_ms, rt_total_ms = answer$rt_total_ms)
          res <- psychTestR::get_global("slt5_probe", state)
          psychTestR::set_global("slt5_probe", rbind(res, row), state)
        })
    })
  })
  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("slt5_probe", NULL, state)
      psychTestR::set_global("slt5_probe_order", sample.int(n), state)
    }),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PROBE_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PROBE_PRACTICE_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    SLT5_practice_pages("probe", "SLT5_PROBE_PROMPT", "SLT5_PROBE", audio_dir, autoplay),
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_PRACTICE_END")),
                                button_text = psychTestR::i18n("CONTINUE")),
    pages,
    psychTestR::code_block(function(state, ...) {
      psychTestR::save_result(state, label = "probe", value = psychTestR::get_global("slt5_probe", state))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )
}

# --- Explizites Wissen, Prolific, Abschluss ------------------------------------

SLT5_explicit <- function() {
  psychTestR::join(
    psychTestR::one_button_page(body = shiny::p(psychTestR::i18n("SLT5_EXPLICIT_INTRO")),
                                button_text = psychTestR::i18n("CONTINUE")),
    psychTestR::NAFC_page(
      label = "explicit_rule_noticed",
      prompt = shiny::p(psychTestR::i18n("SLT5_EXPLICIT_Q1")),
      choices = c("yes", "no"),
      labels = list(psychTestR::i18n("SLT5_YES"), psychTestR::i18n("SLT5_NO")),
      arrange_vertically = FALSE),
    psychTestR::text_input_page(
      label = "explicit_rule_description",
      prompt = shiny::p(psychTestR::i18n("SLT5_EXPLICIT_Q2")),
      one_line = FALSE, width = "500px", height = "120px",
      button_text = psychTestR::i18n("CONTINUE"),
      validate = function(answer, ...) {
        if (nzchar(trimws(answer))) TRUE else psychTestR::i18n("SLT5_ERR_TEXT")
      }),
    psychTestR::NAFC_page(
      label = "explicit_strategy",
      prompt = shiny::p(psychTestR::i18n("SLT5_EXPLICIT_Q3")),
      choices = c("guess", "feeling", "rule"),
      labels = list(psychTestR::i18n("SLT5_EXPLICIT_Q3_GUESS"),
                    psychTestR::i18n("SLT5_EXPLICIT_Q3_FEELING"),
                    psychTestR::i18n("SLT5_EXPLICIT_Q3_RULE")),
      arrange_vertically = TRUE)
  )
}

# Prolific-Parameter aus der URL speichern (PROLIFIC_PID, STUDY_ID, SESSION_ID)
SLT5_save_prolific_ids <- function() {
  psychTestR::code_block(function(state, ...) {
    p <- psychTestR::get_url_params(state)
    for (k in c("PROLIFIC_PID", "STUDY_ID", "SESSION_ID")) {
      if (!is.null(p[[k]])) psychTestR::save_result(state, label = tolower(k), value = p[[k]])
    }
  })
}

# Link zurueck zu Prolific (nur wenn ein Code gesetzt ist)
SLT5_prolific_link <- function(code) {
  if (is.null(code)) return(NULL)
  shiny::p(shiny::tags$a(psychTestR::i18n("SLT5_PROLIFIC_BUTTON"),
                         href = sprintf("https://app.prolific.com/submissions/complete?cc=%s", code),
                         class = "btn btn-default"))
}

# Abschluss: vollstaendig speichern, dann Endseite (ggf. mit Prolific-Link).
# `finished` statt psychTestRs complete-Flag auswerten: HALT::HALT() speichert
# am Modulende mit complete = TRUE, das Flag ist danach dauerhaft gesetzt.
SLT5_final <- function(completion_code = NULL) {
  psychTestR::join(
    psychTestR::code_block(function(state, ...) psychTestR::save_result(state, "finished", TRUE)),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(shiny::div(
      shiny::h4(psychTestR::i18n("SLT5_END_TITLE")),
      shiny::p(psychTestR::i18n("SLT5_END")),
      if (!is.null(completion_code)) shiny::p(psychTestR::i18n("SLT5_END_PROLIFIC")),
      SLT5_prolific_link(completion_code)))
  )
}

#' Haupt-Timeline fuer SLT version 5
#'
#' Ablauf (Begruessung und HALT laufen vorher in \code{SLT()}):
#' Zuteilung (ausgeglichen ueber 4 Zellen) -> Lautstaerke + Tonhoehen-Check ->
#' Exposition (4 Bloecke mit Oddball-Zaehlfrage) -> A-not-A (48 Items) ->
#' Probeton (30 Items) -> Fragen zum expliziten Wissen.
#'
#' @param label Modul-Label (muss dem Label in \code{SLT()} entsprechen)
#' @param audio_dir Basis-URL der Audio-Dateien (ohne abschliessenden "/")
#' @param dict psychTestR-Dictionary
#' @param autoplay Autoplay fuer Item-Audio
#' @param with_halt Wurde der HALT-Check vorgeschaltet? (wird nur gespeichert)
#' @param prolific_completion_code Prolific-Completion-Code; NULL = kein
#'   Prolific-Link auf der Endseite (z. B. private Erhebung)
#' @param dev_skip NUR fuer Entwicklungstests: Teile, die uebersprungen werden
#'   (\code{"screening"}, \code{"exposure"}, \code{"anota"}, \code{"probe"}).
#'   Wird als Ergebnis gespeichert, damit solche Laeufe erkennbar bleiben.
#' @export
main_test5 <- function(label = "SLT",
                       audio_dir,
                       dict = SLT::SLT_dict,
                       autoplay = TRUE,
                       with_halt = TRUE,
                       dev_skip = character(),
                       prolific_completion_code = NULL, ...) {
  run <- function(part) !part %in% dev_skip
  psychTestR::join(
    # Zuteilung + Zwischenspeichern: erst nach Begruessung/HALT, da Speichern
    # vor der ersten Seite in psychTestR scheitert
    SLT5_assign_cell(label = label),
    SLT5_save_prolific_ids(),
    psychTestR::code_block(function(state, ...) {
      psychTestR::save_result(state, "with_halt", with_halt)
      if (length(dev_skip)) psychTestR::save_result(state, "dev_skip", paste(dev_skip, collapse = ","))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE),
    if (run("screening")) SLT5_volume_page(audio_dir),
    if (run("screening")) SLT5_pitch_check(audio_dir, autoplay = autoplay),
    if (run("exposure")) SLT5_exposure(audio_dir),
    if (run("anota")) SLT5_anota(audio_dir, autoplay = autoplay),
    if (run("probe")) SLT5_probe(audio_dir, autoplay = autoplay),
    SLT5_explicit(),
    SLT5_final(prolific_completion_code)
  )
}
