# =============================================================================
# SLT version 6 (Demo, Stand 21.10.2026)
# Bauplan: Siegelman, Bogaerts & Frost (2017), Formen -> Toene.
# Plan: SLT_analysis_and_publications/slt6/SLT6_Umsetzungsplan.md
#
# Ablauf: Zuteilung L1/L2 -> Lautstaerke + Kopfhoerer-Hinweis -> Technik-Test
# (7 Pieptoene, ~15 s) -> Hoerbeispiel Klarinettenton + Uebung -> 4 Lernbloecke
# (Einzeltoene im Browser, Leertasten-RT) -> 34 Testaufgaben (2AFC) -> explizites
# Wissen + Kopfhoerer-Frage -> Ende.
#
# Texte: fuer die Demo direkt auf Deutsch (Sie-Form), noch nicht im SLT_dict.
# Audio: audio_dir enthaelt samples/<farbe>_<Ton>.wav und items/<datei>.mp3.
#   Ein lokaler Ordner wird per shiny::addResourcePath() bereitgestellt.
# =============================================================================

SLT6_n_beeps <- 7     # Technik-Test, Entscheidung 21.10.: ~15 s

# --- Zuteilung L1/L2 (ausgeglichen ueber begonnene Sessions, wie v5) ---------

SLT6_count_languages <- function(results_dir, label = "SLT") {
  counts <- c(L1 = 0L, L2 = 0L)
  files <- list.files(results_dir, pattern = "\\.rds$", full.names = TRUE)
  files <- files[!grepl("pilot=true", basename(files), fixed = TRUE)]
  for (f in files) {
    lg <- tryCatch(as.list(readRDS(f))[[label]]$language, error = function(e) NULL)
    if (length(lg) == 1 && lg %in% names(counts)) counts[[lg]] <- counts[[lg]] + 1L
  }
  counts
}

SLT6_assign_language <- function(label = "SLT") {
  psychTestR::code_block(function(state, opt, ...) {
    counts <- SLT6_count_languages(opt$results_dir, label = label)
    cand <- names(counts)[counts == min(counts)]
    lg <- if (length(cand) == 1) cand else sample(cand, 1)
    messagef("SLT6: Sprachenstand %s -> %s", paste(counts, collapse = "/"), lg)
    psychTestR::set_global("slt6_language", lg, state)
    psychTestR::save_result(state, label = "language", value = lg)
  })
}

# --- Hilfen ---------------------------------------------------------------------

SLT6_text_page <- function(...) {
  psychTestR::one_button_page(body = shiny::div(...), button_text = psychTestR::i18n("CONTINUE"))
}

SLT6_js_module <- function() {
  f <- system.file("js", "slt6_exposure.js", package = "SLT")
  if (!nzchar(f)) stop("SLT6: inst/js/slt6_exposure.js nicht gefunden.", call. = FALSE)
  shiny::tags$script(shiny::HTML(paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")))
}

# --- Seite mit Ton-Ablauf und Leertaste (Technik-Test, Uebung, Lernbloecke) ------
#
# schedule: data.frame(t [s], tone, odd, odd_id, condition). Start per Knopf (der
# Klick startet zugleich die Audioausgabe im Browser). Am Ende schreibt das JS das
# Ergebnis als JSON in input$slt6_result und geht von selbst weiter.
SLT6_timed_page <- function(label, title, text, schedule, audio_dir, on_result) {
  sched_json <- jsonlite::toJSON(schedule, dataframe = "rows", auto_unbox = TRUE)
  tones_json <- jsonlite::toJSON(SLT::SLT6_items$tones)
  js <- sprintf("
    (function () {
      var btn = document.getElementById('slt6_start'), msg = document.getElementById('slt6_msg'),
          bar = document.getElementById('slt6_bar');
      if (!window.slt6PlayerPromise)
        window.slt6PlayerPromise = SLT6Exposure.load({ baseUrl: '%s/samples/', tones: %s });
      window.slt6PlayerPromise.then(function () { btn.disabled = false; btn.textContent = 'Start'; })
        .catch(function (e) { msg.textContent = 'Fehler beim Laden der Töne: ' + e.message; });
      btn.addEventListener('click', function () {
        btn.style.display = 'none'; btn.blur(); msg.textContent = 'Läuft …';
        window.slt6PlayerPromise.then(function (player) {
          player.run({
            schedule: %s,
            onProgress: function (p) { bar.style.width = (100 * p) + '%%'; },
            onDone: function (res) {
              msg.textContent = 'Fertig.';
              Shiny.setInputValue('slt6_result', JSON.stringify(res));
              setTimeout(function () { trigger_button('next'); }, 300);
            },
            onError: function (err) { msg.textContent = 'Fehler: ' + err.message; btn.style.display = 'inline-block'; }
          });
        });
      });
    })();", audio_dir, tones_json, sched_json)
  ui <- shiny::div(
    shiny::h4(title),
    shiny::p(text),
    shiny::tags$button("Lädt …", id = "slt6_start", type = "button", disabled = "disabled",
                       class = "btn btn-default", style = "margin: 10px 0"),
    shiny::div(style = "height:10px;background:#ddd;border-radius:5px;margin:15px auto;max-width:400px",
               shiny::div(id = "slt6_bar", style = "height:100%;width:0;background:#3b7dd8;border-radius:5px")),
    shiny::p(id = "slt6_msg", style = "color:#666"),
    psychTestR::trigger_button("next", psychTestR::i18n("CONTINUE"), style = "display:none"),
    SLT6_js_module(),
    shiny::tags$script(shiny::HTML(js)))
  psychTestR::page(
    ui = ui, label = label, save_answer = FALSE,
    get_answer = function(input, ...) {
      if (is.null(input$slt6_result)) return(NULL)
      jsonlite::fromJSON(input$slt6_result)
    },
    on_complete = function(answer, state, ...) on_result(answer, state))
}

SLT6_summarise_rt <- function(res) {
  od <- as.data.frame(res$oddballs)
  rts <- od$rt[!is.na(od$rt)]
  data.frame(n = nrow(od), hits = res$hits, false_alarms = res$false_alarms,
             rt_mean = if (length(rts)) mean(rts) else NA_real_,
             rt_sd = if (length(rts) > 1) stats::sd(rts) else NA_real_,
             output_latency_ms = if (is.null(res$meta$outputLatency)) NA_real_ else 1000 * res$meta$outputLatency,
             timing_method = res$meta$timing_method)
}

# --- Technik-Test (Entscheidung B, 21.10.) ---------------------------------------

SLT6_technik <- function(audio_dir) {
  psychTestR::reactive_page(function(state, ...) {
    gaps <- stats::runif(SLT6_n_beeps - 1, 1.6, 2.4)
    sched <- data.frame(t = cumsum(c(1, gaps)), tone = "BEEP", odd = TRUE,
                        odd_id = sprintf("beep_%d", seq_len(SLT6_n_beeps)), condition = "beep")
    SLT6_timed_page(
      "technik", "Reaktionstest",
      "Sie hören gleich einige kurze Pieptöne. Drücken Sie bei jedem Piep so schnell wie möglich die Leertaste. Klicken Sie auf Start, wenn Sie bereit sind.",
      sched, audio_dir,
      on_result = function(res, state) {
        psychTestR::save_result(state, "technik", res)
        s <- SLT6_summarise_rt(res)
        psychTestR::save_result(state, "technik_summary", s)
        messagef("SLT6: Technik-Test %d/%d, RT %.0f ms", s$hits, s$n, s$rt_mean)
      })
  })
}

# --- Instruktion, Uebung, Lernbloecke ---------------------------------------------

SLT6_practice <- function(audio_dir) {
  p <- SLT::SLT6_items$practice
  sched <- data.frame(t = p$onset_ms / 1000, tone = p$tone, odd = p$odd,
                      odd_id = ifelse(p$odd, sprintf("prac_%d", p$tone_index), ""),
                      condition = ifelse(p$odd, "practice", ""))
  psychTestR::join(
    # Instruktionstext: Anton, 21.10.2026; Klarinette statt verstimmter Ton seit 23.10.
    psychTestR::one_button_page(
      body = shiny::div(
        shiny::h4("Klarinettentöne"),
        shiny::p("Sie hören Melodien, die auf einem Klavier gespielt werden. Manche Töne werden stattdessen von einer Klarinette gespielt und klingen so:"),
        get_audio_element(sprintf("%s/misc/oddball_example.mp3", audio_dir)),
        shiny::p("Im Beispiel hören Sie zuerst einen Klavierton und dann denselben Ton auf der Klarinette. Danach folgt eine kurze Melodie, in der ein Ton von der Klarinette gespielt wird."),
        shiny::p(shiny::strong("Ihre Aufgabe: Drücken Sie so schnell wie möglich die Leertaste, sobald Sie einen Klarinettenton hören."),
                 " Sie können das Beispiel beliebig oft anhören.")),
      button_text = psychTestR::i18n("CONTINUE")),
    SLT6_timed_page(
      "practice", "Übung",
      "Probieren Sie es aus: Drücken Sie die Leertaste, sobald Sie einen Klarinettenton hören. Klicken Sie auf Start, wenn Sie bereit sind.",
      sched, audio_dir,
      on_result = function(res, state) {
        psychTestR::save_result(state, "practice", SLT6_summarise_rt(res))
        psychTestR::set_global("slt6_practice_hits", res$hits, state)
      }),
    psychTestR::reactive_page(function(state, ...) {
      h <- psychTestR::get_global("slt6_practice_hits", state)
      SLT6_text_page(shiny::p(sprintf("Sie haben %d von 3 Klarinettentönen erkannt.", h)),
                     shiny::p("Im Hauptteil kommen solche Töne seltener vor. Reagieren Sie einfach so schnell wie möglich, wenn Sie einen hören."))
    })
  )
}

SLT6_block_schedule <- function(language, block) {
  s <- SLT::SLT6_items$stream
  k <- SLT::SLT6_items$oddball_key
  s <- s[s$language == language & s$block == block, ]
  k <- k[k$language == language & k$block == block, ]
  m <- match(s$tone_index, k$tone_index)
  data.frame(t = s$onset_ms / 1000, tone = s$tone, odd = !is.na(m),
             odd_id = ifelse(is.na(m), "", sprintf("%s_b%d_%d", language, block, s$tone_index)),
             condition = ifelse(is.na(m), "", k$condition[m]),
             position = s$position, triplet_id = s$triplet_id)
}

SLT6_exposure <- function(audio_dir) {
  blocks <- lapply(1:4, function(b) psychTestR::join(
    psychTestR::reactive_page(function(state, ...) {
      lg <- psychTestR::get_global("slt6_language", state)
      sched <- SLT6_block_schedule(lg, b)
      SLT6_timed_page(
        sprintf("expo_block%d", b), sprintf("Teil %d von 4", b),
        "Hören Sie den Melodien zu. Drücken Sie so schnell wie möglich die Leertaste, sobald Sie einen Klarinettenton hören. Dieser Teil dauert etwa 2 Minuten. Klicken Sie auf Start, wenn Sie bereit sind.",
        sched[, c("t", "tone", "odd", "odd_id", "condition")], audio_dir,
        on_result = function(res, state) {
          od <- as.data.frame(res$oddballs)
          od$block <- b
          od$position <- sched$position[match(od$odd_id, sched$odd_id)]
          od$triplet_id <- sched$triplet_id[match(od$odd_id, sched$odd_id)]
          all <- psychTestR::get_global("slt6_oddballs", state)
          psychTestR::set_global("slt6_oddballs", rbind(all, od), state)
          psychTestR::save_result(state, sprintf("expo_block%d", b), res)
          messagef("SLT6: Block %d, %d/%d Oddballs", b, res$hits, nrow(od))
        })
    }),
    if (b < 4) SLT6_text_page(shiny::h4("Kurze Pause"),
                              shiny::p("Gönnen Sie sich eine kurze Pause. Klicken Sie auf Weiter, wenn Sie bereit sind."))
  ))
  psychTestR::join(
    psychTestR::code_block(function(state, ...) psychTestR::set_global("slt6_oddballs", NULL, state)),
    do.call(psychTestR::join, blocks),
    psychTestR::code_block(function(state, ...) {
      psychTestR::save_result(state, "oddballs", psychTestR::get_global("slt6_oddballs", state))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )
}

# --- Testaufgaben -------------------------------------------------------------------
#
# Optionen erklingen nacheinander; die Ziffer der gerade klingenden Option wird
# hervorgehoben (Einsatzzeiten aus SLT6_items$item_audio). Die Knoepfe erscheinen
# nach dem letzten Ton. RT = Knoepfe sichtbar -> Klick.

SLT6_item_prompt <- function(item) {
  if (item$unit == "pair") return("Welches Tonpaar kam in den Melodien vor?")
  "Welche Tonfolge kam in den Melodien vor?"
}

SLT6_item_page <- function(pos, n_total, audio_dir, autoplay = TRUE) {
  psychTestR::reactive_page(function(state, ...) {
    lg   <- psychTestR::get_global("slt6_language", state)
    ord  <- psychTestR::get_global("slt6_item_order", state)
    it   <- SLT::SLT6_items$items
    item <- it[it$language == lg & it$item == ord[pos], ]
    au   <- SLT::SLT6_items$item_audio
    au   <- au[au$language == lg & au$item == item$item, ]
    k    <- item$n_options
    onsets <- as.numeric(strsplit(au$option_onsets_s, ";", fixed = TRUE)[[1]])
    url  <- sprintf("%s/items/%s", audio_dir, au$file)
    digits <- lapply(seq_len(k), function(i)
      shiny::span(as.character(i), id = sprintf("slt6_d%d", i),
                  style = "display:inline-block;width:44px;height:44px;line-height:44px;margin:6px;border-radius:22px;border:1px solid #bbb;font-size:20px;color:#888"))
    buttons <- lapply(seq_len(k), function(i)
      psychTestR::trigger_button(sprintf("opt%d", i), as.character(i), style = "margin:6px;min-width:60px"))
    js <- sprintf("
      (function () {
        var a = document.getElementById('media'), on = %s, enable = %f, k = %d, shown = false;
        Shiny.setInputValue('audio_stalled', false);   // Shiny-Inputs bleiben sonst von der Vorseite stehen
        function show() {
          if (shown) return; shown = true;
          document.getElementById('response_ui').style.visibility = 'visible';
          Shiny.setInputValue('response_visible_time', Date.now(), {priority: 'event'});
          for (var i = 1; i <= k; i++) { var d = document.getElementById('slt6_d' + i); d.style.background = ''; d.style.color = '#888'; }
        }
        function loop() {
          if (shown) return;
          var t = a.currentTime, cur = 0;
          for (var i = 0; i < k; i++) if (t >= on[i]) cur = i + 1;
          for (var j = 1; j <= k; j++) {
            var d = document.getElementById('slt6_d' + j);
            d.style.background = (j === cur) ? '#3b7dd8' : ''; d.style.color = (j === cur) ? '#fff' : '#888';
          }
          if (t >= enable || a.ended) show(); else requestAnimationFrame(loop);
        }
        a.addEventListener('play', function () {
          Shiny.setInputValue('audio_start_time', Date.now(), {priority: 'event'});
          requestAnimationFrame(loop);
        });
        a.addEventListener('ended', show);
        document.getElementById('response_ui').addEventListener('click', function () {
          Shiny.setInputValue('answer_click_time', Date.now(), {priority: 'event'});
        }, true);
        var p = %s;
        if (p) p.catch(function () { document.getElementById('slt6_play').style.display = 'inline-block'; });
        // Sicherung: haengt die Wiedergabe (z. B. Audiogeraet weg), Knoepfe trotzdem zeigen
        setTimeout(function () {
          if (!shown) { Shiny.setInputValue('audio_stalled', true); show(); }
        }, (enable + 4) * 1000);
      })();", jsonlite::toJSON(onsets), au$answer_enabled_s, k,
      if (autoplay) "a.play()" else "(document.getElementById('slt6_play').style.display = 'inline-block', null)")
    ui <- shiny::div(
      shiny::p(sprintf("Aufgabe %d von %d", pos, n_total), style = "font-size:small;color:#666"),
      shiny::p(SLT6_item_prompt(item)),
      shiny::tags$audio(shiny::tags$source(src = url, type = "audio/mpeg"), id = "media", preload = "auto"),
      shiny::tags$button(shiny::HTML("&#9654; Abspielen"), id = "slt6_play", type = "button",
                         class = "btn btn-default", style = "display:none",
                         onclick = "document.getElementById('media').play(); this.style.display='none';"),
      shiny::div(digits, style = "margin:10px 0"),
      shiny::div(buttons, id = "response_ui", style = "visibility:hidden"),
      shiny::tags$script(shiny::HTML(js)))
    psychTestR::page(
      ui = ui, label = sprintf("item_pos%02d", pos), save_answer = FALSE,
      get_answer = function(input, ...) {
        rt <- compute_rt(input)
        list(chosen = as.integer(gsub("opt", "", input$last_btn_pressed)),
             rt_decision_ms = rt$rt_decision_ms, rt_total_ms = rt$rt_total_ms,
             audio_stalled = isTRUE(input$audio_stalled))
      },
      on_complete = function(answer, state, ...) {
        row <- data.frame(position = pos, item = item$item, block = item$block, kind = item$kind,
                          unit = item$unit, n_options = k, chance = item$chance,
                          chosen = answer$chosen, correct_option = item$correct,
                          correct = as.integer(answer$chosen == item$correct),
                          rt_decision_ms = answer$rt_decision_ms, rt_total_ms = answer$rt_total_ms,
                          audio_stalled = answer$audio_stalled)
        res <- psychTestR::get_global("slt6_item_results", state)
        psychTestR::set_global("slt6_item_results", rbind(res, row), state)
      })
  })
}

SLT6_test <- function(audio_dir, autoplay = TRUE) {
  # Seit 21.10. (E15/E16): nur Wiedererkennung, 34 Items, alle 2AFC
  ids <- sort(unique(SLT::SLT6_items$items$item))
  n <- length(ids)
  pages <- lapply(seq_along(ids), function(p) SLT6_item_page(p, n, audio_dir, autoplay))
  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("slt6_item_order", sample(ids), state)
      psychTestR::set_global("slt6_item_results", NULL, state)
    }),
    SLT6_text_page(shiny::h4("Fragen zu den Melodien"),
                   # Kein Hinweis auf die Struktur (Dreiergruppen): das ist die Hilfe aus Stufe 2
                   shiny::p("Sie hören jetzt jeweils zwei kurze Tonfolgen nacheinander. Die Ziffer der gerade klingenden Tonfolge leuchtet auf. Wählen Sie danach die Tonfolge, die Ihnen aus den Melodien bekannt vorkommt. Wenn Sie unsicher sind, entscheiden Sie aus dem Bauch heraus.")),
    pages,
    psychTestR::code_block(function(state, ...) {
      res <- psychTestR::get_global("slt6_item_results", state)
      psychTestR::save_result(state, "items", res)
      psychTestR::save_result(state, "score", sum(res$correct))
      messagef("SLT6: %d/%d richtig", sum(res$correct), nrow(res))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE)
  )
}

# --- Fragen am Ende, Abschluss -----------------------------------------------------

SLT6_explicit <- function() {
  psychTestR::join(
    psychTestR::NAFC_page(
      label = "explicit_triplets",
      prompt = shiny::p("Haben Sie bemerkt, dass in den Melodien bestimmte Dreiergruppen von Tönen immer wieder vorkamen?"),
      choices = c("yes", "no"), labels = list("Ja", "Nein"), arrange_vertically = FALSE),
    psychTestR::text_input_page(
      label = "explicit_description",
      prompt = shiny::p("Beschreiben Sie kurz, was Ihnen an den Melodien aufgefallen ist (auch wenn es nur eine Vermutung ist)."),
      one_line = FALSE, width = "500px", height = "120px",
      button_text = psychTestR::i18n("CONTINUE"),
      validate = function(answer, ...) if (nzchar(trimws(answer))) TRUE else "Bitte schreiben Sie ein paar Worte."),
    psychTestR::NAFC_page(
      label = "explicit_strategy",
      prompt = shiny::p("Wie haben Sie bei den Fragen zu den Melodien meistens entschieden?"),
      choices = c("guess", "feeling", "rule"),
      labels = list("Geraten", "Nach Gefühl (kam mir bekannt vor)", "Nach einer Regel, die ich bemerkt habe"),
      arrange_vertically = TRUE),
    psychTestR::NAFC_page(
      label = "headphones",
      prompt = shiny::p("Womit haben Sie die Töne gehört?"),
      choices = c("wired", "bluetooth", "speakers", "other"),
      labels = list("Kopfhörer mit Kabel", "Bluetooth-Kopfhörer", "Lautsprecher", "Anderes / weiß nicht"),
      arrange_vertically = TRUE)
  )
}

SLT6_final <- function() {
  psychTestR::join(
    psychTestR::code_block(function(state, ...) psychTestR::save_result(state, "finished", TRUE)),
    psychTestR::elt_save_results_to_disk(complete = TRUE),
    psychTestR::final_page(shiny::div(
      shiny::h4("Vielen Dank!"),
      shiny::p("Sie haben die Studie abgeschlossen. Sie können das Fenster jetzt schließen.")))
  )
}

#' Haupt-Timeline fuer SLT version 6 (Demo)
#'
#' @param label Modul-Label (muss dem Label in \code{SLT()} entsprechen)
#' @param audio_dir Basis-URL der Audio-Dateien (enthaelt samples/ und items/)
#' @param dict psychTestR-Dictionary
#' @param autoplay Autoplay fuer die Testaufgaben
#' @param dev_skip NUR fuer Entwicklungstests: Teile, die uebersprungen werden
#'   (\code{"intro"}, \code{"technik"}, \code{"practice"}, \code{"exposure"},
#'   \code{"test"}, \code{"explicit"}). Wird als Ergebnis gespeichert.
#' @export
main_test6 <- function(label = "SLT", audio_dir, dict = SLT::SLT_dict, autoplay = TRUE,
                       dev_skip = character(), ...) {
  run <- function(part) !part %in% dev_skip
  psychTestR::join(
    # Begruessung zuerst: Speichern vor der ersten Seite scheitert in psychTestR
    SLT6_text_page(
      shiny::h4("Willkommen!"),
      shiny::p("In dieser Studie hören Sie Melodien und beantworten danach Fragen dazu. Sie dauert etwa 20 Minuten."),
      shiny::p(shiny::strong("Bitte benutzen Sie Kopfhörer mit Kabel"), " (keine Bluetooth-Kopfhörer) und einen Computer mit Tastatur. Suchen Sie sich einen ruhigen Ort.")),
    SLT6_assign_language(label = label),
    psychTestR::code_block(function(state, ...) {
      psychTestR::save_result(state, "slt_version", 6)
      if (length(dev_skip)) psychTestR::save_result(state, "dev_skip", paste(dev_skip, collapse = ","))
    }),
    psychTestR::elt_save_results_to_disk(complete = FALSE),
    if (run("intro")) psychTestR::one_button_page(
      body = shiny::div(shiny::h4("Lautstärke einstellen"),
                        shiny::p("Spielen Sie das Beispiel ab und stellen Sie eine angenehme Lautstärke ein."),
                        get_audio_element(sprintf("%s/items/mirror_13.mp3", audio_dir))),
      button_text = psychTestR::i18n("CONTINUE")),
    if (run("technik")) SLT6_technik(audio_dir),
    if (run("practice")) SLT6_practice(audio_dir),
    if (run("exposure")) SLT6_text_page(
      shiny::h4("Hauptteil: Melodien hören"),
      shiny::p("Sie hören jetzt in 4 Teilen jeweils etwa 2 Minuten lang Melodien. Drücken Sie wie in der Übung die Leertaste, sobald Sie einen Klarinettenton hören."),
      shiny::p(shiny::strong("Hören Sie gut zu: Danach folgen Fragen zu den Melodien."))),
    if (run("exposure")) SLT6_exposure(audio_dir),
    if (run("test")) SLT6_test(audio_dir, autoplay = autoplay),
    if (run("explicit")) SLT6_explicit(),
    SLT6_final()
  )
}
