#' Item-Ziehung fuer SLT version 3/4
#'
#' Zieht das komplette Item-Pool eines Grammatik-Typs (Style A + Style B,
#' zu gleichen Teilen) und ordnet die Reihenfolge so an, dass die ersten
#' n_start Items mindestens min_each Items pro Style enthalten.
#'
#' @param grammar_type "reber_fsg" oder "loui_fsg"
#' @param num_items Anzahl Items insgesamt. Default NULL: wird automatisch
#'   aus der Anzahl vorhandener Items des jeweiligen grammar_type in
#'   item_bank ermittelt (aktuell: Reber = 36, Loui = 50 - siehe
#'   compile_item_bank3.R). Kann explizit gesetzt werden, z.B. fuer Tests
#'   mit einem kuenstlich verkleinerten Pool.
#' @param n_start Anzahl der ersten Items, fuer die Style-Balance erzwungen wird
#' @param min_each Mindestanzahl je Style innerhalb der ersten n_start Items
#' @param item_bank Quelle der Items, Default SLT::SLT_item_bank3
#' @export
get_items3 <- function(grammar_type = c("reber_fsg", "loui_fsg"),
                       num_items = NULL,
                       n_start   = 6,
                       min_each  = 2,
                       item_bank = SLT::SLT_item_bank3) {

  grammar_type <- match.arg(grammar_type)

  # Base-R-Subsetting bewusst statt dplyr::filter(), da der Parameter
  # denselben Namen wie die Spalte traegt (NSE-Kollisionsgefahr)
  pool <- item_bank[item_bank$grammar_type == grammar_type, ]

  ### GEÄNDERT START — num_items automatisch aus Pool-Groesse ableiten ###
  if (is.null(num_items)) {
    num_items <- nrow(pool)
  }
  ### GEÄNDERT ENDE ###

  stopifnot(
    "Pool-Groesse stimmt nicht mit num_items ueberein - Item-Bank pruefen" =
      nrow(pool) == num_items
  )

  n_per_style <- floor(num_items / 2)
  max_offset  <- n_start - 2 * min_each
  stopifnot("n_start muss >= 2 * min_each sein" = max_offset >= 0)

  poolA <- pool[pool$style == "A", ]
  poolB <- pool[pool$style == "B", ]
  stopifnot(
    "Pool ist nicht gleichmaessig auf Style A/B balanciert" =
      nrow(poolA) == n_per_style && nrow(poolB) == n_per_style
  )

  offsetA <- sample(0:max_offset, 1)
  offsetB <- max_offset - offsetA

  idxA <- c(rep("start", min_each + offsetA),
            rep("tail",  n_per_style - min_each - offsetA))
  idxB <- c(rep("start", min_each + offsetB),
            rep("tail",  n_per_style - min_each - offsetB))

  poolA$idx <- sample(idxA)
  poolB$idx <- sample(idxB)
  pool <- dplyr::bind_rows(poolA, poolB)

  start_items <- pool[pool$idx == "start", ]
  tail_items  <- pool[pool$idx == "tail", ]

  dplyr::bind_rows(
    start_items[sample(1:nrow(start_items)), ],
    tail_items[sample(1:nrow(tail_items)), ]
  ) %>%
    dplyr::mutate(seq_id = 1:nrow(.))
}

#' Haupt-Timeline fuer SLT version 3/4
#'
#' @param label Modul-Label
#' @param grammar_type "reber_fsg" oder "loui_fsg"
#' @param audio_dir Basis-URL fuer Audio-Dateien
#' @param dict psychTestR-Dictionary
#' @param num_items Default NULL: wird automatisch aus der Anzahl
#'   vorhandener Items je grammar_type in SLT_item_bank3 ermittelt
#'   (aktuell: Reber = 36, Loui = 50).
#' @param n_start,min_each s. get_items3()
#' @param composer_pairs Liste mit GENAU EINEM Paar, z.B. list(c(A="Noa", B="Sam"))
#' @param autoplay Autoplay fuer Item-Audio
#' @export
main_test3 <- function(label,
                       grammar_type,
                       audio_dir,
                       dict = SLT::SLT_dict,
                       num_items = NULL,
                       n_start   = 6,
                       min_each  = 2,
                       composer_pairs = list(c(A = "Noa", B = "Sam")),
                       autoplay = TRUE, ...) {

  stopifnot(
    "composer_pairs muss fuer version 3/4 genau EIN Paar enthalten" =
      length(composer_pairs) == 1
  )

  ### GEÄNDERT START — num_items automatisch aus Item-Bank ableiten ###
  if (is.null(num_items)) {
    num_items <- sum(SLT::SLT_item_bank3$grammar_type == grammar_type)
  }
  ### GEÄNDERT ENDE ###

  item_list <- list()
  for (i in 1:num_items) {
    item_list[[length(item_list) + 1]] <- SLT_item2(
      audio_dir   = audio_dir,
      save_answer = TRUE,
      autoplay    = autoplay
    )
    item_list[[length(item_list) + 1]] <- item_feedback_page()
  }

  psychTestR::join(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_global("block", 0, state)
      psychTestR::set_global("results", data.frame(), state)
      psychTestR::set_global("composer_pairs", composer_pairs, state)
    }),
    block_intro_page(num_items = num_items),
    psychTestR::code_block(function(state, ...) {
      seq_df <- get_items3(
        grammar_type = grammar_type,
        num_items    = num_items,
        n_start      = n_start,
        min_each     = min_each
      )
      psychTestR::set_global("items", seq_df, state)
      psychTestR::set_global("counter", 1, state)
      psychTestR::set_global("block", 1, state)
    }),
    item_list
  )
}
