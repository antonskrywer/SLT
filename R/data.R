#' SLT item bank 3 (deterministische FSG-Versionen: Reber & Loui)
#'
#' Kombinierte Item-Bank fuer SLT version = 3 (Reber-FSG) und
#' version = 4 (Loui-FSG). Unterscheidung ueber die Spalte
#' \code{grammar_type} ("reber_fsg" oder "loui_fsg").
#'
#' WICHTIG: Die Poolgroesse unterscheidet sich zwischen den beiden
#' Grammatiken, da der Reber-Automat bei Melodielaenge 10 nur 36
#' grammatikalisch gueltige Strings insgesamt zulaesst:
#' \itemize{
#'   \item reber_fsg: 36 Items (18 grammatikalisch / Style A, 18 Zufallspermutation / Style B)
#'   \item loui_fsg: 50 Items (25 grammatikalisch / Style A, 25 Zufallspermutation / Style B)
#' }
#'
#' @format Ein Tibble mit den Spalten:
#' \describe{
#'   \item{item_number}{Laufende Nummer innerhalb Grammatik x Style}
#'   \item{style}{"A" (grammatikalisch) oder "B" (Zufallspermutation)}
#'   \item{correct}{Korrekte Antwort ("A" oder "B"), identisch zu style}
#'   \item{file_name}{Dateiname OHNE Endung (SLT_item2() haengt .mp3 an)}
#'   \item{tones}{Tonfolge als Bindestrich-separierter String, zur Doku}
#'   \item{block}{Konstant 1 (kein Block-Design bei v3/v4)}
#'   \item{grammar_type}{"reber_fsg" oder "loui_fsg"}
#' }
#' @name SLT_item_bank3
#' @docType data
NULL
