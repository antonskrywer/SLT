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

#' SLT5 items (SLT version 5, Pilot 2026)
#'
#' Material fuer SLT version = 5 (passive Exposition + A-not-A + Probeton).
#' Erzeugt mit den Skripten in SLT_analysis_and_publications/slt5_pilot/ und
#' zusammengefasst mit data_raw/compile_item_bank5.R. Audio liegt unter
#' https://s3.eu-west-1.amazonaws.com/media.dots.org/stimuli/SLT5/.
#'
#' @format Eine Liste mit den Data Frames:
#' \describe{
#'   \item{anota}{48 A-not-A-Items (24 G1 / 24 G2, 4 E-Stufen)}
#'   \item{probe}{30 Probeton-Aufgaben (TP unter G1/G2, normal und leicht)}
#'   \item{practice}{2 A-not-A- und 1 Probeton-Uebung (in G1 und G2 gleich gueltig)}
#'   \item{pitch_check}{3 Tonhoehen-Aufgaben (Ganzton, "hoeher"/"tiefer"; Kriterium 3/3)}
#'   \item{exposure}{16 Expositionsbloecke (Schwierigkeit x Grammatik x Block) mit Anzahl Oddballs}
#' }
#' In allen Tabellen ist \code{file_name} der Pfad relativ zu audio_dir, ohne ".mp3".
#' @name SLT5_items
#' @docType data
NULL
