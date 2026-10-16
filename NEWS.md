# SLT (unreleased)

* Added version 3 (Reber FSG) and version 4 (Loui FSG, Bohlen-Pierce scale) stimulus sets, alongside the original Markov-chain versions 1 and 2 (`SLT_item_bank3`, `main_test3()`, `get_items3()`)
* Added per-trial reaction time capture (`rt_decision_ms`, `rt_total_ms`) via `audio_NAFC_page_rt()`
* Fixed a bug where `SLT_item2()` always recorded incorrect feedback for versions 3/4, caused by a local `correct` variable being shadowed by the item bank's own `correct` column in `dplyr::mutate()`

# SLT 1.2.1

Added autoplay parameter.

# SLT 0.3.2

Added Nederlands, fixed finish page.

# SLT 0.3.1

* Updated to new lower-case language names

# SLT 0.1.0

* First release
