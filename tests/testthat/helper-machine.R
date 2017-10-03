
final <- function(...) {
  set_attrs(block(...), tail = TRUE)
}

invisible_lang <- return_lang(lang("invisible", NULL))
