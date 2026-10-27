# Bilingual (English / Spanish) validation messages. Non-ASCII characters are
# written as \u escapes so that the source stays portable.
.messages <- list(
  missing_column = c(
    en = "Column '%s' defined in the dictionary is absent from the data.",
    es = "La columna '%s' definida en el diccionario no existe en los datos."
  ),
  required = c(
    en = "Required value is missing.",
    es = "Falta un valor obligatorio."
  ),
  type_numeric = c(
    en = "Value '%s' is not a valid number.",
    es = "El valor '%s' no es un n\u00famero v\u00e1lido."
  ),
  type_integer = c(
    en = "Value '%s' is not a whole number.",
    es = "El valor '%s' no es un n\u00famero entero."
  ),
  type_date = c(
    en = "Value '%s' is not a valid date (expected YYYY-MM-DD).",
    es = "El valor '%s' no es una fecha v\u00e1lida (formato AAAA-MM-DD)."
  ),
  range = c(
    en = "Value %s is outside the allowed range [%s, %s].",
    es = "El valor %s est\u00e1 fuera del rango permitido [%s, %s]."
  ),
  choice = c(
    en = "Value '%s' is not an allowed choice.",
    es = "El valor '%s' no es una opci\u00f3n permitida."
  ),
  duplicate_id = c(
    en = "Duplicate identifier '%s'.",
    es = "Identificador duplicado '%s'."
  )
)

#' Build a translated message (internal)
#' @noRd
registat_message <- function(key, lang, ...) {
  template <- .messages[[key]][[lang]]
  if (is.null(template)) {
    abort("Unknown message key or language: %s / %s", key, lang)
  }
  sprintf(template, ...)
}
