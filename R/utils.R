#' Stop with a formatted message (internal)
#' @noRd
abort <- function(...) {
  stop(sprintf(...), call. = FALSE)
}

#' Identifier variable name of a dictionary (internal)
#' @noRd
id_variable <- function(dict) {
  ids <- vapply(
    dict$variables,
    function(v) if (identical(v$type, "id")) v$name else NA_character_,
    character(1)
  )
  ids[!is.na(ids)][1L]
}

#' Names of all variables of a dictionary (internal)
#' @noRd
variable_names <- function(dict) {
  vapply(dict$variables, function(v) v$name, character(1))
}
