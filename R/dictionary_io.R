#' Dictionary as spreadsheet tables
#'
#' A data dictionary can be written in a spreadsheet instead of YAML, which is
#' easier for people who do not program. The spreadsheet has four sheets:
#'
#' * `Study`: columns `field` and `value` with the rows `id`, `version`,
#'   `title_en` and `title_es`.
#' * `Variables`: one row per variable with `name`, `type`, `required`,
#'   `label_en`, `label_es`, `unit`, `min`, `max`, `description_en` and
#'   `description_es`. Only `name`, `type`, `label_en` and `label_es` are
#'   needed; allowed types are `id`, `integer`, `numeric`, `categorical`,
#'   `date` and `text`.
#' * `Choices`: one row per answer option of a categorical variable, with
#'   `variable`, `value`, `label_en` and `label_es`.
#' * `Rules` (optional): cross-field checks with `id`, `expr`, `message_en` and
#'   `message_es`. The expression must be TRUE when the row is valid.
#'
#' `dictionary_to_tables()` and `dictionary_from_tables()` convert between a
#' dictionary and these tables (as a named list of data frames);
#' [read_dictionary()] reads such a spreadsheet directly and
#' [dictionary_template_xlsx()] writes a ready-to-fill example.
#'
#' @param dict A `registat_dictionary`.
#' @return `dictionary_to_tables()`: a named list with the data frames
#'   `Study`, `Variables`, `Choices` and `Rules`.
#' @examples
#' tabs <- dictionary_to_tables(load_template("t2d"))
#' names(tabs)
#' head(tabs$Variables[, c("name", "type", "label_en")], 3)
#' @export
dictionary_to_tables <- function(dict) {
  chr <- function(x) if (is.null(x)) NA_character_ else as.character(x)
  num <- function(x) if (is.null(x)) NA_character_ else format(x, trim = TRUE)
  study <- data.frame(
    field = c("id", "version", "title_en", "title_es"),
    value = c(chr(dict$study$id), chr(dict$study$version),
              chr(dict$study$title$en), chr(dict$study$title$es)),
    stringsAsFactors = FALSE)
  vars <- lapply(dict$variables, function(v) {
    data.frame(
      name = v$name, type = v$type,
      required = if (isTRUE(v$required)) "yes" else "no",
      label_en = v$label$en, label_es = v$label$es,
      unit = chr(v$unit), min = num(v$min), max = num(v$max),
      description_en = chr(v$description$en), description_es = chr(v$description$es),
      stringsAsFactors = FALSE)
  })
  choices <- lapply(dict$variables, function(v) {
    if (!identical(v$type, "categorical")) return(NULL)
    do.call(rbind, lapply(v$choices, function(ch) {
      data.frame(variable = v$name, value = ch$value, label_en = ch$label$en,
                 label_es = ch$label$es, stringsAsFactors = FALSE)
    }))
  })
  rules <- lapply(dict$rules, function(r) {
    data.frame(id = r$id, expr = r$expr, message_en = r$message$en,
               message_es = r$message$es, stringsAsFactors = FALSE)
  })
  bind <- function(x, cols) {
    x <- Filter(Negate(is.null), x)
    if (length(x) == 0L) {
      return(stats::setNames(data.frame(matrix(character(), 0L, length(cols)),
                                        stringsAsFactors = FALSE), cols))
    }
    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
  }
  list(Study = study,
       Variables = bind(vars, names(vars[[1L]])),
       Choices = bind(choices, c("variable", "value", "label_en", "label_es")),
       Rules = bind(rules, c("id", "expr", "message_en", "message_es")))
}

#' @describeIn dictionary_to_tables Build a dictionary from the tables
#'   described above and validate it.
#' @param tables A named list with the data frames `Study`, `Variables`,
#'   `Choices` and (optionally) `Rules`. Every column is read as text, so that
#'   answer codes such as `01` keep their zeros.
#' @return `dictionary_from_tables()`: a validated `registat_dictionary`.
#'   Problems are reported with the sheet and row where they are.
#' @export
dictionary_from_tables <- function(tables) {
  names(tables) <- canonical_sheet_names(names(tables))
  for (s in c("Study", "Variables")) {
    if (is.null(tables[[s]])) abort("The sheet '%s' is missing.", s)
  }
  tabs <- lapply(tables, function(d) {
    d <- as.data.frame(d, stringsAsFactors = FALSE)
    names(d) <- tolower(trimws(names(d)))
    d[] <- lapply(d, function(col) {
      col <- trimws(as.character(col))
      col[!nzchar(col)] <- NA_character_
      col
    })
    d
  })
  need <- function(d, sheet, cols) {
    miss <- setdiff(cols, names(d))
    if (length(miss)) abort("Sheet '%s' needs the column(s): %s.", sheet, paste(miss, collapse = ", "))
  }
  get <- function(row, col) {
    x <- row[[col]]
    if (is.null(x) || is.na(x)) NULL else x
  }
  both <- function(row, en, es) {
    a <- get(row, en); b <- get(row, es)
    if (is.null(a) && is.null(b)) return(NULL)
    list(en = a %||% b, es = b %||% a)
  }

  st <- tabs$Study
  need(st, "Study", c("field", "value"))
  sv <- stats::setNames(st$value, tolower(st$field))
  study <- list(id = if (!is.na(sv["id"])) unname(sv["id"]) else NULL,
                version = if (!is.na(sv["version"])) unname(sv["version"]) else NULL,
                title = list(en = unname(sv["title_en"]), es = unname(sv["title_es"])))
  if (anyNA(unlist(study$title))) {
    abort("Sheet 'Study': 'title_en' and 'title_es' are required.")
  }

  vt <- tabs$Variables
  need(vt, "Variables", c("name", "type", "label_en", "label_es"))
  ct <- tabs$Choices
  if (!is.null(ct)) need(ct, "Choices", c("variable", "value", "label_en", "label_es"))

  yes <- c("yes", "y", "true", "1", "x", "si", "s\u00ed", "s")
  vars <- lapply(seq_len(nrow(vt)), function(i) {
    r <- vt[i, , drop = FALSE]
    where <- sprintf("Sheet 'Variables', row %d", i + 1L)
    nm <- get(r, "name")
    if (is.null(nm)) abort("%s: 'name' is empty.", where)
    if (is.null(get(r, "label_en")) && is.null(get(r, "label_es"))) {
      abort("%s ('%s'): a label is required (label_en and/or label_es).", where, nm)
    }
    v <- list(name = nm, type = tolower(get(r, "type") %||% ""),
              required = tolower(get(r, "required") %||% "no") %in% yes,
              label = both(r, "label_en", "label_es"))
    if (!is.null(get(r, "unit"))) v$unit <- get(r, "unit")
    for (b in c("min", "max")) {
      raw <- get(r, b)
      if (!is.null(raw)) {
        x <- suppressWarnings(as.numeric(sub(",", ".", raw, fixed = TRUE)))
        if (is.na(x)) abort("%s ('%s'): '%s' must be a number, not '%s'.", where, nm, b, raw)
        v[[b]] <- x
      }
    }
    d <- both(r, "description_en", "description_es")
    if (!is.null(d)) v$description <- d
    if (identical(v$type, "categorical")) {
      rows <- if (is.null(ct)) ct else ct[!is.na(ct$variable) & ct$variable == nm, , drop = FALSE]
      if (is.null(rows) || nrow(rows) == 0L) {
        abort("%s ('%s'): a categorical variable needs rows in the sheet 'Choices'.", where, nm)
      }
      v$choices <- lapply(seq_len(nrow(rows)), function(j) {
        cr <- rows[j, , drop = FALSE]
        if (is.na(cr$value)) abort("Sheet 'Choices', variable '%s': a 'value' is empty.", nm)
        lab <- both(cr, "label_en", "label_es")
        if (is.null(lab)) abort("Sheet 'Choices', variable '%s', value '%s': a label is required.", nm, cr$value)
        list(value = cr$value, label = lab)
      })
    }
    v
  })
  if (!is.null(ct)) {
    unknown <- setdiff(stats::na.omit(unique(ct$variable)), vt$name)
    if (length(unknown)) {
      abort("Sheet 'Choices' refers to variables that are not in 'Variables': %s.",
            paste(unknown, collapse = ", "))
    }
  }

  rules <- NULL
  rt <- tabs$Rules
  if (!is.null(rt) && nrow(rt) > 0L) {
    need(rt, "Rules", c("id", "expr", "message_en", "message_es"))
    rt <- rt[!is.na(rt$id) | !is.na(rt$expr), , drop = FALSE]
    rules <- lapply(seq_len(nrow(rt)), function(i) {
      r <- rt[i, , drop = FALSE]
      msg <- both(r, "message_en", "message_es")
      if (is.na(r$id) || is.na(r$expr) || is.null(msg)) {
        abort("Sheet 'Rules', row %d: 'id', 'expr' and a message are required.", i + 1L)
      }
      list(id = r$id, expr = r$expr, message = msg)
    })
  }

  dict <- list(study = study, variables = vars)
  if (length(rules)) dict$rules <- rules
  validate_dictionary(dict)
}

#' Sheet names accepted in a dictionary spreadsheet (internal)
#' @noRd
canonical_sheet_names <- function(x) {
  key <- tolower(trimws(x))
  map <- c(study = "Study", estudio = "Study", variables = "Variables",
           choices = "Choices", opciones = "Choices", rules = "Rules",
           reglas = "Rules")
  out <- unname(map[key])
  out[is.na(out)] <- x[is.na(out)]
  out
}

#' Read a dictionary from a spreadsheet (internal)
#' @noRd
read_dictionary_xlsx <- function(path) {
  sheets <- readxl::excel_sheets(path)
  canon <- canonical_sheet_names(sheets)
  tabs <- lapply(sheets, function(s) {
    as.data.frame(readxl::read_excel(path, sheet = s, col_types = "text",
                                     .name_repair = "minimal"),
                  stringsAsFactors = FALSE)
  })
  names(tabs) <- canon
  tabs <- tabs[canon %in% c("Study", "Variables", "Choices", "Rules")]
  dictionary_from_tables(tabs)
}

#' Write a dictionary as a YAML file
#'
#' Saves a dictionary in the YAML format read by [read_dictionary()], for
#' example after building it from a spreadsheet, to keep it under version
#' control or share it.
#'
#' @param dict A `registat_dictionary`.
#' @param path Output file (`.yml`).
#' @return `path`, invisibly.
#' @examples
#' f <- tempfile(fileext = ".yml")
#' write_dictionary(load_template("t2d"), f)
#' read_dictionary(f)$study$id
#' @export
write_dictionary <- function(dict, path) {
  if (!inherits(dict, "registat_dictionary")) abort("'dict' must be a registat_dictionary.")
  x <- unclass(dict)
  txt <- yaml::as.yaml(x, indent.mapping.sequence = TRUE, line.sep = "\n")
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(txt)), con)
  invisible(path)
}

#' Write a ready-to-fill dictionary spreadsheet
#'
#' Creates an Excel file with the sheets described in
#' [dictionary_to_tables()] and an `Instructions` sheet (in English and
#' Spanish). Fill it in, save it, and give it to [read_dictionary()] or to the
#' start screen of the app. Requires the `writexl` package.
#'
#' @param path Output file (`.xlsx`).
#' @param example `"minimal"` (a short example to adapt) or `"t2d"` (the
#'   bundled type 2 diabetes template, to modify).
#' @return `path`, invisibly.
#' @examples
#' if (requireNamespace("writexl", quietly = TRUE)) {
#'   f <- tempfile(fileext = ".xlsx")
#'   dictionary_template_xlsx(f)
#'   read_dictionary(f)
#' }
#' @export
dictionary_template_xlsx <- function(path, example = c("minimal", "t2d")) {
  example <- match.arg(example)
  if (!requireNamespace("writexl", quietly = TRUE)) {
    abort("The 'writexl' package is needed to write an Excel file: install.packages(\"writexl\").")
  }
  dict <- if (example == "t2d") load_template("t2d") else minimal_dictionary()
  tabs <- dictionary_to_tables(dict)
  sheets <- c(list(Instructions = dictionary_instructions()), tabs)
  writexl::write_xlsx(sheets, path)
  invisible(path)
}

#' A short example dictionary (internal)
#' @noRd
minimal_dictionary <- function() {
  yn <- function(v, en, es) list(
    name = v, type = "categorical", label = list(en = en, es = es),
    choices = list(
      list(value = "yes", label = list(en = "Yes", es = "S\u00ed")),
      list(value = "no", label = list(en = "No", es = "No"))))
  validate_dictionary(list(
    study = list(id = "my_study", version = "0.1.0",
                 title = list(en = "My study", es = "Mi estudio")),
    variables = list(
      list(name = "record_id", type = "id", required = TRUE,
           label = list(en = "Study record code", es = "C\u00f3digo de registro"),
           description = list(
             en = "Pseudonymised code. Never a name or a hospital file number.",
             es = "C\u00f3digo seudonimizado. Nunca un nombre ni un n\u00famero de expediente.")),
      list(name = "visit_date", type = "date", required = TRUE,
           label = list(en = "Visit date", es = "Fecha de la visita")),
      list(name = "age_years", type = "integer", required = TRUE, unit = "years",
           min = 18, max = 110, label = list(en = "Age", es = "Edad")),
      list(name = "systolic_bp_mmhg", type = "integer", unit = "mmHg", min = 60, max = 260,
           label = list(en = "Systolic blood pressure", es = "Presi\u00f3n sist\u00f3lica")),
      list(name = "diastolic_bp_mmhg", type = "integer", unit = "mmHg", min = 30, max = 160,
           label = list(en = "Diastolic blood pressure", es = "Presi\u00f3n diast\u00f3lica")),
      yn("smoker", "Current smoker", "Fuma actualmente"),
      list(name = "notes", type = "text",
           label = list(en = "Notes", es = "Notas"))),
    rules = list(
      list(id = "bp_order", expr = "diastolic_bp_mmhg < systolic_bp_mmhg",
           message = list(en = "Diastolic pressure must be lower than systolic pressure.",
                          es = "La presi\u00f3n diast\u00f3lica debe ser menor que la sist\u00f3lica.")))))
}

#' Instructions sheet of the dictionary spreadsheet (internal)
#' @noRd
dictionary_instructions <- function() {
  out <- data.frame(
    English = c(
      "RegiStat data dictionary. Fill in the sheets Study, Variables, Choices and Rules.",
      "Study: id, version and the title in English and Spanish.",
      "Variables: one row per variable. 'name' uses letters, digits and underscores and starts with a letter.",
      "type: id (exactly one, a pseudonymised code), integer, numeric, categorical, date or text.",
      "required: yes or no. unit, min and max are optional (use a decimal point).",
      "Choices: one row per answer of each categorical variable (variable, value, labels). Values are the stored codes.",
      "Rules: optional checks across fields. 'expr' must be TRUE when the data are valid, e.g. diastolic_bp_mmhg < systolic_bp_mmhg. Allowed: comparisons, + - * /, & | !, parentheses and is.na().",
      "Labels are needed in English and Spanish (if you write only one, it is copied to the other).",
      "Never use names, initials, dates of birth or hospital file numbers as identifiers."),
    Spanish = c(
      "Diccionario de datos de RegiStat. Llene las hojas Study, Variables, Choices y Rules.",
      "Study: id, versi\u00f3n y el t\u00edtulo en ingl\u00e9s y en espa\u00f1ol.",
      "Variables: una fila por variable. 'name' usa letras, d\u00edgitos y guiones bajos y empieza con letra.",
      "type: id (exactamente uno, un c\u00f3digo seudonimizado), integer, numeric, categorical, date o text.",
      "required: yes o no. unit, min y max son opcionales (use punto decimal).",
      "Choices: una fila por respuesta de cada variable categ\u00f3rica (variable, valor, etiquetas). Los valores son los c\u00f3digos guardados.",
      "Rules: revisiones opcionales entre campos. 'expr' debe ser verdadera cuando los datos son v\u00e1lidos, p. ej. diastolic_bp_mmhg < systolic_bp_mmhg. Permitido: comparaciones, + - * /, & | !, par\u00e9ntesis e is.na().",
      "Las etiquetas se piden en ingl\u00e9s y en espa\u00f1ol (si escribe solo una, se copia a la otra).",
      "Nunca use nombres, iniciales, fechas de nacimiento ni n\u00fameros de expediente como identificadores."),
    check.names = FALSE, stringsAsFactors = FALSE)
  names(out)[2L] <- "Espa\u00f1ol"
  out
}
