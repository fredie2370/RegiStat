write_text <- function(lines, bytes = NULL, ext = ".csv", env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ext, .local_envir = env)
  if (is.null(bytes)) {
    con <- file(f, "wb")
    writeBin(charToRaw(paste0(paste(lines, collapse = "\n"), "\n")), con)
    close(con)
  } else {
    writeBin(bytes, f)
  }
  f
}

test_that("CSV delimiters are detected and values stay as text", {
  semi <- read_table_file(write_text(c("id;age;note", "00123;60;a,b", "A2;;")))
  expect_equal(names(semi), c("id", "age", "note"))
  expect_equal(semi$id, c("00123", "A2"))
  expect_equal(semi$note, c("a,b", NA))
  expect_true(is.na(semi$age[2]))

  comma <- read_table_file(write_text(c("id,age", "\"A,1\",60")))
  expect_equal(comma$id, "A,1")

  tab <- read_table_file(write_text(c("id\tage", "A1\t60"), ext = ".tsv"))
  expect_equal(tab$age, "60")
})

test_that("encodings are detected: UTF-8 with BOM, UTF-8 and Latin-1", {
  txt <- "Código;Peso\nmañana;82,5\n"
  bom <- c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(enc2utf8(txt)))
  a <- read_table_file(write_text(bytes = bom))
  expect_identical(enc2utf8(names(a)[1]), "C\u00f3digo")
  expect_identical(enc2utf8(a[[1]]), "ma\u00f1ana")

  b <- read_table_file(write_text(bytes = charToRaw(enc2utf8(txt))))
  expect_identical(enc2utf8(b[[1]]), "ma\u00f1ana")

  latin <- iconv(txt, from = "UTF-8", to = "latin1", toRaw = TRUE)[[1]]
  d <- read_table_file(write_text(bytes = latin))
  expect_identical(enc2utf8(names(d)[1]), "C\u00f3digo")
  expect_identical(enc2utf8(d[[1]]), "ma\u00f1ana")
})

test_that("empty rows and columns are dropped and bad files are rejected", {
  d <- read_table_file(write_text(c("a;b;", "1;2;", ";;", "3;4;")))
  expect_equal(dim(d), c(2L, 2L))
  expect_error(read_table_file(tempfile(fileext = ".csv")), "not found")
  expect_error(read_table_file(write_text("x", ext = ".pdf")), "Unsupported")
  expect_error(read_table_file(write_text(bytes = raw(0))), "empty")
})

test_that("Excel files are read as text and dates convert from serials", {
  skip_if_not_installed("writexl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(
    list(Datos = data.frame(Codigo = c("00123", "A2"),
                            Fecha = as.Date(c("2024-05-01", "2023-12-31")),
                            Peso = c(82.5, 70)),
         Otra = data.frame(x = 1)),
    f)
  expect_equal(table_sheets(f), c("Datos", "Otra"))
  expect_null(table_sheets(write_text("a")))
  d <- read_table_file(f)
  expect_equal(d$Codigo, c("00123", "A2"))
  expect_equal(d$Peso, c("82.5", "70"))
  expect_match(d$Fecha[1], "^[0-9]{5}")
  expect_equal(read_table_file(f, sheet = "Otra")$x, "1")

  dict <- load_template("t2d")
  conv <- convert_import_values(data.frame(visit_date = d$Fecha), dict)
  expect_equal(conv$visit_date, c("2024-05-01", "2023-12-31"))
})

test_that("columns are matched to variables by name and by label", {
  dict <- load_template("t2d")
  m <- suggest_mapping(
    c("Código de registro del ESTUDIO", "edad", "Weight (kg)?",
      "Body weight", "visit_date", "irrelevante"),
    dict)
  expect_equal(m[["record_id"]], "Código de registro del ESTUDIO")
  expect_equal(m[["age_years"]], "edad")
  # "Weight (kg)?" equals the variable name once punctuation is ignored, so it
  # wins; "Body weight" is then left unused because the variable is taken.
  expect_equal(m[["weight_kg"]], "Weight (kg)?")
  expect_equal(m[["visit_date"]], "visit_date")
  expect_true(is.na(m[["height_cm"]]))
  expect_equal(names(m), variable_names(dict))
  # every column is used at most once
  expect_equal(anyDuplicated(m[!is.na(m)]), 0L)
})

test_that("apply_mapping renames and rejects bad mappings", {
  d <- data.frame(a = "1", b = "2", c = "3")
  out <- apply_mapping(d, c(x = "b", y = "a", z = NA))
  expect_equal(names(out), c("x", "y"))
  expect_equal(out$x, "2")
  expect_error(apply_mapping(d, c(x = "zzz")), "not found")
  expect_error(apply_mapping(d, c(x = "a", y = "a")), "two variables")
})

test_that("dates convert only in the format the user declares", {
  dict <- load_template("t2d")
  x <- data.frame(visit_date = c("01/05/2024", "13/05/2024", "05/13/2024",
                                 "2024-05-01 00:00:00", "31.12.2023",
                                 "30/02/2024", "garbage", NA))
  dmy <- convert_import_values(x, dict, date_format = "dmy")$visit_date
  expect_equal(dmy, c("2024-05-01", "2024-05-13", "05/13/2024", "2024-05-01",
                      "2023-12-31", "30/02/2024", "garbage", NA))
  mdy <- convert_import_values(x, dict, date_format = "mdy")$visit_date
  expect_equal(mdy[1:3], c("2024-01-05", "13/05/2024", "2024-05-13"))
  iso <- convert_import_values(x, dict, date_format = "iso")$visit_date
  expect_equal(iso[1], "01/05/2024")
  expect_equal(iso[4], "2024-05-01")
  # a year alone is not mistaken for an Excel serial
  expect_equal(convert_import_values(data.frame(visit_date = "2024"), dict)$visit_date,
               "2024")
})

test_that("decimal commas and value labels are converted", {
  dict <- load_template("t2d")
  x <- data.frame(weight_kg = c("82,5", "70", "1,2,3", "abc"),
                  sex_at_birth = c("Mujer", "HOMBRE", "male", "otro"),
                  ckd = c("Sí", "si", "No", "quizás"))
  point <- convert_import_values(x, dict, decimal_mark = ".")
  expect_equal(point$weight_kg, x$weight_kg)
  comma <- convert_import_values(x, dict, decimal_mark = ",")
  expect_equal(comma$weight_kg, c("82.5", "70", "1,2,3", "abc"))
  expect_equal(comma$sex_at_birth, c("female", "male", "male", "otro"))
  expect_equal(comma$ckd, c("yes", "yes", "no", "quizás"))
})

test_that("a Spanish-style spreadsheet export imports cleanly end to end", {
  dict <- load_template("t2d")
  sim <- synthetic_t2d(30, seed = 3)$data

  lab <- function(var, code) {
    v <- dict$variables[[which(variable_names(dict) == var)]]
    vapply(code, function(cd) {
      for (ch in v$choices) if (identical(ch$value, cd)) return(ch$label$es)
      cd
    }, character(1), USE.NAMES = FALSE)
  }
  es <- sim
  es$visit_date <- format(es$visit_date, "%d/%m/%Y")
  for (v in dict$variables) {
    if (v$type == "numeric") es[[v$name]] <- sub(".", ",", as.character(es[[v$name]]), fixed = TRUE)
    if (v$type == "categorical") es[[v$name]] <- lab(v$name, es[[v$name]])
  }
  names(es) <- dictionary_table(dict, "es")$label
  # Written by hand as UTF-8 bytes: write.table() would replace accents with
  # <U+XXXX> placeholders when the session locale is not UTF-8.
  es[] <- lapply(es, as.character)
  q <- function(x) paste0("\"", x, "\"")
  lines <- c(paste(q(names(es)), collapse = ";"),
             vapply(seq_len(nrow(es)),
                    function(i) paste(q(unlist(es[i, ])), collapse = ";"),
                    character(1)))
  f <- write_text(enc2utf8(lines))

  raw <- read_table_file(f)
  mapping <- suggest_mapping(names(raw), dict)
  expect_false(anyNA(mapping))
  prepared <- convert_import_values(apply_mapping(raw, mapping), dict,
                                    date_format = "dmy", decimal_mark = ",")

  reg <- new_registry()
  expect_equal(nrow(registry_check(reg, prepared)), 0L)
  registry_import(reg, prepared, user = "tester")
  expect_equal(registry_data(reg), sim, ignore_attr = TRUE)
})

test_that("registry_check reports every problem and writes nothing", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  registry_add(reg, a_record("SYN-2"), user = "tester")
  registry_delete(reg, "SYN-2", user = "tester", reason = "mistake")
  n <- c(n_rows(reg, "records"), n_rows(reg, "audit"))

  d <- data.frame(
    record_id = c("SYN-1", "SYN-2", "NEW-1", NA, "NEW-1"),
    visit_date = "2024-05-01", sex_at_birth = "female",
    age_years = c(60, 60, 250, 60, 60),
    stringsAsFactors = FALSE)
  issues <- registry_check(reg, d, lang = "es")
  key <- paste(issues$row, issues$rule)
  expect_true(all(c("1 duplicate_id", "2 duplicate_id", "3 range",
                    "4 required", "5 duplicate_id") %in% key))
  expect_match(issues$message[issues$rule == "range"], "fuera del rango")
  expect_equal(c(n_rows(reg, "records"), n_rows(reg, "audit")), n)

  expect_error(registry_check(reg, data.frame(nope = 1)), "Unknown variable")
  expect_error(registry_check(reg, data.frame()), "at least one row")
  clean <- data.frame(record_id = "NEW-9", visit_date = "2024-05-01",
                      sex_at_birth = "male", age_years = 40)
  expect_equal(nrow(registry_check(reg, clean)), 0L)
})
