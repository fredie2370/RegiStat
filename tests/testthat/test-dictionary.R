test_that("the bundled t2d template loads and is bilingual", {
  dict <- load_template("t2d")
  expect_s3_class(dict, "registat_dictionary")
  expect_true("t2d" %in% list_templates())
  tab_en <- dictionary_table(dict, "en")
  tab_es <- dictionary_table(dict, "es")
  expect_equal(nrow(tab_en), length(dict$variables))
  expect_equal(tab_en$variable, tab_es$variable)
  expect_false(identical(tab_en$label, tab_es$label))
  expect_identical(sum(tab_en$type == "id"), 1L)
})

test_that("unknown templates and files are rejected", {
  expect_error(load_template("does_not_exist"), "Unknown template")
  expect_error(read_dictionary("no/such/file.yml"), "not found")
})

test_that("malformed dictionaries are rejected", {
  dict <- load_template("t2d")
  idx <- function(name) {
    which(vapply(dict$variables, function(v) v$name, "") == name)
  }

  d <- dict
  d$variables[[2]]$name <- d$variables[[1]]$name
  expect_error(validate_dictionary(d), "Duplicate variable")

  d <- dict
  d$variables[[2]]$type <- "banana"
  expect_error(validate_dictionary(d), "invalid 'type'")

  d <- dict
  d$variables[[2]]$label <- list(en = "only English")
  expect_error(validate_dictionary(d), "Both 'en' and 'es'")

  d <- dict
  d$variables[[idx("age_years")]]$min <- 200
  expect_error(validate_dictionary(d), "greater than")

  d <- dict
  d$variables[[idx("ckd")]]$choices[[1]]$value <- TRUE
  expect_error(validate_dictionary(d), "Quote values")

  d <- dict
  d$variables[[1]]$type <- "text"
  expect_error(validate_dictionary(d), "Exactly one variable")
})

test_that("rule expressions cannot run arbitrary code", {
  dict <- load_template("t2d")

  d <- dict
  d$rules[[1]]$expr <- "system('echo hi') == 0"
  expect_error(validate_dictionary(d), "not allowed")

  d <- dict
  d$rules[[1]]$expr <- "unknown_var < age_years"
  expect_error(validate_dictionary(d), "Unknown variable")

  d <- dict
  d$rules[[1]]$expr <- "age_years <"
  expect_error(validate_dictionary(d), "cannot be parsed")
})

test_that("accented characters survive reading, whatever the system locale", {
  dict <- load_template("t2d")
  tab <- dictionary_table(dict, "es")
  expect_identical(
    enc2utf8(tab$label[tab$variable == "record_id"]),
    "Código de registro del estudio"
  )
  expect_identical(
    enc2utf8(tab$label[tab$variable == "creatinine_mgdl"]),
    "Creatinina sérica"
  )
})

test_that("Spanish validation messages keep their accents", {
  d <- synthetic_t2d(30, seed = 4)$data
  d$age_years[3] <- 250L
  es <- validate_data(d, dict = load_template("t2d"), lang = "es")
  expect_true(grepl("está fuera del rango", es$message))
})
