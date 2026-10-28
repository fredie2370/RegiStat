test_that("a registry can be created, closed and reopened", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  reg <- registry_create(path, load_template("t2d"), user = "tester")
  registry_add(reg, a_record(), user = "tester")
  registry_close(reg)

  reg2 <- registry_open(path)
  withr::defer(registry_close(reg2))
  expect_equal(nrow(registry_data(reg2)), 1L)
  expect_equal(dictionary_table(reg2$dict, "en"),
               dictionary_table(load_template("t2d"), "en"))
  expect_output(print(reg2), "1 active")
})

test_that("create refuses existing files, open refuses foreign files", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  reg <- registry_create(path, load_template("t2d"), user = "tester")
  registry_close(reg)
  expect_error(registry_create(path, load_template("t2d"), user = "tester"),
               "already exists")
  expect_error(registry_open(withr::local_tempfile()), "not found")

  other <- withr::local_tempfile(fileext = ".sqlite")
  con <- DBI::dbConnect(RSQLite::SQLite(), other)
  DBI::dbExecute(con, "CREATE TABLE x (a INTEGER)")
  DBI::dbDisconnect(con)
  expect_error(registry_open(other), "not a RegiStat registry")
})

test_that("user and registry arguments are checked", {
  reg <- new_registry()
  expect_error(registry_add(reg, a_record(), user = ""), "non-empty")
  expect_error(registry_add(list(), a_record(), user = "u"), "registry_create")
  registry_close(reg)
  expect_error(registry_add(reg, a_record(), user = "u"), "closed")
})

test_that("valid records are stored with the right types", {
  reg <- new_registry()
  registry_add(reg, a_record(weight_kg = 82.5, visit_date = as.Date("2024-05-01")),
               user = "tester")
  d <- registry_data(reg)
  expect_s3_class(d$visit_date, "Date")
  expect_type(d$age_years, "integer")
  expect_type(d$weight_kg, "double")
  expect_equal(d$weight_kg, 82.5)
  expect_true(is.na(d$height_cm))
  expect_equal(names(d), variable_names(reg$dict))
})

test_that("invalid records are refused and nothing is written", {
  reg <- new_registry()
  before <- c(n_rows(reg, "records"), n_rows(reg, "audit"))
  err <- tryCatch(registry_add(reg, a_record(age_years = 250), user = "tester"),
                  registat_validation_error = function(e) e)
  expect_s3_class(err, "registat_validation_error")
  expect_equal(err$issues$rule, "range")
  expect_error(registry_add(reg, a_record(age_years = NA), user = "tester"),
               class = "registat_validation_error")
  expect_error(registry_add(reg, a_record(bogus = 1), user = "tester"),
               "Unknown variable")
  expect_equal(c(n_rows(reg, "records"), n_rows(reg, "audit")), before)
})

test_that("validation errors honour the requested language", {
  reg <- new_registry()
  err <- tryCatch(
    registry_add(reg, a_record(age_years = 250), user = "tester", lang = "es"),
    registat_validation_error = function(e) e
  )
  expect_match(conditionMessage(err), "fuera del rango")
})

test_that("identifiers are unique, also against deleted records", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  err <- tryCatch(registry_add(reg, a_record("SYN-1"), user = "tester"),
                  registat_validation_error = function(e) e)
  expect_equal(err$issues$rule, "duplicate_id")

  registry_delete(reg, "SYN-1", user = "tester", reason = "entered by mistake")
  expect_error(registry_add(reg, a_record("SYN-1"), user = "tester"),
               class = "registat_validation_error")
  expect_equal(n_rows(reg, "records"), 1L)
})

test_that("updates need a reason and keep old and new values", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  expect_error(registry_update(reg, "SYN-1", list(age_years = 61),
                               user = "tester", reason = ""),
               "reason")
  registry_update(reg, "SYN-1", list(age_years = 61, weight_kg = 80),
                  user = "ana", reason = "Corrected after source check")
  expect_equal(registry_data(reg)$age_years, 61L)

  log <- registry_audit(reg, "SYN-1")
  expect_equal(log$action, c("add", "update"))
  expect_equal(log$user[2], "ana")
  expect_equal(log$reason[2], "Corrected after source check")
  ch <- jsonlite::fromJSON(log$changes[2], simplifyVector = FALSE)
  expect_equal(ch$age_years$old, "60")
  expect_equal(ch$age_years$new, "61")
  expect_null(ch$weight_kg$old)
  expect_equal(ch$weight_kg$new, "80")
  expect_equal(DBI::dbGetQuery(reg$con, "SELECT version FROM records")$version, 2L)
})

test_that("invalid, empty and identifier-changing updates are refused", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  n <- n_rows(reg, "audit")
  expect_error(registry_update(reg, "SYN-1", list(age_years = 250),
                               user = "t", reason = "x"),
               class = "registat_validation_error")
  expect_error(registry_update(reg, "SYN-1", list(age_years = 60),
                               user = "t", reason = "x"), "No changes")
  expect_error(registry_update(reg, "SYN-1", list(record_id = "SYN-2"),
                               user = "t", reason = "x"), "cannot be changed")
  expect_error(registry_update(reg, "SYN-1", list(age_years = NA),
                               user = "t", reason = "x"),
               class = "registat_validation_error")
  expect_error(registry_update(reg, "SYN-NOPE", list(age_years = 50),
                               user = "t", reason = "x"), "not found")
  expect_error(registry_update(reg, "SYN-1", list(nope = 1),
                               user = "t", reason = "x"), "Unknown variable")
  expect_equal(n_rows(reg, "audit"), n)
})

test_that("delete is soft, needs a reason and blocks later updates", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  registry_add(reg, a_record("SYN-2"), user = "tester")
  expect_error(registry_delete(reg, "SYN-1", user = "t", reason = " "), "reason")
  registry_delete(reg, "SYN-1", user = "t", reason = "duplicate patient")

  expect_equal(registry_data(reg)$record_id, "SYN-2")
  all <- registry_data(reg, include_deleted = TRUE)
  expect_equal(all$record_id, c("SYN-1", "SYN-2"))
  expect_equal(all$.deleted, c(TRUE, FALSE))
  expect_error(registry_delete(reg, "SYN-1", user = "t", reason = "again"),
               "already deleted")
  expect_error(registry_update(reg, "SYN-1", list(age_years = 50),
                               user = "t", reason = "x"), "deleted")
  expect_equal(n_rows(reg, "records"), 2L)
})

test_that("import is all-or-nothing and round-trips synthetic data", {
  reg <- new_registry()
  sim <- synthetic_t2d(50, seed = 11)
  expect_equal(registry_import(reg, sim$data, user = "tester"), 50L)
  expect_equal(registry_data(reg), sim$data, ignore_attr = TRUE)
  expect_true(registry_verify(reg)$ok)

  bad <- synthetic_t2d(40, seed = 12, error_rate = 0.2)$data
  bad$record_id <- sub("SYN-", "BAD-", bad$record_id)
  expect_error(registry_import(reg, bad, user = "tester"),
               class = "registat_validation_error")
  expect_equal(n_rows(reg, "records"), 50L)

  again <- sim$data[1:3, ]
  err <- tryCatch(registry_import(reg, again, user = "tester"),
                  registat_validation_error = function(e) e)
  expect_equal(unique(err$issues$rule), "duplicate_id")
  expect_equal(n_rows(reg, "records"), 50L)
})

test_that("text with accents survives storage and verification", {
  dict <- validate_dictionary(list(
    study = list(id = "mini"),
    variables = list(
      list(name = "pid", type = "id", required = TRUE,
           label = list(en = "ID", es = "ID")),
      list(name = "note", type = "text",
           label = list(en = "Note", es = "Nota"))
    )
  ))
  path <- withr::local_tempfile(fileext = ".sqlite")
  reg <- registry_create(path, dict, user = "tester")
  withr::defer(registry_close(reg))
  note <- "Dolor de cabeza, riñón, áéíóú"
  registry_add(reg, list(pid = "A1", note = note), user = "tester")
  expect_identical(enc2utf8(registry_data(reg)$note), note)
  expect_true(registry_verify(reg)$ok)
})
