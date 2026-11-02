# Server logic of the Shiny modules, tested without a browser.

test_that("every interface key has both languages", {
  s <- RegiStat:::ui_strings()
  for (k in names(s)) {
    expect_true(all(c("en", "es") %in% names(s[[k]])), info = k)
    expect_true(nzchar(s[[k]]$en) && nzchar(s[[k]]$es), info = k)
  }
})

test_that("capture saves a valid record and refuses an invalid one", {
  reg <- new_registry()
  state <- shiny::reactiveValues(reg = reg, tick = 0L)
  shiny::testServer(
    RegiStat:::capture_server,
    args = list(state = state, lang = shiny::reactive("en"),
                user = shiny::reactive("tester")),
    {
      # no user name
      session$setInputs(f_record_id = "A-1", save = 1)
      expect_equal(n_rows(reg, "records"), 0L)
    })

  shiny::testServer(
    RegiStat:::capture_server,
    args = list(state = state, lang = shiny::reactive("en"),
                user = shiny::reactive("tester")),
    {
      session$setInputs(f_record_id = "A-1", f_visit_date = "2024-05-01",
                        f_sex_at_birth = "female", f_age_years = "250",
                        f_weight_kg = "82,5", save = 1)
      expect_equal(n_rows(reg, "records"), 0L)   # age out of range
      session$setInputs(f_age_years = "61", save = 2)
      expect_equal(n_rows(reg, "records"), 1L)
      d <- registry_data(reg)
      expect_equal(d$weight_kg, 82.5)                  # decimal comma accepted
      expect_equal(state$tick, 1L)
      # same code again is a duplicate
      session$setInputs(save = 3)
      expect_equal(n_rows(reg, "records"), 1L)
    })
})

test_that("audit module lists entries and verifies", {
  reg <- new_registry()
  registry_add(reg, a_record("A-1"), user = "tester")
  state <- shiny::reactiveValues(reg = reg, tick = 0L)
  shiny::testServer(
    RegiStat:::audit_server,
    args = list(state = state, lang = shiny::reactive("es"),
                user = shiny::reactive("tester")),
    {
      session$setInputs(verify = 1, expected_head = "")
      html <- as.character(output$verdict$html)
      expect_match(html, "alert-success")
      session$setInputs(verify = 2, expected_head = "0000")
      expect_match(as.character(output$verdict$html), "alert-danger")
    })
})

test_that("audit summaries describe changes in the chosen language", {
  dict <- load_template("t2d")
  ch <- '{"age_years":{"old":"60","new":"61"}}'
  expect_match(RegiStat:::audit_summary("update", ch, dict, "en"), "Age: 60 -> 61")
  expect_equal(RegiStat:::audit_action_label(c("add", "delete"), "es"),
               c("Alta", "Eliminado"))
})

test_that("form values and decimal commas", {
  dict <- load_template("t2d")
  input <- list(f_record_id = " X ", f_weight_kg = "70,5", f_age_years = "",
                f_sex_at_birth = "")
  v <- RegiStat:::form_values(input, dict)
  expect_equal(v$record_id, "X")
  expect_equal(v$weight_kg, "70.5")
  expect_true(is.na(v$age_years))
  expect_true(is.na(v$sex_at_birth))
  expect_true(is.na(v$hba1c_pct))
})

test_that("write_csv_excel writes a BOM, CRLF and escapes", {
  f <- withr::local_tempfile(fileext = ".csv")
  df <- data.frame(a = c("x,y", "say \"hi\""), b = c(NA, "2"),
                   stringsAsFactors = FALSE)
  RegiStat:::write_csv_excel(df, f)
  raw <- readBin(f, "raw", file.info(f)$size)
  expect_identical(raw[1:3], as.raw(c(0xef, 0xbb, 0xbf)))
  txt <- rawToChar(raw[-(1:3)])
  expect_equal(txt, "a,b\r\n\"x,y\",\r\n\"say \"\"hi\"\"\",2\r\n")
})

test_that("records module edits and deletes with a mandatory reason", {
  reg <- new_registry()
  registry_add(reg, a_record("A-1"), user = "tester")
  state <- shiny::reactiveValues(reg = reg, tick = 0L)
  shiny::testServer(
    RegiStat:::records_server,
    args = list(state = state, lang = shiny::reactive("en"),
                user = shiny::reactive("tester")),
    {
      session$setInputs(show_deleted = FALSE, table_rows_selected = 1L)
      session$setInputs(edit = 1)
      # change age but give no reason: nothing written
      session$setInputs(e_age_years = "62", e_visit_date = "2024-05-01",
                        e_sex_at_birth = "female", edit_reason = "", edit_save = 1)
      expect_equal(registry_data(reg)$age_years, 60L)
      session$setInputs(edit_reason = "Source check", edit_save = 2)
      expect_equal(registry_data(reg)$age_years, 62L)
      expect_equal(state$tick, 1L)

      session$setInputs(delete = 1)
      session$setInputs(delete_reason = "", delete_confirm = 1)
      expect_equal(nrow(registry_data(reg)), 1L)
      session$setInputs(delete_reason = "Duplicate", delete_confirm = 2)
      expect_equal(nrow(registry_data(reg)), 0L)
      expect_equal(nrow(registry_data(reg, include_deleted = TRUE)), 1L)
    })
})

test_that("case-finding module activates, saves reviews and re-derives status", {
  reg <- new_registry()
  dir <- withr::local_tempdir()
  reg$path <- file.path(dir, "registry.sqlite")   # screening.sqlite goes next to it
  state <- shiny::reactiveValues(reg = reg, tick = 0L, scr = NULL, stick = 0L)
  withr::defer(if (!is.null(shiny::isolate(state$scr))) screening_close(shiny::isolate(state$scr)))
  shiny::testServer(
    RegiStat:::screening_server,
    args = list(state = state, lang = shiny::reactive("en"),
                user = shiny::reactive("tester")),
    {
      expect_null(state$scr)
      session$setInputs(activate = 1)
      expect_s3_class(state$scr, "registat_screening")
      expect_true(file.exists(file.path(dir, "screening.sqlite")))

      session$setInputs(file_no = "123", review_date = "2024-05-02",
                        study_code = "", note = "", found = NULL, save = 1)
      expect_equal(n_rows(state$scr, "reviews"), 0L)       # found not chosen
      session$setInputs(found = "1", save = 2)
      expect_equal(n_rows(state$scr, "reviews"), 1L)
      expect_true(is.na(screening_reviews(state$scr)$listed))

      screening_load_list(state$scr, c("123", "5"), user = "tester")
      expect_true(screening_reviews(state$scr)$listed)
      session$setInputs(found = "0", file_no = "999", save = 3)
      expect_equal(screening_reviews(state$scr)$listed, c(TRUE, FALSE))
      expect_equal(state$stick, 2L)
    })
})

test_that("every interface text used in the code exists", {
  # Run from the source tree or from an installed package.
  rdir <- if (dir.exists(file.path("..", "..", "R"))) file.path("..", "..", "R") else NULL
  skip_if(is.null(rdir), "R sources not available")
  files <- list.files(rdir, pattern = "[.]R$", full.names = TRUE)
  used <- unique(unlist(lapply(files, function(f) {
    txt <- paste(readLines(f, encoding = "UTF-8", warn = FALSE), collapse = "\n")
    m <- regmatches(txt, gregexpr("ui_text\\(\\s*\"[A-Za-z0-9_]+\"", txt))[[1]]
    sub(".*\"([A-Za-z0-9_]+)\"$", "\\1", m)
  })))
  expect_gt(length(used), 50)
  expect_equal(setdiff(used, names(RegiStat:::ui_strings())), character())
  # keys that YAML would read as booleans would not be found by name
  expect_false(any(names(RegiStat:::ui_strings()) %in% c("TRUE", "FALSE")))
})
