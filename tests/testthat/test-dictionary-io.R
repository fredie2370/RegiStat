same_dict <- function(a, b) {
  norm <- function(x) if (is.list(x)) {
    x <- lapply(x, norm)
    if (!is.null(names(x))) x <- x[order(names(x))]
    x
  } else x
  expect_equal(norm(unclass(a)), norm(unclass(b)))
}

test_that("tables round-trip the bundled template", {
  d <- load_template("t2d")
  d2 <- dictionary_from_tables(dictionary_to_tables(d))
  same_dict(d2, d)
})

test_that("xlsx template is readable and equals the minimal dictionary", {
  skip_if_not_installed("writexl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  dictionary_template_xlsx(f)
  d <- read_dictionary(f)
  expect_s3_class(d, "registat_dictionary")
  same_dict(d, minimal_dictionary())
  f2 <- withr::local_tempfile(fileext = ".xlsx")
  dictionary_template_xlsx(f2, "t2d")
  same_dict(read_dictionary(f2), load_template("t2d"))
})

test_that("YAML written and read back is identical", {
  f <- withr::local_tempfile(fileext = ".yml")
  write_dictionary(load_template("t2d"), f)
  same_dict(read_dictionary(f), load_template("t2d"))
})

test_that("errors name the sheet and row", {
  t <- dictionary_to_tables(minimal_dictionary())
  bad <- t; bad$Variables$min[3] <- "abc"
  expect_error(dictionary_from_tables(bad), "Variables', row 4.*'min'")
  bad <- t; bad$Choices <- bad$Choices[0, ]
  expect_error(dictionary_from_tables(bad), "needs rows in the sheet 'Choices'")
  bad <- t; bad$Variables$type[2] <- "datetime"
  expect_error(dictionary_from_tables(bad), "invalid 'type'")
  bad <- t; bad$Rules$expr <- "system('x')"
  expect_error(dictionary_from_tables(bad), "not allowed")
  bad <- t; bad$Study <- NULL
  expect_error(dictionary_from_tables(bad), "'Study' is missing")
  bad <- t; bad$Choices$variable[1] <- "nope"
  expect_error(dictionary_from_tables(bad), "not in 'Variables'")
})

test_that("a single-language label is copied and codes keep leading zeros", {
  t <- dictionary_to_tables(minimal_dictionary())
  t$Variables$label_es[3] <- NA
  t$Choices$value[1] <- "01"
  t$Choices$value[2] <- "02"
  d <- dictionary_from_tables(t)
  expect_equal(d$variables[[3]]$label$es, d$variables[[3]]$label$en)
  expect_equal(d$variables[[6]]$choices[[1]]$value, "01")
})

test_that("launchers are written for each system", {
  d <- withr::local_tempdir()
  w <- create_launcher(d, "C:\\Users\\x\\RegiStat", "en", "windows")
  txt <- rawToChar(readBin(w, "raw", file.size(w)))
  expect_match(txt, "\r\n", fixed = TRUE)
  expect_match(txt, "RegiStat::run_app(folder = \\\"C:/Users/x/RegiStat\\\"", fixed = TRUE)
  m <- create_launcher(d, "/home/x/R", "es", "macos")
  expect_equal(basename(m), "RegiStat.command")
  if (.Platform$OS.type != "windows") expect_true(file.access(m, 1) == 0)
  expect_match(readLines(m), "lang = \"es\"", fixed = TRUE, all = FALSE)
  expect_error(create_launcher(file.path(d, "nope")), "does not exist")
})
