test_that("file numbers are normalised", {
  n <- RegiStat:::normalize_file_no
  expect_equal(n(c(" 90913.0 ", "ab-12", "007", "", NA, "12.5")),
               c("90913", "AB-12", "007", NA, NA, "12.5"))
  expect_equal(n("ab", list(strip_decimal_zero = TRUE, upper = FALSE)), "ab")
})

test_that("column letters are understood", {
  expect_equal(RegiStat:::column_index("B"), 2L)
  expect_equal(RegiStat:::column_index("aa"), 27L)
  expect_equal(RegiStat:::column_index(3), 3L)
  expect_error(RegiStat:::column_index("B2"), "letters")
})

test_that("a reference list is read from CSV with title rows and separators", {
  f <- withr::local_tempfile(fileext = ".csv")
  writeLines(c("Diabetes list", "n;file", "1;90913.0", "2;123/456", "3;",
               "4; 789 "), f)
  expect_equal(read_reference_list(f, column = "B", first_row = 3),
               c("90913.0", "123", "456", "789"))
  expect_equal(read_reference_list(f, column = 2, first_row = 3, separator = ""),
               c("90913.0", "123/456", "789"))
  expect_error(read_reference_list(f, column = 5), "only")
})

test_that("a reference list is read from Excel", {
  skip_if_not_installed("writexl")
  f <- withr::local_tempfile(fileext = ".xlsx")
  writexl::write_xlsx(list(
    Other = data.frame(a = 1),
    DIABETICS = data.frame(n = c("title", "x"), file = c("hdr", "5/6"))), f)
  expect_equal(read_reference_list(f, sheet = "DIABETICS", column = "B",
                                   first_row = 3), c("5", "6"))
  expect_equal(read_reference_list(f, sheet = "DIABETICS", column = "B",
                                   first_row = 2), c("hdr", "5", "6"))
})

test_that("store creation and opening", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  scr <- screening_create(path, user = "u")
  expect_s3_class(scr, "registat_screening")
  expect_error(screening_create(path, user = "u"), "already exists")
  screening_close(scr)
  scr <- screening_open(path)
  expect_true(screening_verify(scr)$ok)
  screening_close(scr)
  # a registry is not a case-finding store
  rpath <- withr::local_tempfile(fileext = ".sqlite")
  reg <- registry_create(rpath, load_template("t2d"), user = "u")
  registry_close(reg)
  expect_error(screening_open(rpath), "not a RegiStat case-finding")
  scr <- screening_open(path)
  expect_output(print(scr), "case-finding")
  screening_close(scr)
})

test_that("reference list versions, derived status and changes", {
  scr <- new_screening()
  expect_error(screening_load_list(scr, c(NA, ""), user = "u"), "no file numbers")
  expect_equal(screening_load_list(scr, c("100.0", "200", "200", "abc"), user = "u"), 1L)
  expect_equal(screening_lists(scr)$n_items, 3L)
  expect_error(screening_load_list(scr, c("ABC", "200", "100"), user = "u"),
               "identical")

  screening_add_review(scr, "100", "2024-05-02", TRUE, user = "u")
  screening_add_review(scr, " abc ", "2024-05-03", TRUE, user = "u")
  screening_add_review(scr, "300", "2024-06-01", FALSE, user = "u")
  r <- screening_reviews(scr)
  expect_equal(r$listed, c(TRUE, TRUE, FALSE))
  expect_equal(r$found, c(TRUE, TRUE, FALSE))
  expect_equal(screening_pending(scr), "200")

  # new version: 100 leaves the list, 300 joins. Old reviews re-derive.
  expect_equal(screening_load_list(scr, c("200", "300", "ABC"), user = "u"), 2L)
  r2 <- screening_reviews(scr)
  expect_equal(r2$listed, c(FALSE, TRUE, TRUE))
  # but the old version still answers the old way
  expect_equal(screening_reviews(scr, version = 1)$listed, c(TRUE, TRUE, FALSE))
  ch <- screening_changes(scr)
  expect_equal(ch$added, "300")
  expect_equal(ch$removed, "100")
  expect_equal(sort(ch$reviews$file_no), c("100", "300"))
  expect_true(screening_verify(scr)$ok)
})

test_that("status is NA until a list exists, and changes need two versions", {
  scr <- new_screening()
  screening_add_review(scr, "1", "2024-01-01", FALSE, user = "u")
  expect_true(is.na(screening_reviews(scr)$listed))
  expect_error(screening_changes(scr), "two versions")
})

test_that("review validation", {
  scr <- new_screening()
  expect_error(screening_add_review(scr, "", "2024-01-01", TRUE, user = "u"), "file number")
  expect_error(screening_add_review(scr, "1", "2024-02-30", TRUE, user = "u"), "not a valid date")
  expect_error(screening_add_review(scr, "1", "01/02/2024", TRUE, user = "u"), "not a valid date")
  expect_error(screening_add_review(scr, "1", "2024-01-01", NA, user = "u"), "found")
  expect_error(screening_add_review(scr, "1", "2024-01-01", TRUE, user = ""), "user")
  id <- screening_add_review(scr, "1", "2024-01-01", "not_found", user = "u")
  expect_equal(id, 1L)
  expect_false(screening_reviews(scr)$found)
  expect_error(screening_add_review(scr, "1.0", "2024-01-01", TRUE, user = "u",
                                    lang = "es"), "ya tiene una revisi")
  # same file, another date, is fine
  expect_equal(screening_add_review(scr, "1", as.Date("2024-02-01"), TRUE, user = "u"), 2L)
  expect_equal(n_rows(scr, "reviews"), 2L)
  expect_equal(n_rows(scr, "audit"), 3L)
})

test_that("update and delete need a reason and are audited", {
  scr <- new_screening()
  id <- screening_add_review(scr, "1", "2024-01-01", FALSE, user = "u", note = "x")
  expect_error(screening_update_review(scr, id, list(found = TRUE), "u", ""), "reason")
  expect_error(screening_update_review(scr, id, list(found = FALSE), "u", "r"), "No changes")
  expect_error(screening_update_review(scr, id, list(file_no = "2"), "u", "r"), "named list")
  screening_update_review(scr, id, list(found = TRUE, note = NULL), "u", "Mistyped")
  r <- screening_reviews(scr)
  expect_true(r$found)
  expect_true(is.na(r$note))
  expect_error(screening_update_review(scr, 99, list(found = TRUE), "u", "r"), "not found")
  expect_error(screening_delete_review(scr, id, "u", " "), "reason")
  screening_delete_review(scr, id, "u", "Duplicate")
  expect_equal(nrow(screening_reviews(scr)), 0L)
  expect_equal(nrow(screening_reviews(scr, include_deleted = TRUE)), 1L)
  expect_error(screening_update_review(scr, id, list(found = FALSE), "u", "r"), "deleted")
  # a deleted review frees the (file, date) slot
  expect_equal(screening_add_review(scr, "1", "2024-01-01", TRUE, user = "u"), 2L)
  expect_equal(screening_audit(scr)$action,
               c("create", "review_add", "review_update", "review_delete", "review_add"))
  expect_true(screening_verify(scr)$ok)
})

test_that("summary counts by month", {
  scr <- new_screening()
  screening_load_list(scr, c("1", "2"), user = "u")
  screening_add_review(scr, "1", "2024-01-05", TRUE, user = "u")
  screening_add_review(scr, "2", "2024-01-06", TRUE, user = "u")
  screening_add_review(scr, "9", "2024-01-07", FALSE, user = "u")
  screening_add_review(scr, "1", "2024-02-05", TRUE, user = "u")
  s <- screening_summary(scr)
  expect_equal(s$n[s$month == "2024-01" & s$found & s$listed], 2L)
  expect_equal(s$n[s$month == "2024-01" & !s$found & !s$listed], 1L)
  expect_equal(sum(s$n), 4L)
  expect_equal(nrow(screening_summary(new_screening())), 0L)
})

test_that("tampering with the case-finding store is detected", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  scr <- screening_create(path, user = "u")
  withr::defer(screening_close(scr))
  screening_load_list(scr, c("1", "2"), user = "u")
  screening_add_review(scr, "1", "2024-01-05", TRUE, user = "u")
  expect_true(screening_verify(scr)$ok)
  head <- screening_verify(scr)$head

  edit <- function(sql) {
    con <- DBI::dbConnect(RSQLite::SQLite(), path)
    on.exit(DBI::dbDisconnect(con))
    DBI::dbExecute(con, sql)
  }
  edit("UPDATE reviews SET found = 0")
  v <- screening_verify(scr)
  expect_false(v$ok)
  expect_match(v$problems, "Review 1 does not match", all = FALSE)
  edit("UPDATE reviews SET found = 1")
  expect_true(screening_verify(scr)$ok)

  edit("INSERT INTO reflist_items VALUES (1, '999')")
  expect_match(screening_verify(scr)$problems, "version 1 does not match", all = FALSE)
  edit("DELETE FROM reflist_items WHERE item = '999'")
  expect_true(screening_verify(scr)$ok)

  expect_error(edit("DELETE FROM audit WHERE seq = 3"), "append-only")
  screening_add_review(scr, "2", "2024-01-06", TRUE, user = "u")
  expect_false(screening_verify(scr, expected_head = head)$ok)
})
