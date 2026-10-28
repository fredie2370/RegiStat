busy_registry <- function(env = parent.frame()) {
  reg <- new_registry(env)
  registry_import(reg, synthetic_t2d(10, seed = 21)$data, user = "tester")
  registry_update(reg, "SYN-0002", list(age_years = 33), user = "ana",
                  reason = "typo")
  registry_delete(reg, "SYN-0003", user = "ana", reason = "duplicate")
  reg
}

test_that("a fresh and a busy registry both verify", {
  reg <- new_registry()
  expect_true(registry_verify(reg)$ok)
  expect_equal(registry_verify(reg)$n_entries, 1L)

  reg2 <- busy_registry()
  v <- registry_verify(reg2)
  expect_true(v$ok)
  expect_equal(v$n_entries, 1L + 10L + 1L + 1L)
  expect_output(print(v), "no problems")
})

test_that("the audit chain is linked and starts at the genesis hash", {
  reg <- busy_registry()
  a <- DBI::dbGetQuery(reg$con, "SELECT seq, prev_hash, hash FROM audit ORDER BY seq")
  expect_equal(a$seq, seq_len(nrow(a)))
  expect_equal(a$prev_hash[1], strrep("0", 64))
  expect_equal(a$prev_hash[-1], a$hash[-nrow(a)])
  expect_equal(registry_audit(reg)$action[1], "create")
})

test_that("the database refuses to edit or delete audit entries", {
  reg <- busy_registry()
  expect_error(DBI::dbExecute(reg$con, "UPDATE audit SET actor = 'x'"),
               "append-only")
  expect_error(DBI::dbExecute(reg$con, "DELETE FROM audit"), "append-only")
  expect_true(registry_verify(reg)$ok)
})

test_that("editing an audit entry is detected", {
  reg <- busy_registry()
  DBI::dbExecute(reg$con, "DROP TRIGGER audit_no_update")
  DBI::dbExecute(reg$con, "UPDATE audit SET actor = 'mallory' WHERE seq = 4")
  v <- registry_verify(reg)
  expect_false(v$ok)
  expect_match(paste(v$problems, collapse = " "), "entry 4: content altered")
  expect_output(print(v), "PROBLEMS FOUND")
})

test_that("an entry rewritten with a recomputed hash breaks the next link", {
  reg <- busy_registry()
  DBI::dbExecute(reg$con, "DROP TRIGGER audit_no_update")
  e <- DBI::dbGetQuery(reg$con, "SELECT * FROM audit WHERE seq = 4")
  new_hash <- RegiStat:::entry_hash(e$seq, e$ts, "mallory", e$action,
                                    e$record_id, e$changes, e$reason,
                                    e$prev_hash)
  DBI::dbExecute(reg$con, "UPDATE audit SET actor = ?, hash = ? WHERE seq = 4",
                 params = list("mallory", new_hash))
  v <- registry_verify(reg)
  expect_false(v$ok)
  expect_match(paste(v$problems, collapse = " "), "entry 5: broken link")
})

test_that("a removed entry in the middle of the chain is detected", {
  reg <- busy_registry()
  DBI::dbExecute(reg$con, "DROP TRIGGER audit_no_delete")
  DBI::dbExecute(reg$con, "DELETE FROM audit WHERE seq = 3")
  v <- registry_verify(reg)
  expect_false(v$ok)
  expect_match(paste(v$problems, collapse = " "), "sequence gap|broken link")
})

test_that("records edited directly in the database are detected", {
  reg <- busy_registry()
  old <- DBI::dbGetQuery(
    reg$con, "SELECT data FROM records WHERE record_id = 'SYN-0001'")$data
  new <- sub('"age_years":"[0-9]+"', '"age_years":"99"', old)
  expect_false(identical(old, new))
  DBI::dbExecute(reg$con, "UPDATE records SET data = ? WHERE record_id = 'SYN-0001'",
                 params = list(new))
  v <- registry_verify(reg)
  expect_false(v$ok)
  expect_match(paste(v$problems, collapse = " "), "SYN-0001.*audit trail")
})

test_that("flags and rows changed outside RegiStat are detected", {
  reg <- busy_registry()
  DBI::dbExecute(reg$con, "UPDATE records SET deleted = 1 WHERE record_id = 'SYN-0004'")
  expect_match(paste(registry_verify(reg)$problems, collapse = " "),
               "SYN-0004.*deleted flag")

  reg2 <- busy_registry()
  DBI::dbExecute(reg2$con, "DELETE FROM records WHERE record_id = 'SYN-0005'")
  expect_match(paste(registry_verify(reg2)$problems, collapse = " "),
               "SYN-0005.*missing from the table")

  reg3 <- busy_registry()
  DBI::dbExecute(
    reg3$con,
    paste("INSERT INTO records VALUES ('GHOST', '{}', 99,",
          "'2024-01-01T00:00:00.000Z', '2024-01-01T00:00:00.000Z', 1, 0)"))
  expect_match(paste(registry_verify(reg3)$problems, collapse = " "),
               "GHOST.*not in the audit trail")
})

test_that("an externally saved head detects removal of the last entries", {
  reg <- busy_registry()
  registry_add(reg, a_record("SYN-LAST"), user = "tester")
  head <- registry_verify(reg)$head

  # An attacker removes the last record together with its audit entry.
  DBI::dbExecute(reg$con, "DROP TRIGGER audit_no_delete")
  DBI::dbExecute(reg$con, "DELETE FROM audit WHERE record_id = 'SYN-LAST'")
  DBI::dbExecute(reg$con, "DELETE FROM records WHERE record_id = 'SYN-LAST'")

  # Without an outside anchor the registry looks consistent ...
  expect_true(registry_verify(reg)$ok)
  # ... with the saved head the removal is caught.
  v <- registry_verify(reg, expected_head = head)
  expect_false(v$ok)
  expect_match(paste(v$problems, collapse = " "), "head of the audit chain")
})

test_that("the stored dictionary is protected by a hash", {
  path <- withr::local_tempfile(fileext = ".sqlite")
  reg <- registry_create(path, load_template("t2d"), user = "tester")
  DBI::dbExecute(reg$con, paste(
    "UPDATE meta SET value = replace(value, 'Serum creatinine', 'Altered')",
    "WHERE key = 'dictionary'"))
  registry_close(reg)
  expect_warning(reg2 <- registry_open(path), "does not match its recorded hash")
  registry_close(reg2)
})

test_that("a failed write leaves no partial changes behind", {
  reg <- new_registry()
  registry_add(reg, a_record("SYN-1"), user = "tester")
  n <- c(n_rows(reg, "records"), n_rows(reg, "audit"))
  # Force a failure after the audit entry was written: the table insert
  # collides with an existing primary key that the pre-check cannot see.
  DBI::dbExecute(reg$con, "DROP TRIGGER audit_no_delete")
  expect_error(
    RegiStat:::with_write(reg, {
      RegiStat:::append_audit(reg$con, "t", "add", "X", "{}", NA)
      stop("boom")
    }),
    "boom"
  )
  expect_equal(c(n_rows(reg, "records"), n_rows(reg, "audit")), n)
  expect_true(registry_verify(reg)$ok)
})
