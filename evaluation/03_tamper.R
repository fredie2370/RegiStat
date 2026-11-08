# Evaluation 3: detection of alterations of the registry file.
# Each class of alteration is applied directly to the SQLite file, as someone
# with file access would do (the protective triggers are dropped first), at
# random positions, and registry_verify() is run. "Detected without anchor"
# uses only the file; "detected with anchor" also compares the head hash with
# a value recorded earlier (the expected_head argument).
# Run from the repository root:  Rscript evaluation/03_tamper.R
suppressMessages({ library(RegiStat); library(DBI) })
out_dir <- "evaluation/results"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dict <- load_template("t2d"); reps <- 50L; n_rec <- 60L
eh <- RegiStat:::entry_hash

build <- function(seed) {
  p <- tempfile(fileext = ".sqlite"); reg <- registry_create(p, dict, user = "eval")
  registry_import(reg, synthetic_t2d(n_rec, seed = seed)$data, user = "eval")
  set.seed(seed)
  ids <- sample(registry_data(reg)$record_id, 10L)
  for (i in ids[1:6]) registry_update(reg, i, list(age_years = 100L + sample(10, 1)), user = "eval", reason = "correction")
  for (i in ids[7:10]) registry_delete(reg, i, user = "eval", reason = "duplicate")
  head <- registry_verify(reg)$head
  list(reg = reg, path = p, head = head)
}
drop_triggers <- function(con) {
  for (t in c("audit_no_update", "audit_no_delete")) dbExecute(con, paste("DROP TRIGGER IF EXISTS", t))
}
rewrite_chain <- function(con) {   # recompute every hash consistently
  a <- dbGetQuery(con, "SELECT * FROM audit ORDER BY seq"); prev <- a$prev_hash[1]
  for (i in seq_len(nrow(a))) {
    h <- eh(a$seq[i], a$ts[i], a$actor[i], a$action[i], a$record_id[i], a$changes[i], a$reason[i], prev)
    dbExecute(con, "UPDATE audit SET prev_hash=?, hash=? WHERE seq=?", params = list(prev, h, a$seq[i]))
    prev <- h
  }
}
attacks <- list(
  "record value changed, no audit entry" = function(con, s, N) {
    all_r <- dbGetQuery(con, "SELECT record_id, data FROM records WHERE deleted=0")
    r <- all_r[sample(nrow(all_r), 1), ]
    d <- jsonlite::fromJSON(r$data, simplifyVector = FALSE); d$age_years <- "99"
    dbExecute(con, "UPDATE records SET data=? WHERE record_id=?", params = list(jsonlite::toJSON(d, auto_unbox = TRUE, null = "null"), r$record_id)) },
  "audit entry content changed" = function(con, s, N) {
    k <- sample(2:N, 1); dbExecute(con, "UPDATE audit SET changes=changes || ' ' WHERE seq=?", params = list(k)) },
  "audit entry actor or reason changed" = function(con, s, N) {
    k <- sample(2:N, 1); dbExecute(con, "UPDATE audit SET actor='someone_else' WHERE seq=?", params = list(k)) },
  "audit entry deleted (middle)" = function(con, s, N) {
    k <- sample(2:(N - 1), 1); dbExecute(con, "DELETE FROM audit WHERE seq=?", params = list(k)) },
  "audit log truncated (last entries removed)" = function(con, s, N) {
    k <- sample(1:5, 1); dbExecute(con, "DELETE FROM audit WHERE seq>?", params = list(N - k)) },
  "audit entries reordered" = function(con, s, N) {
    k <- sample(2:(N - 2), 1)
    dbExecute(con, "UPDATE audit SET seq=-1 WHERE seq=?", params = list(k))
    dbExecute(con, "UPDATE audit SET seq=? WHERE seq=?", params = list(k, k + 1L))
    dbExecute(con, "UPDATE audit SET seq=? WHERE seq=-1", params = list(k + 1L)) },
  "fake entry appended with wrong hash" = function(con, s, N) {
    dbExecute(con, "INSERT INTO audit(seq,ts,actor,action,record_id,changes,reason,prev_hash,hash) VALUES(?,?,?,?,?,?,?,?,?)",
              params = list(N + 1L, "2026-01-01T00:00:00.000Z", "x", "update", "SYN-0001", "{}", "x", "a", "b")) },
  "record row deleted, no audit entry" = function(con, s, N) {
    r <- dbGetQuery(con, "SELECT record_id FROM records WHERE deleted=0")$record_id
    dbExecute(con, "DELETE FROM records WHERE record_id=?", params = list(sample(r, 1))) },
  "deleted flag cleared, no audit entry" = function(con, s, N) {
    r <- dbGetQuery(con, "SELECT record_id FROM records WHERE deleted=1")$record_id
    dbExecute(con, "UPDATE records SET deleted=0 WHERE record_id=?", params = list(sample(r, 1))) },
  "whole chain rewritten consistently (known limit)" = function(con, s, N) {
    k <- sample(2:N, 1); dbExecute(con, "UPDATE audit SET actor='someone_else' WHERE seq=?", params = list(k)); rewrite_chain(con) })

rows <- list(); blocked <- 0L; attempts <- 0L
for (nm in names(attacks)) {
  det_file <- 0L; det_anchor <- 0L
  for (s in seq_len(reps)) {
    b <- build(s); con <- b$reg$con
    N <- dbGetQuery(con, "SELECT COUNT(*) n FROM audit")$n
    if (grepl("audit|chain", nm) && !grepl("record", nm)) {
      attempts <- attempts + 1L   # first, try without dropping the triggers
      res <- tryCatch({ attacks[[nm]](con, s, N); "done" }, error = function(e) "blocked")
      if (identical(res, "blocked")) blocked <- blocked + 1L
      b <- build(s); con <- b$reg$con
    }
    drop_triggers(con)
    attacks[[nm]](con, s, N)
    v <- registry_verify(b$reg); va <- registry_verify(b$reg, expected_head = b$head)
    det_file <- det_file + !v$ok; det_anchor <- det_anchor + !va$ok
    registry_close(b$reg)
  }
  rows[[length(rows) + 1L]] <- data.frame(alteration = nm, trials = reps,
    detected_file_only = det_file, detected_with_anchor = det_anchor)
}
tab <- do.call(rbind, rows)
tab$rate_file_only <- tab$detected_file_only / tab$trials
tab$rate_with_anchor <- tab$detected_with_anchor / tab$trials
utils::write.csv(tab, file.path(out_dir, "tamper_detection.csv"), row.names = FALSE)
print(tab)
cat(sprintf("\nDirect SQL changes to the audit table blocked by the triggers: %d of %d attempts.\n", blocked, attempts))
# Clean registries must verify
okc <- vapply(seq_len(reps), function(s) { b <- build(s); r <- registry_verify(b$reg)$ok; registry_close(b$reg); r }, logical(1))
cat(sprintf("Unaltered registries that verify: %d of %d.\n", sum(okc), reps))
