# Evaluation 2: detection of data-entry errors at import.
# Known errors are injected into clean synthetic registries; validate_data()
# must report each of them (sensitivity) and nothing else (false positives).
# Run from the repository root:  Rscript evaluation/02_validation.R
suppressMessages(library(RegiStat))
out_dir <- "evaluation/results"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dict <- load_template("t2d")
reps <- 100L; n <- 500L; rate <- 0.10
acc <- list(); fp_rows <- 0L; clean_rows <- 0L; wrong_rule <- 0L; dup_reported <- 0L
for (s in seq_len(reps)) {
  sim <- synthetic_t2d(n, seed = s, error_rate = rate)
  d <- sim$data; truth <- sim$truth
  # Seventh class: impossible or non-ISO dates in rows that are otherwise clean
  free <- setdiff(2:n, truth$row)
  set.seed(1000L + s)
  r <- sample(free, 3L)
  d$visit_date <- as.character(d$visit_date)
  d$visit_date[r] <- c("2024-13-45", "2024-02-30", "31/12/2024")
  truth <- rbind(truth, data.frame(row = r, variable = "visit_date", rule = "date"))
  v <- validate_data(d, dict, "en")
  key_t <- paste(truth$row, truth$variable)
  key_v <- paste(v$row, v$variable)
  for (cl in unique(truth$rule)) {
    idx <- truth$rule == cl
    hit <- key_t[idx] %in% key_v
    acc[[length(acc) + 1L]] <- data.frame(replicate = s, class = cl, injected = sum(idx), detected = sum(hit))
  }
  # Reported issues that were not injected (false positives) among untouched rows
  extra <- v[!(key_v %in% key_t), , drop = FALSE]
  fp_rows <- fp_rows + length(unique(extra$row))
  clean_rows <- clean_rows + (n - length(unique(truth$row)))
}
res <- do.call(rbind, acc)
tab <- aggregate(cbind(injected, detected) ~ class, res, sum)
tab$sensitivity <- tab$detected / tab$injected
ci <- t(mapply(function(x, k) binom.test(x, k)$conf.int, tab$detected, tab$injected))
tab$ci_low <- ci[, 1]; tab$ci_high <- ci[, 2]
all <- data.frame(class = "all classes", injected = sum(tab$injected), detected = sum(tab$detected))
all$sensitivity <- all$detected / all$injected
a_ci <- binom.test(all$detected, all$injected)$conf.int; all$ci_low <- a_ci[1]; all$ci_high <- a_ci[2]
tab <- rbind(tab, all)
utils::write.csv(tab, file.path(out_dir, "validation_by_class.csv"), row.names = FALSE)
fp <- data.frame(replicates = reps, records_per_replicate = n, untouched_records = clean_rows,
                 untouched_flagged = fp_rows, false_positive_rate = fp_rows / clean_rows)
utils::write.csv(fp, file.path(out_dir, "validation_false_positives.csv"), row.names = FALSE)
print(tab, digits = 4); print(fp)

# Import behaviour: one invalid row blocks the whole import (nothing is written)
path <- tempfile(fileext = ".sqlite")
reg <- registry_create(path, dict, user = "eval")
bad <- synthetic_t2d(50, seed = 1, error_rate = 0.1)$data
before <- nrow(registry_data(reg))
res_imp <- tryCatch(registry_import(reg, bad, user = "eval"), error = function(e) e)
after <- nrow(registry_data(reg))
cat(sprintf("\nImport of a file with errors: %s; records before %d, after %d.\n",
            if (inherits(res_imp, "error")) "refused" else "accepted", before, after))
good <- synthetic_t2d(50, seed = 1)$data
registry_import(reg, good, user = "eval")
cat(sprintf("Import of the clean file: records now %d.\n", nrow(registry_data(reg))))
registry_close(reg)
