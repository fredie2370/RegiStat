# Evaluation 4: performance on synthetic registries of increasing size.
# Run from the repository root:  Rscript evaluation/04_performance.R
# Reports the median of 5 runs; hardware and versions are saved with the results.
suppressMessages(library(RegiStat))
out_dir <- "evaluation/results"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dict <- load_template("t2d"); sizes <- c(500, 1000, 5000, 10000); reps <- 5L
tm <- function(expr) unname(system.time(expr)[["elapsed"]])
med <- function(f) stats::median(vapply(seq_len(reps), function(i) f(), numeric(1)))
rows <- lapply(sizes, function(n) {
  d <- synthetic_t2d(n, seed = 1)$data
  one <- function(op) {
    f <- tempfile(fileext = ".sqlite"); reg <- registry_create(f, dict, user = "eval")
    on.exit({ registry_close(reg); unlink(f) })
    t_imp <- tm(registry_import(reg, d, user = "eval"))
    size_mb <- file.size(f) / 1e6
    t_ver <- tm(registry_verify(reg))
    t_read <- tm(dd <- registry_data(reg))
    a <- analysis_prepare(dd, dict, "en")
    t_t1 <- tm(table_one(a, by = "sex_at_birth"))
    t_lg <- tm(logistic_model(a, "hypertension", c("age_years", "sex_at_birth", "systolic_bp_mmhg", "hba1c_pct", "uric_acid_mgdl")))
    t_roc <- tm(roc_biomarkers(a, c("hypertension", "ckd"), c("tyg", "egfr", "uric_acid_mgdl", "hemoglobin_gdl")))
    c(import = t_imp, verify = t_ver, read = t_read, table_one = t_t1, logistic = t_lg, roc = t_roc, size_mb = size_mb)
  }
  r <- vapply(seq_len(reps), function(i) one(i), numeric(7))
  data.frame(records = n, t(apply(r, 1, stats::median)))
})
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(out_dir, "performance.csv"), row.names = FALSE)
print(res, digits = 3)
cpu <- tryCatch(grep("model name", readLines("/proc/cpuinfo"), value = TRUE)[1], error = function(e) NA)
mem <- tryCatch(grep("MemTotal", readLines("/proc/meminfo"), value = TRUE), error = function(e) NA)
writeLines(c(paste("R:", R.version.string), paste("RegiStat:", as.character(packageVersion("RegiStat"))),
             paste("Platform:", R.version$platform), paste("CPU:", cpu), paste("Memory:", mem),
             paste("Logical cores:", parallel::detectCores()), paste("Date:", Sys.Date())),
           file.path(out_dir, "performance_environment.txt"))
