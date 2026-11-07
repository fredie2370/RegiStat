#' Escape text for HTML (internal)
#' @noRd
html_escape <- function(x) {
  x <- gsub("&", "&amp;", as.character(x), fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

#' Non-ASCII characters as numeric HTML entities (internal)
#'
#' Printing text through the console turns non-ASCII characters into
#' <U+XXXX> placeholders in non-UTF-8 locales; entities survive any locale.
#' @noRd
html_ascii <- function(x) {
  vapply(as.character(x), function(s) {
    if (is.na(s)) return("")
    cp <- utf8ToInt(enc2utf8(s))
    if (all(cp < 128L)) return(s)
    paste0(ifelse(cp < 128L, intToUtf8(cp, multiple = TRUE), paste0("&#", cp, ";")),
           collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

#' A data frame as an HTML table (internal)
#'
#' Built by hand and returned as a string, because printing text through the
#' console would turn non-ASCII characters into <U+XXXX> placeholders in
#' non-UTF-8 locales.
#' @noRd
html_table <- function(df) {
  cell <- function(tag, x) paste0("<", tag, ">", html_escape(ifelse(is.na(x), "", x)),
                                  "</", tag, ">")
  head <- paste0("<tr>", paste(cell("th", names(df)), collapse = ""), "</tr>")
  body <- if (nrow(df) == 0L) character() else {
    rows <- do.call(paste0, lapply(df, function(col) cell("td", as.character(col))))
    paste0("<tr>", rows, "</tr>")
  }
  html_ascii(paste0("\n\n<table class='table table-sm table-striped'>\n<thead>", head,
                    "</thead>\n<tbody>\n", paste(body, collapse = "\n"),
                    "\n</tbody>\n</table>\n\n"))
}

#' Reproducible registry report
#'
#' Renders a self-contained HTML report of a registry: study and record
#' count, the descriptive table (see [table_one()], optionally by group),
#' missing data, the distribution of every variable, prevalences with exact
#' intervals, comparisons with each outcome ([compare_continuous()],
#' [associations_categorical()]), correlations ([correlations()]), logistic
#' regression ([logistic_model()]), ROC analysis of biomarkers
#' ([roc_biomarkers()]), a description of the statistical methods, and a reproducibility
#' section with the package and R versions and the result of the audit-trail
#' check (including its head hash, so the report can be tied to an exact
#' state of the registry).
#'
#' Needs the rmarkdown and knitr packages and Pandoc (included with
#' RStudio). Calculated measures are computed on the fly ([derive_variables()]).
#'
#' @param reg A registry from [registry_open()] or [registry_create()].
#' @param file Output file, ending in `.html`.
#' @param lang `"en"` or `"es"`.
#' @param by Optional categorical variable to compare groups.
#' @param outcomes Yes/no variables to analyse (comparisons, regression, ROC).
#'   Default: the yes/no variables with at least 10 events and 10 non-events.
#' @param predictors Variables for the logistic models (default: age, sex,
#'   systolic pressure, hypertension, glucose, haemoglobin, uric acid and TyG
#'   index, when they exist in the dictionary).
#' @param egfr_equation `"2021"` (default) or `"2009"`: CKD-EPI equation for the
#'   calculated eGFR, see [derive_variables()].
#' @param biomarkers Numeric variables for the ROC analysis and the
#'   correlations (default: a standard set when present).
#' @return The path of the report, invisibly.
#' @seealso [analysis_prepare()], [table_one()]
#' @examples
#' \donttest{
#' if (requireNamespace("rmarkdown", quietly = TRUE) && rmarkdown::pandoc_available()) {
#'   path <- tempfile(fileext = ".sqlite")
#'   reg <- registry_create(path, load_template("t2d"), user = "demo")
#'   registry_import(reg, synthetic_t2d(60, seed = 1)$data, user = "demo")
#'   out <- registry_report(reg, tempfile(fileext = ".html"), lang = "en",
#'                          by = "sex_at_birth")
#'   registry_close(reg)
#' }
#' }
#' @export
registry_report <- function(reg, file, lang = c("en", "es"), by = NULL, outcomes = NULL,
                            predictors = NULL, biomarkers = NULL,
                            egfr_equation = c("2021", "2009")) {
  lang <- match.arg(lang)
  check_registry(reg)
  if (!requireNamespace("rmarkdown", quietly = TRUE) ||
      !requireNamespace("knitr", quietly = TRUE)) {
    abort("Reports need the 'rmarkdown' and 'knitr' packages.")
  }
  if (!rmarkdown::pandoc_available()) {
    abort("Reports need Pandoc (it is included with RStudio).")
  }
  if (!is.character(file) || length(file) != 1L || !grepl("[.]html?$", file, ignore.case = TRUE)) {
    abort("'file' must be a path ending in .html")
  }
  egfr_equation <- match.arg(egfr_equation)
  a <- analysis_prepare(registry_data(reg), reg$dict, lang, egfr_equation)
  if (!is.null(by)) check_var(a, by, "categorical")
  outcomes <- outcomes %||% default_outcomes(a)
  for (o in outcomes) check_var(a, o, "categorical")
  predictors <- predictors %||% default_predictors(a)
  biomarkers <- biomarkers %||% default_biomarkers(a)

  v <- registry_verify(reg)
  audit_line <- if (v$ok) {
    ui_text("rep_verified", lang, v$n_entries, v$head)
  } else {
    ui_text("rep_not_verified", lang, length(v$problems))
  }
  pkg_version <- tryCatch(as.character(utils::packageVersion("RegiStat")),
                          error = function(e) "dev")
  title <- reg$dict$study$title
  study_title <- if (is.list(title)) title[[lang]] %||% title[["en"]] else
    reg$dict$study$id %||% ""

  work <- tempfile("registat-report-")
  dir.create(work)
  on.exit(unlink(work, recursive = TRUE), add = TRUE)
  src <- system.file("reports", package = "RegiStat")
  file.copy(file.path(src, c("report.Rmd", "body.Rmd")), work)

  env <- new.env(parent = globalenv())
  env$a <- a
  env$by <- by
  env$outcomes <- outcomes
  env$predictors <- predictors
  env$biomarkers <- biomarkers
  env$study_title <- study_title
  env$audit_line <- audit_line
  env$pkg_version <- pkg_version
  env$body_file <- file.path(work, "body.Rmd")
  env$tx <- function(key, ...) ui_text(key, lang, ...)
  # The report calls the package's own functions.
  env$table_one <- table_one
  env$missing_summary <- missing_summary
  env$outlier_summary <- outlier_summary
  env$fmt_num <- fmt_num
  env$plot_variable <- plot_variable
  for (fn in c("prevalence_table", "plot_prevalence", "compare_continuous",
               "associations_categorical", "correlations", "plot_correlation",
               "logistic_model", "model_table", "plot_forest", "roc_biomarkers",
               "plot_auc_heatmap", "plot_roc")) {
    assign(fn, get(fn, envir = asNamespace("RegiStat")), envir = env)
  }
  env$html_table <- html_table
  env$html_escape <- html_escape
  env$html_ascii <- html_ascii
  env$say <- function(...) cat(html_ascii(paste0(...)))

  out_dir <- dirname(normalizePath(file, mustWork = FALSE))
  if (!dir.exists(out_dir)) abort("Directory does not exist: %s", out_dir)
  rmarkdown::render(file.path(work, "report.Rmd"),
                    output_file = basename(file), output_dir = out_dir,
                    intermediates_dir = work, envir = env, quiet = TRUE,
                    encoding = "UTF-8")
  invisible(normalizePath(file))
}

#' Yes/no variables fit to be analysed as outcomes (internal)
#'
#' At least 10 events and 10 non-events, at most six, calculated "at least
#' one" flags left out.
#' @noRd
default_outcomes <- function(a) {
  cand <- setdiff(names(a$binary), aggregate_vars())
  ok <- vapply(cand, function(v) {
    y <- binary_outcome(a, v)
    sum(y == 1L, na.rm = TRUE) >= 10L && sum(y == 0L, na.rm = TRUE) >= 10L
  }, logical(1))
  utils::head(cand[ok], 6L)
}
