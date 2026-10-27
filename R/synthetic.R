#' Generate synthetic type 2 diabetes registry data
#'
#' Creates a fully synthetic data set that matches the `t2d` template. The
#' values are random numbers with plausible relationships (for example,
#' fasting glucose follows HbA1c); they carry no clinical meaning and describe
#' no real person. Identifiers start with `SYN-`.
#'
#' Optionally, a known share of rows receives exactly one injected error
#' (missing required value, out-of-range value, invalid choice, non-numeric
#' text, duplicated identifier, or systolic pressure not above diastolic).
#' The returned `truth` table lists every injected error, so the sensitivity
#' and specificity of [validate_data()] can be measured. The first row is
#' never modified. The global random seed is left untouched.
#'
#' @param n Number of records (at least 10).
#' @param seed Integer seed for reproducibility.
#' @param error_rate Share of rows (0 to 0.5) that receive an injected error.
#' @return A list with `data` (a data frame) and `truth` (a data frame with
#'   `row`, `variable` and `rule` for each injected error).
#' @examples
#' sim <- synthetic_t2d(n = 50, seed = 123, error_rate = 0.1)
#' nrow(sim$truth)
#' @export
synthetic_t2d <- function(n = 200L, seed = 1L, error_rate = 0) {
  n <- as.integer(n)
  if (length(n) != 1L || is.na(n) || n < 10L) {
    abort("'n' must be a whole number of at least 10.")
  }
  if (!is.numeric(error_rate) || length(error_rate) != 1L ||
      is.na(error_rate) || error_rate < 0 || error_rate > 0.5) {
    abort("'error_rate' must be a number between 0 and 0.5.")
  }
  withr::with_seed(seed, {
    clean <- synthetic_clean_t2d(n)
    inject_errors(clean, error_rate)
  })
}

#' @noRd
synthetic_clean_t2d <- function(n) {
  clip <- function(x, lo, hi) pmin(pmax(x, lo), hi)
  rnorm <- stats::rnorm
  runif <- stats::runif

  sex <- sample(c("female", "male"), n, replace = TRUE, prob = c(0.55, 0.45))
  sex[runif(n) < 0.01] <- "unknown"
  age <- as.integer(round(clip(rnorm(n, 58, 11), 25, 90)))
  height <- round(clip(rnorm(n, ifelse(sex == "male", 169, 156), 7), 130, 205), 1)
  bmi <- clip(rnorm(n, 30, 5), 17, 55)
  weight <- round(clip(bmi * (height / 100)^2, 35, 250), 1)
  sys <- as.integer(round(clip(rnorm(n, 128 + 0.3 * (age - 58), 17), 90, 200)))
  dia <- as.integer(round(clip(rnorm(n, 78, 9), 50, 110)))
  dia <- pmin(dia, sys - 10L)
  hba1c <- round(clip(rnorm(n, 8.1, 1.7), 5, 14), 1)
  glucose <- round(clip(28.7 * hba1c - 46.7 + rnorm(n, 0, 25), 60, 600))
  creat <- round(clip(stats::rlnorm(n, log(0.95), 0.35), 0.4, 8), 2)
  trig <- round(clip(stats::rlnorm(n, log(180), 0.5), 40, 1500))

  yes_no <- function(p) {
    out <- ifelse(runif(n) < p, "yes", "no")
    out[runif(n) < 0.03] <- "unknown"
    out
  }
  hypertension <- yes_no(stats::plogis(-2.2 + 0.03 * (sys - 120) +
                                         0.02 * (age - 55)))
  ckd <- yes_no(stats::plogis(-3 + 1.5 * (creat - 1)))
  retinopathy <- yes_no(stats::plogis(-3.5 + 0.4 * (hba1c - 7)))

  data.frame(
    record_id = sprintf("SYN-%04d", seq_len(n)),
    visit_date = as.Date("2024-01-01") + sample(0:365, n, replace = TRUE),
    sex_at_birth = sex,
    age_years = age,
    weight_kg = weight,
    height_cm = height,
    systolic_bp_mmhg = sys,
    diastolic_bp_mmhg = dia,
    hba1c_pct = hba1c,
    fasting_glucose_mgdl = glucose,
    creatinine_mgdl = creat,
    triglycerides_mgdl = trig,
    hypertension = hypertension,
    ckd = ckd,
    retinopathy = retinopathy,
    stringsAsFactors = FALSE
  )
}

#' @noRd
inject_errors <- function(d, error_rate) {
  n <- nrow(d)
  truth <- data.frame(row = integer(), variable = character(),
                      rule = character(), stringsAsFactors = FALSE)
  k <- min(as.integer(round(error_rate * n)), n - 1L)
  if (k == 0L) return(list(data = d, truth = truth))

  types <- sample(rep_len(c("required", "range", "choice", "type",
                            "duplicate_id", "rule"), k))
  pool <- 2:n
  eligible_rule <- pool[d$diastolic_bp_mmhg[pool] >= 70L]
  used <- integer()
  rows <- integer(k)
  # Rows for the blood-pressure rule are chosen first because they need a
  # diastolic value high enough to stay inside its own allowed range.
  for (i in order(types != "rule")) {
    cand <- setdiff(if (types[i] == "rule") eligible_rule else pool, used)
    if (length(cand) == 0L) abort("Not enough eligible rows to inject errors.")
    r <- if (length(cand) == 1L) cand else sample(cand, 1L)
    rows[i] <- r
    used <- c(used, r)
  }

  if (any(types == "type")) d$hba1c_pct <- as.character(d$hba1c_pct)
  vars <- character(k)
  rules <- types
  for (i in seq_len(k)) {
    r <- rows[i]
    switch(
      types[i],
      required = {
        d$age_years[r] <- NA_integer_
        vars[i] <- "age_years"
      },
      range = {
        d$age_years[r] <- 250L
        vars[i] <- "age_years"
      },
      choice = {
        d$ckd[r] <- "maybe"
        vars[i] <- "ckd"
      },
      type = {
        d$hba1c_pct[r] <- "abc"
        vars[i] <- "hba1c_pct"
      },
      duplicate_id = {
        d$record_id[r] <- d$record_id[1L]
        vars[i] <- "record_id"
      },
      rule = {
        d$systolic_bp_mmhg[r] <- d$diastolic_bp_mmhg[r] - 5L
        vars[i] <- "diastolic_bp_mmhg,systolic_bp_mmhg"
        rules[i] <- "rule:bp_order"
      }
    )
  }
  truth <- data.frame(row = rows, variable = vars, rule = rules,
                      stringsAsFactors = FALSE)
  truth <- truth[order(truth$row), , drop = FALSE]
  rownames(truth) <- NULL
  list(data = d, truth = truth)
}
