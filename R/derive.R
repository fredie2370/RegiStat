# Calculated measures. They are never typed or stored: they are computed from
# the recorded values whenever they are shown or exported, so they cannot
# disagree with the data.

#' Body mass index category (internal)
#' @noRd
bmi_category <- function(bmi) {
  cut(bmi, c(-Inf, 18.5, 25, 30, 35, 40, Inf), right = FALSE,
      labels = c("underweight", "normal", "overweight", "obesity_1",
                 "obesity_2", "obesity_3"))
}

#' eGFR stage G1-G5 (internal)
#' @noRd
egfr_stage <- function(egfr) {
  cut(egfr, c(-Inf, 15, 30, 45, 60, 90, Inf), right = FALSE,
      labels = c("G5", "G4", "G3b", "G3a", "G2", "G1"))
}

#' Add calculated measures to registry data
#'
#' Adds, when the needed columns exist:
#'
#' * `bmi`: weight (kg) / height (m)^2, from `weight_kg` and `height_cm`.
#' * `bmi_category`: WHO categories (underweight < 18.5, normal, overweight
#'   25 to < 30, obesity class 1, 2 and 3 from 30, 35 and 40).
#' * `egfr`: estimated glomerular filtration rate in mL/min/1.73 m^2, from
#'   `creatinine_mgdl`, `age_years` and `sex_at_birth`. The default is the
#'   CKD-EPI 2021 creatinine equation (race-free); `egfr_equation = "2009"`
#'   gives the CKD-EPI 2009 equation without the race coefficient. It is
#'   `NA` unless sex at birth is `"female"` or `"male"`, because the
#'   equations have no term for other values.
#' * `egfr_stage`: KDIGO stage G1 to G5 by eGFR alone. This is **not** a
#'   diagnosis of chronic kidney disease, which also needs persistence over
#'   three months and markers such as albuminuria.
#'
#' * `tyg`: triglyceride-glucose index, ln(triglycerides x glucose / 2), from
#'   `triglycerides_mgdl` and `fasting_glucose_mgdl`.
#' * `n_complications`, `any_complication`: number of "yes" answers among
#'   retinopathy, heart failure, chronic kidney disease, dialysis and
#'   amputation, and whether there is at least one. Unanswered items count
#'   as absent.
#' * `n_comorbidities`, `any_comorbidity`: the same for hypertension and COPD.
#' * `n_comp_comorb`, `any_comp_comorb`: the same for all seven.
#'
#' These values are calculated on demand and never stored. The equation is
#' meant for adults with stable kidney function; it is not valid in acute
#' kidney injury or on dialysis.
#'
#' @param data A data frame, for example from [registry_data()].
#' @param lang `"en"` or `"es"`: language of the category labels.
#' @param egfr_equation `"2021"` (default) or `"2009"`.
#' @return `data` with the calculated columns appended. Categories are
#'   factors.
#' @examples
#' d <- data.frame(sex_at_birth = c("female", "male"), age_years = c(60, 45),
#'                 weight_kg = c(70, 95), height_cm = c(160, 178),
#'                 creatinine_mgdl = c(0.9, 1.4))
#' derive_variables(d)
#' @export
derive_variables <- function(data, lang = c("en", "es"), egfr_equation = c("2021", "2009")) {
  lang <- match.arg(lang)
  egfr_equation <- match.arg(egfr_equation)
  has <- function(...) all(c(...) %in% names(data))
  num <- function(x) suppressWarnings(as.numeric(x))

  if (has("weight_kg", "height_cm")) {
    h <- num(data$height_cm) / 100
    data$bmi <- round(num(data$weight_kg) / h^2, 1)
    cat <- bmi_category(data$bmi)
    levels(cat) <- vapply(levels(cat), function(k) ui_text(paste0("bmi_", k), lang),
                          character(1))
    data$bmi_category <- cat
  }
  if (has("creatinine_mgdl", "age_years", "sex_at_birth")) {
    scr <- num(data$creatinine_mgdl)
    age <- num(data$age_years)
    female <- data$sex_at_birth == "female"
    male <- data$sex_at_birth == "male"
    if (egfr_equation == "2021") {
      kappa <- ifelse(female, 0.7, 0.9)
      alpha <- ifelse(female, -0.241, -0.302)
      e <- 142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^-1.2 *
        0.9938^age * ifelse(female, 1.012, 1)
      digits <- 0
    } else {
      kappa <- ifelse(female, 0.7, 0.9)
      alpha <- ifelse(female, -0.329, -0.411)
      e <- ifelse(female, 144, 141) * pmin(scr / kappa, 1)^alpha *
        pmax(scr / kappa, 1)^-1.209 * 0.993^age
      digits <- 1
    }
    e[!(female | male) | is.na(female)] <- NA_real_
    data$egfr <- round(e, digits)
    data$egfr_stage <- egfr_stage(data$egfr)
  }
  if (has("triglycerides_mgdl", "fasting_glucose_mgdl")) {
    data$tyg <- round(log(num(data$triglycerides_mgdl) *
                            num(data$fasting_glucose_mgdl) / 2), 3)
  }

  yes_flag <- function(vars) {
    vars <- intersect(vars, names(data))
    if (length(vars) == 0L) return(NULL)
    m <- matrix(unlist(lapply(vars, function(v) as.integer(data[[v]] %in% "yes"))),
                nrow = nrow(data), ncol = length(vars))
    n <- rowSums(m, na.rm = TRUE)
    n <- as.integer(n)
    flag <- factor(ifelse(n > 0L, "yes", "no"), levels = c("no", "yes"),
                   labels = c(ui_text("answer_no", lang), ui_text("answer_yes", lang)))
    list(n = n, flag = flag)
  }
  complications <- c("retinopathy", "heart_failure", "ckd", "dialysis", "amputation")
  comorbidities <- c("hypertension", "copd")
  for (spec in list(list(complications, "n_complications", "any_complication"),
                    list(comorbidities, "n_comorbidities", "any_comorbidity"),
                    list(c(comorbidities, complications), "n_comp_comorb",
                         "any_comp_comorb"))) {
    r <- yes_flag(spec[[1L]])
    if (!is.null(r)) {
      data[[spec[[2L]]]] <- r$n
      data[[spec[[3L]]]] <- r$flag
    }
  }
  data
}

#' Names and labels of the calculated measures present in a table (internal)
#' @noRd
derived_labels <- function(lang, egfr_equation = "2021") {
  c(bmi = ui_text("derived_bmi", lang),
    bmi_category = ui_text("derived_bmi_cat", lang),
    egfr = ui_text("derived_egfr_eq", lang, egfr_equation),
    egfr_stage = ui_text("derived_stage", lang),
    tyg = ui_text("derived_tyg", lang),
    n_complications = ui_text("derived_n_comp", lang),
    any_complication = ui_text("derived_any_comp", lang),
    n_comorbidities = ui_text("derived_n_comorb", lang),
    any_comorbidity = ui_text("derived_any_comorb", lang),
    n_comp_comorb = ui_text("derived_n_both", lang),
    any_comp_comorb = ui_text("derived_any_both", lang))
}
