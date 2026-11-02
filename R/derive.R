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
#' * `egfr`: estimated glomerular filtration rate in mL/min/1.73 m^2 with the
#'   CKD-EPI 2021 creatinine equation (race-free), from `creatinine_mgdl`,
#'   `age_years` and `sex_at_birth`. It is `NA` unless sex at birth is
#'   `"female"` or `"male"`, because the equation has no term for other
#'   values.
#' * `egfr_stage`: KDIGO stage G1 to G5 by eGFR alone. This is **not** a
#'   diagnosis of chronic kidney disease, which also needs persistence over
#'   three months and markers such as albuminuria.
#'
#' These values are calculated on demand and never stored. The equation is
#' meant for adults with stable kidney function; it is not valid in acute
#' kidney injury or on dialysis.
#'
#' @param data A data frame, for example from [registry_data()].
#' @param lang `"en"` or `"es"`: language of the category labels.
#' @return `data` with the calculated columns appended. Categories are
#'   factors.
#' @examples
#' d <- data.frame(sex_at_birth = c("female", "male"), age_years = c(60, 45),
#'                 weight_kg = c(70, 95), height_cm = c(160, 178),
#'                 creatinine_mgdl = c(0.9, 1.4))
#' derive_variables(d)
#' @export
derive_variables <- function(data, lang = c("en", "es")) {
  lang <- match.arg(lang)
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
    kappa <- ifelse(female, 0.7, 0.9)
    alpha <- ifelse(female, -0.241, -0.302)
    e <- 142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^-1.2 *
      0.9938^age * ifelse(female, 1.012, 1)
    e[!(female | male) | is.na(female)] <- NA_real_
    data$egfr <- round(e, 0)
    data$egfr_stage <- egfr_stage(data$egfr)
  }
  data
}

#' Names and labels of the calculated measures present in a table (internal)
#' @noRd
derived_labels <- function(lang) {
  c(bmi = ui_text("derived_bmi", lang),
    bmi_category = ui_text("derived_bmi_cat", lang),
    egfr = ui_text("derived_egfr", lang),
    egfr_stage = ui_text("derived_stage", lang))
}
