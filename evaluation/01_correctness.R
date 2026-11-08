# Evaluation 1: statistical correctness of RegiStat against independent
# implementations (stats, MASS, car, pROC) on synthetic and public data.
# Run from the repository root:  Rscript evaluation/01_correctness.R
suppressMessages({ library(RegiStat); library(pROC); library(car); library(MASS) })
out_dir <- "evaluation/results"; dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
rows <- list()
rec <- function(ds, domain, quantity, got, ref, tol = 1e-6) {
  got <- as.numeric(got); ref <- as.numeric(ref)
  stopifnot(length(got) == length(ref))
  for (i in seq_along(got)) {
    d <- abs(got[i] - ref[i]); rel <- d / max(abs(ref[i]), 1e-300)
    ok <- (is.na(got[i]) && is.na(ref[i])) ||
      (!is.na(d) && (d <= 1e-12 || rel <= tol))
    rows[[length(rows) + 1L]] <<- data.frame(dataset = ds, domain = domain,
      quantity = if (length(got) > 1L) paste0(quantity, "[", i, "]") else quantity,
      registat = got[i], reference = ref[i], abs_diff = d, rel_diff = rel, equivalent = ok)
  }
}

# ---- datasets -------------------------------------------------------------
dict <- load_template("t2d")
pima <- rbind(MASS::Pima.tr, MASS::Pima.te)           # public data, n = 532
pima_dict <- validate_dictionary(list(
  study = list(id = "pima", title = list(en = "Pima", es = "Pima")),
  variables = c(list(
    list(name = "record_id", type = "id", label = list(en = "Id", es = "Id"))),
    lapply(c(npreg = "Pregnancies", glu = "Glucose", bp = "Diastolic BP",
             skin = "Skin fold", bmi = "BMI", ped = "Pedigree", age = "Age"),
           function(l) NULL) |> (\(x) Map(function(n, l) list(
             name = n, type = if (n %in% c("npreg", "age")) "integer" else "numeric",
             label = list(en = l, es = l)), names(x), c("Pregnancies", "Glucose",
             "Diastolic BP", "Skin fold", "BMI", "Pedigree", "Age")))(),
    list(list(name = "diabetes", type = "categorical", label = list(en = "Diabetes", es = "Diabetes"),
              choices = list(list(value = "yes", label = list(en = "Yes", es = "Si")),
                             list(value = "no", label = list(en = "No", es = "No"))))))))
pima_d <- data.frame(record_id = sprintf("P%04d", seq_len(nrow(pima))),
                     pima[c("npreg", "glu", "bp", "skin", "bmi", "ped", "age")],
                     diabetes = ifelse(pima$type == "Yes", "yes", "no"))
sets <- list(
  syn150 = list(a = analysis_prepare(synthetic_t2d(150, seed = 1)$data, dict, "en"),
                outcome = "hypertension",
                nums = c("age_years", "hemoglobin_gdl", "fasting_glucose_mgdl", "uric_acid_mgdl", "tyg"),
                cats = c("sex_at_birth", "ckd"), preds = c("age_years", "sex_at_birth", "fasting_glucose_mgdl", "hemoglobin_gdl", "uric_acid_mgdl")),
  syn1000 = list(a = analysis_prepare(synthetic_t2d(1000, seed = 2)$data, dict, "en"),
                 outcome = "hypertension",
                 nums = c("age_years", "hemoglobin_gdl", "fasting_glucose_mgdl", "uric_acid_mgdl", "tyg"),
                 cats = c("sex_at_birth", "ckd"), preds = c("age_years", "sex_at_birth", "fasting_glucose_mgdl", "hemoglobin_gdl", "uric_acid_mgdl")),
  pima532 = list(a = analysis_prepare(pima_d, pima_dict, "en"), outcome = "diabetes",
                 nums = c("npreg", "glu", "bp", "skin", "bmi", "ped", "age"), cats = character(),
                 preds = c("npreg", "glu", "bp", "bmi", "ped", "age")))

for (ds in names(sets)) {
  s <- sets[[ds]]; a <- s$a; d <- a$data
  y <- RegiStat:::binary_outcome(a, s$outcome)
  # Prevalence with exact CI
  pt <- prevalence_table(a, s$outcome)
  k <- sum(y == 1, na.rm = TRUE); n <- sum(!is.na(y)); bt <- binom.test(k, n)
  rec(ds, "prevalence", "percentage", pt$pct, 100 * k / n)
  rec(ds, "prevalence", "CI limits", c(pt$ci_low, pt$ci_high), 100 * bt$conf.int)
  # Normality
  nt <- normality_table(a, s$nums)
  rec(ds, "normality", "Shapiro-Wilk W", nt$w, sapply(s$nums, function(v) shapiro.test(d[[v]])$statistic))
  rec(ds, "normality", "Shapiro-Wilk p", nt$p_value, sapply(s$nums, function(v) shapiro.test(d[[v]])$p.value))
  # Numeric comparison between outcome groups
  cc <- compare_continuous(a, s$outcome, s$nums)
  g <- factor(y)
  ref_p <- mapply(function(v, tst) {
    x <- d[[v]]
    if (grepl("Welch", tst)) t.test(x ~ g)$p.value else wilcox.test(x ~ g, exact = FALSE)$p.value
  }, cc$variable, cc$test)
  rec(ds, "group comparison", "p-value (test chosen by RegiStat)", cc$p_value, ref_p)
  # Categorical associations
  if (length(s$cats)) {
    as_ <- associations_categorical(a, s$outcome, s$cats)
    for (i in seq_len(nrow(as_))) {
      x <- RegiStat:::clean_factor(a, as_$exposure[i])
      tb <- table(x, y)
      rec(ds, "association", paste("p-value", as_$exposure[i]),
          as_$p_value[i], if (grepl("Fisher", as_$test[i])) fisher.test(tb)$p.value
                           else chisq.test(tb, correct = TRUE)$p.value)
    }
  }
  # Correlations
  cr <- correlations(a, s$nums[1:4])
  rp <- mapply(function(v1, v2) suppressWarnings(cor.test(d[[v1]], d[[v2]], method = "spearman", exact = FALSE)),
               cr$var1, cr$var2, SIMPLIFY = FALSE)
  rec(ds, "correlation", "Spearman rho", cr$r, sapply(rp, `[[`, "estimate"))
  rec(ds, "correlation", "p-value", cr$p_value, sapply(rp, `[[`, "p.value"))
  rec(ds, "correlation", "Holm-adjusted p", cr$p_adj, p.adjust(sapply(rp, `[[`, "p.value"), "holm"))
  # Logistic regression, no selection
  m <- logistic_model(a, s$outcome, s$preds, selection = "none")
  fit <- glm(reformulate(m$predictors, "outcome"), data = m$data, family = binomial())
  rec(ds, "logistic", "coefficients", coef(m$model), coef(fit))
  ci <- suppressMessages(confint(fit))[-1, ]
  rec(ds, "logistic", "OR", m$terms$or, exp(coef(fit))[-1])
  rec(ds, "logistic", "OR profile CI", c(m$terms$ci_low, m$terms$ci_high), exp(c(ci[, 1], ci[, 2])), tol = 1e-5)
  rec(ds, "logistic", "Wald p", m$terms$p_value, summary(fit)$coefficients[-1, 4])
  null <- update(fit, . ~ 1); nn <- nobs(fit)
  r2 <- (1 - exp((deviance(fit) - deviance(null)) / nn)) / (1 - exp(-deviance(null) / nn))
  rec(ds, "logistic", "Nagelkerke R2", m$fit$nagelkerke, r2)
  rec(ds, "logistic", "AIC", m$fit$aic, AIC(fit))
  v <- car::vif(fit)
  if (is.matrix(v)) { rv <- v[, "GVIF"]^(1 / v[, "Df"]) } else rv <- v
  rec(ds, "logistic", "VIF/GVIF^(1/Df)", m$vif[names(rv)], rv)
  # Backward selection
  mb <- logistic_model(a, s$outcome, s$preds, selection = "backward")
  sw <- MASS::stepAIC(fit, direction = "backward", trace = 0)
  rec(ds, "selection", "AIC of final model", AIC(mb$model), AIC(sw))
  rec(ds, "selection", "number of selected terms", length(mb$selected), length(attr(terms(sw), "term.labels")))
  same <- setequal(mb$selected, attr(terms(sw), "term.labels"))
  rec(ds, "selection", "same terms selected (1 = yes)", as.numeric(same), 1)
  # ROC for every numeric biomarker
  bm <- s$nums[1:min(4, length(s$nums))]
  rt <- roc_biomarkers(a, s$outcome, bm)
  for (i in seq_len(nrow(rt))) {
    x <- d[[rt$biomarker[i]]]; ok <- !is.na(x) & !is.na(y)
    r <- pROC::roc(y[ok], x[ok], quiet = TRUE, direction = "auto")
    co <- pROC::coords(r, "best", best.method = "youden", ret = c("threshold", "sensitivity", "specificity", "ppv", "npv"))
    co <- unlist(lapply(as.list(co), function(z) unlist(z)[1]))
    tag <- paste(s$outcome, rt$biomarker[i])
    rec(ds, "ROC", paste("AUC", tag), rt$auc[i], as.numeric(pROC::auc(r)))
    ci_r <- as.numeric(pROC::ci.auc(r, method = "delong"))
    rec(ds, "ROC", paste("DeLong CI", tag), c(rt$ci_low[i], rt$ci_high[i]), ci_r[c(1, 3)])
    rec(ds, "ROC", paste("Youden cut-off", tag), rt$cutoff[i], co["threshold"])
    rec(ds, "ROC", paste("sens/spec/PPV/NPV", tag), c(rt$sensitivity[i], rt$specificity[i], rt$ppv[i], rt$npv[i]),
        100 * co[c("sensitivity", "specificity", "ppv", "npv")])
  }
}
res <- do.call(rbind, rows)
utils::write.csv(res, file.path(out_dir, "correctness_all.csv"), row.names = FALSE)
summ <- do.call(rbind, lapply(split(res, list(res$dataset, res$domain), drop = TRUE), function(z)
  data.frame(dataset = z$dataset[1], domain = z$domain[1], comparisons = nrow(z),
             equivalent = sum(z$equivalent), max_rel_diff = max(z$rel_diff, na.rm = TRUE))))
rownames(summ) <- NULL
utils::write.csv(summ, file.path(out_dir, "correctness_summary.csv"), row.names = FALSE)
print(summ, digits = 3)
cat(sprintf("\nTotal: %d comparisons, %d equivalent.\n", nrow(res), sum(res$equivalent)))
bad <- res[!res$equivalent, ]; if (nrow(bad)) print(bad)
