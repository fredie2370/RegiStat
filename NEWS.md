# RegiStat 0.2.0

* Data-dictionary builder: dictionaries can be written in an Excel file
  (sheets Study, Variables, Choices, Rules) and are validated with errors that
  name the sheet and row (`read_dictionary()` now reads `.xlsx`,
  `dictionary_template_xlsx()`, `dictionary_to_tables()`,
  `dictionary_from_tables()`, `write_dictionary()`). The start screen accepts
  Excel or YAML, previews validity and offers downloads.
* Start-up icon: `create_launcher()` (Windows `.bat`, macOS `.command`,
  Linux `.sh`) and `installer/Install-RegiStat.bat` (untested on Windows).
* User manual in Spanish and English as vignettes with screenshots.
* Fix: calculated "at least one" flags failed for a single record.

* Full statistical analysis, built from the analysis scripts of the thesis on
  type 2 diabetes complications and made generic: any yes/no variable can be
  an outcome and any variable a predictor or biomarker.
  - Descriptive: Shapiro-Wilk normality table and histograms with the normal
    curve, prevalences with exact Clopper-Pearson intervals overall and by
    group (`prevalence_table()`, `normality_table()`), mean (SD) or median
    (Q1, Q3) according to normality in `table_one(style = "auto")`.
  - Bivariate: numeric variables by outcome with Welch's t-test or
    Mann-Whitney (`compare_continuous()`), categorical variables with
    chi-squared or Fisher's exact test and crude odds ratios
    (`associations_categorical()`), Spearman/Pearson correlations with Holm
    adjustment (`correlations()`).
  - Logistic regression (`logistic_model()`, `model_table()`,
    `regression_matrix()`): crude and adjusted odds ratios, optional backward
    elimination by AIC, Nagelkerke R-squared, Hosmer-Lemeshow test, variance
    inflation factors, events-per-coefficient warning and the AUC of the
    model.
  - ROC analysis of biomarkers (`roc_biomarkers()`): AUC with DeLong
    interval, interpretation, Youden cut-off with sensitivity, specificity
    and predictive values.
  - Figures in the style of the manuscript, exportable as PNG, SVG or PDF
    (`save_figure()`): population pyramid, counts by group, prevalence,
    normality, boxplots by outcome, correlation map, forest plot, ROC curves,
    predictor-by-outcome matrix and AUC heat map.
  - The statistical core uses base R only; it is checked against the pROC and
    car packages in the tests.
  - Data quality: missing values by variable and extreme values by the IQR rule (`outlier_summary()`, flagged for checking, never removed); scatter plot with an optional loess curve and Spearman's r in the caption (`plot_relation(smooth = "loess")`).
* New calculated measures: TyG index, counts and "at least one" flags of
  complications and comorbidities, and a choice between the CKD-EPI 2021 and
  2009 equations for eGFR.
* The *Analysis* tab has new sections (Comparisons, Logistic regression, ROC
  curves, Methods); every table downloads as CSV and every figure as PNG or
  SVG. The HTML report now includes all of them and a description of the
  statistical methods.
* Fix: report headings and tables no longer show `<U+XXXX>` placeholders in
  non-UTF-8 locales.

# RegiStat 0.1.1

* Initial package skeleton (development version).
* Metadata-driven data dictionary with Spanish and English labels
  (`read_dictionary()`, `validate_dictionary()`, `dictionary_table()`).
* Whitelisted cross-field rules: dictionaries cannot execute arbitrary code.
* Validation engine `validate_data()` with bilingual messages.
* Bundled `t2d` template (`load_template()`).
* Seeded synthetic data generator with known injected errors
  (`synthetic_t2d()`).
* Local SQLite registry (`registry_create()`, `registry_open()`,
  `registry_close()`) storing the dictionary with its hash.
* Validated record operations: `registry_add()`, `registry_update()` (reason
  required, old and new values kept), `registry_delete()` (soft delete, reason
  required) and all-or-nothing `registry_import()`.
* Append-only audit trail with a SHA-256 hash chain, and
  `registry_verify()` with chain and replay checks and an optional external
  head anchor. `registry_data()` and `registry_audit()` read the results.
* Import helpers: `read_table_file()`, `table_sheets()`, `suggest_mapping()`,
  `apply_mapping()`, `convert_import_values()` (Excel dates, day-first dates,
  decimal comma, value labels) and `registry_check()` (report every problem
  without writing).
* Offline Shiny application, `run_app()`: capture form generated from the
  dictionary (Enter moves to the next field), records table with edit and
  soft-delete that require a reason, import with column matching and
  pre-check, audit viewer with integrity check, Spanish/English switch.
  Interface texts live in `inst/i18n/ui.yml`.
* Case finding ("cotejo") in a separate SQLite file: `screening_create()`,
  `screening_open()`, `read_reference_list()`, `screening_load_list()`
  (versioned), `screening_add_review()`, `screening_update_review()`,
  `screening_delete_review()`, `screening_reviews()` (derived status),
  `screening_pending()`, `screening_summary()`, `screening_changes()`,
  `screening_audit()` and `screening_verify()` with the same hash-chained
  audit trail.
* `t2d` template extended: heart failure, dialysis, COPD, amputation, other
  complications and comorbidities (free text), total cholesterol, BUN, urea,
  uric acid, heart rate and respiratory rate.
* `derive_variables()`: BMI and WHO category, eGFR (CKD-EPI 2021) and G stage,
  calculated on demand and never stored. Shown live in the capture form, in
  the records table and in the CSV download.
* Analysis: `analysis_prepare()`, `table_one()` (overall or by group, with
  Wilcoxon / Kruskal-Wallis / Fisher / chi-squared tests), `missing_summary()`,
  `plot_variable()`, `plot_relation()` and `as_interactive()` (plotly).
* `registry_report()`: self-contained offline HTML report (R Markdown) in
  Spanish or English with tables, plots and the audit-trail verification.
* New "Analysis" tab in the app.
