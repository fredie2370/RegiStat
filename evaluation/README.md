# Evaluation scripts

Scripts that produce the evaluation reported in the manuscript. Run each one from the
repository root with RegiStat installed. They use only synthetic data (`synthetic_t2d()`)
and the public Pima Indian diabetes data distributed with the MASS package. Results are
written to `evaluation/results/`.

| Script | Question |
| --- | --- |
| `01_correctness.R` | Do RegiStat statistics agree with stats, MASS, car and pROC? |
| `02_validation.R` | Are injected data-entry errors detected at import? |
| `03_tamper.R` | Are alterations of the registry file detected? |
| `04_performance.R` | How do import, verification and analysis scale with registry size? |

Packages needed besides RegiStat: pROC, car, MASS. Timings depend on the machine; the
environment used is saved in `results/performance_environment.txt`.
