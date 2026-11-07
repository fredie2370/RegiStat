# Installs RegiStat from GitHub and creates a start-up icon on the Desktop.
# Needs R and an internet connection (only for this installation).
# Usage: double-click Install-RegiStat.bat (Windows) or run
#   Rscript Install-RegiStat.R [tag]      e.g.  Rscript Install-RegiStat.R v0.2.0
args <- commandArgs(trailingOnly = TRUE)
ref <- if (length(args)) args[1L] else "main"
options(repos = c(CRAN = "https://cloud.r-project.org"))
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("fredie2370/RegiStat", ref = ref,
                        dependencies = TRUE, upgrade = "never")
desk <- file.path(path.expand("~"), "Desktop")
if (!dir.exists(desk)) desk <- path.expand("~")
f <- RegiStat::create_launcher(desk)
cat("\nRegiStat installed. Start it with the file:\n  ", f, "\n", sep = "")
