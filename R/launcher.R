#' Create a start-up icon for RegiStat
#'
#' Writes a small file that starts RegiStat with a double click, so that
#' users do not have to open R: a `.bat` file on Windows, a `.command` file on
#' macOS and a `.sh` file on Linux. The file looks for R, starts the app and
#' opens it in the browser. R and the RegiStat package must already be
#' installed (see the installation section of the README).
#'
#' @param dir Folder where the file is written, for example the Desktop.
#' @param folder Folder suggested on the start screen as the place of the
#'   registry (default: `Documents/RegiStat` in the user's home).
#' @param lang Initial interface language, `"es"` or `"en"`.
#' @param os Operating system the file is for; the default is the current one.
#' @return The path of the file, invisibly.
#' @examples
#' f <- create_launcher(tempdir(), os = "windows")
#' readLines(f)[1:3]
#' @export
create_launcher <- function(dir = file.path(path.expand("~"), "Desktop"),
                            folder = file.path(path.expand("~"), "Documents", "RegiStat"),
                            lang = c("es", "en"),
                            os = c(detect_os(), "windows", "macos", "linux")) {
  lang <- match.arg(lang)
  os <- match.arg(os)
  if (!dir.exists(dir)) abort("Directory does not exist: %s", dir)
  if (grepl("'", folder, fixed = TRUE)) abort("The folder path must not contain a single quote.")
  if (os == "windows") {
    path <- file.path(dir, "RegiStat.bat")
    lines <- c(
      "@echo off",
      "chcp 65001 >nul",
      "title RegiStat",
      "set RSCRIPT=",
      "where Rscript >nul 2>nul && set RSCRIPT=Rscript",
      "if not defined RSCRIPT (",
      "  for /f \"delims=\" %%i in ('dir /b /s /o-n \"%ProgramFiles%\\R\\Rscript.exe\" 2^>nul') do if not defined RSCRIPT set RSCRIPT=%%i",
      ")",
      "if not defined RSCRIPT (",
      "  echo R was not found / No se encontro R. Install it from https://cran.r-project.org/",
      "  pause",
      "  exit /b 1",
      ")",
      sprintf("if not exist \"%s\" mkdir \"%s\"", win_path(folder), win_path(folder)),
      "echo Starting RegiStat... close this window to stop it. / Iniciando RegiStat... cierre esta ventana para detenerlo.",
      sprintf("\"%%RSCRIPT%%\" -e \"RegiStat::run_app(folder = \\\"%s\\\", lang = \\\"%s\\\")\"",
              gsub("\\", "/", folder, fixed = TRUE), lang),
      "pause")
    writeLines(lines, path, useBytes = TRUE)
    # Windows batch files expect CRLF line endings.
    x <- readBin(path, "raw", file.size(path))
    x <- charToRaw(gsub("\n", "\r\n", rawToChar(x), fixed = TRUE))
    writeBin(x, path)
  } else {
    path <- file.path(dir, if (os == "macos") "RegiStat.command" else "RegiStat.sh")
    lines <- c(
      "#!/bin/sh",
      "if command -v Rscript >/dev/null 2>&1; then RSCRIPT=Rscript",
      "elif [ -x /usr/local/bin/Rscript ]; then RSCRIPT=/usr/local/bin/Rscript",
      "elif [ -x /opt/homebrew/bin/Rscript ]; then RSCRIPT=/opt/homebrew/bin/Rscript",
      "elif [ -x /Library/Frameworks/R.framework/Resources/bin/Rscript ]; then RSCRIPT=/Library/Frameworks/R.framework/Resources/bin/Rscript",
      "else echo 'R was not found / No se encontro R. https://cran.r-project.org/'; read x; exit 1; fi",
      sprintf("mkdir -p '%s'", folder),
      "echo 'Starting RegiStat... press Ctrl+C or close this window to stop it.'",
      sprintf("\"$RSCRIPT\" -e 'RegiStat::run_app(folder = \"%s\", lang = \"%s\")'",
              gsub("\\", "/", folder, fixed = TRUE), lang))
    writeLines(lines, path, useBytes = TRUE)
    Sys.chmod(path, "755")
  }
  invisible(path)
}

#' @noRd
detect_os <- function() {
  s <- Sys.info()[["sysname"]]
  if (s == "Windows") "windows" else if (s == "Darwin") "macos" else "linux"
}

#' @noRd
win_path <- function(x) gsub("/", "\\", x, fixed = TRUE)
