# Create a registry in a temporary file that is closed and removed when the
# calling test finishes.
new_registry <- function(env = parent.frame()) {
  path <- withr::local_tempfile(fileext = ".sqlite", .local_envir = env)
  reg <- registry_create(path, load_template("t2d"), user = "tester")
  withr::defer(registry_close(reg), envir = env)
  reg
}

a_record <- function(id = "SYN-9001", ...) {
  rec <- list(record_id = id, visit_date = "2024-05-01",
              sex_at_birth = "female", age_years = 60)
  utils::modifyList(rec, list(...))
}

n_rows <- function(reg, table) {
  DBI::dbGetQuery(reg$con, sprintf("SELECT COUNT(*) AS n FROM %s", table))$n
}
