# Contributing to RegiStat

Thank you for your interest. Contributions in English or Spanish are welcome.

## Ground rules

* **No real patient data**, ever, in code, tests, examples, issues or pull
  requests. Use `synthetic_t2d()`. The CI fails if database or spreadsheet
  files are tracked.
* Every change needs tests (`tests/testthat/`). Run `devtools::test()` and
  `devtools::check()` before opening a pull request.
* User-facing text must exist in both Spanish and English.
* Document exported functions with roxygen2 and run `devtools::document()`.

## Workflow

1. Open an issue describing the problem or proposal.
2. Fork the repository and create a branch.
3. Make the change, add tests, update `NEWS.md`.
4. Open a pull request linking the issue.

## License

By contributing you agree that your contribution is licensed under the
GNU General Public License, version 3 or later.
