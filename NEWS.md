# RegiStat 0.0.0.9000

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
