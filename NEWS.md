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
