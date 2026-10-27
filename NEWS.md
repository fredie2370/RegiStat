# RegiStat 0.0.0.9000

* Initial package skeleton (development version).
* Metadata-driven data dictionary with Spanish and English labels
  (`read_dictionary()`, `validate_dictionary()`, `dictionary_table()`).
* Whitelisted cross-field rules: dictionaries cannot execute arbitrary code.
* Validation engine `validate_data()` with bilingual messages.
* Bundled `t2d` template (`load_template()`).
* Seeded synthetic data generator with known injected errors
  (`synthetic_t2d()`).
