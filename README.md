# RegiStat

> **Status: pre-alpha (0.0.0.9000).** Early development. The API will change.

**RegiStat** is a free, open-source toolkit for clinical and laboratory
researchers who need structured data capture and reproducible analysis
**without an internet connection**, in **Spanish and English**, for any level
of resources.

**RegiStat** es una herramienta libre y de código abierto para investigadores
clínicos y de laboratorio que necesitan captura estructurada de datos y
análisis reproducible **sin conexión a internet**, en **español e inglés**, y
para cualquier nivel de recursos.

## What exists today / Qué existe hoy

| Component / Componente | Status / Estado |
|---|---|
| Bilingual metadata-driven data dictionary / Diccionario de datos bilingüe | Available / Disponible |
| Validation engine (required, type, range, choices, duplicates, cross-field rules) / Motor de validación | Available / Disponible |
| Seeded synthetic data with known injected errors / Datos sintéticos con errores conocidos | Available / Disponible |
| Offline eCRF capture (Shiny + SQLite) / Captura eCRF sin conexión | Planned / Planeado |
| Audit trail, roles, pseudonymisation / Bitácora, roles, seudonimización | Planned / Planeado |
| Integrated reproducible analysis, tables and plots / Análisis integrado, tablas y gráficas | Planned / Planeado |
| Parameterised R Markdown reports (es/en) / Reportes R Markdown parametrizados | Planned / Planeado |

## Installation / Instalación

Install from a local copy:

```r
install.packages(c("testthat", "shiny", "bslib", "DT", "DBI", "RSQLite", "digest",
+                    "jsonlite", "readxl", "withr", "yaml", "writexl"))
install.packages("path/to/RegiStat", repos = NULL, type = "source")
```

## Quick example / Ejemplo rápido

```r
library(RegiStat)

dict <- load_template("t2d")                       # study dictionary
dictionary_table(dict, "es")                       # codebook in Spanish

sim <- synthetic_t2d(n = 200, seed = 1, error_rate = 0.1)
issues <- validate_data(sim$data, dict, lang = "es")
head(issues)

# Seeded errors are all found, and no clean row is flagged:
setequal(paste(issues$row, issues$rule), paste(sim$truth$row, sim$truth$rule))
```

## Design principles / Principios de diseño

* **Core first.** All logic lives in plain R functions that can be tested and
  run from scripts. The Shiny interface will only call this core.
* **Metadata-driven.** Forms, ranges and labels come from a YAML dictionary,
  so the same code serves different studies.
* **Safe by construction.** Dictionary rules accept only a whitelist of
  operators; a shared dictionary cannot run arbitrary code.
* **Privacy.** Works offline. Records use pseudonymised study codes. Demo data
  are synthetic and carry the `SYN-` prefix.

## Data protection / Protección de datos

Never commit real patient data. See [SECURITY.md](SECURITY.md). RegiStat is
research software, not a medical device.

## Development note

Portions of the code and documentation were drafted with the assistance of an
AI model (Claude, Anthropic). The author is responsible for the content.

## Citation and license / Cita y licencia

See [CITATION.cff](CITATION.cff). Licensed under the
[GNU General Public License v3.0 or later](LICENSE.md).

Copyright (C) 2026 Manuel Gutiérrez Rodríguez.
