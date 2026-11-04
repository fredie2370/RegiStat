# RegiStat

> **Status: pre-alpha (0.1.1).** Early development. The API will change.

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
| Local SQLite registry: add, update, soft-delete, import / Registro SQLite local | Available / Disponible |
| Tamper-evident audit trail (hash chain, mandatory reasons) / Bitácora de auditoría con cadena de hashes | Available / Disponible |
| Offline Shiny app: capture form generated from the dictionary, records table with audited edit/delete, import from CSV/Excel with column matching and pre-check, audit viewer, language switch / App Shiny sin conexión: formulario de captura generado del diccionario, tabla de registros con edición y eliminación auditadas, importación CSV/Excel con relación de columnas y revisión previa, visor de bitácora, cambio de idioma | Available / Disponible |
| Case finding ("cotejo"): reference-list versions, review log, derived status, summary, changes between lists, in a separate file / Cotejo: versiones de la lista de referencia, bitácora de revisiones, estado derivado, resumen, cambios entre listas, en un archivo aparte | Available / Disponible |
| Authentication, roles, pseudonymisation helpers / Autenticación, roles, seudonimización | Planned / Planeado |
| Descriptive analysis: Table 1 (overall or by group, with tests), missing data, interactive plotly/ggplot2 plots / Análisis descriptivo: Tabla 1 (total o por grupo, con pruebas), datos faltantes, gráficas interactivas | Available / Disponible |
| Offline reproducible HTML report (R Markdown, es/en) with audit-trail check / Reporte HTML reproducible sin conexión (R Markdown, es/en) con verificación de bitácora | Available / Disponible |
| Calculated measures: BMI, WHO category, eGFR CKD-EPI 2021 / Medidas calculadas: IMC, categoría OMS, TFGe CKD-EPI 2021 | Available / Disponible |

## Installation / Instalación

Source code: <https://github.com/fredie2370/RegiStat>. Pre-alpha: it is not
on CRAN. Download or clone the repository and install from the folder that
contains the `DESCRIPTION` file.

Código fuente: <https://github.com/fredie2370/RegiStat>. Fase pre-alfa: no
está en CRAN. Descargue o clone el repositorio e instale desde la carpeta que
contiene el archivo `DESCRIPTION`.

```r
# Dependencies / Dependencias
install.packages(c("shiny", "bslib", "DT", "DBI", "RSQLite", "digest",
                   "jsonlite", "readxl", "withr", "yaml", "ggplot2"))
# Optional: reports, interactive plots / Opcional: reportes, gráficas interactivas
install.packages(c("rmarkdown", "knitr", "plotly"))

install.packages("path/to/RegiStat", repos = NULL, type = "source")
```

## Start the app / Iniciar la aplicación

```r
RegiStat::run_app(folder = "my-study", lang = "es")
```

The app opens in your browser and works without internet. Type your name at
the top (it is saved in the audit trail), choose a project folder, and create
or open a registry. The registry is the file `registry.sqlite` in that folder.

La aplicación se abre en su navegador y funciona sin internet. Escriba su
nombre en la parte superior (se guarda en la bitácora), elija una carpeta de
proyecto y cree o abra un registro. El registro es el archivo
`registry.sqlite` de esa carpeta.

### Analysis and report / Análisis y reporte

The *Analysis* tab gives a descriptive table (optionally by group, with
p-values), interactive plots and a one-click offline HTML report. The same
functions work in scripts:

```r
a <- analysis_prepare(registry_data(reg), reg$dict, "es")
table_one(a, by = "sex_at_birth")
plot_variable(a, "hba1c_pct", by = "sex_at_birth")
registry_report(reg, "reporte.html", lang = "es", by = "sex_at_birth")
```

Reports need `rmarkdown`, `knitr` and Pandoc (included with RStudio);
interactive plots need `plotly`. Without them the app still works and says
what is missing. The p-values are descriptive aids with no adjustment for
multiple comparisons.

La pestaña *Análisis* ofrece una tabla descriptiva (opcionalmente por grupo,
con valores p), gráficas interactivas y un reporte HTML sin conexión. Los
reportes requieren `rmarkdown`, `knitr` y Pandoc (incluido con RStudio); las
gráficas interactivas, `plotly`. Los valores p son ayudas descriptivas, sin
ajuste por comparaciones múltiples.

### Case finding ("cotejo") and privacy / Cotejo y privacidad

The *Case finding* tab checks which hospital file numbers appear on a
reference list (for example, the hospital's list of patients with diabetes)
and keeps a log of reviews. Hospital file numbers are **never** stored in the
registry: they live in a separate file, `screening.sqlite`, next to it. The
only optional link is the pseudonymised study code typed in a review.
Whether a file is "on the list" is computed when you look, against the
newest list version, so it cannot be out of date; loading a new list version
re-checks every review at once. Keep `screening.sqlite` in a restricted
folder and never share or publish it.

La pestaña *Cotejo* verifica qué números de expediente del hospital aparecen
en una lista de referencia (por ejemplo, la lista de pacientes con diabetes)
y lleva una bitácora de revisiones. Los números de expediente **nunca** se
guardan en el registro: viven en un archivo aparte, `screening.sqlite`, junto
a él. El único vínculo opcional es el código seudonimizado que se escriba en
una revisión. Si un expediente "está en la lista" se calcula al consultar, con
la versión más reciente, por lo que nunca queda desactualizado; al cargar una
versión nueva de la lista se vuelven a cotejar todas las revisiones. Guarde
`screening.sqlite` en una carpeta de acceso restringido y no lo comparta ni
lo publique.

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

# A local registry with an audit trail (one SQLite file, no network):
reg <- registry_create("study.sqlite", dict, user = "ana")
registry_import(reg, synthetic_t2d(50, seed = 2)$data, user = "ana")
registry_update(reg, "SYN-0001", list(age_years = 61),
                user = "ana", reason = "Corrected after source check")
registry_audit(reg, "SYN-0001")
registry_verify(reg)      # chain check + replay of the audit trail
registry_close(reg)
```

### What the audit trail does and does not guarantee

Every change is validated, then written to the registry and to an
append-only audit trail in one transaction. Each audit entry carries the hash
of the previous one, so editing or removing an entry in the middle of the
chain, or editing records directly in the database file, is detected by
`registry_verify()`.

It is **tamper-evident, not tamper-proof**. Someone with write access to the
file who understands the format could rewrite the whole chain, and removing
the *last* entries leaves a valid chain. Save the `head` hash returned by
`registry_verify()` outside the database and pass it later as
`expected_head` to detect that. User names are self-declared: there is no
authentication yet.

## Design principles / Principios de diseño

* **Core first.** All logic lives in plain R functions that can be tested and
  run from scripts. The Shiny interface only calls this core.
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
