# AGRUPA

<!-- badges: start -->
<!-- badges: end -->

AGRUPA is an internal R package for the **AGRUPA project**. It provides utilities to (i) extract and normalize candidate descriptors from artwork descriptions (comma-separated descriptors or free narrative text), and (ii) compute **SADCAT**-based dictionary metrics such as **global coverage**, **coverage by dimension/facet**, and **mean direction scores**.

The package is designed to work with the dictionaries distributed in the [`SADCAT`](https://github.com/gandalfnicolas/SADCAT) R package, and follows a normalization pipeline compatible with SADCAT lookup tables (lowercasing, removal of all whitespace, and optional diacritic stripping).

## What this package does

### 1) Prepare candidate descriptors
`prepare_descriptors()` supports two input formats:

- **Comma-separated descriptors** (`input_format = "comma"`):  
  turns `"elegante, solemne"` into a wide data frame with `descriptor_1`, `descriptor_2`, ...

- **Narrative text** (`input_format = "text"`):  
  extracts **unigrams** (optionally removing stopwords) and, optionally, **bigrams/trigrams** computed within punctuation-delimited segments (to avoid crossing sentence boundaries).

Optional **UDPipe lemmatization** can be enabled via `lemmatize = "lemma"` or `"both"`.

### 2) Dictionary metrics (SADCAT)
Given prepared descriptors (wide format), the package provides:

- `dict_coverage()` — global in-dictionary coverage (% of candidates found in the dictionary)
- `dict_dim_coverage_all()` — coverage per dictionary membership variable (`*_dict`)
- `dict_dim_dirmean_all()` — mean direction score per direction variable (`*_dir`) + number of contributing descriptors

### 3) Quality control helpers
- `filter_by_coverage()` — filter rows by a coverage threshold expressed in percent (supports coverage in 0–100 or 0–1 scales)

## Installation

You can install the development version of AGRUPA from GitHub with:

```r
# install.packages("pak")
pak::pak("alejandrommingo/AGRUPA")
```

### Dependencies
AGRUPA relies on:
- **SADCAT** (dictionary objects)
- **stringi** and **stopwords** (normalization and stopword removal; required if those options are enabled)
- **udpipe** (optional; required only if `lemmatize != "none"`)

If you plan to use lemmatization, install `udpipe` as well:

```r
install.packages("udpipe")
```

## Quick start

### 1) Comma-separated descriptors

```r
library(AGRUPA)

df_desc <- prepare_descriptors(
  "elegante, solemne",
  input_type = "string",
  input_format = "comma"
)

df_desc
```

### 2) Narrative text (unigrams + optional n-grams)

```r
df_text <- prepare_descriptors(
  "Una obra magistral y compleja, con un aire solemne.",
  input_type = "string",
  input_format = "text",
  remove_stopwords = TRUE,
  stopwords_lang = "es",
  include_ngrams = TRUE
)

head(df_text)
```

### 3) Global dictionary coverage

```r
df_cov <- dict_coverage(df_text, prefix = "descriptor_")
df_cov[, c("n_descriptores_fila", "n_en_diccionario_fila", "cov_pct_global")]
```

### 4) Coverage by dimension/facet

```r
df_dim_cov <- dict_dim_coverage_all(df_text)
head(df_dim_cov)
```

### 5) Mean direction scores

```r
df_dir <- dict_dim_dirmean_all(df_text)
head(df_dir)
```

## Lemmatization with UDPipe (optional)

To lemmatize, you must provide an UDPipe model (either a loaded `udpipe_model` object or a path to a `.udpipe` file):

```r
# Example (Spanish model):
# model_path <- udpipe::udpipe_download_model(language = "spanish")$file_model
# ud_model <- udpipe::udpipe_load_model(model_path)

df_lem <- prepare_descriptors(
  "Las figuras parecen solemnes y elegantes.",
  input_type = "string",
  input_format = "text",
  lemmatize = "both",
  udpipe_model = ud_model
)

head(df_lem)
```

## Notes and conventions

- **Normalization**: descriptors are lowercased and all whitespace is removed.  
  If `remove_diacritics = TRUE`, diacritics are stripped (optionally preserving `ñ` with `keep_enye = TRUE`).
- **N-grams**: generated within punctuation-delimited segments to prevent cross-boundary n-grams.
- **Avoiding double counting**: when `lemmatize = "both"`, lemma columns are included only when they add information (lemmas identical to surface forms are set to `NA` and lemma candidates are deduplicated within-row).

## Vignette

For a complete workflow and recommended usage patterns, see:

```r
vignette("working-with-sadcat-descriptors")
```

## Development

Typical development commands:

```r
devtools::load_all()
devtools::document()
devtools::check()
devtools::build_vignettes()
```

## License

Internal use for the AGRUPA project. (Add a license here if you plan to open-source it.)
