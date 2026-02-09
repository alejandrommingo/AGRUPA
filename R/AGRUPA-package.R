#' AGRUPA: SADCAT descriptor extraction and dictionary-based scoring
#'
#' **AGRUPA** is an internal R package for the AGRUPA project. It provides utilities to:
#' \itemize{
#'   \item extract candidate descriptors from Spanish text (comma-separated lists or narrative descriptions),
#'   \item normalize candidates to match SADCAT lookup conventions (lowercase, remove all whitespace,
#'         optional diacritic stripping, optional preservation of "ñ"),
#'   \item compute dictionary-based metrics: global coverage, coverage by dimension/facet (`*_dict`),
#'         and mean direction scores (`*_dir`).
#' }
#'
#' @section Main user-facing functions:
#' \itemize{
#'   \item \code{\link{prepare_descriptors}}: main wrapper to generate candidate descriptors (unigrams + optional n-grams).
#'   \item \code{\link{split_descriptors}}: legacy helper for comma-separated descriptors (kept for backwards compatibility).
#'   \item \code{\link{dict_coverage}}: global dictionary coverage.
#'   \item \code{\link{dict_dim_coverage_all}}: coverage by dimension/facet.
#'   \item \code{\link{dict_dim_dirmean_all}}: mean direction scores by dimension/facet.
#'   \item \code{\link{filter_by_coverage}}: convenience filter for coverage thresholds.
#' }
#'
#' @section Vignette:
#' See \code{vignette("working-with-sadcat-descriptors")} for an end-to-end workflow.
#'
#' @references
#' Fiske, S. T., Cuddy, A. J. C., Glick, P., & Xu, J. (2002). A model of (often mixed) stereotype content:
#' Competence and warmth respectively follow from perceived status and competition. *Journal of Personality and Social Psychology, 82*(6), 878–902.
#'
#' Nicolas, G., Bai, X., & Fiske, S. T. (2021). Comprehensive stereotype content dictionaries using a semi‐automated method. 
#' *European Journal of Social Psychology*, 51(1), 178-196.
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
