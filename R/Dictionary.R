##################################################
######### Dictionary Functions #################
##################################################

###############################################
# 0) Paquetes
###############################################
# Installing dependencies:
# install.packages("remotes")
# remotes::install_github("gandalfnicolas/SADCAT", dependencies = T)
library(SADCAT)
library(dplyr)
library(stringr)
library(stringi)
library(stringdist)
library(tidyr)
library(tibble)
library(readr)

##################################################
# 1) Cálculo del Coverage global en el diccionario
##################################################

dict_coverage <- function(df,
                          dict = SADCAT::Spanishdicts$Palabra,
                          prefix = "descriptor_",
                          out_pct = "cov_pct_global",
                          out_total = "n_descriptores_fila",
                          out_in_dict = "n_en_diccionario_fila") {
  if (!is.data.frame(df)) stop("df debe ser un data.frame o tibble.")
  
  # columnas descriptor_n según prefijo
  desc_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  if (length(desc_cols) == 0L) {
    stop(sprintf("No se encontraron columnas que empiecen por '%s'.", prefix))
  }
  
  # diccionario limpio (sin NA / vacíos)
  dict <- as.character(dict)
  dict_set <- unique(dict[!is.na(dict) & nzchar(dict)])
  
  # matriz de descriptores (character)
  desc_df <- df[desc_cols]
  desc_mat <- do.call(cbind, lapply(desc_df, function(x) as.character(x)))
  
  # válidos = no NA y no vacíos
  valid <- !is.na(desc_mat) & nzchar(desc_mat)
  
  # membership (solo para los válidos)
  covered <- matrix(FALSE, nrow = nrow(desc_mat), ncol = ncol(desc_mat))
  flat_vals <- desc_mat[valid]
  covered[valid] <- match(flat_vals, dict_set, nomatch = 0L) > 0L
  
  # conteos y porcentaje por fila
  n_total <- rowSums(valid)
  n_in_dict <- rowSums(covered)
  pct <- ifelse(n_total > 0, (n_in_dict / n_total) * 100, NA_real_)
  
  out <- df
  out[[out_total]] <- n_total
  out[[out_in_dict]] <- n_in_dict
  out[[out_pct]] <- pct
  out
}


##################################################
# 2) Cálculo del Coverage por dimensión y faceta
##################################################

dict_dim_coverage_all <- function(df,
                                  dict_df = SADCAT::Spanishdicts,
                                  palabra_col = "Palabra",
                                  prefix = "descriptor_",
                                  dict_vars = NULL,      # si NULL -> todas las que terminen en _dict
                                  out_prefix = "cov_",
                                  out_suffix = "_pct") {
  if (!is.data.frame(df)) stop("df debe ser un data.frame o tibble.")
  if (!is.data.frame(dict_df)) stop("dict_df debe ser un data.frame (p. ej., SADCAT::Spanishdicts).")
  if (!palabra_col %in% names(dict_df)) stop(sprintf("'%s' no está en dict_df.", palabra_col))
  
  # 1) columnas descriptor_n en df
  desc_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  if (length(desc_cols) == 0L) {
    stop(sprintf("No se encontraron columnas que empiecen por '%s'.", prefix))
  }
  
  # 2) variables *_dict a usar (por defecto: TODAS las que terminen en _dict)
  if (is.null(dict_vars)) {
    dict_vars <- grep("_dict$", names(dict_df), value = TRUE)
    if (length(dict_vars) == 0L) stop("No se encontraron columnas que terminen en '_dict' en dict_df.")
  } else {
    if (!is.character(dict_vars)) stop("dict_vars debe ser un vector character.")
    missing <- setdiff(dict_vars, names(dict_df))
    if (length(missing) > 0L) stop(sprintf("Estas columnas no están en dict_df: %s", paste(missing, collapse = ", ")))
  }
  
  # 3) matriz de descriptores (character)
  desc_mat <- do.call(cbind, lapply(df[desc_cols], as.character))
  valid <- !is.na(desc_mat) & nzchar(desc_mat)
  n_total <- rowSums(valid)
  
  flat_words <- desc_mat[valid]
  flat_rows  <- row(desc_mat)[valid]
  
  # 4) preparar "diccionario limpio"
  palabra <- as.character(dict_df[[palabra_col]])
  ok <- !is.na(palabra) & nzchar(palabra)
  palabra <- palabra[ok]
  dict_ok <- dict_df[ok, , drop = FALSE]
  
  # 5) salida = mismo df + nuevas columnas
  out <- df
  
  for (v in dict_vars) {
    vals <- dict_ok[[v]]
    vals[is.na(vals)] <- 0L
    vals <- as.integer(vals)
    
    # si hubiera duplicados en Palabra, nos quedamos con el máximo
    val_map <- tapply(vals, palabra, max)
    
    # lookup (OOV -> NA -> 0)
    hit <- val_map[flat_words]
    hit[is.na(hit)] <- 0L
    
    # sumar por fila
    sums <- rowsum(hit, flat_rows, reorder = FALSE)
    sums_full <- integer(nrow(df))
    sums_full[as.integer(rownames(sums))] <- sums[, 1]
    
    # porcentaje por fila (denominador = nº descriptores no-NA)
    pct <- ifelse(n_total > 0, (sums_full / n_total) * 100, NA_real_)
    
    new_col <- paste0(out_prefix, v, out_suffix)
    out[[new_col]] <- pct
  }
  
  out
}


#######################################################
# 3) Cálculo de puntuación media por dimensión y faceta
#######################################################

dict_dim_dirmean_all <- function(df,
                                 dict_df = SADCAT::Spanishdicts,
                                 palabra_col = "Palabra",
                                 prefix = "descriptor_",
                                 dir_vars = NULL,            # si NULL -> todas las que terminen en _dir
                                 out_prefix = "dirmean_",
                                 out_suffix = "",
                                 n_prefix = "n_dirmean_",
                                 strip_dir_suffix = TRUE) {
  if (!is.data.frame(df)) stop("df debe ser un data.frame o tibble.")
  if (!is.data.frame(dict_df)) stop("dict_df debe ser un data.frame (p. ej., SADCAT::Spanishdicts).")
  if (!palabra_col %in% names(dict_df)) stop(sprintf("'%s' no está en dict_df.", palabra_col))
  
  # 1) columnas descriptor_n en df
  desc_cols <- grep(paste0("^", prefix), names(df), value = TRUE)
  if (length(desc_cols) == 0L) {
    stop(sprintf("No se encontraron columnas que empiecen por '%s'.", prefix))
  }
  
  # 2) variables *_dir a usar (por defecto: TODAS las que terminen en _dir)
  if (is.null(dir_vars)) {
    dir_vars <- grep("_dir$", names(dict_df), value = TRUE)
    if (length(dir_vars) == 0L) stop("No se encontraron columnas que terminen en '_dir' en dict_df.")
  } else {
    if (!is.character(dir_vars)) stop("dir_vars debe ser un vector character.")
    missing <- setdiff(dir_vars, names(dict_df))
    if (length(missing) > 0L) stop(sprintf("Estas columnas no están en dict_df: %s", paste(missing, collapse = ", ")))
  }
  
  # 3) matriz de descriptores (character)
  desc_mat <- do.call(cbind, lapply(df[desc_cols], as.character))
  valid <- !is.na(desc_mat) & nzchar(desc_mat)
  
  flat_words <- desc_mat[valid]
  flat_rows  <- row(desc_mat)[valid]
  
  # 4) preparar diccionario limpio (palabras válidas) y sub-df alineado
  palabra <- as.character(dict_df[[palabra_col]])
  ok <- !is.na(palabra) & nzchar(palabra)
  palabra_ok <- palabra[ok]
  dict_ok <- dict_df[ok, , drop = FALSE]
  
  # 5) salida: mismo df + columnas dir_mean + n_tokens usados
  out <- df
  
  # agregación por palabra para dir si hubiera duplicados:
  # - si todo NA -> NA
  # - si hay valores -> media
  agg_dir <- function(x) {
    x <- x[!is.na(x)]
    if (length(x) == 0L) return(NA_real_)
    mean(as.numeric(x))
  }
  
  for (v in dir_vars) {
    dir_vals <- as.numeric(dict_ok[[v]])
    
    # mapa Palabra -> dir agregado
    dir_map <- tapply(dir_vals, palabra_ok, agg_dir)
    
    # lookup para cada descriptor válido (OOV o no-pertenece -> NA)
    dv <- dir_map[flat_words]
    
    idx_use <- !is.na(dv)  # SOLO estos contribuyen a la media
    if (!any(idx_use)) {
      base_name <- if (strip_dir_suffix) sub("_dir$", "", v) else v
      out[[paste0(out_prefix, base_name, out_suffix)]] <- NA_real_
      out[[paste0(n_prefix, base_name)]] <- 0L
      next
    }
    
    # suma y cuenta por fila
    sums <- rowsum(dv[idx_use], flat_rows[idx_use], reorder = FALSE)
    cnts <- rowsum(rep(1L, sum(idx_use)), flat_rows[idx_use], reorder = FALSE)
    
    mean_full <- rep(NA_real_, nrow(df))
    n_full <- integer(nrow(df))
    
    ridx <- as.integer(rownames(sums))
    mean_full[ridx] <- sums[, 1] / cnts[, 1]
    n_full[ridx] <- cnts[, 1]
    
    base_name <- if (strip_dir_suffix) sub("_dir$", "", v) else v
    out[[paste0(out_prefix, base_name, out_suffix)]] <- mean_full
    out[[paste0(n_prefix, base_name)]] <- n_full
  }
  
  out
}


