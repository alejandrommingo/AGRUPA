##################################################
######### Loading Data Functions #################
##################################################

###############################################
# 0) Paquetes (NO usar library() en scripts de paquete)
###############################################
# Installing dependencies (esto mejor en README/vignette, no en R/):
# install.packages("remotes")
# remotes::install_github("gandalfnicolas/SADCAT", dependencies = TRUE)

# En este script NO se cargan paquetes con library().
# Se usan llamadas explícitas tipo pkg::func() y checks con requireNamespace()
# para dependencias opcionales según parámetros.

###############################################
# 1) Cargar diccionario español desde SADCAT
###############################################

#' Load SADCAT dictionaries
#'
#' Convenience wrapper around objects shipped with the **SADCAT** package.
#'
#' @param language Character. `"es"` returns the Spanish dictionary (`SADCAT::Spanishdicts`).
#'   Any other value returns `SADCAT::All.steps_Dictionaries` (legacy / non-Spanish set).
#'
#' @return A data frame containing SADCAT lexical entries and associated variables.
#'
#' @examples
#' dic_es <- load_dic("es")
#' head(dic_es)
#'
#' @export
load_dic <- function(language = "en"){
  if(language == "es"){
    return(SADCAT::Spanishdicts)
  }
  else{
    return(SADCAT::All.steps_Dictionaries)
  }
}

###############################################
# 2) Procesado de los datos
###############################################

#' Split and normalize comma-separated descriptors (legacy helper)
#'
#' This function takes a comma-separated descriptor field and converts it into a **wide** data frame
#' with columns `descriptor_1`, `descriptor_2`, ... suitable for dictionary lookup.
#'
#' It performs the AGRUPA/SADCAT normalization steps:
#' - trimming,
#' - lowercasing,
#' - removal of *all* whitespace,
#' - optional diacritic stripping (accent removal),
#' - optional UDPipe lemmatization (`lemmatize = "lemma"` or `"both"`).
#'
#' @details
#' This function is kept for backwards compatibility. For new code, prefer
#' [prepare_descriptors()] which supports both comma-separated descriptors and narrative text
#' (unigrams and n-grams).
#'
#' When `lemmatize = "both"`, lemma-based candidates are returned in additional columns with
#' suffix `lemma_suffix`, but only when the lemma adds new information (lemmas identical to
#' the normalized surface form are set to `NA` to avoid double counting).
#'
#' @param x Input descriptors. See `input_type`.
#' @param input_type `"string"`, `"vector"`, or `"data"`. Controls how `x` is interpreted.
#' @param desc_col For `input_type = "data"`, the name of the column in `x` that contains the
#'   comma-separated descriptors.
#' @param prefix Prefix for generated descriptor columns (default `"descriptor_"`).
#' @param drop_desc_col Logical. For `input_type = "data"`, drop `desc_col` from the output.
#' @param remove_diacritics Logical. If `TRUE`, remove accents and diacritics using **stringi**.
#' @param keep_enye Logical. If `TRUE`, preserve `"ñ"` when removing other diacritics.
#' @param lemmatize `"none"`, `"lemma"`, or `"both"`.
#' @param udpipe_model An `udpipe_model` object or a path to a `.udpipe` file (required if lemmatizing).
#' @param lemma_suffix Suffix for lemma columns when `lemmatize = "both"` (default `"_lemma"`).
#'
#' @return A data frame with one row per input row and descriptor columns in wide format.
#'
#' @examples
#' split_descriptors("elegante, solemne", input_type = "string")
#'
#' @export
split_descriptors <- function(x,
                              input_type = c("string", "vector", "data"),
                              desc_col = NULL,
                              prefix = "descriptor_",
                              drop_desc_col = TRUE,
                              remove_diacritics = TRUE,
                              keep_enye = FALSE,
                              lemmatize = c("none", "lemma", "both"),
                              udpipe_model = NULL,
                              lemma_suffix = "_lemma") {
  input_type <- match.arg(input_type)
  lemmatize  <- match.arg(lemmatize)
  
  if (remove_diacritics && !requireNamespace("stringi", quietly = TRUE)) {
    stop("Instala 'stringi' para eliminar tildes/diacríticos: install.packages('stringi').")
  }
  if (lemmatize %in% c("lemma", "both") && !requireNamespace("udpipe", quietly = TRUE)) {
    stop("Instala 'udpipe' para lematizar: install.packages('udpipe').")
  }
  
  # --- helper: elimina diacríticos (y opcionalmente preserva ñ)
  strip_diacritics <- function(s) {
    if (!remove_diacritics) return(s)
    out <- s
    idx <- !is.na(out)
    if (!any(idx)) return(out)
    
    if (keep_enye) {
      tmp <- out[idx]
      tmp <- gsub("ñ", "<<<ENYE>>>", tmp, fixed = TRUE)
      tmp <- stringi::stri_trans_general(tmp, "Latin-ASCII")
      tmp <- gsub("<<<ENYE>>>", "ñ", tmp, fixed = TRUE)
      out[idx] <- tmp
    } else {
      out[idx] <- stringi::stri_trans_general(out[idx], "Latin-ASCII")
    }
    out
  }
  
  # --- helper: normalización final (idéntica a tu pipeline, pero reutilizable)
  finalize_norm <- function(parts) {
    if (length(parts) == 0L) return(character(0))
    parts <- gsub("\\s+", "", parts, perl = TRUE)   # quita TODOS los espacios
    parts <- tolower(parts)
    parts <- strip_diacritics(parts)
    parts <- parts[nzchar(parts)]
    parts
  }
  
  # --- helper: lematiza vector de strings usando udpipe (en bloque)
  lemmatize_udpipe <- function(text_vec, model) {
    if (is.null(model)) {
      stop("Si lemmatize='lemma' o 'both' debes proporcionar 'udpipe_model' (objeto udpipe_model o ruta al modelo).")
    }
    if (is.character(model)) model <- udpipe::udpipe_load_model(model)
    
    text_vec <- as.character(text_vec)
    text_vec <- iconv(text_vec, from = "", to = "UTF-8")
    
    doc_ids <- paste0("doc", seq_along(text_vec))
    anno <- udpipe::udpipe_annotate(model, x = text_vec, doc_id = doc_ids)
    df <- as.data.frame(anno)
    
    lemma2 <- df$lemma
    bad <- is.na(lemma2) | !nzchar(lemma2)
    lemma2[bad] <- df$token[bad]
    
    # reconstruye por doc_id (con espacios); luego se eliminarán espacios en finalize_norm()
    lem_by_doc <- tapply(lemma2, df$doc_id, function(z) paste(z, collapse = " "))
    
    vapply(doc_ids, function(id) {
      v <- lem_by_doc[[id]]
      if (is.null(v) || is.na(v)) "" else v
    }, FUN.VALUE = character(1))
  }
  
  # --- helper: parsea vector de strings y devuelve data.frame ancho
  parse_vector <- function(v) {
    v <- as.character(v)
    
    split_one <- function(s) {
      if (is.na(s) || !nzchar(s)) return(character(0))
      parts <- strsplit(s, ",", fixed = TRUE)[[1]]
      parts <- trimws(parts)
      parts <- parts[nzchar(parts)]
      parts <- tolower(parts)
      parts
    }
    
    parts_list <- lapply(v, split_one)
    
    # Si no hay nada, devolver 0 columnas
    max_len <- max(lengths(parts_list), 0L)
    if (max_len == 0L) {
      return(as.data.frame(matrix(nrow = length(v), ncol = 0)))
    }
    
    # --- Normalizado base (siempre se puede necesitar: none/both)
    norm_list <- lapply(parts_list, finalize_norm)
    
    # --- Lematizado (si aplica)
    lemma_list <- NULL
    if (lemmatize %in% c("lemma", "both")) {
      lens <- lengths(parts_list)
      total <- sum(lens)
      if (total > 0L) {
        flat <- unlist(parts_list, use.names = FALSE)
        flat_lem <- lemmatize_udpipe(flat, udpipe_model)
        
        # reensambla con longitudes originales
        idx <- rep.int(seq_along(parts_list), lens)
        tmp <- split(flat_lem, idx)
        lemma_list <- vector("list", length(parts_list))
        lemma_list[as.integer(names(tmp))] <- tmp
        for (i in seq_along(lemma_list)) if (is.null(lemma_list[[i]])) lemma_list[[i]] <- character(0)
        
        # normaliza tras lematizar
        lemma_list <- lapply(lemma_list, finalize_norm)
      } else {
        lemma_list <- replicate(length(parts_list), character(0), simplify = FALSE)
      }
    }
    
    # --- Decide qué sale
    if (lemmatize == "none") {
      out_list <- norm_list
      max_len <- max(lengths(out_list), 0L)
      
      mat <- t(vapply(out_list, function(p) { length(p) <- max_len; p },
                      FUN.VALUE = character(max_len)))
      out <- as.data.frame(mat, stringsAsFactors = FALSE)
      names(out) <- paste0(prefix, seq_len(max_len))
      return(out)
    }
    
    if (lemmatize == "lemma") {
      out_list <- lemma_list
      max_len <- max(lengths(out_list), 0L)
      
      mat <- t(vapply(out_list, function(p) { length(p) <- max_len; p },
                      FUN.VALUE = character(max_len)))
      out <- as.data.frame(mat, stringsAsFactors = FALSE)
      names(out) <- paste0(prefix, seq_len(max_len))
      return(out)
    }
    
    # lemmatize == "both": devuelve normalizado + lematizado SOLO si aporta info nueva
    max_len_norm  <- max(lengths(norm_list), 0L)
    max_len_lemma <- max(lengths(lemma_list), 0L)
    max_len_out   <- max(max_len_norm, max_len_lemma, 0L)
    
    mat_norm <- t(vapply(norm_list, function(p) { length(p) <- max_len_out; p },
                         FUN.VALUE = character(max_len_out)))
    mat_lem  <- t(vapply(lemma_list, function(p) { length(p) <- max_len_out; p },
                         FUN.VALUE = character(max_len_out)))
    
    # 1) Normalizamos vacíos a NA para comparar bien
    mat_norm[!nzchar(mat_norm)] <- NA_character_
    mat_lem[!nzchar(mat_lem)]   <- NA_character_
    
    # 2) Si lemma == norm, anulamos lemma (no aporta info nueva)
    same <- !is.na(mat_norm) & !is.na(mat_lem) & (mat_norm == mat_lem)
    mat_lem[same] <- NA_character_
    
    # 3) Deduplicación dentro de cada fila del bloque lemma
    mat_lem <- t(apply(mat_lem, 1, function(row) {
      seen <- character(0)
      out  <- row
      for (k in seq_along(out)) {
        v <- out[k]
        if (is.na(v)) next
        if (v %in% seen) out[k] <- NA_character_ else seen <- c(seen, v)
      }
      out
    }))
    
    out_norm <- as.data.frame(mat_norm, stringsAsFactors = FALSE)
    out_lem  <- as.data.frame(mat_lem,  stringsAsFactors = FALSE)
    
    names(out_norm) <- paste0(prefix, seq_len(max_len_out))
    names(out_lem)  <- paste0(prefix, seq_len(max_len_out), lemma_suffix)
    
    cbind(out_norm, out_lem)
    
  }
  
  # --- dispatch
  if (input_type == "string") {
    if (!is.character(x) || length(x) != 1L) {
      stop("Con input_type='string', x debe ser un único string (character de longitud 1).")
    }
    return(parse_vector(x))
  }
  
  if (input_type == "vector") {
    return(parse_vector(x))
  }
  
  # input_type == "data"
  if (!is.data.frame(x)) stop("Con input_type='data', x debe ser un data.frame o tibble.")
  if (is.null(desc_col) || !is.character(desc_col) || length(desc_col) != 1L) {
    stop("Con input_type='data', desc_col debe ser el nombre (character) de la columna de descripciones.")
  }
  if (!desc_col %in% names(x)) stop(sprintf("La columna '%s' no existe en el dataset.", desc_col))
  
  desc_df <- parse_vector(x[[desc_col]])
  
  out <- x
  if (drop_desc_col) out[[desc_col]] <- NULL
  cbind(out, desc_df)
}

# Convierte un umbral en % (p.ej., 30) a escala del vector (0-100 o 0-1)
.coverage_threshold <- function(v, thr_pct) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_real_)
  if (max(v) <= 1) thr_pct / 100 else thr_pct
}

#' Filter rows by a coverage threshold (percentage)
#'
#' Filters a data frame by a coverage column using a threshold expressed as a percentage.
#' The function automatically adapts to coverage columns expressed on a `0-100` or `0-1` scale.
#'
#' @param df A data frame.
#' @param coverage_col Character. Name of the coverage column in `df`.
#' @param thr_pct Numeric. Threshold in percent (e.g., `30` means 30%).
#'
#' @return A filtered data frame (same columns, fewer rows).
#'
#' @examples
#' df <- data.frame(id = 1:3, cov = c(10, 50, 90))
#' filter_by_coverage(df, "cov", 30)
#'
#' @export
filter_by_coverage <- function(df, coverage_col, thr_pct) {
  stopifnot(is.data.frame(df), coverage_col %in% names(df))
  thr <- .coverage_threshold(df[[coverage_col]], thr_pct)
  if (is.na(thr)) return(df[0, , drop = FALSE])  # todo NA -> 0 filas
  df[!is.na(df[[coverage_col]]) & df[[coverage_col]] >= thr, , drop = FALSE]
}


###############################################
# A) Utilidades comunes
###############################################

sd_strip_diacritics <- function(s, remove_diacritics = TRUE, keep_enye = FALSE) {
  if (!remove_diacritics) return(s)
  if (!requireNamespace("stringi", quietly = TRUE)) {
    stop("Instala 'stringi' para eliminar tildes/diacríticos: install.packages('stringi').")
  }
  
  out <- s
  idx <- !is.na(out)
  if (!any(idx)) return(out)
  
  if (keep_enye) {
    tmp <- out[idx]
    tmp <- gsub("ñ", "<<<ENYE>>>", tmp, fixed = TRUE)
    tmp <- stringi::stri_trans_general(tmp, "Latin-ASCII")
    tmp <- gsub("<<<ENYE>>>", "ñ", tmp, fixed = TRUE)
    out[idx] <- tmp
  } else {
    out[idx] <- stringi::stri_trans_general(out[idx], "Latin-ASCII")
  }
  out
}

sd_finalize_norm <- function(parts, remove_diacritics = TRUE, keep_enye = FALSE) {
  if (length(parts) == 0L) return(character(0))
  parts <- gsub("\\s+", "", parts, perl = TRUE)  # quita TODOS los espacios (incluidos internos)
  parts <- tolower(parts)
  parts <- sd_strip_diacritics(parts, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
  parts <- parts[nzchar(parts)]
  parts
}

sd_make_wide <- function(list_vecs, col_prefix) {
  max_len <- max(lengths(list_vecs), 0L)
  if (max_len == 0L) {
    return(as.data.frame(matrix(nrow = length(list_vecs), ncol = 0)))
  }
  mat <- t(vapply(list_vecs, function(v) { length(v) <- max_len; v },
                  FUN.VALUE = character(max_len)))
  out <- as.data.frame(mat, stringsAsFactors = FALSE)
  names(out) <- paste0(col_prefix, seq_len(max_len))
  out
}

sd_get_stopwords <- function(lang = c("es", "en"), stopwords_custom = NULL) {
  if (!is.null(stopwords_custom)) return(tolower(as.character(stopwords_custom)))
  
  lang <- match.arg(lang)
  if (!requireNamespace("stopwords", quietly = TRUE)) {
    stop("Instala 'stopwords' para eliminar stopwords: install.packages('stopwords').")
  }
  # fuente ISO suele ser consistente
  tolower(stopwords::stopwords(lang, source = "stopwords-iso"))
}

###############################################
# B) Lematización (UDPipe) con contexto (texto completo)
###############################################

sd_udpipe_annotate_text <- function(text_vec, udpipe_model) {
  if (is.null(udpipe_model)) {
    stop("Necesitas 'udpipe_model' (objeto udpipe_model o ruta al modelo) para lematizar/anotar.")
  }
  if (!requireNamespace("udpipe", quietly = TRUE)) {
    stop("Instala 'udpipe' para lematizar/anotar: install.packages('udpipe').")
  }
  
  if (is.character(udpipe_model)) {
    udpipe_model <- udpipe::udpipe_load_model(udpipe_model)
  }
  
  text_vec <- as.character(text_vec)
  text_vec <- iconv(text_vec, from = "", to = "UTF-8")
  doc_ids <- paste0("doc", seq_along(text_vec))
  
  anno <- udpipe::udpipe_annotate(udpipe_model, x = text_vec, doc_id = doc_ids)
  as.data.frame(anno)
}

###############################################
# C) Tokenización simple (sin udpipe) para n-gramas
###############################################

sd_segments_regex <- function(text) {
  if (is.na(text) || !nzchar(text)) return(character(0))
  segs <- unlist(strsplit(text, "[\\.!\\?;:,\\n\\r]+", perl = TRUE))
  segs <- trimws(segs)
  segs[nzchar(segs)]
}

sd_tokens_from_segment <- function(segment, keep_numbers = FALSE) {
  if (!nzchar(segment)) return(character(0))
  
  if (keep_numbers) {
    pattern <- "[\\p{L}\\p{N}]+"
  } else {
    pattern <- "[\\p{L}]+"
  }
  
  m <- gregexpr(pattern, segment, perl = TRUE)
  tok <- regmatches(segment, m)[[1]]
  if (length(tok) == 0L) return(character(0))
  
  tok <- tolower(tok)
  tok
}

sd_generate_ngrams <- function(tokens, n = 2L) {
  tokens <- tokens[!is.na(tokens) & nzchar(tokens)]
  if (length(tokens) < n) return(character(0))
  vapply(seq_len(length(tokens) - n + 1L), function(i) {
    paste(tokens[i:(i + n - 1L)], collapse = " ")
  }, FUN.VALUE = character(1))
}

###############################################
# D) Texto narrativo: n-gramas (bi/tri)
###############################################

sd_text_ngrams_df <- function(text_vec,
                              remove_diacritics = TRUE,
                              keep_enye = FALSE,
                              keep_numbers = FALSE,
                              dedupe_within_row = TRUE,
                              max_ngrams = Inf,
                              text_col = "text") {
  text_vec <- as.character(text_vec)
  
  bi_list <- vector("list", length(text_vec))
  tri_list <- vector("list", length(text_vec))
  
  for (i in seq_along(text_vec)) {
    txt <- text_vec[i]
    if (is.na(txt) || !nzchar(txt)) {
      bi_list[[i]] <- character(0)
      tri_list[[i]] <- character(0)
      next
    }
    
    segs <- sd_segments_regex(txt)
    
    bigrams <- character(0)
    trigrams <- character(0)
    
    for (s in segs) {
      toks <- sd_tokens_from_segment(s, keep_numbers = keep_numbers)
      if (length(toks) == 0L) next
      bigrams <- c(bigrams, sd_generate_ngrams(toks, n = 2L))
      trigrams <- c(trigrams, sd_generate_ngrams(toks, n = 3L))
    }
    
    bigrams <- sd_finalize_norm(bigrams, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
    trigrams <- sd_finalize_norm(trigrams, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
    
    if (dedupe_within_row) {
      bigrams <- bigrams[!duplicated(bigrams)]
      trigrams <- trigrams[!duplicated(trigrams)]
    }
    
    if (is.finite(max_ngrams)) {
      bigrams <- head(bigrams, max_ngrams)
      trigrams <- head(trigrams, max_ngrams)
    }
    
    bi_list[[i]] <- bigrams
    tri_list[[i]] <- trigrams
  }
  
  df_text <- data.frame(setNames(list(text_vec), text_col), stringsAsFactors = FALSE)
  df_bi <- sd_make_wide(bi_list, "descriptor_2_gram_")
  df_tri <- sd_make_wide(tri_list, "descriptor_3_gram_")
  
  cbind(df_text, df_bi, df_tri)
}

###############################################
# E) Texto narrativo: unigramas + lemas opcionales
###############################################

sd_text_unigrams_df <- function(text_vec,
                                remove_stopwords = TRUE,
                                stopwords_lang = c("es", "en"),
                                stopwords_custom = NULL,
                                remove_diacritics = TRUE,
                                keep_enye = FALSE,
                                dedupe_within_row = TRUE,
                                lemmatize = c("none", "lemma", "both"),
                                udpipe_model = NULL,
                                lemma_suffix = "_lemma",
                                text_col = "text") {
  text_vec <- as.character(text_vec)
  lemmatize <- match.arg(lemmatize)
  stopwords_lang <- match.arg(stopwords_lang)
  
  sw <- character(0)
  if (remove_stopwords) {
    sw <- sd_get_stopwords(stopwords_lang, stopwords_custom = stopwords_custom)
  }
  
  anno <- NULL
  if (lemmatize %in% c("lemma", "both")) {
    anno <- sd_udpipe_annotate_text(text_vec, udpipe_model)
  }
  
  norm_list <- vector("list", length(text_vec))
  lemma_list <- if (lemmatize %in% c("lemma", "both")) vector("list", length(text_vec)) else NULL
  
  for (i in seq_along(text_vec)) {
    txt <- text_vec[i]
    if (is.na(txt) || !nzchar(txt)) {
      norm_list[[i]] <- character(0)
      if (!is.null(lemma_list)) lemma_list[[i]] <- character(0)
      next
    }
    
    if (is.null(anno)) {
      segs <- sd_segments_regex(txt)
      toks <- unlist(lapply(segs, sd_tokens_from_segment, keep_numbers = FALSE), use.names = FALSE)
      toks <- toks[!is.na(toks) & nzchar(toks)]
      
      if (remove_stopwords) toks <- toks[!(toks %in% sw)]
      
      norm <- sd_finalize_norm(toks, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
      
      if (dedupe_within_row) norm <- norm[!duplicated(norm)]
      norm_list[[i]] <- norm
      
    } else {
      doc_id <- paste0("doc", i)
      dfi <- anno[anno$doc_id == doc_id, , drop = FALSE]
      
      is_word <- grepl("\\p{L}", dfi$token, perl = TRUE)
      dfi <- dfi[is_word, , drop = FALSE]
      
      tok <- tolower(dfi$token)
      lem <- dfi$lemma
      bad <- is.na(lem) | !nzchar(lem)
      lem[bad] <- dfi$token[bad]
      lem <- tolower(lem)
      
      if (remove_stopwords) {
        keep <- !(tok %in% sw)
        tok <- tok[keep]
        lem <- lem[keep]
      }
      
      norm <- sd_finalize_norm(tok, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
      lem2 <- sd_finalize_norm(lem, remove_diacritics = remove_diacritics, keep_enye = keep_enye)
      
      if (dedupe_within_row) {
        norm <- norm[!duplicated(norm)]
        lem2 <- lem2[!duplicated(lem2)]
      }
      
      norm_list[[i]] <- norm
      lemma_list[[i]] <- lem2
    }
  }
  
  df_text <- data.frame(setNames(list(text_vec), text_col), stringsAsFactors = FALSE)
  
  if (lemmatize == "none") {
    df_norm <- sd_make_wide(norm_list, "descriptor_")
    return(cbind(df_text, df_norm))
  }
  
  if (lemmatize == "lemma") {
    df_lem <- sd_make_wide(lemma_list, "descriptor_")
    return(cbind(df_text, df_lem))
  }
  
  max_len_out <- max(max(lengths(norm_list), 0L), max(lengths(lemma_list), 0L), 0L)
  if (max_len_out == 0L) return(df_text)
  
  mat_norm <- t(vapply(norm_list, function(v) { length(v) <- max_len_out; v },
                       FUN.VALUE = character(max_len_out)))
  mat_lem  <- t(vapply(lemma_list, function(v) { length(v) <- max_len_out; v },
                       FUN.VALUE = character(max_len_out)))
  
  mat_norm[!nzchar(mat_norm)] <- NA_character_
  mat_lem[!nzchar(mat_lem)]   <- NA_character_
  
  same <- !is.na(mat_norm) & !is.na(mat_lem) & (mat_norm == mat_lem)
  mat_lem[same] <- NA_character_
  
  mat_lem <- t(apply(mat_lem, 1, function(row) {
    seen <- character(0)
    out <- row
    for (k in seq_along(out)) {
      v <- out[k]
      if (is.na(v)) next
      if (v %in% seen) out[k] <- NA_character_ else seen <- c(seen, v)
    }
    out
  }))
  
  df_norm <- as.data.frame(mat_norm, stringsAsFactors = FALSE)
  df_lem  <- as.data.frame(mat_lem,  stringsAsFactors = FALSE)
  
  names(df_norm) <- paste0("descriptor_", seq_len(max_len_out))
  names(df_lem)  <- paste0("descriptor_", seq_len(max_len_out), lemma_suffix)
  
  cbind(df_text, df_norm, df_lem)
}

###############################################
# F) Caso original: lista separada por comas
###############################################

sd_comma_descriptors_df <- function(v,
                                    prefix = "descriptor_",
                                    remove_diacritics = TRUE,
                                    keep_enye = FALSE,
                                    lemmatize = c("none", "lemma", "both"),
                                    udpipe_model = NULL,
                                    lemma_suffix = "_lemma") {
  lemmatize <- match.arg(lemmatize)
  v <- as.character(v)
  
  split_one <- function(s) {
    if (is.na(s) || !nzchar(s)) return(character(0))
    parts <- strsplit(s, ",", fixed = TRUE)[[1]]
    parts <- trimws(parts)
    parts <- parts[nzchar(parts)]
    tolower(parts)
  }
  
  parts_list <- lapply(v, split_one)
  norm_list <- lapply(parts_list, sd_finalize_norm,
                      remove_diacritics = remove_diacritics, keep_enye = keep_enye)
  
  if (lemmatize == "none") {
    df <- sd_make_wide(norm_list, prefix)
    return(df)
  }
  
  if (lemmatize %in% c("lemma", "both")) {
    if (!requireNamespace("udpipe", quietly = TRUE)) {
      stop("Instala 'udpipe' para lematizar: install.packages('udpipe').")
    }
    if (is.null(udpipe_model)) {
      stop("Si lemmatize='lemma' o 'both' debes proporcionar 'udpipe_model'.")
    }
    if (is.character(udpipe_model)) udpipe_model <- udpipe::udpipe_load_model(udpipe_model)
    
    lens <- lengths(parts_list)
    total <- sum(lens)
    flat <- if (total > 0L) unlist(parts_list, use.names = FALSE) else character(0)
    
    flat <- iconv(flat, from = "", to = "UTF-8")
    doc_ids <- paste0("doc", seq_along(flat))
    
    if (length(flat) > 0L) {
      anno <- udpipe::udpipe_annotate(udpipe_model, x = flat, doc_id = doc_ids)
      dfanno <- as.data.frame(anno)
      
      lem <- dfanno$lemma
      bad <- is.na(lem) | !nzchar(lem)
      lem[bad] <- dfanno$token[bad]
      lem <- tolower(lem)
      
      flat_lem <- lem
      
      idx <- rep.int(seq_along(parts_list), lens)
      tmp <- split(flat_lem, idx)
      lemma_list <- vector("list", length(parts_list))
      lemma_list[as.integer(names(tmp))] <- tmp
      for (i in seq_along(lemma_list)) if (is.null(lemma_list[[i]])) lemma_list[[i]] <- character(0)
      
      lemma_list <- lapply(lemma_list, sd_finalize_norm,
                           remove_diacritics = remove_diacritics, keep_enye = keep_enye)
    } else {
      lemma_list <- replicate(length(parts_list), character(0), simplify = FALSE)
    }
  }
  
  if (lemmatize == "lemma") {
    df <- sd_make_wide(lemma_list, prefix)
    return(df)
  }
  
  max_len_out <- max(max(lengths(norm_list), 0L), max(lengths(lemma_list), 0L), 0L)
  if (max_len_out == 0L) return(as.data.frame(matrix(nrow = length(v), ncol = 0)))
  
  mat_norm <- t(vapply(norm_list, function(p) { length(p) <- max_len_out; p },
                       FUN.VALUE = character(max_len_out)))
  mat_lem  <- t(vapply(lemma_list, function(p) { length(p) <- max_len_out; p },
                       FUN.VALUE = character(max_len_out)))
  
  mat_norm[!nzchar(mat_norm)] <- NA_character_
  mat_lem[!nzchar(mat_lem)]   <- NA_character_
  
  same <- !is.na(mat_norm) & !is.na(mat_lem) & (mat_norm == mat_lem)
  mat_lem[same] <- NA_character_
  
  mat_lem <- t(apply(mat_lem, 1, function(row) {
    seen <- character(0)
    out <- row
    for (k in seq_along(out)) {
      v <- out[k]
      if (is.na(v)) next
      if (v %in% seen) out[k] <- NA_character_ else seen <- c(seen, v)
    }
    out
  }))
  
  out_norm <- as.data.frame(mat_norm, stringsAsFactors = FALSE)
  out_lem  <- as.data.frame(mat_lem,  stringsAsFactors = FALSE)
  
  names(out_norm) <- paste0(prefix, seq_len(max_len_out))
  names(out_lem)  <- paste0(prefix, seq_len(max_len_out), lemma_suffix)
  
  cbind(out_norm, out_lem)
}

###############################################
# G) Función final de uso: unifica "comma" vs "text"
###############################################

#' Prepare candidate descriptors for SADCAT lookup
#'
#' High-level wrapper that extracts and normalizes candidate descriptors either from:
#' - comma-separated descriptor strings (`input_format = "comma"`), or
#' - narrative text (`input_format = "text"`) as unigrams plus optional bigrams/trigrams.
#'
#' The output is always a **wide** data frame with columns starting with `prefix` (by default
#' `descriptor_`), ready to be consumed by `dict_coverage()`, `dict_dim_coverage_all()`,
#' and `dict_dim_dirmean_all()`.
#'
#' @section Normalization:
#' The pipeline mirrors the format used by the AGRUPA/SADCAT lookup tables:
#' - lowercase,
#' - remove *all* whitespace (including internal spaces),
#' - optionally strip diacritics (accent removal),
#' - optionally keep `"ñ"` untouched.
#'
#' @section Lemmatization:
#' If `lemmatize` is `"lemma"` or `"both"`, UDPipe is used. For narrative text, lemmatization
#' is run on the full document to preserve context (UDPipe annotation), then tokens/lemmas are
#' normalized. With `lemmatize = "both"`, lemma columns are included only when they add
#' information (lemmas identical to surface forms are set to `NA`) and deduplicated within-row.
#'
#' @section N-grams:
#' For `input_format = "text"`, bigrams and trigrams are generated *within punctuation-delimited segments*,
#' preventing n-grams from crossing punctuation boundaries. N-grams are normalized by removing whitespace,
#' so `"alta calidad"` becomes `"altacalidad"`.
#'
#' @param x Input object (string, vector, or data frame). See `input_type`.
#' @param input_type `"string"`, `"vector"`, or `"data"`.
#' @param input_format `"comma"` or `"text"`.
#' @param desc_col For `input_type = "data"`, the name of the column containing descriptors/text.
#' @param prefix Prefix for descriptor columns (default `"descriptor_"`).
#' @param drop_desc_col Logical. For `input_type = "data"`, drop `desc_col` from the output.
#' @param remove_diacritics Logical. Remove diacritics using **stringi**.
#' @param keep_enye Logical. Preserve `"ñ"` when stripping other diacritics.
#' @param lemmatize `"none"`, `"lemma"`, or `"both"`.
#' @param udpipe_model An `udpipe_model` object or a path to a `.udpipe` file (required if lemmatizing).
#' @param lemma_suffix Suffix for lemma columns when `lemmatize = "both"` (default `"_lemma"`).
#' @param include_ngrams Logical. For `input_format = "text"`, include bigrams and trigrams.
#' @param remove_stopwords Logical. For `input_format = "text"`, remove stopwords for unigrams.
#' @param stopwords_lang `"es"` or `"en"`. Language used for stopword lists (stopwords-iso).
#' @param stopwords_custom Optional character vector of custom stopwords.
#' @param max_ngrams Maximum number of bigrams and trigrams per row (default `Inf`).
#' @param text_col Name of the column that holds the original text in the output (default `"text"`).
#'
#' @return A data frame with one row per input row. Descriptor columns are in wide format.
#'
#' @examples
#' # Comma-separated descriptors:
#' prepare_descriptors("elegante, solemne", input_type = "string", input_format = "comma")
#'
#' # Narrative text:
#' prepare_descriptors("Una obra magistral y compleja.", input_type = "string", input_format = "text")
#'
#' @export
prepare_descriptors <- function(x,
                                input_type = c("string", "vector", "data"),
                                input_format = c("comma", "text"),
                                desc_col = NULL,
                                prefix = "descriptor_",
                                drop_desc_col = TRUE,
                                remove_diacritics = TRUE,
                                keep_enye = FALSE,
                                lemmatize = c("none", "lemma", "both"),
                                udpipe_model = NULL,
                                lemma_suffix = "_lemma",
                                # extras para texto
                                include_ngrams = TRUE,
                                remove_stopwords = TRUE,
                                stopwords_lang = c("es", "en"),
                                stopwords_custom = NULL,
                                max_ngrams = Inf,
                                text_col = "text") {
  input_type <- match.arg(input_type)
  input_format <- match.arg(input_format)
  lemmatize <- match.arg(lemmatize)
  stopwords_lang <- match.arg(stopwords_lang)
  
  process_vec <- function(v) {
    if (input_format == "comma") {
      return(sd_comma_descriptors_df(v,
                                     prefix = prefix,
                                     remove_diacritics = remove_diacritics,
                                     keep_enye = keep_enye,
                                     lemmatize = lemmatize,
                                     udpipe_model = udpipe_model,
                                     lemma_suffix = lemma_suffix))
    }
    
    # input_format == "text"
    df_uni <- sd_text_unigrams_df(v,
                                  remove_stopwords = remove_stopwords,
                                  stopwords_lang = stopwords_lang,
                                  stopwords_custom = stopwords_custom,
                                  remove_diacritics = remove_diacritics,
                                  keep_enye = keep_enye,
                                  dedupe_within_row = TRUE,
                                  lemmatize = lemmatize,
                                  udpipe_model = udpipe_model,
                                  lemma_suffix = lemma_suffix,
                                  text_col = text_col)
    
    if (!include_ngrams) return(df_uni)
    
    df_ng <- sd_text_ngrams_df(v,
                               remove_diacritics = remove_diacritics,
                               keep_enye = keep_enye,
                               keep_numbers = FALSE,
                               dedupe_within_row = TRUE,
                               max_ngrams = max_ngrams,
                               text_col = text_col)
    
    # Evita duplicar text_col
    df_ng[[text_col]] <- NULL
    cbind(df_uni, df_ng)
  }
  
  if (input_type == "string") {
    if (!is.character(x) || length(x) != 1L) {
      stop("Con input_type='string', x debe ser un único string (character de longitud 1).")
    }
    return(process_vec(x))
  }
  
  if (input_type == "vector") {
    return(process_vec(x))
  }
  
  # input_type == "data"
  if (!is.data.frame(x)) stop("Con input_type='data', x debe ser un data.frame o tibble.")
  if (is.null(desc_col) || !is.character(desc_col) || length(desc_col) != 1L) {
    stop("Con input_type='data', desc_col debe ser el nombre (character) de la columna de descripciones/texto.")
  }
  if (!desc_col %in% names(x)) stop(sprintf("La columna '%s' no existe en el dataset.", desc_col))
  
  add_df <- process_vec(x[[desc_col]])
  
  out <- x
  if (drop_desc_col) out[[desc_col]] <- NULL
  cbind(out, add_df)
}
