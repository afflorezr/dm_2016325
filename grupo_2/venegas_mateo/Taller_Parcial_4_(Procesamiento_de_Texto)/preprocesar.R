# ==============================================================================
# preprocesar.R
# Taller Parcial 4 - Sistema de Recuperación de Información
# Autor: Mateo Venegas Clavijo
# Descripción: Construcción del corpus, representaciones vectoriales (TF-IDF y LSA)
#              y reducción dimensional. Genera objetos .rds listos para la app Shiny.
# ==============================================================================

library(DBI)
library(RSQLite)
library(dplyr)
library(stringr)
library(tidytext)
library(Matrix)
library(irlba)

# ==============================================================================
# 1. PARÁMETROS GLOBALES
# ==============================================================================
DB_PATH   <- "revista_q1_2025.sqlite"   # Base de datos SQLite
K_LSA     <- 100                        # Componentes LSA (Truncated SVD)
MIN_DF    <- 2                          # Mínimo nº de documentos que deben contener el término
MAX_DF    <- 0.80                       # Máximo df_relativo del término (excluir los muy comunes)
MIN_CHARS <- 3                          # Longitud mínima del token

cat("=== TALLER 4 - Preprocesamiento del Corpus ===\n\n")

# ==============================================================================
# 2. CARGA DEL CORPUS DESDE SQLITE
# ==============================================================================
cat("[1/7] Cargando artículos desde la base de datos...\n")

con <- dbConnect(RSQLite::SQLite(), DB_PATH)

papers_raw <- dbGetQuery(con, "
  SELECT p.paper_id,
         p.title,
         p.abstract,
         p.publication_date,
         p.year,
         p.doi,
         p.url,
         p.topic_label,
         p.citations,
         COALESCE(
           (SELECT GROUP_CONCAT(a.author_name, ', ')
            FROM paper_authors pa
            JOIN authors a ON pa.author_id = a.author_id
            WHERE pa.paper_id = p.paper_id
            ORDER BY pa.author_order),
           'Sin autores'
         ) AS authors_raw
  FROM papers p
  ORDER BY p.paper_id
")

dbDisconnect(con)

n_total <- nrow(papers_raw)
cat(sprintf("   → %d artículos cargados.\n", n_total))

# --- Diagnóstico de faltantes ------------------------------------------------
n_sin_titulo    <- sum(is.na(papers_raw$title) | papers_raw$title == "")
n_sin_abstract  <- sum(is.na(papers_raw$abstract) | papers_raw$abstract == "")
n_sin_doi       <- sum(is.na(papers_raw$doi) | papers_raw$doi == "")

cat(sprintf("   → Sin título:    %d (%.1f%%)\n", n_sin_titulo,   100*n_sin_titulo/n_total))
cat(sprintf("   → Sin resumen:   %d (%.1f%%)\n", n_sin_abstract, 100*n_sin_abstract/n_total))
cat(sprintf("   → Sin DOI:       %d (%.1f%%)\n", n_sin_doi,      100*n_sin_doi/n_total))

# ==============================================================================
# 3. CONSTRUCCIÓN DEL CORPUS TEXTUAL
# ==============================================================================
# Estrategia: concatenar título (x2 para darle más peso) + abstract.
# Los artículos sin resumen usan únicamente el título duplicado.
# No se inventa contenido: los valores faltantes se reemplazan por cadena vacía.
cat("\n[2/7] Construyendo texto del corpus...\n")

corpus_df <- papers_raw |>
  mutate(
    title_clean    = coalesce(str_squish(title),    ""),
    abstract_clean = coalesce(str_squish(abstract), ""),
    # Título repetido dos veces para amplificar su peso en la representación
    doc_text = paste(title_clean, title_clean, abstract_clean, sep = " "),
    doc_text = str_squish(doc_text)
  ) |>
  filter(nchar(doc_text) > 20)  # excluir docs prácticamente vacíos

n_corpus <- nrow(corpus_df)
cat(sprintf("   → %d artículos incluidos en el corpus (excluidos: %d con texto vacío).\n",
            n_corpus, n_total - n_corpus))

# ==============================================================================
# 4. PREPROCESAMIENTO DEL TEXTO
# ==============================================================================
cat("\n[3/7] Preprocesando texto (tokenización + stopwords)...\n")

# Stopwords en inglés (SMART + Snowball) + algunas en español que puedan colarse
data("stop_words", package = "tidytext")
extra_stops <- tibble(
  word    = c("al","de","la","en","que","se","el","del","los","las","un","una",
              "es","su","con","por","para","este","esta","estos","estas","como",
              "article","paper","results","result","method","methods","approach",
              "using","used","propose","proposed","show","shows","shown",
              "based","also","may","can","two","one","three","new","different"),
  lexicon = "custom"
)
all_stops <- bind_rows(stop_words, extra_stops)

tokens <- corpus_df |>
  select(paper_id, doc_text) |>
  unnest_tokens(word, doc_text) |>
  # Eliminar tokens numéricos puros y demasiado cortos
  filter(!str_detect(word, "^[0-9]+$"), nchar(word) >= MIN_CHARS) |>
  # Eliminar stopwords
  anti_join(all_stops, by = "word") |>
  # Conteo de ocurrencias
  count(paper_id, word, name = "n")

n_vocab_raw <- n_distinct(tokens$word)
cat(sprintf("   → Vocabulario antes de filtrar: %d términos únicos.\n", n_vocab_raw))

# --- Filtrado por document frequency ----------------------------------------
n_docs <- n_corpus
df_counts <- tokens |>
  group_by(word) |>
  summarise(df = n(), .groups = "drop")

vocab_filtrado <- df_counts |>
  filter(df >= MIN_DF, df / n_docs <= MAX_DF)

tokens <- tokens |>
  semi_join(vocab_filtrado, by = "word")

n_vocab_final <- n_distinct(tokens$word)
cat(sprintf("   → Vocabulario después de filtrar (df∈[%d, %.0f%%]): %d términos.\n",
            MIN_DF, 100*MAX_DF, n_vocab_final))

# ==============================================================================
# 5. REPRESENTACIÓN TF-IDF
# ==============================================================================
cat("\n[4/7] Calculando TF-IDF...\n")

tokens_tfidf <- tokens |>
  bind_tf_idf(word, paper_id, n)

# Tabla de vocabulario con IDF (para proyectar consultas en tiempo real)
vocab_idf <- tokens_tfidf |>
  group_by(word) |>
  summarise(idf = first(idf), .groups = "drop")

cat(sprintf("   → Vocabulario TF-IDF: %d términos.\n", nrow(vocab_idf)))

# Matriz dispersa: documentos × términos
tfidf_sparse <- tokens_tfidf |>
  cast_sparse(paper_id, word, tf_idf)

dim_orig <- dim(tfidf_sparse)
sparsidad <- 1 - nnzero(tfidf_sparse) / prod(dim_orig)
cat(sprintf("   → Matriz TF-IDF: %d docs × %d términos (dispersión: %.1f%%).\n",
            dim_orig[1], dim_orig[2], 100*sparsidad))

# Normalización L2 por fila (para similitud coseno)
normalize_rows_sparse <- function(m) {
  norms <- sqrt(Matrix::rowSums(m^2))
  norms[norms == 0] <- 1
  result <- Diagonal(x = 1/norms) %*% m
  rownames(result) <- rownames(m)  # preservar rownames
  colnames(result) <- colnames(m)
  result
}

tfidf_norm <- normalize_rows_sparse(tfidf_sparse)

# ==============================================================================
# 6. REDUCCIÓN DIMENSIONAL: LSA (Truncated SVD via irlba)
# ==============================================================================
cat("\n[5/7] Aplicando Truncated SVD (LSA)...\n")

set.seed(7391)
k_use <- min(K_LSA, min(dim_orig) - 1)  # k no puede exceder min(nrow, ncol) - 1
svd_result <- irlba(tfidf_sparse, nv = k_use, maxit = 1000)

# Varianza explicada acumulada
varianza_total <- sum(svd_result$d^2)
var_acum <- cumsum(svd_result$d^2) / varianza_total
k_80 <- which(var_acum >= 0.80)[1]
k_90 <- which(var_acum >= 0.90)[1]

cat(sprintf("   → Dimensión original:  %d términos\n", dim_orig[2]))
cat(sprintf("   → Dimensión reducida:  %d componentes (k=%d)\n", k_use, k_use))
cat(sprintf("   → Varianza explicada (k=%d): %.1f%%\n", k_80, 100*var_acum[k_80]))
cat(sprintf("   → Varianza explicada (k=%d): %.1f%%\n", k_90, 100*var_acum[k_90]))
cat(sprintf("   → Varianza explicada (k=%d): %.1f%%\n", k_use, 100*var_acum[k_use]))

# Coordenadas de documentos en espacio LSA: U * diag(d)
lsa_docs <- svd_result$u %*% diag(svd_result$d)
rownames(lsa_docs) <- rownames(tfidf_sparse)

# Normalización L2 por fila para similitud coseno en espacio LSA
normalize_rows_dense <- function(m) {
  norms <- sqrt(rowSums(m^2))
  norms[norms == 0] <- 1
  sweep(m, 1, norms, "/")
}

lsa_docs_norm <- normalize_rows_dense(lsa_docs)

# ==============================================================================
# 7. GUARDAR OBJETOS PRECOMPUTADOS
# ==============================================================================
cat("\n[6/7] Guardando objetos precomputados (.rds)...\n")

# corpus_info.rds: datos de cada artículo (para mostrar resultados)
corpus_info <- corpus_df |>
  select(paper_id, title, authors_raw, publication_date, year, doi, url,
         topic_label, citations, abstract)

saveRDS(corpus_info, "corpus_info.rds")
cat("   → corpus_info.rds\n")

# vocab_idf.rds: vocabulario con valores IDF (para transformar consultas)
saveRDS(vocab_idf, "vocab_idf.rds")
cat("   → vocab_idf.rds\n")

# tfidf_obj.rds: matriz TF-IDF normalizada + metadatos
tfidf_obj <- list(
  matrix   = tfidf_norm,
  doc_ids  = rownames(tfidf_norm),
  vocab    = colnames(tfidf_norm),
  dim_orig = dim_orig
)
saveRDS(tfidf_obj, "tfidf_obj.rds")
cat("   → tfidf_obj.rds\n")

# lsa_obj.rds: SVD + doc matrix normalizada + metadatos
lsa_obj <- list(
  V         = svd_result$v,           # Matriz de carga de términos (términos x k)
  d         = svd_result$d,           # Valores singulares
  k         = k_use,
  vocab     = colnames(tfidf_sparse), # Vocabulario (para alinear consulta)
  docs_norm = lsa_docs_norm,          # Documentos normalizados en espacio LSA (docs x k)
  doc_ids   = rownames(tfidf_sparse),
  dim_orig  = dim_orig[2],
  var_acum  = var_acum
)
saveRDS(lsa_obj, "lsa_obj.rds")
cat("   → lsa_obj.rds\n")

cat("\n[7/7] ¡Preprocesamiento completado!\n")
cat("   Los objetos están listos para ser cargados por app.R\n\n")

cat("=== RESUMEN ===\n")
cat(sprintf("  Artículos en corpus:  %d\n",  n_corpus))
cat(sprintf("  Vocabulario final:    %d términos\n",  n_vocab_final))
cat(sprintf("  Dim. original TF-IDF: %d × %d\n", dim_orig[1], dim_orig[2]))
cat(sprintf("  Dim. reducida LSA:    %d × %d\n", n_corpus, k_use))
cat(sprintf("  Varianza exp. (k=%d): %.1f%%\n", k_use, 100*var_acum[k_use]))
