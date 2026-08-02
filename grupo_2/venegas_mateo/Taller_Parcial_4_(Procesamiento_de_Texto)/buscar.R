# ==============================================================================
# buscar.R
# Módulo de recuperación de información para el Taller Parcial 4
# Autor: Mateo Venegas Clavijo
# Descripción: Funciones de búsqueda por similitud coseno usando TF-IDF y LSA.
#              Se asume que los objetos precomputados ya están cargados en memoria.
# ==============================================================================

library(tidytext)
library(dplyr)
library(stringr)
library(Matrix)

# Stopwords consistentes con preprocesar.R
data("stop_words", package = "tidytext")
IR_STOPS <- bind_rows(
  stop_words,
  tibble(
    word    = c("al","de","la","en","que","se","el","del","los","las","un","una",
                "es","su","con","por","para","este","esta","estos","estas","como",
                "article","paper","results","result","method","methods","approach",
                "using","used","propose","proposed","show","shows","shown",
                "based","also","may","can","two","one","three","new","different"),
    lexicon = "custom"
  )
)

# ==============================================================================
# Función: vectorizar_query
# Convierte una consulta en texto a un vector TF-IDF en el vocabulario del corpus.
# Devuelve un vector numérico nombrado (nombre = término).
# ==============================================================================
vectorizar_query <- function(query_text, vocab_idf_df) {
  query_tokens <- tibble(text = tolower(query_text)) |>
    unnest_tokens(word, text) |>
    filter(!str_detect(word, "^[0-9]+$"), nchar(word) >= 3) |>
    anti_join(IR_STOPS, by = "word") |>
    count(word, name = "n") |>
    mutate(tf = n / sum(n)) |>
    left_join(vocab_idf_df, by = "word") |>
    filter(!is.na(idf)) |>
    mutate(tf_idf = tf * idf)

  if (nrow(query_tokens) == 0) return(NULL)
  query_tokens
}

# ==============================================================================
# Función: buscar_tfidf
# Recupera artículos usando TF-IDF + similitud coseno (recuperación léxica).
# ==============================================================================
buscar_tfidf <- function(query_text, tfidf_obj, vocab_idf_df, corpus_info_df,
                          n_results = 10) {
  qt <- vectorizar_query(query_text, vocab_idf_df)
  if (is.null(qt)) return(NULL)

  # Construir vector consulta en el espacio vocabulario
  vocab <- tfidf_obj$vocab
  q_vec <- numeric(length(vocab))
  names(q_vec) <- vocab

  matched <- intersect(qt$word, vocab)
  if (length(matched) == 0) return(NULL)

  for (w in matched) {
    q_vec[w] <- qt$tf_idf[qt$word == w]
  }

  # Normalización L2
  q_norm <- sqrt(sum(q_vec^2))
  if (q_norm == 0) return(NULL)
  q_vec <- q_vec / q_norm

  # Similitud coseno: docs (normalizadas) × query
  sims <- as.vector(tfidf_obj$matrix %*% q_vec)
  names(sims) <- tfidf_obj$doc_ids

  # Ranking
  sims_sorted <- sort(sims, decreasing = TRUE)[seq_len(min(n_results, length(sims)))]
  result_ids  <- as.integer(names(sims_sorted))

  data.frame(paper_id = result_ids, score = round(as.numeric(sims_sorted), 5),
             stringsAsFactors = FALSE) |>
    left_join(corpus_info_df, by = "paper_id") |>
    mutate(
      rank             = row_number(),
      abstract_frag    = str_trunc(coalesce(abstract, "Sin resumen"), 300),
      publication_date = coalesce(publication_date, "—"),
      doi_link         = ifelse(!is.na(doi) & doi != "",
                                paste0("https://doi.org/", doi), url)
    ) |>
    select(rank, title, authors_raw, publication_date, topic_label,
           doi_link, score, abstract_frag)
}

# ==============================================================================
# Función: buscar_lsa
# Recupera artículos usando LSA (TF-IDF + Truncated SVD) + similitud coseno.
# Recuperación semántica con reducción dimensional.
# ==============================================================================
buscar_lsa <- function(query_text, lsa_obj, vocab_idf_df, corpus_info_df,
                        n_results = 10) {
  qt <- vectorizar_query(query_text, vocab_idf_df)
  if (is.null(qt)) return(NULL)

  # Construir vector consulta en espacio vocabulario (igual que TF-IDF)
  vocab <- lsa_obj$vocab
  q_vec <- numeric(length(vocab))
  names(q_vec) <- vocab

  matched <- intersect(qt$word, vocab)
  if (length(matched) == 0) return(NULL)

  for (w in matched) {
    q_vec[w] <- qt$tf_idf[qt$word == w]
  }

  # Proyectar la consulta al espacio latente LSA: q_lsa = q_vec %*% V
  # V tiene dimensión (términos × k), q_vec tiene (1 × términos)
  q_lsa <- as.vector(q_vec %*% lsa_obj$V)  # resultado: vector de longitud k

  # Normalización L2
  q_norm <- sqrt(sum(q_lsa^2))
  if (q_norm == 0) return(NULL)
  q_lsa_norm <- q_lsa / q_norm

  # Similitud coseno en espacio LSA: docs_norm × query_norm
  sims <- as.vector(lsa_obj$docs_norm %*% q_lsa_norm)
  names(sims) <- lsa_obj$doc_ids

  # Ranking
  sims_sorted <- sort(sims, decreasing = TRUE)[seq_len(min(n_results, length(sims)))]
  result_ids  <- as.integer(names(sims_sorted))

  data.frame(paper_id = result_ids, score = round(as.numeric(sims_sorted), 5),
             stringsAsFactors = FALSE) |>
    left_join(corpus_info_df, by = "paper_id") |>
    mutate(
      rank             = row_number(),
      abstract_frag    = str_trunc(coalesce(abstract, "Sin resumen"), 300),
      publication_date = coalesce(publication_date, "—"),
      doi_link         = ifelse(!is.na(doi) & doi != "",
                                paste0("https://doi.org/", doi), url)
    ) |>
    select(rank, title, authors_raw, publication_date, topic_label,
           doi_link, score, abstract_frag)
}
