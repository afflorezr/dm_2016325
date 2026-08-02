# Taller Parcial 4 — JAIR Dashboard + Sistema de Recuperación de Información

**Autor:** Mateo Venegas Clavijo — CC. 1075878496  
**Curso:** Minería de Datos (2016325) — Universidad Nacional de Colombia, 2026

## Descripción

Extensión del tablero Shiny del Taller 2 con un **sistema de búsqueda semántica de artículos científicos** del *Journal of Artificial Intelligence Research (JAIR)*.

El nuevo módulo **"Buscador IR"** permite escribir una consulta en lenguaje natural y obtener artículos ordenados por relevancia usando dos estrategias:

| Estrategia | Tipo | Reducción dimensional |
|---|---|---|
| **TF-IDF + Similitud Coseno** | Léxica | No |
| **LSA + Similitud Coseno** | Semántica | Sí (Truncated SVD, k=100) |

## App Desplegada

**https://mateovecla12.shinyapps.io/JAIR-Dashboard-T4/**

## Estructura de Archivos

```
Taller_Parcial_4_(Procesamiento_de_Texto)/
├── app.R                     # Aplicación Shiny principal (UI + Server)
├── buscar.R                  # Módulo de recuperación de información (IR)
├── preprocesar.R             # Script de preprocesamiento (genera .rds)
├── Taller_4.Rmd              # Documento reproducible completo
├── revista_q1_2025.sqlite    # Base de datos SQLite (234 artículos JAIR)
│
├── corpus_info.rds           # Metadatos del corpus (generado por preprocesar.R)
├── vocab_idf.rds             # Vocabulario con valores IDF
├── tfidf_obj.rds             # Matriz TF-IDF normalizada + metadatos
├── lsa_obj.rds               # Modelo LSA (SVD truncado k=100) + docs normalizados
│
├── rsconnect/                # Configuración de despliegue en shinyapps.io
├── Clases/                   # Material de clase (clases 20-24)
└── README.md                 # Este archivo
```

## Requisitos

### Paquetes de R

```r
install.packages(c(
  "shiny", "bslib", "DBI", "RSQLite",
  "ggplot2", "dplyr", "tidyr", "DT",
  "rvest", "httr", "jsonlite", "stringr", "plotly", "scales",
  "tidytext", "Matrix", "irlba"
))
```

### Generación de objetos precomputados

Si los archivos `.rds` no existen (primera ejecución):

```r
setwd("ruta/a/Taller_Parcial_4_(Procesamiento_de_Texto)/")
source("preprocesar.R")
```

Esto genera `corpus_info.rds`, `vocab_idf.rds`, `tfidf_obj.rds` y `lsa_obj.rds`.

## Ejecución Local

```r
shiny::runApp("grupo_2/venegas_mateo/Taller_Parcial_4_(Procesamiento_de_Texto)/")
```

O desde RStudio: abrir `app.R` y presionar **Run App**.

## Funcionalidades del Tablero

| Pestaña | Descripción |
|---------|-------------|
| **Resumen General** | Value boxes, distribución por tema, histograma de citas, top 10 más citados. |
| **Autores** | Ranking de autores, coautorías, directorio completo. |
| **Explorador** | Búsqueda por filtros SQL (título, tema, autor, DOI, fechas, citas). |
| **🆕 Buscador IR** | Búsqueda semántica con TF-IDF o LSA. Comparación lado a lado. |
| **Actualizar Datos** | Web Scraping de nuevos volúmenes JAIR. |
| **Acerca de** | Información del proyecto y modelos. |

## Descripción del Sistema IR

### Corpus
- **234 artículos** de JAIR (volúmenes 82-86, 2025)
- Campos: título (×2 para mayor peso) + resumen
- Preprocesamiento: minúsculas, stopwords inglés/español, mínimo 3 caracteres, df ∈ [2, 80%]

### TF-IDF
- Vocabulario: **2,226 términos**
- Matriz: 234 × 2,226 (dispersa, 97.4% esparsidad)
- Similitud coseno con vectores L2-normalizados

### LSA (Latent Semantic Analysis)
- Método: Truncated SVD via `irlba`
- Dimensión reducida: **100 componentes**
- Varianza explicada: k=69 → 80.4%, k=84 → 90.5%
- Proyección de consultas: q_LSA = q_TF-IDF × V_k

## Notas Técnicas

- Los objetos `.rds` se calculan **una sola vez** con `preprocesar.R` y se cargan al inicio de la app.
- La app no reconstruye la matriz vectorial ni el modelo LSA en cada búsqueda.
- Tiempo de respuesta por consulta: < 15 ms (corpus de 234 docs).
- Memoria total de modelos precomputados: < 5 MB.
- No se usan APIs de pago ni modelos neuronales de gran tamaño.
