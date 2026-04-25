# ==================================================================================================
#               EXTRACCIÓN DE INFORMACIÓN - JOURNAL OF STATISTICAL SOFTWARE (JSS)
# ==================================================================================================
# El Journal of Statistical Software es una revista de acceso abierto de gran importancia
# en la comunidad estadística y de ciencia de datos. Publica artículos sobre software,
# algoritmos y métodos computacionales ampliamente utilizados en estadística aplicada.
#
# La cantidad de citaciones por artículo y la referencias usadas en el artículo no pueden accederse
# directamente desde la página del JSS por lo que se harán consultas a otras herramientas externas. 
# 
# Autor           : Michel Mendivenson Barragán Zabala
# Materia         : Minería de Datos
# ==================================================================================================

paquetes <- c("rvest", "xml2", "dplyr", "stringr",
              "purrr", "janitor", "readr", "knitr")
invisible(lapply(paquetes, library, character.only = TRUE))

# ------------------------- PASO 1: RECOLECCIÓN INFORMACIÓN VOLÚMENES ------------------------------
# Cada uno de los volumenes son guardados en una nueva instancia por lo que si se conoce la url 
# de cada uno de los volumenes se puede hacer scrapping de los artículos para cada uno de los
# volúmenes
# 
# Toda la información recolectada del archive de JSS (https://www.jstatsoft.org/issue/archive) se\
# guardará en la tabla vol con los siguientes campos:
#   - id = Número del volumen
#   - order = Orden de publicación en el año
#   - year = Año de publicación
#   - url = URL en que se aloja ese volumen

cat("\n\n* Retrieving volumes info: PROCESSING ⌛\r")

archive <- read_html('https://www.jstatsoft.org/issue/archive')

vol <- data.frame(
  id = archive |> 
           html_elements(".media-heading .title") |> 
           html_text2(), 
  url    = archive |> 
           html_elements(".media-heading .title") |>
           html_attr('href')
)

vol$year <- str_remove(archive |> 
                       html_elements(".media-heading") |> 
                       html_text2(),
                       paste0(vol$id, collapse = '|', end = ' , ')) |>
            as.numeric()

vol$id <- vol$id |> str_remove('Volume ') |> as.numeric()

vol <- vol |> 
  dplyr::arrange(year, id)

vol$order <- sequence(table(vol$year))

vol <- vol[, c("id","order","year","url")]

cat("* Retrieving volumes info: CHECK ✅     \n\n")

# ------------------------- PASO 2: RECOLECCIÓN SISTEMÁTICA DE ARTÍCULOS ---------------------------
# Ahora se recorreran las URLs de todos los volumenes para extraer las urls de los articulos (Así como
# sus títulos) y de forma sistemática recorrer las urls de esos artículos para extraer la información
# necesaria de los artículos. 

# URLS DE LOS ARTÍCULOS
total.vol <- nrow(vol)
articles <- data.frame()
count <- 1
for (id in vol$id){
  volume = vol$url[vol$id == id]
  cat(sprintf('* Retrieving articles URLs: (%d/%d) Vol. ⌛\r', count, total.vol))
  articles <- rbind(
    articles,
    cbind(vol.id = id,
          title = read_html(volume) |>
            html_elements('.media-heading a') |>
            html_text2() |>  str_trim(),

          issue = read_html(volume) |>
            html_elements('.col-sm-3') |>
            html_text2() |>
            str_trim() |>
            (\(x) sub(".*,\\s*", "", x))() |>
            str_remove('Issue '),

          url = read_html(volume) |>
            html_elements('.media-heading a') |>
            html_attr('href'))
  )
  Sys.sleep(1)
  count <- count + 1
}
cat('* Retrieving articles URLs: CHECK ✅              \n\n')


# INFORMACIÓN DE LOS ARTÍCULOS
art.info <- data.frame()
total.art <- nrow(articles)
count <- 1
for (url in articles$url){
  cat(sprintf('* Retrieving articles info: (%d/%d) ⌛\r', count, total.art))
  
  article <- read_html(url)
  
  art.info <- rbind(
    art.info,
    cbind(url = url, 
          abstract = article|> 
            html_element('.article-abstract') |> 
            html_text2(), 
          date = article|> 
            html_elements(".col-sm-8") |> 
            html_text2() |> 
            (\(x) (x[c(2)]))(),
          DOI = article|> 
            html_elements(".col-sm-8") |> 
            html_text2() |> 
            (\(x) (x[c(3)]))(),
          authors = article |> 
            html_elements('.authors a') |> 
            html_attr('href') |> 
            (\(x) ifelse(length(x) == 0, '', paste0(x, collapse=' | ')))())
  )
  count <- count + 1
}

cat('* Retrieving articles info: CHECK ✅              \n\n')

# Autores
# pagina |> 
#   html_elements('.authors strong') |> 
#   html_text2()
