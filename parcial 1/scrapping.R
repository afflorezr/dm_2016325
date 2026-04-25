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
# Ahora se recorrera la información de todos los volumenes para extraer las urls de los articulos y
# evetualmente ir a esas urls para extraer la información disponible de los artículos

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

