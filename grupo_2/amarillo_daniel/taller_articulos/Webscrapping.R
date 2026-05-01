paquetes <- c(
  "rvest", "xml2", "dplyr", "stringr",
  "purrr", "janitor", "readr", "knitr","chromote"
)
# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)
  
url <- "https://psycnet.apa.org/PsycARTICLES/journal/bul/152/1"

# Leemos el HTML de la página
pagina_quotes <- read_html_live(url)
doi <- pagina_quotes %>%
      html_element("a") 
