paquetes <- c(
  "rvest", "xml2", "httr2", "dplyr", "stringr",
  "purrr", "tibble", "janitor", "readr", "knitr"
)

# Verificamos qué paquetes faltan
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)

if (length(pendientes) > 0) {
  install.packages(pendientes)
}

# Cargamos los paquetes sin mostrar mensajes
lapply(paquetes, library, character.only = TRUE)
  
url <- "https://www.akjournals.com/view/journals/2006/14/1/2006.14.issue-1.xml"

# Definimos un user-agent similar al de un navegador real
user_agent_navegador <- paste(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
  "AppleWebKit/537.36 (KHTML, like Gecko)",
  "Chrome/122.0.0.0 Safari/537.36"
)

# Construimos la solicitud HTTP y añadimos encabezados para 
# que parezca una visita normal
respuesta <- request(url) %>%
  req_user_agent(user_agent_navegador) %>%
  req_headers(`accept-language` = "en-US,en;q=0.9") %>%
  req_perform()

# Convertimos el cuerpo de la respuesta en un documento HTML
# analizable con rvest/xml2
pagina <- respuesta %>%
  resp_body_html()

#################
#TITULO
#################
titulo <- pagina %>%
  html_element("[data-testid='block-primitivetitle']") %>%
  html_text()
titulo <- titulo[-length(titulo)]

extraccion_general <- function(nodo) {
  # Extraemos el títulos de los articulos
  titulo <- nodo %>%
    html_element("[data-testid='block-primitivetitle']") %>%
    html_text2()
  titulo <- titulo[-length(titulo)]
  
  # Extraemos el enlace relativo hacia la página del producto
  doi <- nodo |> 
    html_elements( " a,[target='_blank'], .c-Button--link") |>
    html_text()
  doi <- doi[grep(pattern="https://doi*",doi)]
  
  # Extraemos el precio mostrado en el resultado de búsqueda
  precio <- nodo %>%
    html_element(".a-price .a-offscreen") %>%
    html_text2()
  
  # Extraemos el texto de la valoración, por ejemplo '4.5 out of 5 stars'
  rating <- nodo %>%
    html_element(".a-icon-alt") %>%
    html_text2()
  
  # Extraemos el número de reseñas o valoraciones asociadas al producto
  n_resenas <- nodo %>%
    html_element("[aria-label$='ratings'], .s-link-style .s-underline-text") %>%
    html_text2()
  
  # Construimos una fila con los campos extraídos para este producto
  tibble(
    titulo = titulo,
    # Si el enlace no existe, dejamos un valor faltante; si existe, construimos la URL completa
    enlace = ifelse(
      is.na(enlace_relativo),
      NA_character_,
      paste0("https://www.amazon.com", enlace_relativo)
    ),
    precio = precio,
    rating = rating,
    n_resenas = n_resenas
  )
}
