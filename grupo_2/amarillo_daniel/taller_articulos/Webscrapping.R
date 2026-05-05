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
  
url <- "https://www.akjournals.com/search?access=all&fromDate=2025&pageSize=50&q1=Journal+of+Behavioral+Addictions&sort=datedescending&toDate=2025&type_0=journalarticle"

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

# Mostramos el objeto HTML obtenido
pagina
response <- html_elements(pagina, ".c-Button--link")
