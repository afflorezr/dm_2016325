url_chinook <- "https://raw.githubusercontent.com/lerocha/chinook-database/master/ChinookDatabase/DataSources/Chinook_Sqlite.sqlite"
ruta_db     <- "chinook.db"

if (!file.exists(ruta_db)) {
  download.file(url_chinook, destfile = ruta_db, mode = "wb")
  cat("Base de datos descargada en:", ruta_db, "\n")
} else {
  cat("La base de datos ya existe:", ruta_db, "\n")
}

# Instalamos los paquetes necesarios
paquetes  <- c("DBI", "RSQLite", "dplyr", "knitr")
instalados <- rownames(installed.packages())
pendientes <- setdiff(paquetes, instalados)
if (length(pendientes) > 0) install.packages(pendientes)

library(DBI)
library(RSQLite)
library(dplyr)

# Abrimos la conexión
con <- dbConnect(RSQLite::SQLite(), "chinook.db")
cat("Conexión establecida.\n")

# Listamos las tablas disponibles
dbListTables(con)

# Columnas de la tabla Track
dbListFields(con, "Track")
dbListFields(con, "Artist")
dbListFields(con, "Album")
dbListFields(con, "Invoice")
dbListFields(con, "InvoiceLine")
dbListFields(con, "Customer")

#############################################################################

resultado_compras <- dbGetQuery(con, "
                      SELECT 
                          c.FirstName || ' ' || c.LastName AS Cliente,
                          c.Country AS Pais,
                          t.Name AS Pista,
                          g.Name AS Genero,
                          il.UnitPrice AS Precio
                      FROM Customer c
                      INNER JOIN Invoice i     ON c.CustomerId = i.CustomerId
                      INNER JOIN InvoiceLine il ON i.InvoiceId = il.InvoiceId
                      INNER JOIN Track t       ON il.TrackId = t.TrackId
                      INNER JOIN Genre g       ON t.GenreId = g.GenreId
                      LIMIT 15
")
resultado_compras

#############################################################################

pistas_no_vendidas <- dbGetQuery(con, "
                                 SELECT t.TrackId As N_Track,
                                        t.Name AS Pista_No_Vendida
                                 FROM Track t
                                 LEFT JOIN InvoiceLine il ON il.TrackId = t.TrackId
                                 WHERE il.InvoiceLineId IS NULL;
                                 ")
pistas_no_vendidas


# Para ver cuántas pistas nunca se han vendido
cat("Total de pistas sin ventas:", nrow(pistas_no_vendidas))

#############################################################################


