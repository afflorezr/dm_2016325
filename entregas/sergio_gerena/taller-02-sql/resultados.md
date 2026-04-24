## Ejercicio 1: Análisis de ventas con JOINs y CTEs

**Escribe una consulta con INNER JOIN que devuelva: nombre del cliente, país, nombre de la pista comprada, género y precio unitario. Limita a 15 filas.**

| nombre_cliente | pais    | nombre_pista          | genero | precio_unitario |
| -------------- | ------- | --------------------- | ------ | --------------- |
| Leonie Köhler  | Germany | Balls to the Wall     | Rock   | 0.99            |
| Leonie Köhler  | Germany | Restless and Wild     | Rock   | 0.99            |
| Bjørn Hansen   | Norway  | Put The Finger On You | Rock   | 0.99            |
| Bjørn Hansen   | Norway  | Inject The Venom      | Rock   | 0.99            |
| Bjørn Hansen   | Norway  | Evil Walks            | Rock   | 0.99            |
| Bjørn Hansen   | Norway  | Breaking The Rules    | Rock   | 0.99            |
| Daan Peeters   | Belgium | Dog Eat Dog           | Rock   | 0.99            |
| Daan Peeters   | Belgium | Overdose              | Rock   | 0.99            |
| Daan Peeters   | Belgium | Love In An Elevator   | Rock   | 0.99            |
| Daan Peeters   | Belgium | Janie's Got A Gun     | Rock   | 0.99            |
| Daan Peeters   | Belgium | Deuces Are Wild       | Rock   | 0.99            |
| Daan Peeters   | Belgium | Angel                 | Rock   | 0.99            |
| Mark Philips   | Canada  | Right Through You     | Rock   | 0.99            |
| Mark Philips   | Canada  | Not The Doctor        | Rock   | 0.99            |
| Mark Philips   | Canada  | Bleed The Freak       | Rock   | 0.99            |

**Usa un LEFT JOIN para encontrar pistas que nunca han sido vendidas (que no aparecen en InvoiceLine).**

A continuacion se muestran 10 de las 1519 pistas sin ventas encontradas
| pista |
|--------------------------|
| Let's Get It Up |
| C.O.D. |
| Let There Be Rock |
| Bad Boy Boogie |
| Whole Lotta Rosie |
| Walk On Water |
| Dude (Looks Like A Lady) |
| Cryin' |
| The Other Side |
| Crazy |

**Con una CTE, calcula el total de ingresos por artista (suma de UnitPrice × Quantity de sus pistas vendidas). Muestra los 10 artistas con más ingresos.**

| artista                    | total_artista |
|----------------------------|---------------|
| Iron Maiden                | 138.60        |
| U2                         | 105.93        |
| Metallica                  | 90.09         |
| Led Zeppelin               | 86.13         |
| Lost                       | 81.59         |
| The Office                 | 49.75         |
| Os Paralamas Do Sucesso    | 44.55         |
| Deep Purple                | 43.56         |
| Faith No More              | 41.58         |
| Eric Clapton               | 39.60         |

**Extiende la CTE anterior para agregar: La participación porcentual de cada artista sobre el total de ingresos y un RANK() por ingresos.**

Se muestra el top 6 resultados
| artista        | porcentaje_participacion | rankin_ingresos  |
|----------------|--------------------------|------------------|
| Iron Maiden    | 5.95                     | 1                |
| U2             | 4.55                     | 2                |
| Metallica      | 3.87                     | 3                |
| Led Zeppelin   | 3.70                     | 4                |
| Lost           | 3.50                     | 5                |
| The Office     | 2.14                     | 6                |

## Ejercicio 2: Análisis temporal con funciones de ventana
**Calcula el total de ventas por año y mes (SUBSTR(InvoiceDate, 1, 7)). Ordena cronológicamente.**
| mes     | total_ventas |
|---------|--------------|
| 2021-01 | 35.64        |
| 2021-02 | 37.62        |
| 2021-03 | 37.62        |
| 2021-04 | 37.62        |
| 2021-05 | 37.62        |
| 2021-06 | 37.62        |
| 2021-07 | 37.62        |
| 2021-08 | 37.62        |
| 2021-09 | 37.62        |
| 2021-10 | 37.62        |
| 2021-11 | 37.62        |
| 2021-12 | 37.62        |
| 2022-01 | 52.62        |
| 2022-02 | 46.62        |
| 2022-03 | 44.62        |
| 2022-04 | 37.62        |
| 2022-05 | 37.62        |
| 2022-06 | 37.62        |
| 2022-07 | 37.62        |
| 2022-08 | 37.62        |
| 2022-09 | 36.63        |
| 2022-10 | 37.62        |
| 2022-11 | 37.62        |
| 2022-12 | 37.62        |
| 2023-01 | 37.62        |
| 2023-02 | 37.62        |
| 2023-03 | 37.62        |
| 2023-04 | 51.62        |
| 2023-05 | 42.62        |
| 2023-06 | 50.62        |
| 2023-07 | 37.62        |
| 2023-08 | 37.62        |
| 2023-09 | 37.62        |
| 2023-10 | 37.62        |
| 2023-11 | 23.76        |
| 2023-12 | 37.62        |
| 2024-01 | 37.62        |
| 2024-02 | 37.62        |
| 2024-03 | 37.62        |
| 2024-04 | 37.62        |
| 2024-05 | 37.62        |
| 2024-06 | 37.62        |
| 2024-07 | 39.62        |
| 2024-08 | 47.62        |
| 2024-09 | 46.71        |
| 2024-10 | 42.62        |
| 2024-11 | 37.62        |
| 2024-12 | 37.62        |
| 2025-01 | 37.62        |
| 2025-02 | 27.72        |
| 2025-03 | 37.62        |
| 2025-04 | 33.66        |
| 2025-05 | 37.62        |
| 2025-06 | 37.62        |
| 2025-07 | 37.62        |
| 2025-08 | 37.62        |
| 2025-09 | 37.62        |
| 2025-10 | 37.62        |
| 2025-11 | 49.62        |
| 2025-12 | 38.62        |

**Agrega a la consulta anterior:
LAG: ventas del mes anterior.
Variación absoluta respecto al mes anterior.
Media móvil de 3 meses (ROWS BETWEEN 2 PRECEDING AND CURRENT ROW).**
| mes     | total_ventas | ventas_mes_anterior | variacion_absoluta | media_movil_3m |
|---------|-------------|--------------------|--------------------|----------------|
| 2021-01 | 35.64       | NA                 | NA                 | 35.64000       |
| 2021-02 | 37.62       | 35.64              | 1.98               | 36.63000       |
| 2021-03 | 37.62       | 37.62              | 0.00               | 36.96000       |
| 2021-04 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-05 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-06 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-07 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-08 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-09 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-10 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-11 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2021-12 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2022-01 | 52.62       | 37.62              | 15.00              | 42.62000       |
| 2022-02 | 46.62       | 52.62              | -6.00              | 45.62000       |
| 2022-03 | 44.62       | 46.62              | -2.00              | 47.95333       |
| 2022-04 | 37.62       | 44.62              | -7.00              | 42.95333       |
| 2022-05 | 37.62       | 37.62              | 0.00               | 39.95333       |
| 2022-06 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2022-07 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2022-08 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2022-09 | 36.63       | 37.62              | -0.99              | 37.29000       |
| 2022-10 | 37.62       | 36.63              | 0.99               | 37.29000       |
| 2022-11 | 37.62       | 37.62              | 0.00               | 37.29000       |
| 2022-12 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-01 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-02 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-03 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-04 | 51.62       | 37.62              | 14.00              | 42.28667       |
| 2023-05 | 42.62       | 51.62              | -9.00              | 43.95333       |
| 2023-06 | 50.62       | 42.62              | 8.00               | 48.28667       |
| 2023-07 | 37.62       | 50.62              | -13.00             | 43.62000       |
| 2023-08 | 37.62       | 37.62              | 0.00               | 41.95333       |
| 2023-09 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-10 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2023-11 | 23.76       | 37.62              | -13.86             | 33.00000       |
| 2023-12 | 37.62       | 23.76              | 13.86              | 33.00000       |
| 2024-01 | 37.62       | 37.62              | 0.00               | 33.00000       |
| 2024-02 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2024-03 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2024-04 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2024-05 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2024-06 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2024-07 | 39.62       | 37.62              | 2.00               | 38.28667       |
| 2024-08 | 47.62       | 39.62              | 8.00               | 41.62000       |
| 2024-09 | 46.71       | 47.62              | -0.91              | 44.65000       |
| 2024-10 | 42.62       | 46.71              | -4.09              | 45.65000       |
| 2024-11 | 37.62       | 42.62              | -5.00              | 42.31667       |
| 2024-12 | 37.62       | 37.62              | 0.00               | 39.28667       |
| 2025-01 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2025-02 | 27.72       | 37.62              | -9.90              | 34.32000       |
| 2025-03 | 37.62       | 27.72              | 9.90               | 34.32000       |
| 2025-04 | 33.66       | 37.62              | -3.96              | 33.00000       |
| 2025-05 | 37.62       | 33.66              | 3.96               | 36.30000       |
| 2025-06 | 37.62       | 37.62              | 0.00               | 36.30000       |
| 2025-07 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2025-08 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2025-09 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2025-10 | 37.62       | 37.62              | 0.00               | 37.62000       |
| 2025-11 | 49.62       | 37.62              | 12.00              | 41.62000       |
| 2025-12 | 38.62       | 49.62              | -11.00             | 41.95333       |

**Usando NTILE(4), clasifica cada mes en cuartiles según su nivel de ventas. ¿Cuáles meses pertenecen al cuartil superior (Q4)?**

| mes     | total_ventas | cuartil |
|---------|--------------|---------|
| 2025-08 | 37.62        | 4       |
| 2025-09 | 37.62        | 4       |
| 2025-10 | 37.62        | 4       |
| 2025-12 | 38.62        | 4       |
| 2024-07 | 39.62        | 4       |
| 2023-05 | 42.62        | 4       |
| 2024-10 | 42.62        | 4       |
| 2022-03 | 44.62        | 4       |
| 2022-02 | 46.62        | 4       |
| 2024-09 | 46.71        | 4       |
| 2024-08 | 47.62        | 4       |
| 2025-11 | 49.62        | 4       |
| 2023-06 | 50.62        | 4       |
| 2023-04 | 51.62        | 4       |
| 2022-01 | 52.62        | 4       |

**Con RANK(), identifica el mes con mayores ventas de cada año. Usa una CTE para calcular primero las ventas mensuales, luego aplica RANK() con PARTITION BY el año.**

| año  | mes     | total_ventas |
|------|---------|--------------|
| 2021 | 2021-02 | 37.62        |
| 2021 | 2021-03 | 37.62        |
| 2021 | 2021-04 | 37.62        |
| 2021 | 2021-05 | 37.62        |
| 2021 | 2021-06 | 37.62        |
| 2021 | 2021-07 | 37.62        |
| 2021 | 2021-08 | 37.62        |
| 2021 | 2021-09 | 37.62        |
| 2021 | 2021-10 | 37.62        |
| 2021 | 2021-11 | 37.62        |
| 2021 | 2021-12 | 37.62        |
| 2022 | 2022-01 | 52.62        |
| 2023 | 2023-04 | 51.62        |
| 2024 | 2024-08 | 47.62        |
| 2025 | 2025-11 | 49.62        |
