# Minería de Datos - Repositorio de Talleres

Este repositorio está destinado a que los estudiantes del curso de **Minería de Datos** almacenen y organicen sus talleres a lo largo del semestre.

## Objetivo

Centralizar la entrega y el seguimiento de las actividades del curso en un único repositorio, manteniendo una estructura ordenada, consistente y fácil de revisar.

## Estructura del repositorio

Las entregas se organizan por grupo, por actividad y por estudiante.

```text
dm_2016325/
├── README.md
├── grupo_1/
│   ├── taller_01/
│   │   ├── enunciado.md
│   │   └── entregas/
│   │       ├── jerez_tomas/
│   │       ├── gomez_juan_jose/
│   │       ├── canelo_janith/
│   │       ├── tabaco_sebastian/
│   │       ├── luciano_winston/
│   │       ├── correa_keiner/
│   │       ├── rava_oscar/
│   │       ├── guerrero_luisa/
│   │       └── aldana_juan_pablo/
├── grupo_2/
│   ├── taller_01/
│   │   ├── enunciado.md
│   │   └── entregas/
│   │       ├── bejarano_elian/
│   │       ├── gomez_sergio/
│   │       ├── angulo_daniel/
│   │       ├── ortiz_owen/
│   │       ├── roncancio_daniela/
│   │       ├── vargas_mateo/
│   │       ├── sanchez_jorge/
│   │       ├── camacho_juan/
│   │       └── ardila_cristian/
```

Cada clase tiene una actividad. Por esa razón, a lo largo del semestre se irán creando carpetas como `taller_01`, `taller_02`, `taller_03` y así sucesivamente, siguiendo siempre la misma estructura.

## Instrucciones paso a paso para entregar una actividad

### 1. Identifica tu grupo

Ubica si perteneces a `grupo_1` o a `grupo_2`.

### 2. Identifica la actividad de la clase

Cada actividad se publica en una carpeta con nombre `taller_XX`, por ejemplo:

- `taller_01`
- `taller_02`
- `taller_03`

Debes entregar únicamente en la carpeta correspondiente a la actividad asignada en esa clase.

### 3. Revisa el enunciado

Dentro de cada actividad encontrarás el archivo `enunciado.md`, donde estarán las instrucciones de trabajo.

Ruta de ejemplo:

```text
grupo_1/taller_01/enunciado.md
```

### 4. Ubica tu carpeta personal dentro de `entregas`

Dentro de cada actividad existe una carpeta llamada `entregas`, y allí ya está creada una carpeta para cada estudiante.

Debes subir tus archivos solamente dentro de tu carpeta personal.

Ruta de ejemplo:

```text
grupo_1/taller_01/entregas/jerez_tomas/
```

### 5. Coloca allí todos los archivos de tu entrega

Dentro de tu carpeta puedes incluir, según corresponda:

- notebooks de Jupyter (`.ipynb`)
- scripts en Python (`.py`)
- archivos de datos pequeños
- imágenes, gráficas o reportes
- un `README.md` breve si necesitas explicar la solución o la ejecución

### 6. Mantén separadas las entregas por actividad

Cada nueva actividad debe quedar en su carpeta correspondiente.

Ejemplo:

- La actividad de `taller_01` va en `grupo_X/taller_01/entregas/tu_carpeta/`
- La actividad de `taller_02` va en `grupo_X/taller_02/entregas/tu_carpeta/`

No debes mezclar archivos de distintas clases en una misma carpeta de actividad.

### 7. No cambies la estructura del repositorio

Para facilitar la revisión:

- no cambies el nombre de tu carpeta
- no subas archivos fuera de tu carpeta personal
- no borres carpetas de otros compañeros
- no modifiques la organización de `grupo_1`, `grupo_2`, `taller_XX` o `entregas`

## Buenas prácticas

- Verifica que tus notebooks y scripts abran correctamente antes de subirlos.
- Usa nombres de archivo claros y descriptivos.
- Incluye únicamente el material necesario para revisar la actividad.
- Evita archivos demasiado pesados o innecesarios.
- Si tu entrega requiere instrucciones de uso, agrega un `README.md` dentro de tu carpeta.

## Nota

Si en una nueva clase aparece una actividad adicional, se seguirá el mismo patrón de organización: `grupo -> taller -> entregas -> estudiante`.

