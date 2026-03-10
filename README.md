# Community Policing does not build citizen trust in police or reduce crime in the Global South
> **Proyecto de replicación - Gestión de proyectos de investigación y Ciencia abierta (2026-1)**

## 1. Paper Seleccionado
**Título original:** *"Community Policing does not build citizen trust in police or reduce crime in the Global South"*

## 2. Equipo de Trabajo

| Integrantes | Rol Asignado |
|--------------|---------------|
| **Laura Díaz** | Project Manager: seguimiento del cronograma, organización de reuniones y control de calidad |
| **Vivian Cabanzo** | Coordinadora de identificación, descargas y preparación de datos. Control de consistencia en las variables |
| **Juan Esteban Díaz** | Ejecución del código, reproducción de modelos estadísticos y validación de resultados del artículo |
| **Fabián Vidal** | Mantenedor del Repositorio en GitHub (documentación, README) e integración de los avances |


## 3. Descripción General
El objetivo del proyecto es lograr la replicabilidad de los resultados presentados en el artículo seleccionado. En este repositorio van a estar contenidos los datos originales que proporcionan los autores como son: las encuestas anonimizadas a ciudadanos y entidades gubernamentales. Además, los códigos con el procesamiento de los datos con procedimientos como: limpieza, construcción de índices primarios, implementación de regresiones lineales, efectos fijos, gráficos y tablas, etc. El proyecto tiene como objetivo principal el poner en práctica los temas de organización adecuada de directorios, ciencia abierta y replicabilidad, por lo que todos los pasos y procedimientos van a estar alojados en este repositorio.

**Licencia:**
El repositorio opera bajo una licencia MIT que permite a cualquier persona pueda distribuir, modificar y usar comercialmente los códigos alojados aquí, sin embargo, estos deben nombrar a los autores y el software se entrega sin garantía alguna.

## 4. Estructura de Directorios
En este repositorio vamos a aplicar el protocolo de documentación *Teaching Integrity in Empirical Research* (**TIER**) para la organización de archivos en investigación cuantitativa que facilite la replicabilidad del trabajo. 

* **`/data`**: contiene los archivos originales (Raw Data) y los datos procesados en la replicabilidad del trabajo.
* **`/documents`**: documentación del proyecto y manuscrito final.
* **`/renv`**: aseguramiento del entorno de trabajo y gestión de dependencias de R.
* **`/scripts`**: códigos con todo el proceso computacional de la replicación, divididos en:
  * **`/prep_scripts`**: scripts relacionados al procesamiento y limpieza para generar los datos procesados.
  * **`/article_scripts`**: códigos utilizados específicamente para generar las tablas y gráficos del artículo.
  * **`/helper_functions_and_themes`**: funciones auxiliares y estandarización de temas visuales para el proyecto.
* **`/seguimiento`**: documentos de gestión del proyecto, incluyendo la Estructura de Desglose del Trabajo (EDT) de replicación.
* **`/views`**: salidas finales de la replicación, tablas y Gráficos, que van a ser comparadas con los resultados del paper original.


## 5. Requisitos Iniciales Identificados
Para poder realizar el proceso de replicabilidad del artículo seleccionado, necesitamos los siguientes requerimientos técnicos y de información:

* **Sistemas Operativos:** Windows 11, macOS Sequoia (15.2) o Ubuntu 24.04.4
* **Software:** The R project (4.5.2), RStudio (2026.01.0) como IDE y Overleaf como editor de texto para LaTeX.
* **Control de Versiones:** Git (2.53.0) y GitHub Desktop (3.5.4).
* **Acceso a Datos:** los datos originales están disponibles en *The Center for Open Science*, en específico en el Open Science Framework (OSF), para los seis países estudiados en el artículo y vienen en formatos csv y dta.
* **Librerías/Paquetes (R):** La gestión de dependencias y aislamiento del entorno se realizará mediante `renv`. Para la lectura de datos, manipulación, modelado espacial/estadístico y visualización, se requerirán las siguientes librerías: `blockTools`, `colorout`, `DeclareDesign`, `dplyr`, `FNN`, `foreign`, `glue`, `gt`, `haven`, `Hmisc`, `kableExtra`, `lmtest`, `lubridate`, `metafor`, `nnet`, `ogrDrivers`, `ogrInfo`, `patchwork`, `randomizr`, `RANN`, `readOGR`, `readstata13`, `readxl`, `rgdal`, `ri2`, `RItools`, `sf`, `showtext`, `sp`, `stargazer`, `stdidx` y `tidyverse`.

## 6. Requerimientos de Software (Librerías)

Para garantizar la reproducibilidad computacional del artículo original, se requiere la instalación de diversos paquetes en R. A continuación se detallan y clasifican según su propósito dentro del flujo de análisis:

| Nombre del Paquete | Función |
| :--- | :--- |
| **Manipulación y Limpieza de Datos** | |
| `tidyverse`, `dplyr` | Manipulación, limpieza y visualización de datos. |
| `fastDummies` | Creación eficiente de variables dicotómicas. |
| `lubridate` | Manejo simplificado de fechas y horas. |
| `stdidx` | Estandarización de índices. |
| **Importación y Exportación de Archivos** | |
| `haven` | Importación de datos en formatos externos (Stata, SPSS). |
| `readxl` | Importación de archivos de datos desde Excel. |
| `foreign`, `readstata13` | Lectura de bases de datos de otros software estadísticos. |
| **Estadística y Econometría** | |
| `lmtest` | Pruebas de diagnóstico para modelos lineales. |
| `metafor` | Realización de metanálisis y modelos de efectos. |
| `nnet` | Modelos de regresión logística multinomial. |
| `FNN`, `RANN` | Algoritmos de búsqueda de vecinos más cercanos. |
| `Hmisc` | Funciones misceláneas para análisis de datos y estadísticas. |
| **Diseño Experimental y Aleatorización** | |
| `DeclareDesign` | Diagnóstico y simulación de diseños de investigación. |
| `ri2` | Ejecución de inferencia de aleatorización. |
| `blockTools`, `randomizr`, `RItools`| Asignación aleatoria y bloqueo de tratamientos. |
| **Análisis de Datos Espaciales (GIS)** | |
| `sf`, `sp` | Manejo y análisis de datos geográficos. |
| `rgdal`, `readOGR` | Lectura y proyección de datos vectoriales y mapas. |
| `ogrDrivers`, `ogrInfo` | Exploración de controladores y metadatos espaciales. |
| **Visualización y Formateo de Tablas** | |
| `gt` | Generación de tablas de presentación de alta calidad. |
| `kableExtra` | Personalización avanzada de tablas profesionales. |
| `stargazer` | Exportación estructurada de tablas de regresión a LaTeX. |
| `patchwork` | Composición de múltiples gráficos en una sola imagen. |
| `showtext` | Gestión de fuentes y tipografía en gráficos. |
| `glue` | Formateo dinámico de cadenas de texto. |
| `colorout` | Personalización de colores en la salida de la consola. |


## 7. Localización y acceso a los datos procesados
Los microdatos procesados utilizados por los autores también son de acceso totalmente abierto. El repositorio oficial que contiene las encuestas anonimizadas y la documentación primaria puede ser consultado en el **Open Science Framework (OSF)** a través del siguiente enlace: [OSF - Community Policing Data](https://osf.io/2juyz/overview).

Para facilitar la ejecución de los códigos y garantizar la reproducibilidad autónoma de este proyecto, una copia estática de estas bases de datos (en formato `.RDS`) ha sido alojada directamente en este repositorio dentro del directorio `data/out`. 

