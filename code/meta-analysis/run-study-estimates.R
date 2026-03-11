################################################################################
#                           Run study estimates                                #
################################################################################

# 1. Alistar ambiente de trabajo
if (!require("pacman")) install.packages("pacman")
pacman::p_load(rmarkdown, here, tidyverse)


cat("\n--- Cargar paquetes necesarios ---\n")
source(here("code", "install-packages.R"))

cat("\n--- Iniciando Ejecución Global de Países ---\n")

# 2. Definir la lista de scripts a ejecutar
# Usamos una lista nombrada para que los mensajes en consola sean claros
scripts_paises <- list(
  "Brasil"      = here("code", "brazil", "run-brazil.R"),
  "Colombia"    = here("code", "colombia", "run-colombia.R"),
  "Liberia"     = here("code", "liberia", "run-liberia.R"),
  "Pakistán"    = here("code", "pakistan", "run-pakistan.R"),
  "Filipinas"   = here("code", "philippines", "run-phillippines.R"),
  "Uganda"      = here("code", "uganda", "run-uganda.R")
)

# 3. Ejecución iterativa con manejo de errores
resultados <- list()

for (nombre_pais in names(scripts_paises)) {
  ruta <- scripts_paises[[nombre_pais]]
  
  cat(paste0("\n[PROCESANDO] ", nombre_pais, "...\n"))
  cat(paste0("Ruta: ", ruta, "\n"))
  
  tryCatch({
    # Ejecutar el script del país
    # local = FALSE asegura que los objetos se guarden en el Global Env si es necesario
    source(ruta, local = FALSE, encoding = "UTF-8")
    
    cat(paste0("ok ", nombre_pais, " completado con éxito.\n"))
    resultados[[nombre_pais]] <- "Éxito"
    
  }, error = function(e) {
    cat(paste0("no ERROR en ", nombre_pais, ":\n"), e$message, "\n")
    resultados[[nombre_pais]] <- paste("Error:", e$message)
  })
}

# 4. Resumen final en consola
cat("\n====================================================")
cat("\nRESUMEN DE EJECUCIÓN:")
print(enframe(unlist(resultados), name = "País", value = "Estado"))
cat("====================================================\n")