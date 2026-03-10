# ---------------------------- Run Colombia -----------------------------------
cat("\n>>> Processing: COLOMBIA <<<\n")

# Definimos el orden exacto según tus archivos locales
scripts_colombia <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-clean-officer.R",     # Paso adicional
  "4-construction.R",
  "5-estimation.R",        # Nombre simplificado
  "6-het-effects.R",       # Reordenado
  "7-balance-tests.R",     # Paso adicional
  "8-het-effects-test.R"
)

# Ejecución con verificación de archivos
for (s in scripts_colombia) {
  ruta_completa <- here("code", "colombia", s)
  
  if (file.exists(ruta_completa)) {
    cat("  [EJECUTANDO]:", s, "\n")
    tryCatch({
      source(ruta_completa, local = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ❌ Error en", s, ":", e$message, "\n")
      stop("Interrumpiendo ejecución de Colombia por error crítico.")
    })
  } else {
    cat("  ⚠️  Aviso: No se encontró el archivo", s, "- saltando al siguiente.\n")
  }
}

cat("\n>>> Colombia: Proceso completado con éxito <<<\n")