# ---------------------------- Run pakistan -------------------------------------

cat("\n>>> Processing: Pakistán <<<\n")

cat("\n>>> Iniciando Pipeline: PAKISTÁN <<<\n")

# Definimos el orden exacto basado en tu script de bash
scripts_pakistan <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-clean-officer.R", # Limpieza de oficiales
  "4-construction.R",
  "5-estimation.R",
  "6-het-effects.R",
  "7-balance-tests.R", # Balance tests
  "8-het-effects-test.R"
)

# Ejecución secuencial con validación
for (s in scripts_pakistan) {
  ruta_completa <- here("code", "pakistan", s)
  
  if (file.exists(ruta_completa)) {
    cat("  [EJECUTANDO]:", s, "\n")
    tryCatch({
      source(ruta_completa, local = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ❌ Error en", s, ":", e$message, "\n")
      # El stop() emula el "|| exit 1" de tu script original
      stop(paste("Fallo crítico en Pakistán durante el paso:", s))
    })
  } else {
    cat("  ⚠️  Aviso: No se encontró el archivo", s, "\n")
  }
}

cat("\n>>> Pakistán: Proceso completado con éxito <<<\n")