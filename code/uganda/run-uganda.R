# ---------------------------- Run Uganda -------------------------------------
cat("\n>>> Processing: Uganda <<<\n")

# Definimos el orden exacto basado en tu script de bash para Uganda
scripts_uganda <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-clean-officer.R", # Incluye limpieza de oficiales
  "4-construction.R",
  "5-estimation.R",
  "6-het-effects.R",
  "7-balance-tests.R", # Pruebas de balance en el paso 7
  "8-het-effects-test.R"
)

# Ejecución secuencial
for (s in scripts_uganda) {
  ruta_completa <- here("code", "uganda", s)
  
  if (file.exists(ruta_completa)) {
    cat("  [EJECUTANDO]:", s, "\n")
    tryCatch({
      source(ruta_completa, local = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ❌ Error en", s, ":", e$message, "\n")
      # Emula el "|| exit 1" deteniendo el script de Uganda
      stop(paste("Fallo crítico en Uganda durante el paso:", s))
    })
  } else {
    cat("  ⚠️  Aviso: No se encontró el archivo", s, "\n")
  }
}

cat("\n>>> Uganda: Proceso completado <<<\n")