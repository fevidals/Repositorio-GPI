# ---------------------------- Run Phillippines -------------------------------------
cat("\n>>> Processing: Phillippines <<<\n")


# Definimos el orden exacto basado en tu script de bash para Philippines
scripts_philippines <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-clean-officer.R", # Limpieza de oficiales incluida
  "4-construction.R",
  "5-estimation.R",
  "6-balance-tests.R", # Balance tests en el paso 6
  "7-het-effects-test.R"
)

# Ejecución secuencial
for (s in scripts_philippines) {
  ruta_completa <- here("code", "philippines", s)
  
  if (file.exists(ruta_completa)) {
    cat("  [EJECUTANDO]:", s, "\n")
    tryCatch({
      source(ruta_completa, local = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ❌ Error en", s, ":", e$message, "\n")
      # Emula el "|| exit 1" deteniendo el script del país
      stop(paste("Fallo crítico en Filipinas durante el paso:", s))
    })
  } else {
    cat("  ⚠️  Aviso: No se encontró el archivo", s, "\n")
  }
}

cat("\n>>> Filipinas: Proceso completado <<<\n")