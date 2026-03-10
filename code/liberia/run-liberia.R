# ---------------------------- Run liberia -------------------------------------
cat("\n>>> Processing: Liberia <<<\n")

cat("\n>>> Iniciando Pipeline: LIBERIA <<<\n")

# Definimos el orden exacto según tu estructura de Rscript
scripts_liberia <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-construction.R",
  "4-estimation.R",
  "5-balance-tests.R",
  "6-het-effects.R",
  "7-het-effects-test.R"
)

# Ejecución secuencial
for (s in scripts_liberia) {
  ruta_completa <- here("code", "liberia", s)
  
  if (file.exists(ruta_completa)) {
    cat("  [EJECUTANDO]:", s, "\n")
    tryCatch({
      # Usamos encoding UTF-8 por estándar de reproducibilidad
      source(ruta_completa, local = FALSE, encoding = "UTF-8")
    }, error = function(e) {
      cat("  ❌ Error en", s, ":", e$message, "\n")
      # El stop() asegura que el Main Runall sepa que Liberia falló
      stop(paste("Fallo en el script:", s))
    })
  } else {
    cat("  ⚠️  Aviso: No se encontró el archivo", s, "\n")
  }
}

cat("\n>>> Liberia: Proceso completado <<<\n")