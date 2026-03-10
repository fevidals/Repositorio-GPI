# ---------------------------- Run Brazil -------------------------------------
cat("\n>>> Processing: BRAZIL <<<\n")

scripts <- c(
  "0-clean-units.R",
  "1-clean-admin.R",
  "2-clean-citizen.R",
  "3-construction.R",
  "4-estimation-first-stage.R",
  "5-estimation-main-results.R",
  "7-het-effects.R",
  "8-het-effects-test.R" # Corrected extension to .R
)

for (s in scripts) {
  cat("  Executing:", s, "\n")
  source(here("code", "brazil", s))
}

cat(">>> Brazil: Finished <<<\n")