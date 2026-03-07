mkiv_theme <-
  function() {
    theme_minimal() +
      theme(
        strip.background = element_rect(fill = "#f0f0f0", colour = "#f0f0f0"),
        strip.text = element_text(colour = "black"),
        text = element_text(family = "Helvetica Neue Medium")
      )
  }
