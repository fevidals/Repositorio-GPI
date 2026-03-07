suppressMessages({
  library(tidyverse)
  library(showtext)
  font_add("Helvetica Neue Medium", "/System/Library/Fonts/HelveticaNeue.ttc")
  font_add("Courier", "/System/Library/Fonts/Courier.dfont")
  showtext_auto()
  library(patchwork)
})

meta_estimates_main_hypotheses <- readRDS("data/out/meta-estimates-main-hypotheses.RDS")

secondary_hypotheses <- readRDS("data/out/meta-estimates-secondary-hypotheses.RDS") %>% 
  filter(label %in% c("Crime victimization idx. (administrative data)"))

meta_estimates_main_hypotheses <- 
  filter(meta_estimates_main_hypotheses, hypothesis == "1a") %>% 
  bind_rows(secondary_hypotheses) %>% 
  bind_rows(filter(meta_estimates_main_hypotheses, hypothesis != "1a"))
  
source("article/code/0-mkiv-theme.R")

fig1_df <- 
  meta_estimates_main_hypotheses %>%
  filter(hypothesis != "C") %>%
  mutate(label = str_replace(label, " idx.", "")) %>%
  mutate(type = case_when(
    str_starts(hypothesis, "M") == TRUE ~ "mechanism",
    str_starts(hypothesis, "S") == TRUE ~ "secondary",
    TRUE ~ "primary"),
    type = factor(type, 
                  levels = c("primary", "mechanism", "secondary"),
                  labels = c("Primary Outcomes", "Mechanisms", "Secondary")),
    label = forcats::fct_reorder(as.factor(label), 
                                 as.numeric(as.factor(hypothesis)), .desc = TRUE)) %>% 
  # prepare p-values for printing
  mutate(
    est_print = format(round(estimate, 3), digits = 3),
    se_print = format(round(std.error, 2), digits = 2),
    p_print = format(round(p.value, 2), digits = 2),
    p.adj_print = if_else(is.na(p.value.adj), "", as.character(format(round(p.value.adj, 2), digits = 2)))
  )


p <- 
  fig1_df %>% 
  ggplot(aes(estimate, label, group = "hypothesis")) +
  mkiv_theme() +
  geom_vline(xintercept = 0, lty = 2, lwd = .5, colour = "black") +
  geom_errorbarh(
    aes(xmin = conf.low, xmax = conf.high), 
    height = 0.1, position = position_dodge(width = 0.3), 
    colour = "black", na.rm = TRUE
  ) +
  labs(y = "", x = "Estimated treatment effect with 95% confidence intervals") +
  geom_point(
    size = 2, pch = 23, fill = "black", 
    position = position_dodge(width = 1), colour = "black", na.rm = TRUE
  ) +
  facet_grid(type ~ ., switch = "y", scales = "free", space = "free") +
  geom_hline(yintercept = -Inf, color = "white", size = 6) +
  theme(
    strip.text.y = element_text(angle = -90),
    plot.margin = margin(l = -0.5, t = 0, b = 0.5, r = 0.5, unit = "lines"),
    axis.title.x = element_text(size = 8, vjust = -1.5),
    panel.grid.minor.y = element_blank()
  )

p2 <- 
  fig1_df %>% 
  ggplot(aes(estimate, label, group = "hypothesis")) +
  mkiv_theme() +
  labs(x = "", y = "") +
  geom_text(aes(label = est_print), 
            family = "Helvetica Neue Medium", x = -0.2, size = 2.625, hjust = 0, show.legend = FALSE) +
  geom_text(aes(label = se_print), 
            family = "Helvetica Neue Medium", x = 0.2, size = 2.625, hjust = 0, show.legend = FALSE) +
  geom_text(aes(label = p_print), 
            family = "Helvetica Neue Medium", x = 0.55, size = 2.625, hjust = 0, show.legend = FALSE) +
  geom_text(aes(label = p.adj_print), 
            family = "Helvetica Neue Medium", x = 0.9, size = 2.625, hjust = 0, show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  facet_grid(type ~ ., switch = "y", scales = "free", space = "free") +
  labs(title = "Est.     S.E.      p      Adj. p", x = "", y = "", colour = "", shape = "") +
  theme(
    plot.margin = margin(l = -0.5, t = 0, b = 0.5, r = 0.5, unit = "lines"),
    axis.title.x = element_text(vjust = -1.5),
    strip.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.y = element_blank(),
    plot.title = element_text(size = 8, hjust = 0.5)
  )
# p2

gg <- p + p2 + plot_layout(widths = c(.7, .3))
# gg

ggsave(gg, height = 5.5, width = 6.5, filename = "article/figures/figure2.pdf")
extrafont::embed_fonts("article/figures/figure2.pdf")


