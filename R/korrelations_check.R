# =============================================================================
# Export Korrelationsmatrix der zehn Kandidatenvariablen (inkl. mittlere_zimmerzahl)
# Dient als Beleg für den Ausschluss von mittlere_zimmerzahl (r = -0.81 mit anteil_mieter)
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 1. Korrelationsmatrix berechnen
# -----------------------------------------------------------------------------
raster_indikatoren <- readRDS(file.path(path_processed, "03_raster_indikatoren.rds"))

zellen_beide <- raster_indikatoren |>
  count(id_500, name = "n_perioden") |>
  filter(n_perioden == 2) |>
  pull(id_500)

cor_matrix_10 <- raster_indikatoren |>
  filter(id_500 %in% zellen_beide, period == "t1") |>
  select(anteil_einpersonen, anteil_neubau, anteil_mfh, anteil_mieter,
         mittlere_zimmerzahl,
         anteil_0_19, anteil_20_44, anteil_65plus,
         anteil_tertiaer, erwerbstaetigenquote) |>
  cor() |>
  round(2)

# -----------------------------------------------------------------------------
# 2. PNG-Export (corrplot)
# -----------------------------------------------------------------------------
png(file.path(path_figures, "04_korrelationsmatrix_10vars.png"),
    width = 2200, height = 2200, res = 300)
corrplot(cor_matrix_10,
         method      = "color",
         type        = "upper",
         diag        = FALSE,
         addCoef.col = "black",
         number.cex  = 0.65,
         tl.col      = "black",
         tl.srt      = 45)
dev.off()

# -----------------------------------------------------------------------------
# 3. Word-Export (flextable)
# -----------------------------------------------------------------------------
cor_df <- as_tibble(cor_matrix_10, rownames = "Variable")

cor_ft <- flextable(cor_df) |>
  set_caption(paste("Korrelationsmatrix der zehn Kandidatenvariablen (t1-Niveau,",
                    "7'145 Zellen). mittlere_zimmerzahl wird aufgrund der",
                    "Korrelation r = -0.81 mit anteil_mieter aus dem Clustering",
                    "ausgeschlossen.")) |>
  theme_booktabs() |>
  align(j = 1, align = "left", part = "all") |>
  align(j = -1, align = "center", part = "all") |>
  bold(part = "header") |>
  bold(j = 1, part = "body") |>
  autofit()

save_as_docx(cor_ft,
             path = file.path(path_tables, "04_korrelationsmatrix_10vars.docx"))

cat("Exporte erstellt:\n",
    "- output/figures/04_korrelationsmatrix_10vars.png\n",
    "- output/tables/04_korrelationsmatrix_10vars.docx\n")