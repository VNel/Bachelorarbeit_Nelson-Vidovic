# =============================================================================
# Skript 07 - Raeumliche Analyse: Moran's I und LISA (UF3)
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript beantwortet Unterfrage 3: Treten die Aufwertungsprozesse
#   raeumlich konzentriert auf oder sind sie ueber den Raum verteilt?
#
#   Zwei aufeinander aufbauende Verfahren:
#     - Globaler Moran's I: eine einzige Kennzahl je Score. Sie misst, ob
#       benachbarte Zellen insgesamt aehnliche Aufwertungswerte aufweisen.
#       Ein positiver, signifikanter Wert bedeutet raeumliche Buendelung.
#     - Lokaler Moran's I (LISA, Anselin 1995): ein Wert je Zelle. Er zeigt,
#       WO sich Cluster befinden, und teilt jede signifikante Zelle in einen
#       der vier Typen ein:
#         High-High = Zelle und Nachbarn hoch  -> Aufwertungs-Hotspot
#         Low-Low   = Zelle und Nachbarn tief  -> Coldspot
#         High-Low / Low-High = raeumliche Ausreisser
#
#   Die Analyse wird getrennt fuer den sozialen und den baulichen
#   Aufwertungs-Score durchgefuehrt. Der bauliche Score liegt nur fuer die
#   Zellen mit belastbarer Mietveraenderung vor; seine raeumliche Analyse
#   beschraenkt sich daher auf diese Teilmenge.
#
#   Raeumliche Gewichte: Koenigin-Kontiguitaet - zwei Zellen gelten als
#   benachbart, wenn sich ihre 500-m-Quadrate eine Kante oder Ecke teilen
#   (auf dem Raster die bis zu acht umliegenden Zellen). Die Gewichte sind
#   zeilenstandardisiert (Stil "W").
#
#   Das Skript ist deterministisch (analytische Tests, keine Zufallskomponente).
#   Folgeskript: 08 erzeugt aus 07_lisa_ergebnis.rds die finalen Karten.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
aufwertung <- readRDS(file.path(path_processed, "06_aufwertung.rds"))

# -----------------------------------------------------------------------------
# 1. Geometrie rekonstruieren
# -----------------------------------------------------------------------------
# id_500 codiert das Zellzentrum als "<X>_<Y>" in Landeskoordinaten (LV95,
# EPSG:2056). Daraus werden die Zentren extrahiert und je Zelle ein
# 500-m-Quadrat (Zentrum +/- 250 m) als Polygon gebildet.
aufwertung_xy <- aufwertung |>
  separate(id_500, into = c("x_zentrum", "y_zentrum"), sep = "_",
           remove = FALSE, convert = TRUE)

# Hilfsfunktion: baut aus einem Zellzentrum ein quadratisches Polygon.
quadrat_bauen <- function(x, y, halb = 250) {
  st_polygon(list(rbind(
    c(x - halb, y - halb), c(x + halb, y - halb),
    c(x + halb, y + halb), c(x - halb, y + halb),
    c(x - halb, y - halb)
  )))
}

raster_geom <- map2(aufwertung_xy$x_zentrum, aufwertung_xy$y_zentrum,
                    quadrat_bauen) |>
  st_sfc(crs = 2056)

raster_sf <- st_sf(aufwertung_xy, geometry = raster_geom)

# -----------------------------------------------------------------------------
# 2. Konstanten fuer die LISA-Klassifikation
# -----------------------------------------------------------------------------
lisa_levels <- c("High-High", "Low-Low", "High-Low", "Low-High",
                 "nicht signifikant")
lisa_farben <- c("High-High"         = "#b2182b",   # dunkelrot  - Hotspot
                 "Low-Low"           = "#2166ac",   # dunkelblau - Coldspot
                 "High-Low"          = "#ef8a62",   # raeuml. Ausreisser
                 "Low-High"          = "#67a9cf",   # raeuml. Ausreisser
                 "nicht signifikant" = "grey85",
                 "keine Mietdaten"   = "grey97")

# -----------------------------------------------------------------------------
# 3. Hilfsfunktion: globale und lokale raeumliche Analyse
# -----------------------------------------------------------------------------
# Fuehrt fuer einen Score die gesamte raeumliche Analyse durch und gibt die
# globale Kennzahl, die zellweise LISA-Klassifikation und die Gewichtsmatrix
# zurueck.
lisa_berechnen <- function(sf_data, varname, praefix) {
  
  # Nachbarschaft (Koenigin-Kontiguitaet) und zeilenstandardisierte Gewichte.
  # zero.policy = TRUE laesst Zellen ohne Nachbarn zu (isolierte Zellen).
  nb    <- poly2nb(sf_data, queen = TRUE)
  listw <- nb2listw(nb, style = "W", zero.policy = TRUE)
  
  x <- sf_data[[varname]]
  
  # Globaler Moran's I (einseitig getestet auf positive Autokorrelation).
  global <- moran.test(x, listw, zero.policy = TRUE)
  
  # Lokaler Moran's I je Zelle. Spalte 5 enthaelt den p-Wert (Pr(z != E(Ii))).
  lokal  <- localmoran(x, listw, zero.policy = TRUE)
  # Korrektur fuer multiples Testen (Benjamini-Hochberg / FDR), da je Zelle
  # ein eigener Test gerechnet wird.
  p_fdr  <- p.adjust(lokal[, 5], method = "fdr")
  
  # Klassifikation: Vorzeichen des standardisierten Werts (z) und seines
  # raeumlich verzoegerten Mittels (Wz, Mittel der Nachbarwerte) ergeben die
  # vier LISA-Quadranten. Nur Zellen mit FDR-korrigiertem p < 0.05 gelten als
  # signifikant.
  z  <- as.numeric(scale(x))
  wz <- lag.listw(listw, z, zero.policy = TRUE)
  kategorie <- case_when(
    is.na(p_fdr) | p_fdr >= 0.05 ~ "nicht signifikant",
    z > 0 & wz > 0               ~ "High-High",
    z < 0 & wz < 0               ~ "Low-Low",
    z > 0 & wz < 0               ~ "High-Low",
    TRUE                         ~ "Low-High"
  )
  
  global_tab <- tibble(
    variable       = varname,
    morans_i       = round(global$estimate[["Moran I statistic"]], 4),
    erwartungswert = round(global$estimate[["Expectation"]], 4),
    z_wert         = round(as.numeric(global$statistic), 3),
    p_wert         = signif(global$p.value, 3),
    n_zellen       = length(x),
    n_isoliert     = sum(card(nb) == 0)
  )
  
  lokal_tab <- tibble(
    id_500    = sf_data$id_500,
    p         = p_fdr,
    kategorie = kategorie
  ) |>
    rename_with(~ paste0(praefix, "_", .x), c(p, kategorie))
  
  list(global = global_tab, lokal = lokal_tab, listw = listw)
}

# -----------------------------------------------------------------------------
# 4. Analyse je Aufwertungsdimension
# -----------------------------------------------------------------------------
# Sozialer Score: vollstaendig, Analyse auf allen Zellen.
# Baulicher Score: nur Zellen mit belastbarer Mietveraenderung.
sf_sozial  <- raster_sf
sf_baulich <- raster_sf |> filter(!is.na(score_baulich))

res_sozial  <- lisa_berechnen(sf_sozial,  "score_sozial",  "lisa_sozial")
res_baulich <- lisa_berechnen(sf_baulich, "score_baulich", "lisa_baulich")

# -----------------------------------------------------------------------------
# 5. LISA-Ergebnisse an den Rasterdatensatz anfuegen
# -----------------------------------------------------------------------------
# Nicht analysierte Zellen (bauliche Dimension) erhalten beim Join NA.
raster_sf <- raster_sf |>
  left_join(res_sozial$lokal,  by = "id_500") |>
  left_join(res_baulich$lokal, by = "id_500")

# -----------------------------------------------------------------------------
# 6. Auswertungstabellen
# -----------------------------------------------------------------------------
# 6.1 Globaler Moran's I beider Scores.
moran_global <- bind_rows(res_sozial$global, res_baulich$global) |>
  mutate(variable = case_when(
    variable == "score_sozial"  ~ "Sozialer Aufwertungs-Score",
    variable == "score_baulich" ~ "Baulicher Aufwertungs-Score"
  ))
moran_global

# 6.2 Verteilung der LISA-Kategorien je Score.
lisa_sozial_uebersicht <- raster_sf |>
  st_drop_geometry() |>
  count(kategorie = lisa_sozial_kategorie, name = "n_zellen") |>
  mutate(anteil = round(n_zellen / sum(n_zellen), 3))
lisa_sozial_uebersicht

lisa_baulich_uebersicht <- raster_sf |>
  st_drop_geometry() |>
  mutate(kat = replace_na(lisa_baulich_kategorie, "keine Mietdaten")) |>
  count(kategorie = kat, name = "n_zellen") |>
  mutate(anteil = round(n_zellen / sum(n_zellen), 3))
lisa_baulich_uebersicht

# Konsolenuebersicht
cat("Globaler Moran's I - sozial:", moran_global$morans_i[1],
    "(p =", moran_global$p_wert[1], ") | baulich:", moran_global$morans_i[2],
    "(p =", moran_global$p_wert[2], ")\n")

# 6.3 Export der drei Tabellen in ein Word-Dokument.
save_as_docx(
  "Globaler Moran's I" =
    flextable(moran_global) |> autofit(),
  "LISA-Verteilung - soziale Aufwertung" =
    flextable(lisa_sozial_uebersicht) |> autofit(),
  "LISA-Verteilung - bauliche Aufwertung" =
    flextable(lisa_baulich_uebersicht) |> autofit(),
  path = file.path(path_tables, "07_raeumliche_analyse.docx")
)

# -----------------------------------------------------------------------------
# 7. Visualisierungen
# -----------------------------------------------------------------------------
# 7.1 Moran-Streudiagramme: Score gegen raeumlich verzoegerten Score. Die
#     Steigung der Geraden entspricht dem globalen Moran's I.
png(file.path(path_figures, "07_moran_scatter_sozial.png"),
    width = 1800, height = 1500, res = 300)
moran.plot(sf_sozial$score_sozial, res_sozial$listw, zero.policy = TRUE,
           labels = FALSE, quiet = TRUE,
           xlab = "Sozialer Aufwertungs-Score",
           ylab = "Raeumlich verzoegerter Score",
           main = "Moran-Streudiagramm: soziale Aufwertung")
dev.off()

png(file.path(path_figures, "07_moran_scatter_baulich.png"),
    width = 1800, height = 1500, res = 300)
moran.plot(sf_baulich$score_baulich, res_baulich$listw, zero.policy = TRUE,
           labels = FALSE, quiet = TRUE,
           xlab = "Baulicher Aufwertungs-Score",
           ylab = "Raeumlich verzoegerter Score",
           main = "Moran-Streudiagramm: bauliche Aufwertung")
dev.off()

# 7.2 LISA-Karten: raeumliche Verteilung der Cluster. Graue Zellen sind nicht
#     signifikant; farbige Zellen bilden Hotspots (rot) bzw. Coldspots (blau).
karte_sozial <- raster_sf |>
  mutate(lisa_sozial_kategorie = factor(lisa_sozial_kategorie,
                                        levels = lisa_levels)) |>
  ggplot() +
  geom_sf(aes(fill = lisa_sozial_kategorie), colour = NA) +
  scale_fill_manual(values = lisa_farben, name = "LISA-Kategorie") +
  annotation_scale(location = "bl") +
  labs(title = "Raeumliche Cluster der sozialen Aufwertung (LISA)") +
  theme_void()
ggsave(file.path(path_figures, "07_lisa_karte_sozial.png"), karte_sozial,
       width = 8, height = 6, dpi = 300, bg = "white")

karte_baulich <- raster_sf |>
  mutate(kat = replace_na(lisa_baulich_kategorie, "keine Mietdaten"),
         kat = factor(kat, levels = c(lisa_levels, "keine Mietdaten"))) |>
  ggplot() +
  geom_sf(aes(fill = kat), colour = NA) +
  scale_fill_manual(values = lisa_farben, name = "LISA-Kategorie") +
  annotation_scale(location = "bl") +
  labs(title = "Raeumliche Cluster der baulichen Aufwertung (LISA)") +
  theme_void()
ggsave(file.path(path_figures, "07_lisa_karte_baulich.png"), karte_baulich,
       width = 8, height = 6, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 8. Speichern
# -----------------------------------------------------------------------------
# raster_sf enthaelt je Zelle Geometrie, Scores, Aufwertungstyp,
# Clusterzugehoerigkeit und die LISA-Kategorien - Grundlage fuer die Karten
# in Skript 08.
saveRDS(raster_sf, file.path(path_processed, "07_lisa_ergebnis.rds"))
