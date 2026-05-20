# =============================================================================
# Skript 08 - Kartografische Darstellung der Ergebnisse
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript erzeugt die finalen Karten zu allen drei Unterfragen aus dem
#   in Skript 07 zusammengefuehrten raeumlichen Datensatz (07_lisa_ergebnis.rds,
#   enthaelt Geometrie, Cluster, Aufwertungs-Scores und LISA-Kategorien).
#
#   Erzeugte Karten:
#     UF1  - Nachbarschaftstypen (fuenf Cluster) auf dem Raster
#     UF2  - Aufwertungstyp je Zelle (kategorial)
#     UF2  - bivariate Karte: soziale und bauliche Aufwertung gemeinsam
#     UF3  - LISA-Cluster der sozialen und der baulichen Aufwertung
#
#   Hinweis zur baulichen LISA-Karte: Die raeumliche Analyse (Skript 07) hat
#   praktisch keine signifikanten Cluster gefunden; die LISA-Karten sind daher
#   weitgehend einfarbig. Das ist das inhaltliche Ergebnis von UF3 (raeumlich
#   verteilte, nicht konzentrierte Aufwertung) und wird bewusst so dargestellt.
#
#   Hinweis zum Hintergrund: theme_void() setzt den Plot-Hintergrund auf
#   transparent; beim PNG-Export erscheint dieser sonst schwarz. Das Karten-
#   Theme setzt den Hintergrund daher explizit auf weiss.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
raster_sf <- readRDS(file.path(path_processed, "07_lisa_ergebnis.rds"))

# -----------------------------------------------------------------------------
# 1. Gemeinsames Karten-Theme, Farbpaletten und Beschriftungen
# -----------------------------------------------------------------------------
# Einheitliches Theme fuer alle Karten. plot.background wird explizit weiss
# gesetzt (siehe Hinweis im Kopf).
karten_theme <- theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    legend.position = "right",
    plot.title      = element_text(face = "bold", size = 13),
    plot.subtitle   = element_text(size = 10)
  )

# Klartextnamen der fuenf Nachbarschaftstypen (Cluster 1-5 aus Skript 05).
cluster_namen <- c(
  "1" = "Innerstädtisch-gebildetes Quartier",
  "2" = "Eigentümergeprägtes Einfamilienhaus-Gebiet",
  "3" = "Neubau-Entwicklungsgebiet",
  "4" = "Statusschwächeres Geschosswohnungsgebiet",
  "5" = "Überaltertes Quartier"
)
raster_sf <- raster_sf |>
  mutate(nachbarschaftstyp = factor(cluster_namen[as.character(cluster)],
                                    levels = cluster_namen))

# Farbpalette fuer die Nachbarschaftstypen (qualitativ, gut unterscheidbar).
typ_farben <- c(
  "Innerstädtisch-gebildetes Quartier"         = "#1b9e77",
  "Eigentümergeprägtes Einfamilienhaus-Gebiet" = "#d95f02",
  "Neubau-Entwicklungsgebiet"                  = "#7570b3",
  "Statusschwächeres Geschosswohnungsgebiet"   = "#e7298a",
  "Überaltertes Quartier"                      = "#e6ab02"
)

# Farbpalette fuer den Aufwertungstyp. Die Kandidaten werden kraeftig
# hervorgehoben, die uebrigen Kategorien gedaempft dargestellt.
aufwertung_farben <- c(
  "sozial & baulich"      = "#b2182b",
  "nur sozial"            = "#ef8a62",
  "nur baulich"           = "#67a9cf",
  "keine Aufwertung"      = "grey80",
  "nicht klassifizierbar" = "grey95"
)

# Farbpalette und Reihenfolge fuer die LISA-Kategorien.
lisa_levels <- c("High-High", "Low-Low", "High-Low", "Low-High",
                 "nicht signifikant")
lisa_farben <- c("High-High"         = "#b2182b",
                 "Low-Low"           = "#2166ac",
                 "High-Low"          = "#ef8a62",
                 "Low-High"          = "#67a9cf",
                 "nicht signifikant" = "grey85",
                 "keine Mietdaten"   = "grey96")

# -----------------------------------------------------------------------------
# 2. UF1 - Karte der Nachbarschaftstypen
# -----------------------------------------------------------------------------
karte_typen <- raster_sf |>
  ggplot() +
  geom_sf(aes(fill = nachbarschaftstyp), colour = NA) +
  scale_fill_manual(values = typ_farben, name = "Nachbarschaftstyp") +
  annotation_scale(location = "bl") +
  labs(title = "Nachbarschaftstypen auf dem 500-m-Raster",
       subtitle = "Fünf Cluster (K-Means) der urbanen Rasterzellen") +
  karten_theme
ggsave(file.path(path_figures, "08_karte_nachbarschaftstypen.png"),
       karte_typen, width = 10, height = 6.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 3. UF2 - Karte des Aufwertungstyps (kategorial)
# -----------------------------------------------------------------------------
# Zeigt je Zelle die Klassifikation aus Skript 06. Die Kandidaten (in beiden
# Dimensionen ueberdurchschnittlich aufgewertet) sind kraeftig rot.
karte_aufwertungstyp <- raster_sf |>
  ggplot() +
  geom_sf(aes(fill = aufwertungstyp), colour = NA) +
  scale_fill_manual(values = aufwertung_farben, name = "Aufwertungstyp",
                    drop = FALSE) +
  annotation_scale(location = "bl") +
  labs(title = "Aufwertungstyp je Rasterzelle",
       subtitle = "Klassifikation aus sozialem und baulichem Aufwertungs-Score") +
  karten_theme
ggsave(file.path(path_figures, "08_karte_aufwertungstyp.png"),
       karte_aufwertungstyp, width = 10, height = 6.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 4. UF2 - Bivariate Karte: soziale und bauliche Aufwertung gemeinsam
# -----------------------------------------------------------------------------
# bi_class teilt beide Scores in je drei Quantilsgruppen und kombiniert sie zu
# einem 3x3-Raster. Eine Zelle ist umso dunkler, je staerker sie in BEIDEN
# Dimensionen aufgewertet hat. Zellen ohne baulichen Score werden separat grau
# dargestellt.
raster_bivariat <- raster_sf |>
  filter(!is.na(score_baulich)) |>
  bi_class(x = score_sozial, y = score_baulich, style = "quantile", dim = 3)

raster_ohne_miete <- raster_sf |> filter(is.na(score_baulich))

karte_bivariat <- ggplot() +
  geom_sf(data = raster_ohne_miete, fill = "grey92", colour = NA) +
  geom_sf(data = raster_bivariat, aes(fill = bi_class), colour = NA,
          show.legend = FALSE) +
  bi_scale_fill(pal = "GrPink", dim = 3) +
  annotation_scale(location = "br") +
  labs(title = "Soziale und bauliche Aufwertung (bivariat)",
       subtitle = "Graue Zellen: keine Mietdaten") +
  karten_theme

# Separate 3x3-Legende fuer die bivariate Darstellung.
bi_legende <- bi_legend(pal = "GrPink", dim = 3, size = 7,
                        xlab = "soziale Aufwertung",
                        ylab = "bauliche Aufwertung")

# Karte und Legende mit cowplot zusammenfuegen (Legende als Einschub).
karte_bivariat_final <- cowplot::ggdraw() +
  cowplot::draw_plot(karte_bivariat) +
  cowplot::draw_plot(bi_legende, x = 0.04, y = 0.07,
                     width = 0.26, height = 0.26)
ggsave(file.path(path_figures, "08_karte_aufwertung_bivariat.png"),
       karte_bivariat_final, width = 9.5, height = 6.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 5. UF3 - LISA-Karten der raeumlichen Cluster
# -----------------------------------------------------------------------------
# Soziale Aufwertung.
karte_lisa_sozial <- raster_sf |>
  mutate(lisa_sozial_kategorie = factor(lisa_sozial_kategorie,
                                        levels = lisa_levels)) |>
  ggplot() +
  geom_sf(aes(fill = lisa_sozial_kategorie), colour = NA) +
  scale_fill_manual(values = lisa_farben, name = "LISA-Kategorie",
                    drop = FALSE) +
  annotation_scale(location = "bl") +
  labs(title = "Räumliche Cluster der sozialen Aufwertung (LISA)",
       subtitle = "signifikante Cluster nach FDR-Korrektur (p < 0.05)") +
  karten_theme
ggsave(file.path(path_figures, "08_karte_lisa_sozial.png"),
       karte_lisa_sozial, width = 10, height = 6.5, dpi = 300, bg = "white")

# Bauliche Aufwertung. Zellen ohne Mietdaten werden als eigene Kategorie
# ausgewiesen.
karte_lisa_baulich <- raster_sf |>
  mutate(kat = replace_na(lisa_baulich_kategorie, "keine Mietdaten"),
         kat = factor(kat, levels = c(lisa_levels, "keine Mietdaten"))) |>
  ggplot() +
  geom_sf(aes(fill = kat), colour = NA) +
  scale_fill_manual(values = lisa_farben, name = "LISA-Kategorie",
                    drop = FALSE) +
  annotation_scale(location = "bl") +
  labs(title = "Räumliche Cluster der baulichen Aufwertung (LISA)",
       subtitle = "signifikante Cluster nach FDR-Korrektur (p < 0.05)") +
  karten_theme
ggsave(file.path(path_figures, "08_karte_lisa_baulich.png"),
       karte_lisa_baulich, width = 10, height = 6.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 6. Abschluss
# -----------------------------------------------------------------------------
cat("Karten erstellt in output/figures/:\n",
    "- 08_karte_nachbarschaftstypen.png   (UF1)\n",
    "- 08_karte_aufwertungstyp.png        (UF2)\n",
    "- 08_karte_aufwertung_bivariat.png   (UF2)\n",
    "- 08_karte_lisa_sozial.png           (UF3)\n",
    "- 08_karte_lisa_baulich.png          (UF3)\n")

