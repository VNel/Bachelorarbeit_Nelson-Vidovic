# =============================================================================
# Skript 04 - z-Standardisierung
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript bereitet die in Skript 03 aggregierten Rasterindikatoren fuer
#   die beiden Analyseteile auf. Es erzeugt zwei Datensaetze:
#
#     A) cluster_input  -> Grundlage fuer das Clustering (UF1)
#        Die neun Clustering-Variablen werden auf den Niveauwerten der Periode
#        t1 z-standardisiert. t1 dient als Basis-Typologie.
#
#     B) veraenderung   -> Grundlage fuer den Aufwertungs-Score (UF2)
#        Fuer die sechs Score-Indikatoren wird die Veraenderung t1 -> t2 je
#        Zelle berechnet und anschliessend z-standardisiert.
#
#   Warum z-Standardisierung: K-Means (UF1) beruht auf Distanzen, und der
#   Composite-Score (UF2) kombiniert mehrere Indikatoren. In beiden Faellen
#   wuerden Variablen mit grossem Zahlenbereich (z. B. Mietbetraege gegenueber
#   Anteilen zwischen 0 und 1) das Ergebnis dominieren. Die z-Standardisierung
#   bringt jede Variable auf Mittelwert 0 und Standardabweichung 1 und macht sie
#   damit vergleichbar.
#
#   Analysepopulation: Alle drei Unterfragen bauen auf den Zellen auf, die in
#   BEIDEN Perioden vorkommen. Nur fuer diese laesst sich eine Veraenderung
#   berechnen; indem auch das Clustering auf dieselbe Zellmenge beschraenkt
#   wird, beruhen UF1, UF2 und UF3 auf einer einheitlichen Grundgesamtheit.
#
#   Folgeskripte: 05 (Clustering) liest cluster_input, 06 (Aufwertungs-Score)
#   liest veraenderung.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
raster_indikatoren <- readRDS(file.path(path_processed, "03_raster_indikatoren.rds"))

# -----------------------------------------------------------------------------
# 1. Variablengruppen
# -----------------------------------------------------------------------------
# Clustering-Variablen (UF1): alle neun Indikatoren AUSSER der Miete pro Zimmer.
# Die Miete ist strukturell unvollstaendig (sie existiert nur fuer Mieter-
# haushalte) und wurde daher bewusst aus dem Clustering ausgeschlossen, damit
# eigentuemergepraegte Zellen nicht verworfen oder Werte erfunden werden
# muessen. Die Wohndimension ist im Clustering ueber Mieteranteil, Bautyp,
# Baualter und Zimmerzahl weiterhin vertreten.
cluster_vars <- c(
  "anteil_einpersonen", "anteil_neubau", "anteil_mfh", "anteil_mieter",
  "anteil_0_19", "anteil_20_44", "anteil_65plus",
  "anteil_tertiaer", "erwerbstaetigenquote"
)

# Indikatoren des Aufwertungs-Scores (UF2). Alle sechs sind so ausgerichtet,
# dass eine Zunahme eine Aufwertung bedeutet.
#   - sozialer Score:  Bildung, Erwerbstaetigkeit, junge Erwachsene, Einpersonen
#   - baulicher Score: Miete pro Zimmer, Neubauanteil
score_vars_sozial  <- c("anteil_tertiaer", "erwerbstaetigenquote",
                        "anteil_20_44", "anteil_einpersonen")
score_vars_baulich <- c("median_miete_zimmer", "anteil_neubau")
score_vars         <- c(score_vars_sozial, score_vars_baulich)

# -----------------------------------------------------------------------------
# 2. Hilfsfunktion: z-Standardisierung
# -----------------------------------------------------------------------------
# z = (Wert - Mittelwert) / Standardabweichung. na.rm = TRUE, da die
# Mietvariable fehlende Werte enthalten kann; fehlende Werte bleiben dann auch
# nach der Standardisierung fehlend.
z_standard <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}

# -----------------------------------------------------------------------------
# 3. Analysepopulation bestimmen: Zellen mit beiden Perioden
# -----------------------------------------------------------------------------
zellen_beide <- raster_indikatoren |>
  count(id_500, name = "n_perioden") |>
  filter(n_perioden == 2) |>
  pull(id_500)

cat("Zellen in beiden Perioden (Analysepopulation):", length(zellen_beide), "\n")

analyse_basis <- raster_indikatoren |>
  filter(id_500 %in% zellen_beide)

# -----------------------------------------------------------------------------
# 4. Teil A - Clustering-Input (UF1)
# -----------------------------------------------------------------------------
# Niveauwerte der Periode t1 als Basis-Typologie. Die neun Clustering-Variablen
# werden z-standardisiert; die Rohwerte bleiben erhalten (sie werden in
# Skript 05 fuer die Beschreibung der Clusterprofile in Originaleinheiten
# gebraucht). Die standardisierten Spalten tragen das Praefix "z_".
cluster_input <- analyse_basis |>
  filter(period == "t1") |>
  select(id_500, all_of(cluster_vars)) |>
  mutate(across(all_of(cluster_vars), z_standard, .names = "z_{.col}"))

# -----------------------------------------------------------------------------
# 5. Teil B - Veraenderungs-Input (UF2)
# -----------------------------------------------------------------------------
# 5.1 Score-Indikatoren je Zelle in t1- und t2-Spalten nebeneinander bringen.
#     Aus jeder Zelle (zwei Zeilen: t1, t2) wird eine Zeile mit den Spalten
#     <indikator>_t1 und <indikator>_t2.
veraenderung_wide <- analyse_basis |>
  select(id_500, period, all_of(score_vars)) |>
  pivot_wider(names_from = period, values_from = all_of(score_vars))

# 5.2 Veraenderung t1 -> t2 berechnen (positiver Wert = Zunahme = Aufwertung).
veraenderung <- veraenderung_wide |>
  mutate(
    delta_anteil_tertiaer      = anteil_tertiaer_t2      - anteil_tertiaer_t1,
    delta_erwerbstaetigenquote = erwerbstaetigenquote_t2 - erwerbstaetigenquote_t1,
    delta_anteil_20_44         = anteil_20_44_t2         - anteil_20_44_t1,
    delta_anteil_einpersonen   = anteil_einpersonen_t2   - anteil_einpersonen_t1,
    delta_median_miete_zimmer  = median_miete_zimmer_t2  - median_miete_zimmer_t1,
    delta_anteil_neubau        = anteil_neubau_t2        - anteil_neubau_t1
  ) |>
  select(id_500, starts_with("delta_"))

# 5.3 Veraenderungswerte z-standardisieren.
#     Wichtig fuer die Interpretation: Der z-Wert misst die Veraenderung einer
#     Zelle RELATIV zur durchschnittlichen Veraenderung aller Zellen. Ein
#     positiver z-Wert bedeutet "staerker aufgewertet als der Durchschnitt".
#     Erst diese gemeinsame Skala erlaubt es, die sechs Indikatoren in Skript 06
#     zu einem sozialen und einem baulichen Aufwertungs-Score zu kombinieren.
#     Die Veraenderung der Miete kann fehlen, wenn in mindestens einer Periode
#     kein belastbarer Mietmedian vorlag (siehe Diagnose 6b).
veraenderung <- veraenderung |>
  mutate(across(starts_with("delta_"), z_standard, .names = "z_{.col}"))

# -----------------------------------------------------------------------------
# 6. Diagnose
# -----------------------------------------------------------------------------
# 6a. Clustering-Input: Vollstaendigkeit und Kontrolle der Standardisierung.
#     Die z-Variablen muessen Mittelwert ~0 und Standardabweichung ~1 haben.
cat("cluster_input - Zeilen:", nrow(cluster_input),
    " fehlende Werte:", sum(is.na(cluster_input)), "\n")

z_kontrolle <- tibble(
  variable   = paste0("z_", cluster_vars),
  mittelwert = round(sapply(cluster_input[paste0("z_", cluster_vars)], mean), 4),
  sd         = round(sapply(cluster_input[paste0("z_", cluster_vars)], sd), 4)
)
z_kontrolle

# 6b. Veraenderungsdatensatz: fehlende Werte je Veraenderungsvariable.
#     Erwartet ist, dass nur die Mietveraenderung fehlende Werte aufweist
#     (Zellen ohne belastbaren Mietmedian in t1 und/oder t2). Skript 06
#     beruecksichtigt dies bei der Bildung des baulichen Scores.
veraenderung |>
  summarise(across(starts_with("delta_"), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na")

# 6c. Korrelationen der neun Clustering-Variablen (vor dem Clustering).
#     Starke Korrelationen - etwa zwischen Bautyp, Mieteranteil und Zimmerzahl -
#     sind sachlogisch erwartbar und fuer eine explorative Typologie vertretbar.
#     Sehr hohe Werte (|r| nahe 1) waeren ein Hinweis, eine Variable zu
#     entfernen. (z-Standardisierung veraendert Korrelationen nicht, daher hier
#     auf den Rohwerten.)
cluster_input |>
  select(all_of(cluster_vars)) |>
  cor() |>
  round(2)

# 6d. Strukturueberblick der beiden Ausgabedatensaetze.
glimpse(cluster_input)
glimpse(veraenderung)

# -----------------------------------------------------------------------------
# 7. Speichern
# -----------------------------------------------------------------------------
saveRDS(cluster_input, file.path(path_processed, "04_cluster_input.rds"))
saveRDS(veraenderung,  file.path(path_processed, "04_veraenderung.rds"))
