# =============================================================================
# Skript 05b - Vergleich verschiedener Clusterzahlen (Dokumentation)
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Elbow- und Silhouetten-Methode liefern fuer die vorliegenden Daten kein
#   eindeutiges k: Die Silhouette favorisiert k = 2 - fuer eine inhaltlich
#   aussagekraeftige Typologie zu grob -, der Elbow-Verlauf ist glatt. Die Wahl
#   der Clusterzahl wird daher zusaetzlich ueber die Interpretierbarkeit
#   begruendet. Dieses Skript fuehrt das Clustering fuer die Kandidaten
#   k = 2, 3, 4 und 5 aus und exportiert Profile, Kennzahlen und Abbildungen,
#   damit die Wahl von k transparent dokumentiert werden kann.
#
#   Dieses Skript ersetzt Skript 05 NICHT. Es dient nur der Dokumentation des
#   Variantenvergleichs; das endgueltige Clustering mit dem gewaehlten k bleibt
#   Skript 05.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten und Vorbereitung
# -----------------------------------------------------------------------------
cluster_input <- readRDS(file.path(path_processed, "04_cluster_input.rds"))

# Namen der neun Clustering-Variablen (aus den z-Spalten abgeleitet).
cluster_vars <- cluster_input |>
  select(starts_with("z_")) |>
  names() |>
  str_remove("^z_")

# Numerische Matrix der z-standardisierten Variablen.
cluster_matrix <- cluster_input |>
  select(starts_with("z_")) |>
  as.matrix()

# Zu vergleichende Clusterzahlen.
k_kandidaten <- c(2, 3, 4, 5)

# -----------------------------------------------------------------------------
# 1. Clustering je Kandidat: Profile, Kennzahlen, Merkmalsraum-Plot
# -----------------------------------------------------------------------------
# Algorithmus: MacQueen (MacQueen 1967) mit erhoehter Iterationszahl. Der
# R-Standard (Hartigan-Wong) erzeugt auf diesen Daten eine Konvergenzwarnung;
# MacQueen vermeidet dies und ist zugleich das in der Arbeit zitierte Verfahren.
profile_export <- list()
kennzahlen     <- tibble()

for (k in k_kandidaten) {
  set.seed(2026)
  km <- kmeans(cluster_matrix, centers = k, nstart = 25,
               iter.max = 100, algorithm = "MacQueen")
  
  ergebnis <- cluster_input |> mutate(cluster = factor(km$cluster))
  
  # z-Profil je Cluster, transponiert: Merkmale als Zeilen, Cluster als
  # Spalten. So bleibt die Tabelle auch fuer groessere k im Word lesbar.
  profil <- ergebnis |>
    group_by(cluster) |>
    summarise(n_zellen = n(),
              across(starts_with("z_"), ~ round(mean(.x), 2)),
              .groups = "drop") |>
    pivot_longer(-cluster, names_to = "Merkmal", values_to = "wert") |>
    pivot_wider(names_from = cluster, values_from = wert,
                names_prefix = "Cluster ")
  profile_export[[paste0("Clusterprofile k = ", k)]] <- profil
  
  # Kennzahlen. Hinweis: Die erklaerte Streuung steigt mit k zwangslaeufig an
  # und ist daher kein Auswahlkriterium fuer sich, sondern nur beschreibend.
  kennzahlen <- bind_rows(kennzahlen, tibble(
    k                  = k,
    erklaerte_streuung = round(km$betweenss / km$totss, 3),
    kleinster_cluster  = min(km$size),
    groesster_cluster  = max(km$size)
  ))
  
  # Merkmalsraum-Plot (Projektion auf die zwei wichtigsten Hauptkomponenten).
  feature_plot <- fviz_cluster(km, data = cluster_matrix, geom = "point",
                               pointsize = 0.5, ellipse.type = "norm") +
    labs(title = paste0("Cluster im Merkmalsraum, k = ", k))
  ggsave(file.path(path_figures, paste0("05b_featureraum_k", k, ".png")),
         feature_plot, width = 7, height = 5, dpi = 300)
}

kennzahlen

# -----------------------------------------------------------------------------
# 2. Profile und Kennzahlen als Word-Dokument exportieren
# -----------------------------------------------------------------------------
# flextable erzeugt formatierte Word-Tabellen. Alle Tabellen werden in EIN
# Dokument geschrieben; der jeweils angegebene Name erscheint als Ueberschrift.
save_as_docx(
  "Kennzahlen je Clusterzahl" =
    flextable(kennzahlen) |> autofit(),
  "Clusterprofile k = 2" =
    flextable(profile_export[["Clusterprofile k = 2"]]) |> autofit(),
  "Clusterprofile k = 3" =
    flextable(profile_export[["Clusterprofile k = 3"]]) |> autofit(),
  "Clusterprofile k = 4" =
    flextable(profile_export[["Clusterprofile k = 4"]]) |> autofit(),
  "Clusterprofile k = 5" =
    flextable(profile_export[["Clusterprofile k = 5"]]) |> autofit(),
  path = file.path(path_tables, "05b_k_vergleich.docx")
)

# -----------------------------------------------------------------------------
# 3. Korrelationsmatrix der Clustering-Variablen exportieren
# -----------------------------------------------------------------------------
# Dokumentiert die Zusammenhaenge der neun finalen Clustering-Variablen.
# corrplot zeichnet in ein Grafikgeraet; daher png() ... dev.off().
cor_matrix <- cluster_input |>
  select(all_of(cluster_vars)) |>
  cor() |>
  round(2)

png(file.path(path_figures, "05b_korrelationsmatrix.png"),
    width = 2000, height = 2000, res = 300)
corrplot(cor_matrix, method = "color", type = "upper", diag = FALSE,
         addCoef.col = "black", number.cex = 0.65,
         tl.col = "black", tl.srt = 45)
dev.off()

# -----------------------------------------------------------------------------
# 4. Hinweis
# -----------------------------------------------------------------------------
cat("Exporte erstellt:\n",
    "- output/tables/05b_k_vergleich.docx\n",
    "- output/figures/05b_featureraum_k2.png / _k3.png / _k4.png / _k5.png\n",
    "- output/figures/05b_korrelationsmatrix.png\n")