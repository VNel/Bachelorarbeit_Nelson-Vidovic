# =============================================================================
# Skript 05 - Clustering: Nachbarschaftstypen (UF1)
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript beantwortet Unterfrage 1: Welche Nachbarschaftstypen lassen
#   sich auf dem 500-m-Raster anhand der sozio-strukturellen Merkmale
#   unterscheiden? Dazu werden die 7'145 Zellen mit dem K-Means-Verfahren
#   gruppiert - auf Basis der neun z-standardisierten Niveau-Variablen der
#   Periode t1 (Basis-Typologie aus Skript 04).
#
#   Ablauf:
#     1. Bestimmung einer geeigneten Clusterzahl k (Elbow- und
#        Silhouetten-Methode).
#     2. K-Means-Clustering mit dem gewaehlten k.
#     3. Beschreibung der Cluster ueber ihre Merkmalsprofile - das ist die
#        eigentliche Antwort auf UF1: die Clusternummern erhalten erst durch
#        die Profile eine inhaltliche Bedeutung.
#
#   K-Means arbeitet distanzbasiert; die Eingangsvariablen sind deshalb bereits
#   in Skript 04 z-standardisiert worden (Mittelwert 0, SD 1), damit keine
#   Variable das Ergebnis aufgrund ihres Wertebereichs dominiert.
#
#   Folgeskripte: 06 (Aufwertung) und 07 (raeumliche Analyse) verknuepfen die
#   Clusterzugehoerigkeit ueber id_500; 08 erzeugt die Karten.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
cluster_input <- readRDS(file.path(path_processed, "04_cluster_input.rds"))

# -----------------------------------------------------------------------------
# 1. Parameter
# -----------------------------------------------------------------------------
# Anzahl Cluster. Der Wert wird anhand der Diagnose in Abschnitt 3 (Elbow- und
# Silhouetten-Methode) gewaehlt. 5 ist ein Startwert fuer eine interpretierbare
# Nachbarschaftstypologie - nach Betrachten der beiden Diagramme anpassen
# und das Skript erneut ausfuehren.
k_final <- 5

# Namen der neun Clustering-Variablen, aus den z-Spalten abgeleitet (ohne
# Praefix). So bleibt die Liste automatisch konsistent mit Skript 04.
cluster_vars <- cluster_input |>
  select(starts_with("z_")) |>
  names() |>
  str_remove("^z_")

# -----------------------------------------------------------------------------
# 2. Clustering-Matrix vorbereiten
# -----------------------------------------------------------------------------
# K-Means benoetigt eine rein numerische Matrix. Verwendet werden die neun
# z-standardisierten Variablen (Praefix z_); id_500 ist nur der Schluessel und
# geht nicht in die Berechnung ein.
cluster_matrix <- cluster_input |>
  select(starts_with("z_")) |>
  as.matrix()

cat("Clustering-Matrix:", nrow(cluster_matrix), "Zellen x",
    ncol(cluster_matrix), "Variablen\n")
cat("Fehlende Werte in der Matrix:", sum(is.na(cluster_matrix)), "\n")  # 0

# -----------------------------------------------------------------------------
# 3. Bestimmung der Clusterzahl k
# -----------------------------------------------------------------------------
# Die "richtige" Clusterzahl ist nicht eindeutig bestimmbar; zwei etablierte
# Verfahren liefern Anhaltspunkte:
#  - Elbow-Methode: Gesamt-Streuung innerhalb der Cluster (WSS) in Abhaengigkeit
#    von k. Der "Knick" markiert den Punkt, ab dem zusaetzliche Cluster nur noch
#    wenig zusaetzliche Erklaerung bringen.
#  - Silhouetten-Methode: durchschnittliche Silhouettenbreite je k. Hoehere
#    Werte stehen fuer klarer voneinander getrennte Cluster.
# Beide Diagramme werden angezeigt und als Abbildung gespeichert; k_final
# (Abschnitt 1) wird auf ihrer Grundlage bewusst gewaehlt.
# Hinweis: Die Silhouetten-Berechnung kann ein bis zwei Minuten dauern.
set.seed(2026)
elbow_plot <- fviz_nbclust(cluster_matrix, kmeans, method = "wss",
                           k.max = 10, nstart = 25, iter.max = 100, algorithm = "MacQueen") +
  labs(title = "Elbow-Methode: Streuung innerhalb der Cluster")
elbow_plot

set.seed(2026)
silhouette_plot <- fviz_nbclust(cluster_matrix, kmeans, method = "silhouette",
                                k.max = 10, nstart = 25, iter.max = 100, algorithm = "MacQueen") +
  labs(title = "Silhouetten-Methode")
silhouette_plot

ggsave(file.path(path_figures, "05_elbow.png"), elbow_plot,
       width = 7, height = 4.5, dpi = 300)
ggsave(file.path(path_figures, "05_silhouette.png"), silhouette_plot,
       width = 7, height = 4.5, dpi = 300)

# -----------------------------------------------------------------------------
# 4. K-Means-Clustering
# -----------------------------------------------------------------------------
# nstart = 25: Der Algorithmus wird 25-mal mit unterschiedlichen zufaelligen
# Startzentren ausgefuehrt; behalten wird die Loesung mit der geringsten
# Streuung. Das macht das Ergebnis stabil und unabhaengig von einem einzelnen
# zufaelligen Start. set.seed sichert die Reproduzierbarkeit der
# Zufallskomponente. iter.max = 100 stellt sicher, dass der Algorithmus
# konvergiert.
set.seed(2026)

km <- kmeans(cluster_matrix, centers = k_final, nstart = 25,
             iter.max = 100, algorithm = "MacQueen")

# Clusterzugehoerigkeit an die Zellen anhaengen. km$cluster steht in derselben
# Reihenfolge wie die Matrixzeilen, also wie cluster_input.
cluster_ergebnis <- cluster_input |>
  mutate(cluster = factor(km$cluster))

# -----------------------------------------------------------------------------
# 5. Clusterprofile - inhaltliche Interpretation (Antwort auf UF1)
# -----------------------------------------------------------------------------
# Die Clusternummern (1..k) sind willkuerliche Bezeichnungen ohne Rangordnung.
# Erst die Merkmalsprofile machen die Nachbarschaftstypen interpretierbar.
# Zwei Sichten:
#  - cluster_profil   : Mittelwerte in Originaleinheiten - absolute Beschreibung
#    jedes Typs (Grundlage fuer die Benennung der Typen im Ergebnisteil).
#  - cluster_profil_z : Mittelwerte der z-Werte - zeigt, in welchen Merkmalen
#    ein Cluster ueber (positiver Wert) oder unter (negativer Wert) dem
#    Gesamtdurchschnitt aller Zellen liegt; erleichtert das Erkennen der
#    praegenden Merkmale eines Typs.

# Profile in Originaleinheiten
cluster_profil <- cluster_ergebnis |>
  group_by(cluster) |>
  summarise(
    n_zellen = n(),
    across(all_of(cluster_vars), ~ round(mean(.x), 3)),
    .groups = "drop"
  )
cluster_profil

# Profile als z-Werte (Abweichung vom Gesamtdurchschnitt)
cluster_profil_z <- cluster_ergebnis |>
  group_by(cluster) |>
  summarise(across(starts_with("z_"), ~ round(mean(.x), 2)),
            .groups = "drop")
cluster_profil_z

# -----------------------------------------------------------------------------
# 6. Diagnose und Guete
# -----------------------------------------------------------------------------
# 6a. Clustergroessen - eine sehr ungleiche Verteilung (z. B. ein winziger
#     Cluster) ist ein Hinweis, die Clusterzahl zu ueberdenken.
cluster_ergebnis |> count(cluster, name = "n_zellen")

# 6b. Guete der Loesung: Anteil der durch das Clustering erklaerten Streuung
#     (between_SS / total_SS). Hoehere Werte bedeuten kompaktere, klarer
#     getrennte Cluster.
cat("Erklaerte Streuung (between_SS / total_SS):",
    round(km$betweenss / km$totss, 3), "\n")

# 6c. Visualisierung der Cluster im Merkmalsraum. fviz_cluster projiziert die
#     neun Variablen auf die zwei wichtigsten Hauptkomponenten und faerbt die
#     Zellen nach Cluster - eine methodische Kontrolle, ob sich die Cluster
#     ueberhaupt voneinander abheben. (Die geografische Karte folgt in Skript 08.)
cluster_featureraum <- fviz_cluster(
  km, data = cluster_matrix, geom = "point", pointsize = 0.6,
  ellipse.type = "norm"
) +
  labs(title = "Cluster im Merkmalsraum (Hauptkomponenten-Projektion)")
cluster_featureraum

ggsave(file.path(path_figures, "05_cluster_featureraum.png"),
       cluster_featureraum, width = 7, height = 5, dpi = 300)

# 6d. Strukturueberblick des Ergebnisdatensatzes.
glimpse(cluster_ergebnis)

# -----------------------------------------------------------------------------
# 7. Speichern
# -----------------------------------------------------------------------------
# cluster_ergebnis : alle Zellen mit ihrer Clusterzugehoerigkeit (id_500 dient
#                    den Folgeskripten als Verknuepfungsschluessel).
# cluster_profil   : die Typologie-Tabelle (Antwort auf UF1).
saveRDS(cluster_ergebnis, file.path(path_processed, "05_cluster_ergebnis.rds"))
saveRDS(cluster_profil,   file.path(path_processed, "05_cluster_profil.rds"))

