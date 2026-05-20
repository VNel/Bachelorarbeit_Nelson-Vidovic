# =============================================================================
# Skript 06 - Aufwertungs-Score (UF2)
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript beantwortet Unterfrage 2: Welche Rasterzellen bzw.
#   Nachbarschaftstypen zeigen im Beobachtungszeitraum eine gleichzeitige
#   soziale und bauliche Aufwertung?
#
#   Vorgehen (Composite-Score-Ansatz in Anlehnung an Liu et al. 2019):
#     1. Aus den z-standardisierten Veraenderungen t1 -> t2 (Skript 04) wird je
#        Dimension ein Aufwertungs-Score gebildet: der Mittelwert der
#        zugehoerigen z-Veraenderungen.
#     2. Eine Zelle gilt als Gentrifizierungskandidat, wenn BEIDE Scores positiv
#        sind - sie sich also in der sozialen UND der baulichen Dimension
#        ueberdurchschnittlich aufgewertet hat.
#     3. Ueber die Clusterzugehoerigkeit aus Skript 05 wird die Aufwertung
#        zusaetzlich je Nachbarschaftstyp ausgewertet (Typen-Teil von UF2).
#
#   Interpretation: Die Scores sind Mittelwerte z-standardisierter
#   Veraenderungen und damit um null zentriert. "Positiv" ist relativ zu
#   verstehen - staerker aufgewertet als die durchschnittliche urbane Zelle.
#   Gemessen werden Aufwertungstendenzen, nicht abgeschlossene Gentrifizierung
#   (siehe Limitationen der Arbeit).
#
#   Folgeskripte: 07 (raeumliche Analyse) prueft die raeumliche Buendelung der
#   Scores; 08 erzeugt die Karten.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
veraenderung     <- readRDS(file.path(path_processed, "04_veraenderung.rds"))
cluster_ergebnis <- readRDS(file.path(path_processed, "05_cluster_ergebnis.rds"))

# -----------------------------------------------------------------------------
# 1. Variablengruppen
# -----------------------------------------------------------------------------
# Die z-standardisierten Veraenderungsvariablen aus Skript 04, getrennt nach
# den beiden Aufwertungsdimensionen.
z_delta_sozial  <- c("z_delta_anteil_tertiaer", "z_delta_erwerbstaetigenquote",
                     "z_delta_anteil_20_44", "z_delta_anteil_einpersonen")
z_delta_baulich <- c("z_delta_median_miete_zimmer", "z_delta_anteil_neubau")

# -----------------------------------------------------------------------------
# 2. Aufwertungs-Scores berechnen
# -----------------------------------------------------------------------------
# Jeder Score ist der zeilenweise Mittelwert der z-standardisierten
# Veraenderungen seiner Dimension. pick() waehlt die zugehoerigen Spalten aus,
# rowMeans bildet den Mittelwert je Zelle.
#  - score_sozial : Mittelwert der vier sozialen z-Veraenderungen. Alle vier
#    sind vollstaendig, der Score existiert daher fuer alle Zellen.
#  - score_baulich: Mittelwert der zwei baulichen z-Veraenderungen. Die
#    Mietveraenderung fehlt fuer Zellen ohne belastbaren Mietmedian in beiden
#    Perioden. Fuer diese Zellen bleibt der bauliche Score bewusst NA (rowMeans
#    ohne na.rm): Der Score wird so nicht stillschweigend auf einen einzelnen
#    Indikator (Neubau) reduziert. Solche Zellen sind nicht klassifizierbar.
aufwertung <- veraenderung |>
  mutate(
    score_sozial  = rowMeans(pick(all_of(z_delta_sozial))),
    score_baulich = rowMeans(pick(all_of(z_delta_baulich)))
  )

# -----------------------------------------------------------------------------
# 3. Klassifikation der Zellen
# -----------------------------------------------------------------------------
# Jede Zelle wird anhand der Vorzeichen beider Scores einem Aufwertungstyp
# zugeordnet. "sozial & baulich" sind die Gentrifizierungskandidaten gemaess
# UF2 (Aufwertung in beiden Dimensionen). Zellen ohne baulichen Score sind
# nicht klassifizierbar.
aufwertung <- aufwertung |>
  mutate(
    aufwertungstyp = case_when(
      is.na(score_baulich)                 ~ "nicht klassifizierbar",
      score_sozial > 0 & score_baulich > 0 ~ "sozial & baulich",
      score_sozial > 0                     ~ "nur sozial",
      score_baulich > 0                    ~ "nur baulich",
      TRUE                                 ~ "keine Aufwertung"
    ),
    aufwertungstyp = factor(aufwertungstyp,
                            levels = c("sozial & baulich", "nur sozial", "nur baulich",
                                       "keine Aufwertung", "nicht klassifizierbar")),
    # Logische Markierung der Kandidaten; NA, wo nicht klassifizierbar.
    ist_kandidat = if_else(is.na(score_baulich), NA,
                           score_sozial > 0 & score_baulich > 0)
  )

# -----------------------------------------------------------------------------
# 4. Clusterzugehoerigkeit anhaengen
# -----------------------------------------------------------------------------
# Die Nachbarschaftstypen aus Skript 05 werden ueber id_500 angefuegt, um die
# Aufwertung je Typ auswerten zu koennen.
aufwertung <- aufwertung |>
  left_join(cluster_ergebnis |> select(id_500, cluster), by = "id_500")

# -----------------------------------------------------------------------------
# 5. Auswertungstabellen
# -----------------------------------------------------------------------------
# 5.1 Durchschnittliche Veraenderung je Indikator - deskriptiver Ueberblick,
#     wie sich die sechs Score-Indikatoren im Mittel veraendert haben (in
#     Originaleinheiten: Anteile bzw. CHF pro Zimmer).
veraenderung_uebersicht <- veraenderung |>
  summarise(across(starts_with("delta_"),
                   ~ round(mean(.x, na.rm = TRUE), 4))) |>
  pivot_longer(everything(), names_to = "indikator",
               values_to = "mittlere_veraenderung")
veraenderung_uebersicht

# 5.2 Verteilung der Aufwertungstypen ueber alle Zellen.
klassifikation_uebersicht <- aufwertung |>
  count(aufwertungstyp, name = "n_zellen") |>
  mutate(anteil = round(n_zellen / sum(n_zellen), 3))
klassifikation_uebersicht

# 5.3 Aufwertung je Nachbarschaftstyp. anteil_kandidaten bezieht sich auf die
#     klassifizierbaren Zellen des jeweiligen Clusters (ohne die nicht
#     klassifizierbaren).
aufwertung_nach_cluster <- aufwertung |>
  group_by(cluster) |>
  summarise(
    n_zellen                = n(),
    n_nicht_klassifizierbar = sum(is.na(ist_kandidat)),
    mittel_score_sozial     = round(mean(score_sozial), 3),
    mittel_score_baulich    = round(mean(score_baulich, na.rm = TRUE), 3),
    n_kandidaten            = sum(ist_kandidat, na.rm = TRUE),
    anteil_kandidaten       = round(mean(ist_kandidat, na.rm = TRUE), 3),
    .groups = "drop"
  )
aufwertung_nach_cluster

# Konsolenuebersicht
cat("Zellen gesamt:", nrow(aufwertung),
    "| klassifizierbar:", sum(!is.na(aufwertung$ist_kandidat)),
    "| Gentrifizierungskandidaten:", sum(aufwertung$ist_kandidat, na.rm = TRUE),
    "\n")

# Deskriptive Streuungskennzahlen der beiden Scores (fuer den Ergebnistext).
score_kennzahlen <- tibble(
  score   = c("sozial", "baulich"),
  mittel  = c(round(mean(aufwertung$score_sozial), 3),
              round(mean(aufwertung$score_baulich, na.rm = TRUE), 3)),
  sd      = c(round(sd(aufwertung$score_sozial), 3),
              round(sd(aufwertung$score_baulich, na.rm = TRUE), 3)),
  minimum = c(round(min(aufwertung$score_sozial), 3),
              round(min(aufwertung$score_baulich, na.rm = TRUE), 3)),
  maximum = c(round(max(aufwertung$score_sozial), 3),
              round(max(aufwertung$score_baulich, na.rm = TRUE), 3))
)
score_kennzahlen

# 5.4 Export der drei Tabellen in ein Word-Dokument.
save_as_docx(
  "Durchschnittliche Veraenderung je Indikator" =
    flextable(veraenderung_uebersicht) |> autofit(),
  "Verteilung der Aufwertungstypen" =
    flextable(klassifikation_uebersicht) |> autofit(),
  "Aufwertung je Nachbarschaftstyp" =
    flextable(aufwertung_nach_cluster) |> autofit(),
  path = file.path(path_tables, "06_aufwertung.docx")
)

# -----------------------------------------------------------------------------
# 6. Visualisierungen
# -----------------------------------------------------------------------------
# 6.1 Streudiagramm der beiden Scores. Die gestrichelten Nulllinien teilen die
#     Ebene in vier Quadranten; oben rechts (beide Scores positiv) liegen die
#     Gentrifizierungskandidaten. Nicht klassifizierbare Zellen sind hier
#     ausgeblendet.
scatter_plot <- aufwertung |>
  filter(!is.na(score_baulich)) |>
  ggplot(aes(x = score_sozial, y = score_baulich, colour = aufwertungstyp)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(alpha = 0.4, size = 0.8) +
  labs(title = "Soziale und bauliche Aufwertung je Rasterzelle",
       x = "Sozialer Aufwertungs-Score",
       y = "Baulicher Aufwertungs-Score",
       colour = "Aufwertungstyp") +
  theme_minimal()
scatter_plot
ggsave(file.path(path_figures, "06_aufwertung_scatter.png"),
       scatter_plot, width = 7, height = 5.5, dpi = 300, bg = "white")

# 6.2 Anteil Gentrifizierungskandidaten je Nachbarschaftstyp - zeigt, welche
#     Typen ueberdurchschnittlich haeufig aufwerten.
cluster_balken <- aufwertung_nach_cluster |>
  ggplot(aes(x = cluster, y = anteil_kandidaten)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = scales::percent(anteil_kandidaten, accuracy = 0.1)),
            vjust = -0.4, size = 3) +
  labs(title = "Anteil Gentrifizierungskandidaten je Nachbarschaftstyp",
       x = "Nachbarschaftstyp (Cluster)",
       y = "Anteil Kandidaten") +
  theme_minimal()
cluster_balken
ggsave(file.path(path_figures, "06_kandidaten_je_cluster.png"),
       cluster_balken, width = 7, height = 4.5, dpi = 300, bg = "white")

# -----------------------------------------------------------------------------
# 7. Speichern
# -----------------------------------------------------------------------------
# aufwertung enthaelt je Zelle die beiden Scores, die Klassifikation und die
# Clusterzugehoerigkeit - Grundlage fuer die raeumliche Analyse (Skript 07)
# und die Karten (Skript 08).
saveRDS(aufwertung, file.path(path_processed, "06_aufwertung.rds"))

