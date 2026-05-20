# =============================================================================
# BACHELORARBEIT  -  "Machine Learning for Gentrification"
# Skript 02  -  ZUSAMMENFUEHRUNG ZUM ANALYSEDATENSATZ
# -----------------------------------------------------------------------------
# Autor    : Nelson Vidovic
# R-Version: 4.5.0
#
# Zweck dieses Skripts:
#   Aus den in Skript 01 bereinigten Einzeldatensaetzen wird der raeumlich
#   verortete, urbane Analysedatensatz gebildet. Konkret:
#     - die beiden Haushalts- bzw. Haushaltsmitglieder-Datensaetze werden zu
#       je einem Datensatz ueber den Zeitraum 2012-2017 gestapelt,
#     - jedem Haushalt-Jahr wird seine Periode (t1/t2) zugewiesen,
#     - jedem Haushalt-Jahr werden die Rasterzellen (100/500/1000 m) und die
#       Agglomerationszugehoerigkeit angehaengt,
#     - Haushalte ohne Wohnkoordinate und Haushalte ausserhalb von
#       Agglomerationen werden geprueft, dokumentiert und begruendet entfernt.
#
# Input  (aus Skript 01, in Daten/processed/):
#   01_hh_12_15_clean.rds, 01_hh_16_17_clean.rds
#   01_hhm_12_15_clean.rds, 01_hhm_16_17_clean.rds
#   01_hh_coordinates_grid.rds
#   01_agglo.rds
#
# Output (in Daten/processed/):
#   02_hh_analyse.rds   - urbane, verortete Haushalte 2012-2017
#   02_hhm_analyse.rds  - Mitglieder ebendieser Haushalte
#
# Hinweis: Die Mindestfallzahl pro Rasterzelle wird erst bei der Aggregation
#          in Skript 03 angewendet, nicht hier.
# =============================================================================


# =============================================================================
# 0. SETUP UND INPUT
# =============================================================================
source(here::here("R", "00_setup.R"))   # Pakete, Pfade, Optionen, Konstanten

hh_12_15  <- readRDS(file.path(path_processed, "01_hh_12_15_clean.rds"))
hh_16_17  <- readRDS(file.path(path_processed, "01_hh_16_17_clean.rds"))
hhm_12_15 <- readRDS(file.path(path_processed, "01_hhm_12_15_clean.rds"))
hhm_16_17 <- readRDS(file.path(path_processed, "01_hhm_16_17_clean.rds"))
hh_coordinates_grid <- readRDS(file.path(path_processed, "01_hh_coordinates_grid.rds"))
agglo     <- readRDS(file.path(path_processed, "01_agglo.rds"))


# =============================================================================
# 1. DATENSAETZE STAPELN UND PERIODE ZUWEISEN
# =============================================================================

# -----------------------------------------------------------------------------
# 1.1 Haushaltsdatensaetze stapeln
# -----------------------------------------------------------------------------
# Beide Datensaetze haben dieselbe Struktur und decken disjunkte Jahre ab
# (2012-2015 bzw. 2016-2017). bind_rows() haengt sie untereinander an. Es wird
# bewusst nicht dedupliziert: Da householdyearlyid das Jahr enthaelt und die
# Jahre nicht ueberlappen, kann keine Zeile doppelt vorkommen.
hh_12_17 <- bind_rows(hh_12_15, hh_16_17)

# -----------------------------------------------------------------------------
# 1.2 Haushaltsmitglieder-Datensaetze stapeln
# -----------------------------------------------------------------------------
hhm_12_17 <- bind_rows(hhm_12_15, hhm_16_17)

# -----------------------------------------------------------------------------
# 1.3 Periode zuweisen und auf die Analysejahre einschraenken
# -----------------------------------------------------------------------------
# t1 = 2013-2014, t2 = 2016-2017 (zwei 2-Jahres-Pools).
# 2012 (keine Mietdaten) und 2015 (keine Koordinaten) gehoeren keiner
# Periode an und werden hier entfernt.
hh_13_17 <- hh_12_17 |>
  mutate(period = case_when(
    statyear %in% jahre_t1 ~ "t1",
    statyear %in% jahre_t2 ~ "t2"
  )) |>
  filter(!is.na(period))

hh_13_17 |> count(period)


# =============================================================================
# 2. RASTERZUORDNUNG ANHAENGEN
# =============================================================================
# Ziel: Jedem Haushalt-Jahr seine Rasterzelle zuordnen. Verknuepfungsschluessel
# ist householdyearlyid + statyear. Aus hh_coordinates_grid werden nur die
# Zell-IDs uebernommen (100/500/1000 m); die Zentroidkoordinaten liegen bereits
# in den grid_sf-Objekten und werden hier nicht benoetigt -> schlanker Join.

# -----------------------------------------------------------------------------
# 2.1 Schlanke Koordinatentabelle
# -----------------------------------------------------------------------------
coordinates_slim <- hh_coordinates_grid |>
  select(householdyearlyid, statyear, id_100, id_500, id_1000)

# -----------------------------------------------------------------------------
# 2.2 Zell-IDs an die Haushaltsdaten anhaengen
# -----------------------------------------------------------------------------
# left_join: alle Haushalte bleiben vorerst erhalten. Haushalte ohne
# Koordinaten-Treffer erhalten NA in den Zell-IDs. Diese Faelle werden in
# Abschnitt 4 analysiert und in Abschnitt 5 begruendet entfernt.
hh_13_17 <- hh_13_17 |>
  left_join(coordinates_slim, by = c("householdyearlyid", "statyear"))


# =============================================================================
# 3. AGGLOMERATIONEN ANHAENGEN
# =============================================================================
# Die Untersuchung beschraenkt sich gemaess Forschungsfrage auf urbane Gebiete.
# Diese werden ueber die BFS-Agglomerationen abgegrenzt. Jeder Haushalt wird
# ueber seine Wohngemeinde einer Agglomeration zugeordnet.

# -----------------------------------------------------------------------------
# 3.1 Agglomerationsreferenz vorbereiten
# -----------------------------------------------------------------------------
# Nur die benoetigten Spalten; sprechende Namen; Gemeindenummer als integer,
# damit der Join-Schluessel denselben Typ hat wie res_mun in den Haushaltsdaten.
agglo_ref <- agglo |>
  transmute(
    bfs_gde_nummer       = as.integer(bfs_gde_nummer),
    gemeindename,
    agglo_id             = str_trim(agglomerationen_2012_8),
    agglo_groessenklasse = str_trim(agglomerationsgrossenklasse)
  )

# -----------------------------------------------------------------------------
# 3.2 Gemeindenummern harmonisieren
# -----------------------------------------------------------------------------
# Durch Gemeindefusionen weichen Wohngemeindenummern der Strukturerhebung vom
# Gemeindestand der Agglomerationsreferenz ab. Die Harmonisierung erfolgt
# automatisch ueber die offizielle BFS-Mutationsliste ("Mutierte Gemeinden",
# Zeitraum 2011-2026), nicht von Hand.
# Voraussetzung: Die Datei liegt unter Daten/Mutierte_Gemeinden.xlsx.

# (a) BFS-Mutationsliste einlesen. Die Datei hat eine zweizeilige Kopfzeile
#     (Vorgaenger / Nachfolger), daher 2 Zeilen ueberspringen und die 10
#     Spalten selbst benennen. Reine Namensaenderungen (alte Nr = neue Nr)
#     sind fuer die Harmonisierung irrelevant und werden entfernt.
mutationen <- read_excel(
  file.path(path_raw, "Mutierte_Gemeinden.xlsx"),
  sheet     = "Daten",
  skip      = 2,
  col_names = c("mutationsnummer",
                "kanton_v", "bezirk_v", "gde_alt", "gdename_v",
                "kanton_n", "bezirk_n", "gde_neu", "gdename_n",
                "datum")
) |>
  transmute(
    gde_alt = as.integer(gde_alt),
    gde_neu = as.integer(gde_neu)
  ) |>
  filter(!is.na(gde_alt), !is.na(gde_neu), gde_alt != gde_neu) |>
  distinct(gde_alt, gde_neu)

# Kontrolle: Jede alte Nummer darf nur EINE Nachfolgenummer haben.
mutationen |> count(gde_alt) |> filter(n > 1)        # Erwartung: 0 Zeilen

# (b) Aufloesungsfunktion. Jede Gemeindenummer wird Schritt fuer Schritt auf
#     ihren Nachfolger umgeschluesselt, bis sie im Gemeindestand der
#     Agglomerationsreferenz vorkommt. So werden auch mehrstufige Fusionen
#     (A -> B -> C) korrekt aufgeloest, ohne ueber den Stand der Referenz
#     hinaus zu harmonisieren.
gde_in_referenz <- agglo_ref$bfs_gde_nummer

resolve_gemeinde <- function(nummern, mut, referenz, max_iter = 20) {
  aktuell <- nummern
  for (i in seq_len(max_iter)) {
    offen <- !(aktuell %in% referenz) & (aktuell %in% mut$gde_alt)
    if (!any(offen, na.rm = TRUE)) break
    idx     <- match(aktuell, mut$gde_alt)
    aktuell <- if_else(offen, mut$gde_neu[idx], aktuell)
  }
  aktuell
}

# (c) Korrekturtabelle aufbauen: pro vorkommender Wohngemeinde die
#     harmonisierte Nummer; behalten werden nur tatsaechliche Aenderungen.
gemeinde_korrektur <- hh_13_17 |>
  distinct(res_mun) |>
  filter(!is.na(res_mun)) |>
  mutate(res_mun_harm = resolve_gemeinde(res_mun, mutationen, gde_in_referenz)) |>
  filter(res_mun != res_mun_harm)

gemeinde_korrektur                                   # umgeschluesselte Gemeinden

# (d) Harmonisierte Gemeindenummer an die Haushaltsdaten anhaengen.
hh_13_17 <- hh_13_17 |>
  left_join(gemeinde_korrektur, by = "res_mun") |>
  mutate(res_mun_harm = coalesce(res_mun_harm, res_mun))

# -----------------------------------------------------------------------------
# 3.3 Agglomerationszugehoerigkeit anhaengen
# -----------------------------------------------------------------------------
# Join ueber die (harmonisierte) Wohngemeindenummer.
hh_13_17 <- hh_13_17 |>
  left_join(agglo_ref, by = c("res_mun_harm" = "bfs_gde_nummer"))

# -----------------------------------------------------------------------------
# 3.4 Diagnose: nicht zugeordnete Gemeinden
# -----------------------------------------------------------------------------
# Nach der Harmonisierung sollten praktisch keine Gemeinden mehr offen sein.
# Erwartet bleibt ein kleiner Rest von Gemeinden, die NACH dem Stand der
# Agglomerationsreferenz neu entstanden sind (Fusionen ab 2020: Thurnen,
# Villaz, Prez, Verzasca). Diese liegen alle im laendlichen Raum und werden
# vom Agglomerationsfilter in Abschnitt 5 ohnehin ausgeschlossen.
agglo_na <- hh_13_17 |>
  filter(is.na(agglo_id)) |>
  count(res_mun_harm, name = "n_haushalte") |>
  arrange(desc(n_haushalte))

agglo_na
sum(agglo_na$n_haushalte)


# -----------------------------------------------------------------------------
# 3.5 Diagnose: urbane vs. nicht-urbane Haushalte
# -----------------------------------------------------------------------------
# Ueberblick ueber die Agglomerationszuordnung, bevor in Abschnitt 5 gefiltert
# wird. Drei Gruppen:
#   "Agglomeration"       : echte Agglomeration            -> wird behalten
#   "keine Agglomeration" : Code "0", nicht-urban           -> wird entfernt
#   "nicht zugeordnet"    : agglo_id ist NA (4 Gemeinden ab 2020, siehe 3.4)
hh_status <- hh_13_17 |>
  transmute(
    period,
    agglo_status = case_when(
      is.na(agglo_id) ~ "nicht zugeordnet (NA)",
      agglo_id == "0" ~ "keine Agglomeration (Code 0)",
      TRUE            ~ "Agglomeration"
    )
  )

# Gesamtuebersicht: wie viele Haushalte fallen weg, wie viele bleiben?
hh_status |>
  count(agglo_status) |>
  mutate(anteil = round(n / sum(n), 4))

# Dieselbe Aufteilung je Periode: zeigt die urbane Stichprobengroesse
# in t1 und t2 und ob das Verhaeltnis zwischen den Perioden stabil ist.
hh_status |>
  count(period, agglo_status) |>
  group_by(period) |>
  mutate(anteil = round(n / sum(n), 4)) |>
  ungroup()


# =============================================================================
# 4. DIAGNOSE DER KOORDINATEN-ABDECKUNG
# =============================================================================
# Befund: hh_coordinates_grid deckt weniger Haushalt-Jahre ab als die
# Haushaltsdaten. Bevor Faelle entfernt werden, wird das Ausmass praezise
# gemessen und geprueft, ob sich die nicht verorteten Haushalte systematisch
# von den verorteten unterscheiden (Verzerrungspruefung).

# Hilfsmerkmal: hat dieser Haushalt eine Rasterzelle?
hh_13_17 <- hh_13_17 |>
  mutate(hat_koordinaten = !is.na(id_500))

# -----------------------------------------------------------------------------
# 4.1 Abdeckung pro Jahr und Periode
# -----------------------------------------------------------------------------
# Zeigt, ob fehlende Koordinaten gleichmaessig verteilt sind oder sich auf
# einzelne Jahre konzentrieren (Letzteres waere ein strukturelles Datenproblem).
abdeckung_jahr <- hh_13_17 |>
  group_by(statyear) |>
  summarise(
    n_haushalte  = n(),
    n_mit_koord  = sum(hat_koordinaten),
    n_ohne_koord = sum(!hat_koordinaten),
    anteil_ohne  = round(n_ohne_koord / n_haushalte, 4),
    .groups = "drop"
  )
abdeckung_jahr

abdeckung_periode <- hh_13_17 |>
  group_by(period) |>
  summarise(
    n_haushalte  = n(),
    n_ohne_koord = sum(!hat_koordinaten),
    anteil_ohne  = round(n_ohne_koord / n_haushalte, 4),
    .groups = "drop"
  )
abdeckung_periode

# -----------------------------------------------------------------------------
# 4.2 Gegenpruefung: Koordinaten ohne passenden Haushalt
# -----------------------------------------------------------------------------
# Falls die Koordinatentabelle Haushalt-Jahre enthaelt, die in den bereinigten
# Haushaltsdaten gar nicht vorkommen, deutet das auf unterschiedliche
# Grundgesamtheiten der Datenlieferungen hin.
koord_ohne_hh <- coordinates_slim |>
  anti_join(hh_13_17, by = c("householdyearlyid", "statyear"))

nrow(koord_ohne_hh)                          # idealerweise 0 oder sehr klein
koord_ohne_hh |>
  count(statyear)                           # beinhaltet nun alle Haushalte aus dem 2012, da es zum Jahr 2015 keine Koordinaten gab und ich dann die Zeiträume t1 und t2 anpassen musste

# -----------------------------------------------------------------------------
# 4.3 Verzerrungspruefung: verortete vs. nicht verortete Haushalte
# -----------------------------------------------------------------------------
# Das Entfernen nicht verorteter Haushalte ist nur dann unkritisch, wenn diese
# sich strukturell nicht systematisch von den verorteten unterscheiden.
# Das ist nach einer Anpassung des Untersuchungszeitraumes t1 und t2 nun der Fall.
# vorher: t1 = 2012-2014, t2 = 2015-2017; Nacher/Jetzt: t1 = 2013-2014, t2 = 2016-2017

# (a) Wohnungsgroesse und Mietniveau
hh_13_17 |>
  group_by(hat_koordinaten) |>
  summarise(
    n             = n(),
    mittel_zimmer = round(mean(numberofrooms, na.rm = TRUE), 2),
    median_zimmer = median(numberofrooms, na.rm = TRUE),
    median_miete  = median(rentnet, na.rm = TRUE),
    .groups = "drop"
  )

# (b) Eigentumsverhaeltnis (Anteile je Gruppe)
hh_13_17 |>
  count(hat_koordinaten, typeofownership) |>
  group_by(hat_koordinaten) |>
  mutate(anteil = round(n / sum(n), 3)) |>
  ungroup()

# (c) Haushaltstyp (Anteile je Gruppe)
hh_13_17 |>
  count(hat_koordinaten, hh_type_bfs_agg) |>
  group_by(hat_koordinaten) |>
  mutate(anteil = round(n / sum(n), 3)) |>
  ungroup()

# (d) Stadt/Land: Liegen nicht verortete Haushalte ueberproportional ausserhalb
#     von Agglomerationen? Falls ja, betrifft der Verlust v. a. den ohnehin
#     ausgeschlossenen laendlichen Raum -> fuer die urbane Analyse entlastend.
hh_13_17 |>
  mutate(in_agglo = !is.na(agglo_id) & agglo_id != "0") |>
  count(hat_koordinaten, in_agglo) |>
  group_by(hat_koordinaten) |>
  mutate(anteil = round(n / sum(n), 3)) |>
  ungroup()

# =============================================================================
# 5. ANALYSEDATENSATZ BILDEN
# =============================================================================
# Aus dem vollstaendigen Haushaltsdatensatz wird der finale Analysedatensatz
# gebildet. Zwei Filter werden angewendet und transparent dokumentiert:
#
#  Filter A - nur raeumlich verortete Haushalte (Rasterzelle vorhanden)
#    Begruendung: Die gesamte Analyse findet auf Rasterebene statt
#    (Aggregation, Clustering UF1, Veraenderungsanalyse UF2, raeumliche
#    Statistik UF3). Ein Haushalt ohne Wohnkoordinate kann keiner Zelle
#    zugeordnet werden und zu keinem dieser Schritte beitragen. Eine
#    Imputation der Lage waere eine Erfindung raeumlicher Information und
#    wird nicht vorgenommen.
#
#  Filter B - nur Haushalte in Agglomerationen (agglo_id != "0", nicht NA)
#    Begruendung: Die Forschungsfrage bezieht sich explizit auf urbane
#    Gebiete. Diese werden ueber die BFS-Agglomerationen operationalisiert.
#    Der Code "0" bezeichnet Gemeinden, die weder zu einer Agglomeration noch
#    zu den Kerngemeinden ausserhalb von Agglomerationen gehoeren; sie liegen
#    ausserhalb des Untersuchungsgegenstands.

# -----------------------------------------------------------------------------
# 5.1 Filtertrichter (Dokumentation des Stichprobenverlusts)
# -----------------------------------------------------------------------------
n_start     <- nrow(hh_13_17)
n_mit_koord <- hh_13_17 |>
  filter(hat_koordinaten) |>
  nrow()
n_agglo     <- hh_13_17 |>
  filter(hat_koordinaten, !is.na(agglo_id), agglo_id != "0") |>
  nrow()

filtertrichter <- tibble(
  schritt     = c("Ausgangsbestand 2013+2014 & 2016+2017",
                  "nach Filter A: mit Rasterzelle",
                  "nach Filter B: in Agglomeration"),
  n_haushalte = c(n_start, n_mit_koord, n_agglo)
) |>
  mutate(
    entfernt   = n_start - n_haushalte,
    anteil_kum = round(n_haushalte / n_start, 4)
  )
filtertrichter

# Tabelle in Word-Dokument speichern
filtertrichter |>
  flextable() |>
  set_header_labels(schritt = "Schritt", n_haushalte = "Haushalte",
                    entfernt = "Entfernt", anteil_kum = "Anteil kumuliert") |>
  autofit() |>
  save_as_docx(path = file.path(path_tables, "filtertrichter.docx"))

# -----------------------------------------------------------------------------
# 5.2 Finalen Haushalts-Analysedatensatz erstellen
# -----------------------------------------------------------------------------
hh_analyse <- hh_13_17 |>
  filter(hat_koordinaten, !is.na(agglo_id), agglo_id != "0") |>
  mutate(
    miete_pro_zimmer = if_else(
      !is.na(rentnet) & !is.na(numberofrooms) & numberofrooms > 0,
      rentnet / numberofrooms,
      NA_real_
    )
  ) |>
  select(-hat_koordinaten)              # Hilfsmerkmal aus der Diagnose entfernen


# =============================================================================
# 6. HAUSHALTSMITGLIEDER-ANALYSEDATENSATZ BILDEN
# =============================================================================
# Die Mitglieder erben ihre raeumlichen und gemeindebezogenen Merkmale vom
# Haushalt: Wohnlage und Gemeinde sind pro Haushalt eindeutig. Statt die Joins
# fuer hhm zu wiederholen, werden die bereits finalisierten Merkmale aus
# hh_analyse uebernommen. Das garantiert, dass Mitglieder und Haushalte exakt
# denselben Bestand und dieselbe Zuordnung haben (keine Divergenz moeglich).

# Konsistenzpruefung: Stimmt die Gemeindenummer der Mitglieder mit jener des
# Haushalts ueberein? (Erwartung: keine Abweichungen.)
hhm_12_17 |>
  inner_join(hh_13_17 |> select(householdyearlyid, statyear, res_mun),
             by = c("householdyearlyid", "statyear")) |>
  summarise(
    n_geprueft     = n(),
    n_abweichungen = sum(hh_res_mun != res_mun, na.rm = TRUE)
  )

nrow(hhm_12_17)

# Finale Haushaltsmerkmale, die an die Mitglieder uebergeben werden.
hh_merkmale <- hh_analyse |>
  select(householdyearlyid, statyear, period, hh_weight_pool_2y,
         id_100, id_500, id_1000,
         res_mun_harm, gemeindename, agglo_id, agglo_groessenklasse)

# inner_join filtert hhm zugleich auf die Mitglieder der finalen Haushalte
# und haengt die geerbten Merkmale an.
hhm_analyse <- hhm_12_17 |>
  inner_join(hh_merkmale, by = c("householdyearlyid", "statyear")) |>
  select(-hh_res_mun)


# =============================================================================
# 7. QUALITAETSKONTROLLE
# =============================================================================

# 7a. Keine fehlenden Schluesselmerkmale im finalen Haushaltsdatensatz.
hh_analyse |>
  summarise(
    na_id_500 = sum(is.na(id_500)),
    na_agglo  = sum(is.na(agglo_id)),
    na_period = sum(is.na(period))
  )
# Erwartung: ueberall 0.

# 7b. Kein Haushalt mehr mit Agglomerationscode "0".
hh_analyse |> filter(agglo_id == "0") |> nrow()        # Erwartung: 0

# 7c. Konsistenz hh <-> hhm: Jedes Mitglied in hhm_analyse gehoert zu einem
#     Haushalt in hh_analyse.
hhm_analyse |>
  anti_join(hh_analyse, by = c("householdyearlyid", "statyear")) |>
  nrow()                                               # Erwartung: 0

# Selbe Überprüfung noch umgekehrt
hh_analyse |>
  anti_join(hhm_analyse, by = c("householdyearlyid", "statyear")) |>
  nrow()                                  # Erwartung: 0

# 7d. Verteilung ueber die Perioden.
hh_analyse  |> count(period)
hhm_analyse |> count(period)

# 7e. Strukturueberblick.
glimpse(hh_analyse)
glimpse(hhm_analyse)

### 7f. Übersicht aller Datensätze
# 1. Datensätze in eine Liste packen
datasets <- list(
  hh_12_17 = hh_12_17,
  hhm_12_17 = hhm_12_17,
  hh_13_17 = hh_13_17,
  hh_analyse = hh_analyse,
  hhm_analyse = hhm_analyse
  
  
)

# 2. Übersichtstabelle erstellen
overview_table <- imap_dfr(datasets, ~ tibble(
  datensatz = .y,
  zeilen = nrow(.x),
  spalten = ncol(.x),
  vollstaendige_zeilen = sum(complete.cases(.x)),
  unvollstaendige_zeilen = nrow(.x) - sum(complete.cases(.x)),
  anteil_vollstaendig_prozent = round(sum(complete.cases(.x)) / nrow(.x) * 100, 2)
))

# 3. Tabelle anzeigen
overview_table


# Zusätzliche Übersicht hh_analyse + hhm_analyse
skim(hh_analyse)
skim(hhm_analyse)


# =============================================================================
# 8. SPEICHERN
# =============================================================================
saveRDS(hh_analyse,  file.path(path_processed, "02_hh_analyse.rds"))
saveRDS(hhm_analyse, file.path(path_processed, "02_hhm_analyse.rds"))

# =============================================================================
# ENDE SKRIPT 02
# Ergebnis fuer Skript 03 (Aggregation auf 500-m-Raster):
#   02_hh_analyse.rds   - urbane, verortete Haushalte mit Periode, Zell-IDs
#                         und Agglomerationszugehoerigkeit
#   02_hhm_analyse.rds  - Mitglieder ebendieser Haushalte, mit denselben
#                         raeumlichen Merkmalen
# =============================================================================