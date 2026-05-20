# =============================================================================
# Skript 03 - Aggregation auf das 500-m-Raster
# Bachelorarbeit "Machine Learning for Gentrification" - Nelson Vidovic
# =============================================================================
# Zweck:
#   Dieses Skript fasst die Haushalts- und Personendaten aus Skript 02 zu
#   Rasterzellen von 500 m x 500 m zusammen. Ergebnis ist eine Tabelle mit
#   genau einer Zeile pro Zelle (id_500) und Periode (t1 / t2), die alle
#   Indikatoren enthaelt, auf denen die weitere Analyse aufbaut:
#     - UF1 (Nachbarschaftstypen)   -> Clustering auf den Indikator-Niveaus
#     - UF2 (Aufwertung)            -> Veraenderung der Indikatoren t1 -> t2
#     - UF3 (raeumliche Buendelung) -> raeumliche Autokorrelation der Aufwertung
#
# Aggregationslogik:
#   Die Indikatoren stammen aus zwei Quellen. Gebaeude-, Miet- und
#   Haushaltsmerkmale liegen je Haushalt vor und werden ueber die Haushalte
#   einer Zelle aggregiert (Abschnitt 3). Bildung, Erwerbsstatus und Alter
#   liegen je Person vor und werden ueber die Haushaltsmitglieder einer Zelle
#   aggregiert (Abschnitt 4). Alle Kennzahlen werden mit dem BFS-Hochrechnungs-
#   gewicht (hh_weight_pool_2y) gewichtet, da die Strukturerhebung eine
#   gewichtete Stichprobe ist; ungewichtete Anteile waeren verzerrt.
#
#   Das Skript arbeitet auf dem 500-m-Raster. Die Sensitivitaetsanalyse auf
#   100 m / 1000 m laesst sich erzeugen, indem der Gruppierungsschluessel
#   id_500 durch id_100 bzw. id_1000 ersetzt wird.
#   Die Zellgeometrie (01_grid_sf_500.rds) wird hier nicht benoetigt; sie wird
#   erst fuer die raeumliche Analyse und die Karten angehaengt.
# =============================================================================

source(here::here("R", "00_setup.R"))

# -----------------------------------------------------------------------------
# 0. Eingabedaten laden
# -----------------------------------------------------------------------------
hh_analyse  <- readRDS(file.path(path_processed, "02_hh_analyse.rds"))
hhm_analyse <- readRDS(file.path(path_processed, "02_hhm_analyse.rds"))

# -----------------------------------------------------------------------------
# 1. Parameter
# -----------------------------------------------------------------------------
# Die steuernden Werte sind hier gebuendelt, damit sie nachvollziehbar und an
# einer einzigen Stelle anpassbar sind.

# Mindestfallzahl: Eine Rasterzelle ist eine kleine Stichprobe. Zellen mit zu
# wenigen Haushalten liefern unzuverlaessige Schaetzwerte; sie werden in
# Abschnitt 7 ausgeschlossen. Der hier gesetzte Wert ist ein Startwert - die
# Diagnose in Abschnitt 6 zeigt, wie viele Zellen bei verschiedenen Schwellen
# verbleiben. Danach diesen Wert bewusst festlegen und das Skript erneut
# ausfuehren.
min_haushalte <- 10                  # angehoben von 5
min_mieter    <- 5                   # Mindestzahl Mieterhaushalte fuer einen belastbaren Mietmedian

# typeofownership-Codes, die einen Mieterhaushalt bezeichnen.
# Code 1 = Mieter, Untermieter ; Code 2 = Genossenschafter/in
codes_mieter <- c(1, 2)

# gbaups-Klassen, die als "Neubau" gelten (Baujahr nach 2000):
#   8020 = 2001-2005, 8021 = 2006-2010, 8022 = 2011-2015, 8023 = > 2015
gbaups_neubau <- c(8020, 8021, 8022, 8023)

# Namen der elf inhaltlichen Indikatoren (zur spaeteren Weiterverwendung).
indikator_spalten <- c(
  "anteil_einpersonen", "anteil_neubau", "anteil_mfh", "anteil_mieter",
  "mittlere_zimmerzahl", "median_miete_zimmer",
  "anteil_0_19", "anteil_20_44", "anteil_65plus",
  "anteil_tertiaer", "erwerbstaetigenquote"
)

# -----------------------------------------------------------------------------
# 2. Hilfsfunktion: gewichteter Median
# -----------------------------------------------------------------------------
# Fuer die Miete pro Zimmer wird der Median verwendet, da er robust gegenueber
# Ausreissern ist. Base R kennt keinen gewichteten Median, daher diese Funktion:
# Sie sortiert die Werte aufsteigend und gibt den Wert zurueck, bei dem die
# kumulierte Gewichtssumme erstmals die Haelfte des Gesamtgewichts erreicht.
# Fehlende Werte (x oder w) werden vorab entfernt.
gewichteter_median <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(NA_real_)
  x <- x[ok]
  w <- w[ok]
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

# -----------------------------------------------------------------------------
# 3. Haushaltsbasierte Indikatoren je Zelle und Periode
# -----------------------------------------------------------------------------
# Aggregation von hh_analyse ueber id_500 (Rasterzelle) und period.
#
# NA-Behandlung: Bei den Anteilen werden Haushalte mit fehlender Merkmalsangabe
# weder im Zaehler noch im Nenner gezaehlt (na.rm = TRUE). Beim Operator %in%
# ist Vorsicht geboten - er gibt fuer NA nicht NA, sondern FALSE zurueck. Damit
# fehlende Angaben nicht faelschlich als "trifft nicht zu" zaehlen, wird NA vor
# dem %in% mit if_else() explizit erhalten.
ind_haushalt <- hh_analyse |>
  group_by(id_500, period) |>
  summarise(
    # Fallzahlen (ungewichtet) - Grundlage fuer die Mindestfallzahl.
    n_haushalte = n(),
    n_mieter    = sum(!is.na(miete_pro_zimmer)),   # Haushalte mit Mietangabe
    
    # Anteil Einpersonenhaushalte (hh_type_bfs_agg == 11).
    anteil_einpersonen = weighted.mean(hh_type_bfs_agg == 11,
                                       hh_weight_pool_2y, na.rm = TRUE),
    
    # Anteil Haushalte in Gebaeuden mit Baujahr nach 2000 (Neubau-Indikator).
    anteil_neubau = weighted.mean(
      if_else(is.na(gbaups), NA, gbaups %in% gbaups_neubau),
      hh_weight_pool_2y, na.rm = TRUE),
    
    # Anteil Mehrfamilienhaeuser (gkats == 1025).
    anteil_mfh = weighted.mean(gkats == 1025,
                               hh_weight_pool_2y, na.rm = TRUE),
    
    # Anteil Mieterhaushalte (typeofownership in codes_mieter).
    anteil_mieter = weighted.mean(
      if_else(is.na(typeofownership), NA, typeofownership %in% codes_mieter),
      hh_weight_pool_2y, na.rm = TRUE),
    
    # Durchschnittliche Zimmerzahl der Wohnung.
    mittlere_zimmerzahl = weighted.mean(numberofrooms,
                                        hh_weight_pool_2y, na.rm = TRUE),
    
    # Median der Miete pro Zimmer. miete_pro_zimmer ist nur fuer Mieterhaushalte
    # gesetzt, der Median bezieht sich daher automatisch auf diese Teilmenge.
    median_miete_zimmer = gewichteter_median(miete_pro_zimmer,
                                             hh_weight_pool_2y),
    .groups = "drop"
  )


# Mietmedian nur behalten, wenn er auf genuegend Mieterhaushalten beruht;
# andernfalls NA (eine Zelle mit <5 Mietverhaeltnissen liefert keinen
# belastbaren Median).
ind_haushalt <- ind_haushalt |>
  mutate(median_miete_zimmer = if_else(n_mieter >= min_mieter,
                                       median_miete_zimmer, NA_real_))


# -----------------------------------------------------------------------------
# 4. Personenbasierte Indikatoren je Zelle und Periode
# -----------------------------------------------------------------------------
# Aggregation von hhm_analyse (Haushaltsmitglieder) ueber id_500 und period.
# Jedes Mitglied traegt das Gewicht seines Haushalts (in Skript 02 vererbt);
# es werden alle Mitglieder beruecksichtigt.

# 4.1 Altersband und Arbeitsalter-Markierung ableiten.
#     ist_25_64 ist NA-sicher gebaut: eine fehlende Altersangabe ergibt FALSE.
hhm_analyse <- hhm_analyse |>
  mutate(
    altersband = case_when(
      hh_age >= 0  & hh_age <= 19 ~ "0_19",
      hh_age >= 20 & hh_age <= 44 ~ "20_44",
      hh_age >= 45 & hh_age <= 64 ~ "45_64",
      hh_age >= 65                ~ "65plus"
    ),
    ist_25_64 = !is.na(hh_age) & hh_age >= 25 & hh_age <= 64
  )

# 4.2 Aggregation.
#     Die Altersanteile beziehen sich auf alle Personen mit gueltiger
#     Altersangabe (der Anteil 45-64 wird bewusst nicht gebildet, da er sich als
#     Rest ergibt - siehe Variablenkonzept).
#     Tertiaer- und Erwerbstaetigenquote werden bewusst nur auf den 25- bis
#     64-Jaehrigen berechnet: So wird der Bildungsindikator nicht durch den
#     Kohorteneffekt aelterer Generationen und die Erwerbsquote nicht durch
#     Rentner/Studierende verzerrt. Die Teilmenge der 25- bis 64-Jaehrigen wird
#     mit [ist_25_64] gebildet, bevor der gewichtete Anteil berechnet wird.
ind_person <- hhm_analyse |>
  group_by(id_500, period) |>
  summarise(
    n_personen = n(),
    
    # Altersanteile (Basis: alle Personen der Zelle mit gueltiger Altersangabe).
    anteil_0_19   = weighted.mean(altersband == "0_19",
                                  hh_weight_pool_2y, na.rm = TRUE),
    anteil_20_44  = weighted.mean(altersband == "20_44",
                                  hh_weight_pool_2y, na.rm = TRUE),
    anteil_65plus = weighted.mean(altersband == "65plus",
                                  hh_weight_pool_2y, na.rm = TRUE),
    
    # Anteil Personen mit Tertiaerabschluss (hh_highestcompleduaggii == 6),
    # Basis: Personen 25-64.
    anteil_tertiaer = weighted.mean(
      (hh_highestcompleduaggii == 6)[ist_25_64],
      hh_weight_pool_2y[ist_25_64], na.rm = TRUE),
    
    # Erwerbstaetigenquote (hh_curractivitystatusi == 1: Erwerbstaetige),
    # Basis: Personen 25-64.
    erwerbstaetigenquote = weighted.mean(
      (hh_curractivitystatusi == 1)[ist_25_64],
      hh_weight_pool_2y[ist_25_64], na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------------------------------------------------------
# 5. Haushalts- und Personenindikatoren zusammenfuehren
# -----------------------------------------------------------------------------
# Beide Tabellen haben denselben Schluessel (id_500, period). Der full_join
# wuerde einen etwaigen Schluesselunterschied sichtbar machen (NA-Spalten);
# Abschnitt 8 prueft, dass kein solcher Fall auftritt.
# weighted.mean kann bei leerer Basis (z. B. Zelle ohne 25- bis 64-Jaehrige)
# NaN liefern; NaN wird einheitlich auf NA gesetzt.
raster_indikatoren <- ind_haushalt |>
  full_join(ind_person, by = c("id_500", "period")) |>
  mutate(across(all_of(indikator_spalten),
                ~ if_else(is.nan(.x), NA_real_, .x)))

# -----------------------------------------------------------------------------
# 6. Diagnose: Verteilung der Fallzahlen
# -----------------------------------------------------------------------------
# Vor dem Anwenden der Mindestfallzahl: Wie sind die Haushaltszahlen je Zelle
# verteilt, und wie viele Zellen-Perioden ueberleben welche Schwelle? Anhand
# dieser beiden Tabellen wird der Parameter min_haushalte (Abschnitt 1) gewaehlt.
fallzahl_verteilung <- raster_indikatoren |>
  summarise(
    zellen_perioden = n(),
    min    = min(n_haushalte),
    q25    = quantile(n_haushalte, 0.25),
    median = median(n_haushalte),
    q75    = quantile(n_haushalte, 0.75),
    max    = max(n_haushalte)
  )
fallzahl_verteilung

schwellen_check <- tibble(schwelle = c(3, 5, 10, 15, 20, 30)) |>
  mutate(
    zellen_perioden = map_int(schwelle,
                              ~ sum(raster_indikatoren$n_haushalte >= .x)),
    anteil          = round(zellen_perioden / nrow(raster_indikatoren), 3)
  )
schwellen_check


# -----------------------------------------------------------------------------
# 6b. Vergleichscheck: Rasterauflösung 100 m / 500 m / 1000 m
# -----------------------------------------------------------------------------
# Dieser Check belegt die Wahl des 500-m-Rasters mit Zahlen. Er zaehlt die
# Haushalte je Zelle - ohne Indikatoren, ohne weitere Analyse - und zwar fuer
# alle drei Aufloesungen. Verglichen wird, wie dicht die Zellen besetzt sind
# und wie viele Zellen die Mindestfallzahl erreichen.
#
# Argument hinter dem Check:
#   - 100 m:  sehr viele, aber duenn besetzte Zellen -> kaum eine erreicht die
#             Mindestfallzahl, die Zell-Indikatoren waeren stark verrauscht.
#   - 1000 m: wenige, dicht besetzte Zellen -> stabile Werte, aber die feine
#             raeumliche Aufloesung (der Beitrag dieser Arbeit) geht verloren.
#   - 500 m:  Kompromiss aus Fallzahl je Zelle und raeumlicher Granularitaet.

# Hilfsfunktion: fasst hh_analyse fuer eine gegebene Raster-ID zu einer Zeile
# der Vergleichstabelle zusammen.
raster_kennzahlen <- function(id_spalte) {
  
  # Fallzahl je Zelle und Periode
  zellen <- hh_analyse |>
    group_by(zelle = .data[[id_spalte]], period) |>
    summarise(n_haushalte = n(), .groups = "drop")
  
  # Zellen, die NACH der Mindestfallzahl in beiden Perioden vorkommen -
  # nur diese koennen ueberhaupt in die Veraenderungsanalyse (UF2) eingehen.
  beidperiodig <- zellen |>
    filter(n_haushalte >= min_haushalte) |>
    count(zelle, name = "n_perioden") |>
    filter(n_perioden == 2) |>
    nrow()
  
  tibble(
    raster                = id_spalte,
    zellen_perioden       = nrow(zellen),
    median_hh_je_zelle    = median(zellen$n_haushalte),
    anteil_ueber_schwelle = round(mean(zellen$n_haushalte >= min_haushalte), 3),
    zellen_perioden_ueber = sum(zellen$n_haushalte >= min_haushalte),
    beidperiodige_zellen  = beidperiodig
  )
}

# Check fuer alle drei Aufloesungen.
raster_vergleich <- bind_rows(
  raster_kennzahlen("id_100"),
  raster_kennzahlen("id_500"),
  raster_kennzahlen("id_1000")
)
raster_vergleich


# -----------------------------------------------------------------------------
# 6c. Vergleichstabelle als Word-Dokument exportieren
# -----------------------------------------------------------------------------
# Formatiert raster_vergleich leserlich auf und speichert die Tabelle als
# .docx im Tabellenordner, damit sie direkt in die Arbeit uebernommen werden

# Aufbereitung: sprechende Spaltennamen, Tausendertrennung mit Apostroph,
# Anteil als Prozentwert mit Dezimalkomma.
raster_vergleich_export <- raster_vergleich |>
  transmute(
    `Auflösung` = case_when(
      raster == "id_100"  ~ "100 m",
      raster == "id_500"  ~ "500 m",
      raster == "id_1000" ~ "1000 m"
    ),
    `Zellen-Perioden`         = format(zellen_perioden,
                                       big.mark = "'", trim = TRUE),
    `Median HH/Zelle`         = median_hh_je_zelle,
    `Anteil ≥ 10 HH`          = paste0(formatC(anteil_ueber_schwelle * 100,
                                               format = "f", digits = 1,
                                               decimal.mark = ","), " %"),
    `Zellen-Perioden ≥ 10 HH` = format(zellen_perioden_ueber,
                                       big.mark = "'", trim = TRUE),
    `Beidperiodige Zellen`    = format(beidperiodige_zellen,
                                       big.mark = "'", trim = TRUE)
  )

# flextable aufbauen: schlichtes Layout, Kopfzeile fett, Zahlen zentriert.
raster_vergleich_ft <- flextable(raster_vergleich_export) |>
  set_caption(paste("Vergleich der Rasterauflösungen: Zellbesetzung und",
                    "nutzbare Analysebasis (Mindestfallzahl 10 Haushalte)")) |>
  theme_booktabs() |>
  align(align = "center", part = "all") |>
  align(j = 1, align = "left", part = "all") |>
  bold(part = "header") |>
  autofit()

# Als Word-Dokument speichern.
save_as_docx(raster_vergleich_ft,
             path = file.path(path_tables, "03_raster_vergleich.docx"))


# -----------------------------------------------------------------------------
# 7. Mindestfallzahl anwenden
# -----------------------------------------------------------------------------
# Zellen-Perioden unterhalb der Mindestfallzahl werden ausgeschlossen, da ihre
# Indikatorwerte auf zu wenigen Beobachtungen beruhen.
n_vor <- nrow(raster_indikatoren)
raster_indikatoren <- raster_indikatoren |>
  filter(n_haushalte >= min_haushalte)
n_nach <- nrow(raster_indikatoren)

cat("Mindestfallzahl:", min_haushalte, "Haushalte\n")
cat("Zellen-Perioden  vorher:", n_vor,
    " nachher:", n_nach,
    " entfernt:", n_vor - n_nach, "\n")

# -----------------------------------------------------------------------------
# 8. Schlusskontrollen
# -----------------------------------------------------------------------------
# 8a. Schluesselkontrolle: Nach dem full_join darf keine Zelle-Periode in nur
#     einer der beiden Quellen vorkommen (sonst NA in n_haushalte/n_personen).
raster_indikatoren |>
  summarise(
    na_n_haushalte = sum(is.na(n_haushalte)),
    na_n_personen  = sum(is.na(n_personen))
  )                                                  # Erwartung: 0 / 0

# 8b. Fehlende Indikatorwerte je Spalte. Einzelne NA sind erwartbar bei
#     median_miete_zimmer (Zellen ganz ohne Mieterhaushalte) und ggf. bei den
#     Personenindikatoren (Zellen ohne 25- bis 64-Jaehrige). Skript 04 muss
#     diese Faelle beim Standardisieren / Clustern beruecksichtigen.
raster_indikatoren |>
  summarise(across(everything(), ~ sum(is.na(.x)))) |>
  pivot_longer(everything(), names_to = "variable", values_to = "n_na")

# 8c. Wie viele Zellen sind in beiden Perioden vertreten? Nur diese koennen in
#     die Veraenderungsanalyse (UF2) eingehen.
raster_indikatoren |>
  count(id_500, name = "n_perioden") |>
  count(n_perioden, name = "n_zellen")

# Aufteilung der einperiodigen Zellen auf nur-t1 und nur-t2
raster_indikatoren |>
  count(id_500, period) |>
  count(id_500, name = "n_perioden") |>
  filter(n_perioden == 1) |>
  left_join(distinct(raster_indikatoren, id_500, period), by = "id_500") |>
  count(period, name = "n_einperiodige_zellen")

# 8d. Strukturueberblick des fertigen Rasterdatensatzes.
glimpse(raster_indikatoren)

# -----------------------------------------------------------------------------
# 9. Speichern
# -----------------------------------------------------------------------------
saveRDS(raster_indikatoren,
        file.path(path_processed, "03_raster_indikatoren.rds"))