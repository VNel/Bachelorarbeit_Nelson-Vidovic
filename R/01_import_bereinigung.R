# =============================================================================
# BACHELORARBEIT  -  "Machine Learning for Gentrification"
# Skript 01  -  DATENIMPORT UND DATENBEREINIGUNG
# -----------------------------------------------------------------------------
# Autor   : Nelson Vidovic
# R-Version: 4.5.0
#
# Zweck dieses Skripts:
#   Einlesen der BFS-Rohdaten (Strukturerhebung, GWR-Koordinaten,
#   Agglomerationsreferenz), Aufbereitung der Haushaltsdaten sowie
#   Zuordnung der Haushalte zu Rasterzellen (100 m / 500 m / 1000 m).
#   Ergebnis sind bereinigte Analyseobjekte, auf denen die spaetere
#   Aggregation, das Clustering und die raeumliche Analyse aufbauen.
#
# Gliederung:
#   0.  Setup und Input
#   1.  Datenimport
#       1.1  Haushaltsdaten 2011-2015
#       1.2  Ergaenzungsdatensatz Nettomieten 2013-2015 + Join
#       1.3  Haushaltsdaten 2016-2020
#       1.4  Zielpersonen-Daten 2011-2015 / 2016-2020
#       1.5  Haushaltsmitglieder-Daten 2011-2015 / 2016-2020
#       1.6  Haushalts-Koordinaten 2011-2017
#       1.7  Agglomerationsreferenz
#       1.8  Import-Uebersicht
#   2.  Bereinigung der Haushaltsdaten
#       2.1  Definition der benoetigten Variablen
#       2.2  hh_11_15  -  bereinigen
#       2.3  hh_11_15  -  Qualitaetskontrolle
#       2.4  hh_16_20  -  bereinigen
#       2.5  hh_16_20  -  Qualitaetskontrolle
#       2.6  hhm_11_15 -  bereinigen
#       2.7  hhm_16_20 -  bereinigen
#   3.  Rasterzuordnung der Haushalts-Koordinaten
#       3.1  Koordinaten bereinigen
#       3.2  500-m-Raster bilden
#       3.3  Variablennamen vereinheitlichen
#       3.4  Kontrolle der Rasterbildung
#       3.5  Zellreferenz-Tabellen
#       3.6  sf-Objekte je Aufloesung
#       3.7  Überprüfen der HH-Koordinaten-Datensätze
#   4.  Speichern der relevanten Ergebnisse für Folgeskripte
# =============================================================================


# =============================================================================
# 0. SETUP UND INPUT
# =============================================================================
source(here::here("R", "00_setup.R"))   # Pakete, Pfade, Optionen, Konstanten


# =============================================================================
# 1.  DATENIMPORT
# =============================================================================
# Alle BFS-Dateien liegen im Unterordner "Daten/". Nach jedem Import werden die
# Spaltennamen mit clean_names() vereinheitlicht und das urspruenglich
# geladene Objekt mit rm() entfernt, um den Arbeitsspeicher schlank zu halten.


# -----------------------------------------------------------------------------
# 1.1  Haushaltsdaten 2011-2015
# -----------------------------------------------------------------------------
load("Daten/zpers.hh_11.15.rda")
hh_11_15 <- zpers.hh_11.15 |>
  clean_names()
rm(zpers.hh_11.15)


# -----------------------------------------------------------------------------
# 1.2  Ergaenzungsdatensatz Nettomieten 2013-2015 + Join
# -----------------------------------------------------------------------------
# Ziel    : Fuer einen Teil der Haushaltsdaten fehlt die Nettomiete. Der
#           Zusatzdatensatz se13.15_hh liefert die Mietangaben fuer 2013-2015
#           und wird ueber Haushalts-ID und Erhebungsjahr angehaengt.
# Vorgehen: Import, Namen vereinheitlichen, Join-Schluessel als integer setzen.

load("Daten/se13.15_hh.Rda")
mieten_13_15 <- hh.13.15 |>
  clean_names() |>
  mutate(householdyearlyid = as.integer(householdyearlyid))
rm(hh.13.15)

# Fehlende Nettomieten 2013-2015 an die Haushaltsdaten anfuegen.
hh_11_15 <- hh_11_15 |>
  left_join(
    mieten_13_15,
    by = c("householdyearlyid" = "householdyearlyid",
           "statyear"          = "statyear")
  )

# Kontrolle: Wertebereich der Nettomiete nach dem Join (inkl. Sondercodes).
min(hh_11_15$rentnet, na.rm = TRUE)
max(hh_11_15$rentnet, na.rm = TRUE)

# Kontrolle: Kennzahlen der gueltigen (positiven) Mietangaben nach dem Join.
hh_11_15 |>
  filter(rentnet > 0) |>
  summarise(
    n          = n(),
    n_mit_wert = sum(!is.na(rentnet)),
    mittelwert = mean(rentnet, na.rm = TRUE),
    median     = median(rentnet, na.rm = TRUE),
    max        = max(rentnet, na.rm = TRUE),
    min        = min(rentnet, na.rm = TRUE)
  )
# Befund: Durch den Zusatzdatensatz wurden 377'574 Mietangaben dazugewonnen.


# -----------------------------------------------------------------------------
# 1.3  Haushaltsdaten 2016-2020
# -----------------------------------------------------------------------------
load("Daten/zpers.hh_16.20.rda")
hh_16_20 <- zpers.hh_16.20 |>
  clean_names()
rm(zpers.hh_16.20)


# -----------------------------------------------------------------------------
# 1.4  Zielpersonen-Daten 2011-2015 / 2016-2020 ----> braucht es nicht mehr
# -----------------------------------------------------------------------------
# Daten zur Zielperson (Referenzperson des Haushalts).

#    load("Daten/zpers_11.15.rda")
#   zp_11_15 <- zpers_11.15 |>
#      clean_names()
#    rm(zpers_11.15)
    
#    load("Daten/zpers_16.20.rda")
#    zp_16_20 <- zpers_16.20 |>
#      clean_names()
#    rm(zpers_16.20)


# -----------------------------------------------------------------------------
# 1.5  Haushaltsmitglieder-Daten 2011-2015 / 2016-2020
# -----------------------------------------------------------------------------
# Daten zu allen Haushaltsmitgliedern.

load("Daten/hh_11.15.rda")
hhm_11_15 <- hh_11.15 |>
  clean_names()
rm(hh_11.15)

load("Daten/hh_16.20.rda")
hhm_16_20 <- hh_16.20 |>
  clean_names()
rm(hh_16.20)


# -----------------------------------------------------------------------------
# 1.6  Haushalts-Koordinaten 2011-2017 (inkl. BFS-Rasterzellen)
# -----------------------------------------------------------------------------
# Enthaelt die Wohnkoordinaten der Haushalte sowie die bereits vom BFS
# vergebenen 100-m- und 1000-m-Rasterzellen.

load("Daten/SE_20112017_ha.Rda")
hh_coordinates_11_17 <- full_data_ha |>
  clean_names() |>
  mutate(householdyearlyid = as.integer(householdyearlyid))
rm(full_data_ha)


# -----------------------------------------------------------------------------
# 1.7  Agglomerationsreferenz
# -----------------------------------------------------------------------------
# Raumgliederungs-Referenz des BFS. Wird hier importiert und spaeter genutzt,
# um die Haushalte auf die urbanen Agglomerationen einzugrenzen.
# skip = 1 ueberspringt die Titelzeile, slice(-1) entfernt die Unter-Kopfzeile.

agglo <- read_excel("Daten/be-d-00.04-rgs-01.xlsx", sheet = "Daten", skip = 1) |>
  slice(-1) |>
  clean_names() |>
  select(
    bfs_gde_nummer,
    gemeindename,
    agglomerationen_2012_8,
    agglomerationsgrossenklasse
  )


# -----------------------------------------------------------------------------
# 1.8  Import-Uebersicht
# -----------------------------------------------------------------------------
# Kurzer Strukturueberblick ueber die zentralen importierten Datensaetze.
glimpse(hh_11_15)
glimpse(hh_16_20)
glimpse(hh_coordinates_11_17)
glimpse(agglo)
glimpse(hhm_11_15)
glimpse(hhm_16_20)



# =============================================================================
# 2.  BEREINIGUNG DER HAUSHALTSDATEN
# =============================================================================


# -----------------------------------------------------------------------------
# 2.1  Definition der benoetigten Variablen
# -----------------------------------------------------------------------------
# Liste der fuer die Analyse benoetigten Variablen. Dient als Referenz fuer
# die select()-Schritte und fuer die spaeteren Vollstaendigkeitspruefungen.
# Wird fuer beide Haushaltsdatensaetze (2011-2015 und 2016-2020) verwendet.

required_vars_hh <- c(
  "householdyearlyid",
  "statyear",
  "hh_weight_pool_2y",
  "hh_countof00_19",
  "hh_countof20_44",
  "hh_countof45_64",
  "hh_countof65_plus",
  "gkats",
  "gbaups",
  "typeofownership",
  "numberofrooms",
  "rentnet",
  "hh_type_bfs_agg",
  "res_mun"
)


# -----------------------------------------------------------------------------
# 2.2  hh_11_15  -  bereinigen
# -----------------------------------------------------------------------------
# Ziel    : Den Haushaltsdatensatz 2011-2015 analysebereit machen.
# Vorgehen:
#   - Altersklassen zu groeberen Klassen zusammenfassen
#     (00-19, 20-44, 65+; die Klasse 45-64 ist bereits vorhanden).
#   - Stichprobengewicht auf das spaetere 2-Jahres-Pooling umskalieren.
#   - Datentypen korrekt setzen (statyear, rentnet als integer).
#   - Auf den Untersuchungszeitraum eingrenzen.
#   - Nur die benoetigten Variablen behalten.
#   - Negative BFS-Sondercodes in den Analysevariablen zu NA setzen.

hh_12_15_clean <- hh_11_15 |>
  # Negative BFS-Sondercodes in den Analysevariablen zu NA setzen, damit sie
  # die spaeteren Berechnungen nicht verzerren
  mutate(
    across(
      c(
        hh_countof00_04,
        hh_countof05_15,
        hh_countof16_19,
        hh_countof20_24,
        hh_countof25_44,
        hh_countof45_64,
        hh_countof65_79,
        hh_countof80,
        gkats,
        gbaups,
        typeofownership,
        numberofrooms,
        rentnet,
        hh_type_bfs_agg,
        res_mun
      ),
      ~ replace(.x, .x < 0, NA)
    )
  ) |>
  mutate(
    # Altersklassen zusammenfassen
    hh_countof00_19   = hh_countof00_04 + hh_countof05_15 + hh_countof16_19,
    hh_countof20_44   = hh_countof20_24 + hh_countof25_44,
    hh_countof65_plus = hh_countof65_79 + hh_countof80,
    # Gewicht auf 2-Jahres-Pooling umskalieren
    hh_weight_pool_2y = (hh_weight_2011_2015 * 5) / 2,
    # Datentypen setzen
    statyear          = as.integer(statyear),
    rentnet           = as.integer(rentnet)
  ) |>
  # Auf den Untersuchungszeitraum eingrenzen
  filter(statyear >= 2012 & statyear <= 2015) |>
  # Nur benoetigte Variablen behalten
  select(
    householdyearlyid,
    statyear,
    hh_weight_pool_2y,
    hh_countof00_19,
    hh_countof20_44,
    hh_countof45_64,
    hh_countof65_plus,
    gkats,
    gbaups,
    typeofownership,
    numberofrooms,
    rentnet,
    hh_type_bfs_agg,
    res_mun
  )


# -----------------------------------------------------------------------------
# 2.3  hh_11_15  -  Qualitaetskontrolle
# -----------------------------------------------------------------------------

# Vollstaendigkeitspruefung: Fehlt eine benoetigte Variable?
setdiff(required_vars_hh, names(hh_12_15_clean))
# Befund: leer  ->  alle benoetigten Variablen vorhanden.

# Datentyp-Kontrolle.
glimpse(hh_12_15_clean)
# Befund: alle Datentypen korrekt.

# Kontrolle: Nach der Bereinigung duerfen in den Analysevariablen keine
# negativen Sondercodes mehr vorkommen (Erwartung: ueberall 0).
hh_12_15_clean |>
  summarise(
    across(
      all_of(required_vars_hh),
      ~ sum(.x < 0, na.rm = TRUE),
      .names = "negative_{.col}"
    )
  )

# NA-Analyse je Erhebungsjahr: Anteil fehlender Werte pro Variable und Jahr.
na_analysis_by_year_12_15 <- hh_12_15_clean |>
  group_by(statyear) |>
  summarise(
    n_rows_year = n(),
    across(
      everything(),
      ~ sum(is.na(.x)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = starts_with("na_"),
    names_to  = "variable",
    values_to = "n_na"
  ) |>
  mutate(
    variable = sub("^na_", "", variable),
    share_na = n_na / n_rows_year
  ) |>
  arrange(statyear, desc(share_na))

na_analysis_by_year_12_15 |>
  filter(n_na > 0)
# Befund: Die NA-Werte aller Variablen ausser "rentnet" sind unbedenklich,
#         da sie nur einen Bruchteil aller Beobachtungen ausmachen.

# Herkunft der fehlenden Nettomieten pruefen.
hh_12_15_clean |>
  filter(statyear %in% 2013:2015, is.na(rentnet)) |>
  count(typeofownership, name = "n_missing_rentnet") |>
  mutate(
    share_of_all_missing_rentnet = n_missing_rentnet / sum(n_missing_rentnet)
  ) |>
  arrange(desc(share_of_all_missing_rentnet))
# Befund: Die fehlenden Nettomieten stammen vollstaendig von Eigentuemern
#         oder Sondersituationen (z. B. mietfreie Wohnung von Verwandten
#         oder vom Arbeitgeber).


# -----------------------------------------------------------------------------
# 2.4  hh_16_20  -  bereinigen
# -----------------------------------------------------------------------------
# Gleiches Vorgehen wie bei hh_11_15 (siehe Abschnitt 2.2). Unterschiede:
#   - Gewichtsspalte heisst hier hh_weight_2016_2020.
#   - Kein Join eines Mieten-Zusatzdatensatzes noetig.


hh_16_17_clean <- hh_16_20 |>
  # Negative BFS-Sondercodes in den Analysevariablen zu NA setzen
  mutate(
    across(
      c(
        hh_countof00_04,
        hh_countof05_15,
        hh_countof16_19,
        hh_countof20_24,
        hh_countof25_44,
        hh_countof45_64,
        hh_countof65_79,
        hh_countof80,
        gkats,
        gbaups,
        typeofownership,
        numberofrooms,
        rentnet,
        hh_type_bfs_agg,
        res_mun
      ),
      ~ replace(.x, .x < 0, NA)
    )
  ) |>
  mutate(
    # Altersklassen zusammenfassen
    hh_countof00_19   = hh_countof00_04 + hh_countof05_15 + hh_countof16_19,
    hh_countof20_44   = hh_countof20_24 + hh_countof25_44,
    hh_countof65_plus = hh_countof65_79 + hh_countof80,
    # Gewicht auf 2-Jahres-Pooling umskalieren  (siehe PRUEFEN-Hinweis oben)
    hh_weight_pool_2y = (hh_weight_2016_2020 * 5) / 2,
    # Datentyp setzen
    statyear          = as.integer(statyear)
  ) |>
  # Auf den Untersuchungszeitraum eingrenzen (effektiv 2016-2017,
  # da der Datensatz erst ab 2016 beginnt)
  filter(statyear >= 2016 & statyear <= 2017) |>
  # Nur benoetigte Variablen behalten
  select(
    householdyearlyid,
    statyear,
    hh_weight_pool_2y,
    hh_countof00_19,
    hh_countof20_44,
    hh_countof45_64,
    hh_countof65_plus,
    gkats,
    gbaups,
    typeofownership,
    numberofrooms,
    rentnet,
    hh_type_bfs_agg,
    res_mun
  ) 


# -----------------------------------------------------------------------------
# 2.5  hh_16_20  -  Qualitaetskontrolle
# -----------------------------------------------------------------------------

# Vollstaendigkeitspruefung: Fehlt eine benoetigte Variable?
setdiff(required_vars_hh, names(hh_16_17_clean))
# Befund: leer  ->  alle benoetigten Variablen vorhanden.

# Datentyp-Kontrolle.
glimpse(hh_16_17_clean)
# Befund: alle Datentypen korrekt.

# Kontrolle: Nach der Bereinigung duerfen in den Analysevariablen keine
# negativen Sondercodes mehr vorkommen (Erwartung: ueberall 0).
hh_16_17_clean |>
  summarise(
    across(
      all_of(required_vars_hh),
      ~ sum(.x < 0, na.rm = TRUE),
      .names = "negative_{.col}"
    )
  )

# NA-Analyse je Erhebungsjahr: Anteil fehlender Werte pro Variable und Jahr.
na_analysis_by_year_16_17 <- hh_16_17_clean |>
  group_by(statyear) |>
  summarise(
    n_rows_year = n(),
    across(
      everything(),
      ~ sum(is.na(.x)),
      .names = "na_{.col}"
    ),
    .groups = "drop"
  ) |>
  pivot_longer(
    cols      = starts_with("na_"),
    names_to  = "variable",
    values_to = "n_na"
  ) |>
  mutate(
    variable = sub("^na_", "", variable),
    share_na = n_na / n_rows_year
  ) |>
  arrange(statyear, desc(share_na))

na_analysis_by_year_16_17 |>
  filter(n_na > 0)
# Befund: Die NA-Werte aller Variablen ausser "rentnet" sind unbedenklich,
#         da sie nur einen Bruchteil aller Beobachtungen ausmachen.

# Herkunft der fehlenden Nettomieten pruefen.
hh_16_17_clean |>
  filter(statyear %in% 2016:2017, is.na(rentnet)) |>
  count(typeofownership, name = "n_missing_rentnet") |>
  mutate(
    share_of_all_missing_rentnet = n_missing_rentnet / sum(n_missing_rentnet)
  ) |>
  arrange(desc(share_of_all_missing_rentnet))
# Befund: Die fehlenden Nettomieten stammen vollstaendig von Eigentuemern
#         oder Sondersituationen (z. B. mietfreie Wohnung von Verwandten
#         oder vom Arbeitgeber).



# -----------------------------------------------------------------------------
# 2.6  hhm_11_15  -  bereinigen
# -----------------------------------------------------------------------------
# Ziel    : Den Haushaltsmitgliederdatensatz 2011-2015 analysebereit machen.
# Vorgehen:
#   - Nur die benoetigten Variablen behalten.
#   - Negative BFS-Sondercodes in den Analysevariablen zu NA setzen.
#   - Nur die Jahre 2012-2015 behalten, da berücksichtigter Zeitraum der Bachelorarbeit 2012-2017 ist

hhm_12_15_clean <- hhm_11_15 |>
  mutate(
    across(
      c(
        hh_personnumber,
        hh_sex,
        hh_age,
        hh_curractivitystatusi,
        hh_highestcompleduaggii,
        hh_res_mun
      ),
      ~ replace(.x, .x < 0, NA)
    )
  )|>
  filter(statyear >= 2012 & statyear <= 2015) |>
  select(
    householdyearlyid,
    statyear,
    hh_personnumber,
    hh_sex,
    hh_age,
    hh_curractivitystatusi,
    hh_highestcompleduaggii,
    hh_res_mun
  )


# Überprüfen, ob der Datensatz wie erwartet angepasst wurde
glimpse(hhm_12_15_clean)



# -----------------------------------------------------------------------------
# 2.7  hhm_16_20  -  bereinigen
# -----------------------------------------------------------------------------
# Ziel    : Den Haushaltsmitgliederdatensatz 2011-2015 analysebereit machen.
# Vorgehen:
#   - Nur die benoetigten Variablen behalten.
#   - Negative BFS-Sondercodes in den Analysevariablen zu NA setzen.
#   - Nur die Jahre 2016-2017 behalten, da berücksichtigter Zeitraum der Bachelorarbeit 2012-2017 ist

hhm_16_17_clean <- hhm_16_20 |>
  mutate(
    across(
      c(
        hh_personnumber,
        hh_sex,
        hh_age,
        hh_curractivitystatusi,
        hh_highestcompleduaggii,
        hh_res_mun
      ),
      ~ replace(.x, .x < 0, NA)
    )
  )|>
  filter(statyear >= 2016 & statyear <= 2017) |>
  select(
    householdyearlyid,
    statyear,
    hh_personnumber,
    hh_sex,
    hh_age,
    hh_curractivitystatusi,
    hh_highestcompleduaggii,
    hh_res_mun
  )


# Überprüfen, ob der Datensatz wie erwartet angepasst wurde
glimpse(hhm_16_17_clean)

# =============================================================================
# 3.  RASTERZUORDNUNG DER HAUSHALTS-KOORDINATEN
# =============================================================================
# Ziel: Jeden Haushalt eindeutig einer Rasterzelle in drei Aufloesungen
#       (100 m, 500 m, 1000 m) zuordnen, damit die spaetere Aggregation und
#       Analyse wahlweise auf einer dieser Ebenen durchgefuehrt werden kann.
#
# Ausgangslage:
#   - Das 100-m- und das 1000-m-Raster sind im BFS-Datensatz bereits
#     enthalten (id_100, id_1000 sowie die zugehoerigen Zentroidkoordinaten).
#     Diese werden uebernommen, nicht neu berechnet, um Abweichungen von der
#     offiziellen BFS-Rasterung auszuschliessen.
#   - Das 500-m-Raster fehlt und wird in Abschnitt 3.2 selbst gebildet.


# -----------------------------------------------------------------------------
# 3.1  Koordinaten bereinigen
# -----------------------------------------------------------------------------

# Kontrolle: Stimmen statyear und statyear_harm ueberein?
hh_coordinates_11_17 |>
  summarise(
    all_same    = all(statyear == statyear_harm, na.rm = TRUE),
    n_different = sum(statyear != statyear_harm, na.rm = TRUE)
  )
# Befund: beide Variablen sind ueberall identisch erfasst.

# Auf den Untersuchungszeitraum eingrenzen, Datentyp setzen und nur die
# benoetigten Koordinaten- und Raster-Variablen behalten.
hh_coordinates_11_17_clean <- hh_coordinates_11_17 |>
  filter(statyear >= 2012 & statyear <= 2017) |>
  mutate(statyear = as.integer(statyear)) |>
  select(
    householdyearlyid,
    statyear,
    res_geoe_ha,
    res_geon_ha,
    res_centroids_100_x,
    res_centroids_100_y,
    id_100,
    res_centroids_1000_x,
    res_centroids_1000_y,
    id_1000,
    res_centroids_100_lon,
    res_centroids_100_lat,
    res_centroids_1000_lon,
    res_centroids_1000_lat
  )

glimpse(hh_coordinates_11_17_clean)


# -----------------------------------------------------------------------------
# 3.2  500-m-Raster bilden
# -----------------------------------------------------------------------------
# res_geoe_ha / res_geon_ha sind die Wohnkoordinaten im Schweizer
# Koordinatensystem LV95 (EPSG:2056), bereits auf 100 m gerundet
# (Suffix _ha = Hektare).
#
# Logik der Zellbildung:
#   - floor(Koordinate / 500) * 500 schneidet die Koordinate auf die linke
#     bzw. untere Kante der 500-m-Zelle ab.
#   - + 250 (halbe Aufloesung) verschiebt den Punkt in die Zellmitte
#     (Zentroid). Dies entspricht der Logik, mit der der BFS die 100-m-
#     Zentroide bildet (dort + 50).
#   - Schweizer LV95-Koordinaten sind ausnahmslos positiv, daher ist floor()
#     hier eindeutig und unproblematisch.

hh_coordinates_grid <- hh_coordinates_11_17_clean |>
  mutate(
    # 500-m-Zentroid (selbst berechnet)
    centroid_500_x = floor(res_geoe_ha / 500) * 500 + 250,
    centroid_500_y = floor(res_geon_ha / 500) * 500 + 250,
    # Eindeutige 500-m-Zell-ID als String aus den beiden Zentroidkoordinaten
    id_500 = paste(centroid_500_x, centroid_500_y, sep = "_")
  )


# -----------------------------------------------------------------------------
# 3.3  Variablennamen vereinheitlichen
# -----------------------------------------------------------------------------
# Damit die drei Rasterebenen konsistent benannt sind, werden die bereits
# vorhandenen BFS-Spalten umbenannt. Schema:
#   id_<aufloesung>            = Zell-ID
#   centroid_<aufl>_x / _y     = Zentroid in LV95 (Meter)
#   centroid_<aufl>_lon / _lat = Zentroid in WGS84 (Grad)
# Die 100-m- und 1000-m-Werte stammen unveraendert vom BFS.

hh_coordinates_grid <- hh_coordinates_grid |>
  rename(
    # 100-m-Raster (BFS-Originalwerte)
    centroid_100_x   = res_centroids_100_x,
    centroid_100_y   = res_centroids_100_y,
    centroid_100_lon = res_centroids_100_lon,
    centroid_100_lat = res_centroids_100_lat,
    # 1000-m-Raster (BFS-Originalwerte)
    centroid_1000_x   = res_centroids_1000_x,
    centroid_1000_y   = res_centroids_1000_y,
    centroid_1000_lon = res_centroids_1000_lon,
    centroid_1000_lat = res_centroids_1000_lat
  ) |>
  # Einheitliche, sprechende Reihenfolge der Spalten
  select(
    householdyearlyid, statyear,
    res_geoe_ha, res_geon_ha,                       # Rohkoordinaten LV95 (100 m gerundet)
    id_100,  centroid_100_x,  centroid_100_y,
    centroid_100_lon,  centroid_100_lat,
    id_500,  centroid_500_x,  centroid_500_y,
    id_1000, centroid_1000_x, centroid_1000_y,
    centroid_1000_lon, centroid_1000_lat
  )


# -----------------------------------------------------------------------------
# 3.4  Kontrolle der Rasterbildung
# -----------------------------------------------------------------------------
# Diese Pruefungen stellen sicher, dass die Rasterzuordnung korrekt und mit
# den BFS-Rastern konsistent ist.

# 3.4a  Keine fehlenden Werte in den Raster-IDs?  (Erwartung: ueberall 0)
hh_coordinates_grid |>
  summarise(
    na_id_100  = sum(is.na(id_100)),
    na_id_500  = sum(is.na(id_500)),
    na_id_1000 = sum(is.na(id_1000))
  )
# Befund: ueberall 0.

# 3.4b  Plausibilitaet: Anzahl Zellen und Belegung je Aufloesung.
#       Erwartung: je groesser die Zelle, desto weniger Zellen und desto mehr
#       Haushalte pro Zelle.
belegung_pro_ebene <- function(daten, id_spalte) {
  daten |>
    group_by(zelle = .data[[id_spalte]]) |>
    summarise(n_haushalte = n(), .groups = "drop") |>
    summarise(
      ebene             = id_spalte,
      anzahl_zellen     = n(),
      hh_pro_zelle_min  = min(n_haushalte),
      hh_pro_zelle_med  = median(n_haushalte),
      hh_pro_zelle_mean = round(mean(n_haushalte), 1),
      hh_pro_zelle_max  = max(n_haushalte)
    )
}

bind_rows(
  belegung_pro_ebene(hh_coordinates_grid, "id_100"),
  belegung_pro_ebene(hh_coordinates_grid, "id_500"),
  belegung_pro_ebene(hh_coordinates_grid, "id_1000")
)

# 3.4c  Hierarchie-Check: Jede feinere Zelle muss vollstaendig in GENAU einer
#       groeberen Zelle liegen. Trifft dies zu, sind die drei Raster sauber
#       ineinander verschachtelt.
hh_coordinates_grid |>
  group_by(id_100) |>
  summarise(n_500 = n_distinct(id_500), .groups = "drop") |>
  count(n_500)
# Erwartet: nur eine Zeile mit n_500 == 1.

hh_coordinates_grid |>
  group_by(id_500) |>
  summarise(n_1000 = n_distinct(id_1000), .groups = "drop") |>
  count(n_1000)
# Erwartet: nur eine Zeile mit n_1000 == 1.


# -----------------------------------------------------------------------------
# 3.5  Zellreferenz-Tabellen je Aufloesung
# -----------------------------------------------------------------------------
# Eine Zeile pro Zelle mit numerischen Zentroidkoordinaten. Diese Tabellen
# werden nach der Aggregation an die Indikatordaten angehaengt und bilden die
# Geometrie-Grundlage fuer Karten sowie fuer die raeumliche Statistik
# (Moran's I / LISA), die echte Koordinaten - keine String-IDs - benoetigt.

grid_ref_100 <- hh_coordinates_grid |>
  distinct(id_100, centroid_100_x, centroid_100_y)

grid_ref_500 <- hh_coordinates_grid |>
  distinct(id_500, centroid_500_x, centroid_500_y)

grid_ref_1000 <- hh_coordinates_grid |>
  distinct(id_1000, centroid_1000_x, centroid_1000_y)

# Optionale Kontrolle: Jede Zell-ID darf nur EIN Zentroid besitzen.
# Erwartung: alle drei Aufrufe liefern 0 Zeilen.
grid_ref_100  |> count(id_100)  |> filter(n > 1)
grid_ref_500  |> count(id_500)  |> filter(n > 1)
grid_ref_1000 |> count(id_1000) |> filter(n > 1)


# -----------------------------------------------------------------------------
# 3.6  sf-Objekte je Aufloesung
# -----------------------------------------------------------------------------
# Punktgeometrien der Zellzentroide fuer raeumliche Statistik und Karten.
# CRS = EPSG:2056 (LV95). remove = FALSE behaelt die numerischen
# Koordinatenspalten zusaetzlich zur Geometrie.

grid_sf_100 <- grid_ref_100 |>
  st_as_sf(coords = c("centroid_100_x", "centroid_100_y"),
           crs = 2056, remove = FALSE)

grid_sf_500 <- grid_ref_500 |>
  st_as_sf(coords = c("centroid_500_x", "centroid_500_y"),
           crs = 2056, remove = FALSE)

grid_sf_1000 <- grid_ref_1000 |>
  st_as_sf(coords = c("centroid_1000_x", "centroid_1000_y"),
           crs = 2056, remove = FALSE)



# =============================================================================
# 3.7 ÜBERPRÜFEN der Datensätze
# =============================================================================

glimpse(hh_coordinates_grid)
glimpse(grid_sf_500)
glimpse(hh_12_15_clean)
glimpse(hh_16_17_clean)
glimpse(hhm_12_15_clean)
glimpse(hhm_16_17_clean)
glimpse(agglo)

min(hhm_16_17_clean$hh_res_mun)
max(hhm_16_17_clean$hh_res_mun)
min(agglo$bfs_gde_nummer)
max(agglo$bfs_gde_nummer)

# =============================================================================
# 4.0 SPEICHERN
# =============================================================================
# Bereinigte Haushaltsdaten
saveRDS(hh_12_15_clean,      file.path(path_processed, "01_hh_12_15_clean.rds"))
saveRDS(hh_16_17_clean,      file.path(path_processed, "01_hh_16_17_clean.rds"))

# Bereinigte Daten zu Haushaltsmitglieder
saveRDS(hhm_12_15_clean,      file.path(path_processed, "01_hhm_12_15_clean.rds"))
saveRDS(hhm_16_17_clean,      file.path(path_processed, "01_hhm_16_17_clean.rds"))

# Haushalt-zu-Rasterzelle-Zuordnung
saveRDS(hh_coordinates_grid, file.path(path_processed, "01_hh_coordinates_grid.rds"))

# Zellreferenz mit Geometrie (fuer Karten und raeumliche Statistik)
saveRDS(grid_sf_500,         file.path(path_processed, "01_grid_sf_500.rds"))
saveRDS(grid_sf_100,         file.path(path_processed, "01_grid_sf_100.rds"))   # nur fuer Sensitivitaetsanalyse
saveRDS(grid_sf_1000,        file.path(path_processed, "01_grid_sf_1000.rds"))  # nur fuer Sensitivitaetsanalyse

# Agglomerationsreferenz
saveRDS(agglo,               file.path(path_processed, "01_agglo.rds"))



# =============================================================================
# ENDE SKRIPT 01
# Zentrale Ergebnisobjekte fuer die folgenden Skripte:
#   hh_12_15_clean, hh_16_17_clean      - bereinigte Haushaltsdaten
#   hhm_12_15_clean, hhm_16_17_clean    - bereinigte Daten zu Haushaltsmitglieder
#   hh_coordinates_grid                 - Haushalte mit Rasterzuordnung
#   grid_ref_100 / 500 / 1000           - Zellreferenz-Tabellen
#   grid_sf_100 / 500 / 1000            - Zellzentroide als sf-Objekte
#   agglo                               - Agglomerationsreferenz
# =============================================================================