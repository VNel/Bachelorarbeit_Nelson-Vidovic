glimpse(hh_11.15)
glimpse(hh_16.20)
glimpse(zpers_11.15)

# Überprüfen, ob die Zielperson der Befragung auch im Haushaltsmitgliederdatensatz enthalten ist 

zpers_11.15 |>
  filter(HOUSEHOLDYEARLYID == 2011000003) |>
  select(HOUSEHOLDYEARLYID, AGE, SEX, HIGHESTCOMPLEDUAGGII, CURRACTIVITYSTATUSI)

hh_11.15 |>
  filter(HOUSEHOLDYEARLYID == 2011000003) |>
  select(HOUSEHOLDYEARLYID, HH_AGE, HH_SEX, HH_HIGHESTCOMPLEDUAGGII, HH_CURRACTIVITYSTATUSI, HH_PERSONNUMBER)

# Die Zielperson ist im Datensatz der Haushaltmitglieder unter der Personennummer 1 enthalten. Das haben einzelne Stichproben ergeben. Nun überprüfe ich das noch für alle.


# Hilfsfunktion: Werte vergleichen, auch wenn beide NA sind
same_value <- function(x, y) {
  coalesce(x == y, FALSE) | (is.na(x) & is.na(y))
}

# Zielpersonen-Datensatz vorbereiten
zpers_check <- zpers_11.15 |>
  select(
    HOUSEHOLDYEARLYID,
    AGE_zpers = AGE,
    SEX_zpers = SEX,
    EDU_zpers = HIGHESTCOMPLEDUAGGII,
    ACT_zpers = CURRACTIVITYSTATUSI
  )

# Haushaltsmitglieder-Datensatz vorbereiten: nur Personennummer 1
hh_person1 <- hh_11.15 |>
  filter(HH_PERSONNUMBER == 1) |>
  select(
    HOUSEHOLDYEARLYID,
    HH_PERSONNUMBER,
    AGE_hh = HH_AGE,
    SEX_hh = HH_SEX,
    EDU_hh = HH_HIGHESTCOMPLEDUAGGII,
    ACT_hh = HH_CURRACTIVITYSTATUSI
  )

# Vergleich Zielperson vs. Haushaltsmitglied mit Personennummer 1
check_target_person <- zpers_check |>
  left_join(hh_person1, by = "HOUSEHOLDYEARLYID") |>
  mutate(
    found_personnumber_1 = !is.na(HH_PERSONNUMBER),
    age_match = same_value(AGE_zpers, AGE_hh),
    sex_match = same_value(SEX_zpers, SEX_hh),
    edu_match = same_value(EDU_zpers, EDU_hh),
    act_match = same_value(ACT_zpers, ACT_hh),
    target_matches_hh_person1 =
      found_personnumber_1 &
      age_match &
      sex_match &
      edu_match &
      act_match
  )

# Ergebnis anzeigen
check_target_person

check_target_person |>
  count(target_matches_hh_person1)

check_target_person |>
  filter(!target_matches_hh_person1) |>
  View()


# überprüfen, ob die FALSE Werte nur Einpershaushalte betreffen
df_hh_11_15 |>
  filter(householdyearlyid == 2011000043) |>
  select(householdyearlyid, hh_type_bfs_agg)

# Nach Überprüfung mehrer householdyearlyid's habe ich bemerkt, dass die Zielpersonen welche im Datensatz der Haushaltsmitglieder nicht erfasst sind, im Haushalts-Datensatz ebenfalls nicht erfasst sind. 
# Deshalb muss ich bei der Datenbereinigung berücksichtigen, dass ich zuerst sicherstelle, dass die Datensätze zum Haushalt, zur Zielperson und zu den Haushaltsmitglieder diesselben housholdyearlyid's besitzen, 
# da es sonst viele unvollständige Zeilen im gemeinsamen Datensatz geben wird.

glimpse(df_hh_11_15)


#---------------------

# Sind die Gewichtungen der Haushalte und Personen vollständig erfasst?


hh_11_15 |>
  summarise(
    n_missing = sum(is.na(hh_weight_2011_2015)),
    n_available = sum(!is.na(hh_weight_2011_2015)),
    n_missing_p = sum(is.na(p_weight_2011_2015)),
    n_available_p = sum(!is.na(p_weight_2011_2015)),
    n_total = n()
  )


hh_16_20 |>
  summarise(
    n_missing = sum(is.na(hh_weight_2016_2020)),
    n_available = sum(!is.na(hh_weight_2016_2020)),
    n_missing_p = sum(is.na(p_weight_2016_2020)),
    n_available_p = sum(!is.na(p_weight_2016_2020)),
    n_total = n()
  )


hhm_11_15 |>
  summarise(
    n_missing = sum(is.na()),
    n_available = sum(!is.na(hh_weight_2011_2015)),
    n_missing_p = sum(is.na(p_weight_2011_2015)),
    n_available_p = sum(!is.na(p_weight_2011_2015)),
    n_total = n()
  )

# Gewichtungen sind alle vollständig erfasst, aber die p_weights sind sehr wahrscheinlich nur für die zielperson gedacht. Im User-Code Excel und im User-Syn Excel steht leider nichts näher dazu.

glimpse(hhm_11_15)
glimpse(hhm_16_20)


# Überprüfen ob es mehrere Einträge für die selbe hh-id gibt oder nicht. Und dann schauen, ob hh_weight immer anders oder gleich ist.

hh_11_15 |>
  select(hh_weight_2011_2015, statyear, householdyearlyid) |>
  head()

hh_11_15 |>
  filter(householdyearlyid == 2012155550) |>
  select(hh_weight_2011_2015, statyear, householdyearlyid)

hh_11_15 |>
  count(householdyearlyid) |>
  filter(n > 1)

hh_16_20 |>
  count(householdyearlyid) |>
  filter(n > 1)

# wichtige Erkenntnis: HH-id ist ganz sicher eindeutig pro jahr. Genau so wie bereits vermutet, ist es nun noch bestätigt worden.






# Überprüfen wie die Gewichtungen über die 5 Jahres-Zeitspanne zusammengestellt wurden

hh_11_15 |>
  group_by(statyear) |>
  summarise(
    sum_hh_weight = sum(hh_weight_2011_2015, na.rm = TRUE),
    .groups = "drop"
  )


hh_16_20 |>
  group_by(statyear) |>
  summarise(
    sum_hh_weight = sum(hh_weight_2016_2020, na.rm = TRUE),
    .groups = "drop"
  )
