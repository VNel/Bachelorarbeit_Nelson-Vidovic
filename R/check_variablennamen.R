# 1. Gleiche Variablennamen prüfen
setequal(names(hh_11_15), names(hh_16_20))

# 2. Fehlende oder zusätzliche Variablen prüfen
setdiff(names(hh_11_15), names(hh_16_20))
setdiff(names(hh_16_20), names(hh_11_15))

type_check <- tibble(
  variable = common_vars,
  type_11_15 = map_chr(hh_11_15[common_vars], ~ paste(class(.x), collapse = "/")),
  type_16_20 = map_chr(hh_16_20[common_vars], ~ paste(class(.x), collapse = "/"))
) |>
  mutate(same_type = type_11_15 == type_16_20)