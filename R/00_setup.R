# =============================================================================
# BACHELORARBEIT  -  "Machine Learning for Gentrification"
# Skript 00  -  SETUP
# -----------------------------------------------------------------------------
# Autor    : Nelson Vidovic
# R-Version: 4.5.0
#
# Zweck dieses Skripts:
#   Zentrale Konfiguration des Projekts. Dieses Skript wird zu Beginn jedes
#   anderen Skripts mit source() eingebunden und stellt sicher, dass ueberall
#   dieselben Pakete, Pfade und Einstellungen gelten. So bleibt die gesamte
#   Pipeline konsistent und reproduzierbar.
#
# Einbindung in jedem anderen Skript (jeweils als erste Codezeile):
#   source(here::here("R", "00_setup.R"))
# =============================================================================


# =============================================================================
# 1.  PAKETE
# =============================================================================
# Datenaufbereitung und Analyse
#install.packages("tidyverse")
#install.packages("readxl")
#install.packages("janitor")
#install.packages("skimr")¨
#install.packages("remotes")
#remotes::install_version("rlang", version = "1.1.7")
library(tidyverse)   # dplyr, tidyr, readr, purrr, stringr, ggplot2, ...
library(readxl)      # Excel-Import (Agglomerationsreferenz)
library(janitor)     # clean_names(), tabyl()
library(skimr)       # schnelle deskriptive Datenuebersicht

# Raeumliche Daten und Statistik
#install.packages("sf")
#install.packages("terra")
#install.packages("spdep")
library(sf)          # Vektor-Geodaten, Koordinatensystem EPSG:2056
library(terra)       # Rasterdaten (nur dort, wo benoetigt)
library(spdep)       # raeumliche Gewichte, Moran's I, LISA

# Clustering
#install.packages("cluster")
#install.packages("factoextra")
library(cluster)     # Silhouettenanalyse
library(factoextra)  # Elbow-/Silhouette-Plots, Cluster-Visualisierung

# Visualisierung
#install.packages("biscale")
#install.packages("cowplot")
#install.packages("patchwork")
#install.packages("ggspatial")
#install.packages("corrplot")
#install.packages("flextable")
library(biscale)     # bivariate Choroplethenkarten (Unterfrage 2)
library(cowplot)     # Karte und Legende kombinieren (fuer biscale)
library(patchwork)   # mehrere ggplot-Objekte zu einer Abbildung
library(ggspatial)   # Massstab und Nordpfeil fuer ggplot-Karten
library(corrplot)    # Korrelationsmatrix der Indikatoren
library(flextable)   # Tabellen im Word-Format exportieren

# Hinweis: Werden mehrere raeumliche Pakete geladen, koennen einzelne
# Funktionsnamen ueberdeckt sein (z. B. extract, filter). Im Zweifelsfall
# die Funktion eindeutig mit Paketpraefix aufrufen, z. B. dplyr::filter().


# =============================================================================
# 2.  PROJEKTPFADE
# =============================================================================
# here() bildet Pfade relativ zur Projektwurzel (Ort der .Rproj-Datei) und
# macht den Code unabhaengig vom aktuellen Arbeitsverzeichnis und vom Rechner.

path_raw       <- here::here("Daten")               # rohe BFS-Dateien
path_processed <- here::here("Daten", "processed")  # bereinigte Zwischenstaende (.rds)
path_figures   <- here::here("output", "figures")   # Karten und Abbildungen
path_tables    <- here::here("output", "tables")    # exportierte Tabellen

# Ausgabeordner anlegen, falls noch nicht vorhanden
for (p in c(path_processed, path_figures, path_tables)) {
  if (!dir.exists(p)) dir.create(p, recursive = TRUE)
}


# =============================================================================
# 3.  GLOBALE EINSTELLUNGEN
# =============================================================================
# Fester Zufalls-Seed: macht zufallsabhaengige Schritte (v. a. K-Means)
# reproduzierbar. In Skripten mit Zufallskomponente den Seed zur Sicherheit
# direkt vor dem betreffenden Aufruf erneut setzen.
set.seed(2026)

# Zahlen ohne wissenschaftliche Notation ausgeben (bessere Lesbarkeit)
options(scipen = 999)


# =============================================================================
# 4.  PROJEKTKONSTANTEN
# =============================================================================
# Schweizer Koordinatensystem LV95
crs_lv95 <- 2056

# Zuordnung der Erhebungsjahre zu den beiden Beobachtungsperioden
jahre_t1 <- 2013:2014   # Periode 1
jahre_t2 <- 2016:2017   # Periode 2


# =============================================================================
# 5.  HILFSFUNKTIONEN 
# =============================================================================
# Funktionen, die in mehreren Skripten gebraucht werden, hier zentral
# definieren. Skript-spezifische Funktionen verbleiben in ihrem Skript.