library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(zoo)
library(plotly)
library(sp)
library(tmap)

url1 <- "https://opendata.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0.csv"
url2 <- "https://opendata.arcgis.com/datasets/b2e6d8854d9744ca88144d30bef06a76_1.csv"
url3 <- "https://biogeo.ucdavis.edu/data/gadm3.6/Rsf/gadm36_DEU_2_sf.rds"

covid_DE <- read_csv(url1) %>%
  mutate(IdLandkreis = as.numeric(IdLandkreis))

landkreis_DE <- read_csv(url2) %>%
  select(RS, EWZ) %>%
  rename(IdLandkreis = RS) %>%
  mutate(IdLandkreis = as.numeric(IdLandkreis))

covid_DE.BER <- covid_DE %>%
  filter(Bundesland == "Berlin") %>%
  group_by(Bundesland, Meldedatum) %>%
  summarise(Faelle = sum(AnzahlFall), Tode = sum(AnzahlTodesfall), IdLandkreis = 11000, Landkreis = "SK Berlin") %>%
  relocate(IdLandkreis, .before = Bundesland) %>%
  relocate(Landkreis, .after = Bundesland) %>%
  mutate(IdLandkreis = as.numeric(IdLandkreis), Meldedatum = as.Date(Meldedatum))

covid_DE.subset <- covid_DE %>%
  group_by(IdLandkreis, Bundesland, Landkreis, Meldedatum) %>%
  summarise(Faelle = sum(AnzahlFall), Tode = sum(AnzahlTodesfall)) %>%
  ungroup() %>%
  filter(Bundesland != "Berlin") %>%
  mutate(Meldedatum = as.Date(Meldedatum), IdLandkreis = as.numeric(IdLandkreis)) %>%
  bind_rows(covid_DE.BER)

daten <- seq(as.Date("2020/1/28"), max(covid_DE.subset$Meldedatum), "days")

covid_DE.lk <- covid_DE.subset %>%
  expand(nesting(IdLandkreis, Bundesland, Landkreis), daten) %>%
  rename(Meldedatum = daten) %>%
  left_join(covid_DE.subset) %>%
  replace_na(list(Faelle = 0, Tode = 0)) %>%
  left_join(landkreis_DE, by = "IdLandkreis") %>%
  group_by(Landkreis) %>%
  mutate(Faelle.ms = rollapply(Faelle,7,sum,align="right",fill=0), Tode.ms = rollapply(Tode,7,sum,align="right",fill=0), Faelle.ma = rollapply(Faelle,7,mean,align="right",fill=0), Tode.ma = rollapply(Tode,7,mean,align="right",fill=0)) %>%
  mutate(Faelle.ms.p100k = (Faelle.ms / EWZ) * 100000, Tode.ms.p100k = (Tode.ms /EWZ) * 100000)

plot_ly(data = covid_DE.lk, x = covid_DE.lk$Meldedatum[covid_DE.lk$Landkreis == "SK München"], y = covid_DE.lk$Faelle.ms.p100k[covid_DE.lk$Landkreis == "SK München"], type = 'scatter', mode = "lines", name = "Fälle (7 Tage) / 100.000") %>%
  add_segments(x = min(covid_DE.lk$Meldedatum), xend = max(covid_DE.lk$Meldedatum), y = 35, yend = 35, line=list(color='#E59900'), name = "Grenzwert 35") %>%
  add_segments(x = min(covid_DE.lk$Meldedatum), xend = max(covid_DE.lk$Meldedatum), y = 50, yend = 50, line=list(color='#E50000'),  name = "Grenzwert 50")

tmap_mode("view")

download.file(url3, "DEU.rds")

de.sf <- readRDS("DEU.rds") %>%
  rename(IdLandkreis = CC_2) %>%
  mutate(IdLandkreis = as.numeric(IdLandkreis))

ott.geo <- de.sf %>%
  filter(NAME_2 == "Göttingen" | NAME_2 == "Osterode am Harz") %>%
  summarise()

de.sf$geometry[de.sf$NAME_2 == "Göttingen"] <- ott.geo$geometry
de.sf$IdLandkreis[de.sf$NAME_2 == "Göttingen"] <- 3159

de.sf <- de.sf[de.sf$NAME_2 != "Osterode am Harz",]

covid_DE.map <- de.sf %>%
  left_join(covid_DE.lk[covid_DE.lk$Meldedatum == "2020-10-15",], by = "IdLandkreis")

tm_shape(covid_DE.map) + 
  tm_polygons("Faelle.ms.p100k", palette = "YlOrBr", id="Landkreis", style = "fixed", breaks = c(0, 35, 50, 100, 200)) +
  tm_basemap(server = "CartoDB.DarkMatterNoLabels")


