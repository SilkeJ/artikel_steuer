#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Daten Reginalstatstik 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


library("tidyverse")
library("here")
library("janitor")  
library("wiesbaden")
library("httr")

set_config(use_proxy(url="http://px-internetweb.muenchen.de:80", port=80))

save_credentials(db='regio', user='RE006929', password='Schwan@68!')

# Zeichenerklärung
# - nicht vorhanden
# . Zahlenwert unbekannt oder geheim zu halten
# ... Angabe fällt später an
# / keine Angabe, da Zahlenwert nicht sicher
# X Tabellenfach gesperrt, da Aussage nicht sinnvoll
# p vorläufige Zahl
# r berichtigte Zahl
# s geschätzte Zahl


# Labels Gemeinden und Kreise ----

gemeinden <- retrieve_valuelabel("GEMEIN", genesis=c(db='regio'))

kreise <- retrieve_valuelabel("KREISE", genesis=c(db='regio'))


# Übersicht Tabellen zu einem Thema ----
d <- retrieve_datalist(tableseries="73111*", genesis=c(db='regio')) 
filter(d, str_detect(description, "Kreise"))


73111KJ001
73111KJ002
73111GJ001
73111KJ003


# Metadaten prüfen
retrieve_metadata(tablename = "73111KJ001", genesis = c(db = 'regio'))

retrieve_varinfo("EKF002", genesis = c(db = 'regio'))

retrieve_valuelabel("EKF002", genesis = c(db = 'regio'))


# Steuerstatistik 73111* ----

retrieve_metadata(tablename = "73111KJ001", genesis = c(db = 'regio'))

data <- retrieve_data(tablename = "73111KJ001", genesis = c(db = 'regio'))

df1 <- data %>%
  left_join(kreise, by = "KREISE")

attr(df1, 'datum') <- "02024-10-28"
attr(df1, 'quelle') <- "Regionalstatistik"
attr(df1, 'tabellencode') <- "73111KJ001"
attr(df1, 'bautaetigkeit') <- "Einkommensteuerstatistik"
attr(df1, 'LSNW01') <- "Lohn- und Einkommensteuerpflichtige (Anzahl)"
attr(df1, 'EKF002') <- "Gesamt der Einkünfte Tsd. EUR"
attr(df1, 'LSNW02') <- "Lohn- und Einkommensteuer Tsd. EUR"
attr(df1, 'raumbezug') <- "Kreise"
attr(df1, 'zeit') <- "1992 - 2020"

str(attributes(df1))


retrieve_metadata(tablename = "73111KJ002", genesis = c(db = 'regio'))

data <- retrieve_data(tablename = "73111KJ002", genesis = c(db = 'regio'))

retrieve_valuelabel("EKFGK7", genesis = c(db = 'regio'))

df2 <- data %>%
  left_join(kreise, by = "KREISE") %>%
  mutate(EKFGK7_f = case_when(EKFGK7 == "EKF0" ~ "0",
                              EKFGK7 == "EKFE001B2500" ~ "1 - 2500",
                              EKFGK7 == "EKFE250B050" ~ "2500 - 5000",
                              EKFGK7 == "EKFE500B075" ~ "5000 - 7500",
                              EKFGK7 == "EKFE750B100" ~ "7500 - 10000",
                              EKFGK7 == "EKFF100B0125" ~ "10000 - 12500",
                              EKFGK7 == "EKFF125B015" ~ "12500 - 15000",
                              EKFGK7 == "EKFF150B020" ~ "15000 - 20000",
                              EKFGK7 == "EKFF200B025" ~ "20000 - 25000",
                              EKFGK7 == "EKFF250B0375" ~ "25000 - 37500",
                              EKFGK7 == "EKFF375B050" ~ "37500 - 50000",
                              EKFGK7 == "EKFF500B125" ~ "50000 - 125000",
                              EKFGK7 == "EKFG125UM" ~ "125000 und mehr",
                              EKFGK7 == "INSGESAMT" ~ "INSGESAMT"))


attr(df2, 'datum') <- "02024-10-28"
attr(df2, 'quelle') <- "Regionalstatistik"
attr(df2, 'tabellencode') <- "73111KJ002"
attr(df2, 'bautaetigkeit') <- "Einkommensteuerstatistik"
attr(df2, 'LSNW01') <- "Lohn- und Einkommensteuerpflichtige (Anzahl)"
attr(df2, 'EKF002') <- "Gesamt der Einkünfte Tsd. EUR"
attr(df2, 'LSNW02') <- "Lohn- und Einkommensteuer Tsd. EUR"
attr(df2, 'EKFGK7') < "Größenklassen des Gesamtbetrags der Einkünfte"
attr(df2, 'raumbezug') <- "Kreise"
attr(df2, 'zeit') <- "1992 - 2020"

str(attributes(df2))



retrieve_metadata(tablename = "73111KJ003", genesis = c(db = 'regio'))

data <- retrieve_data(tablename = "73111KJ003", genesis = c(db = 'regio'))

retrieve_valuelabel("EKFGK8", genesis = c(db = 'regio'))

df3 <- data %>%
  left_join(kreise, by = "KREISE") %>%
  mutate(EKFGK8_f = case_when(EKFGK8 == "EKF0" ~ "0",
                              EKFGK8 == "EKFE000B5000" ~ "0 - 5000",
                              EKFGK8 == "EKFE001B5000" ~ "1 - 5000",
                              EKFGK8 == "EKFE100B015" ~ "10000 - 15000",
                              EKFGK8 == "EKFE150B020" ~ "15000 - 20000",
                              EKFGK8 == "EKFE200B025" ~ "20000 - 25000",
                              EKFGK8 == "EKFE250B030" ~ "25000 - 30000",
                              EKFGK8 == "EKFE300B035" ~ "30000 - 35000",
                              EKFGK8 == "EKFE350B050" ~ "35000 - 50000",
                              EKFGK8 == "EKFE500B100" ~ "5000 - 10000",
                              EKFGK8 == "EKFF375B050" ~ "37500 - 50000",
                              EKFGK8 == "EKFE500B125" ~ "50000 - 125000",
                              EKFGK8 == "EKFG125UM" ~ "125000 und mehr",
                              EKFGK8 == "INSGESAMT" ~ "INSGESAMT"))
df3 %>% count(EKFGK8, EKFGK8_f)

attr(df3, 'datum') <- "02024-10-28"
attr(df3, 'quelle') <- "Regionalstatistik"
attr(df3, 'tabellencode') <- "73111KJ003"
attr(df3, 'bautaetigkeit') <- "Einkommensteuerstatistik"
attr(df3, 'LSNW01') <- "Lohn- und Einkommensteuerpflichtige (Anzahl)"
attr(df3, 'EKF002') <- "Gesamt der Einkünfte Tsd. EUR"
attr(df3, 'LSNW02') <- "Lohn- und Einkommensteuer Tsd. EUR"
attr(df3, 'EKFGK8') < "Größenklassen des Gesamtbetrags der Einkünfte"
attr(df3, 'raumbezug') <- "Kreise"
attr(df3, 'zeit') <- "2007 - 2019"

str(attributes(df3))




retrieve_metadata(tablename = "73111GJ001", genesis = c(db = 'regio'))

data <- retrieve_data(tablename = "73111GJ001", genesis = c(db = 'regio'))



df4 <- data %>%
  left_join(gemeinden, by = "GEMEIN") 

attr(df4, 'datum') <- "02024-10-28"
attr(df4, 'quelle') <- "Regionalstatistik"
attr(df4, 'tabellencode') <- "73111GJ001"
attr(df4, 'bautaetigkeit') <- "Einkommensteuerstatistik"
attr(df4, 'LSNW01') <- "Lohn- und Einkommensteuerpflichtige (Anzahl)"
attr(df4, 'EKF002') <- "Gesamt der Einkünfte Tsd. EUR"
attr(df4, 'LSNW02') <- "Lohn- und Einkommensteuer Tsd. EUR"
attr(df4, 'raumbezug') <- "Gemeinden"
attr(df4, 'zeit') <- "2007 - 2020"

str(attributes(df4))


save(df1, df2, df3, df4, file = "Daten/regionalstatistik.rData")





saveRDS(df, here::here("regionalstatistik", "fertig_wohnungen_gesamt.rds"))
