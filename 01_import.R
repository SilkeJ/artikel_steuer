#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Steuer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)


# read excel sheets
sheets <- readxl::excel_sheets("Daten/steuerjahr_2020_tidy.xlsx")
lst <- lapply(sheets, function(sheet) 
  readxl::read_excel("Daten/steuerjahr_2020_tidy.xlsx", 
                     sheet = sheet, skip = 2, na = ".")
)
names(lst) <- sheets

# kurze Var zum Filtern stbz oder stbzt erstellen
# Dann filtern auf 01, 03, 04 und 02, 03, 04
lst <- lst %>%
  map(~mutate(., raum_f = case_when(str_length(raumbezug) == 2 ~ "01",
                                    str_length(raumbezug) == 3 ~ "02",
                                    raumbezug == "Keine Zuordnung möglich" ~ "03",
                                    raumbezug == "Insgesamt" ~ "04"))) 

# shove them into global environment
list2env(lst, envir = .GlobalEnv)

# Schnellfilter stbz und stbzt erstellen mit sort, so dass
# nz und insgesamt unten stehen

filter_stbz <-function(
  filter(raum_f %in% c("01", "03", "04")) %>%
    arrange(raum_f, raumbezug) %>%
    select(-raum_f))

tab_1 %>% filter_stbz



