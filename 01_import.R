#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Import Steuer
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(tidyverse)
library(rlang)

# read excel sheets
sheets <- readxl::excel_sheets("Daten/steuerjahr_2020_tidy.xlsx")
lst <- lapply(sheets, function(sheet) 
  readxl::read_excel("Daten/steuerjahr_2020_tidy.xlsx", 
                     sheet = sheet, skip = 2, na = ".")
)
names(lst) <- sheets

# kurze Var zum Filtern stbz oder stbzt erstellen
lst <- lst %>%
  map(~mutate(., raum_f = case_when(str_length(raumbezug) == 2 ~ "01",
                                    str_length(raumbezug) == 3 ~ "02",
                                    raumbezug == "Keine Zuordnung möglich" ~ "03",
                                    raumbezug == "Insgesamt" ~ "04"))) 

# shove them into global environment
list2env(lst, envir = .GlobalEnv)


# Pattern zum Filtern für Stbz oder Stbzt inklusive NZ und Gesamtzeile
f_stbz <- exprs(.data$raum_f %in% c("01", "03", "04"))
f_stbzt <- exprs(.data$raum_f %in% c("02", "03", "04"))

# Test Filter
tab_1 %>%
  filter(!!!f_stbzt) %>%
  arrange(raum_f, raumbezug) %>%
  select(-raum_f) %>% print(n = Inf)







