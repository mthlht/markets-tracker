library(tidyverse)
library(lubridate)
library(tidyquant)

# Liste des valeurs boursières à télécharger

list_of_markets <- c("^DJI", # Dow Jones Industrial Average
                     "^GDAXI", # Dax
                     "^FCHI", # CAC 40
                     "^N225", # NIKKEI 225
                     "^FTSE", # FTSE 100
                     "000001.SS", # SSE COMPOSITE INDEX
                     "^HSI" # HANG SENG
)

# Téléchargement des valeurs

markets_data <- purrr::map_dfr(list_of_markets, function(x) {
  
  tq_get(x, from="2025-01-01", to=Sys.Date())
  
})

# Tri des données de marché
tidy_markets_data <- markets_data %>%
  mutate(market=case_when(
    symbol == "^DJI" ~ "New York",
    symbol == "^GDAXI" ~ "Francfort", # Dax
    symbol == "^FCHI" ~ "Paris", # CAC 40
    symbol == "^N225" ~ "Tokyo", # NIKKEI 225
    symbol == "^FTSE" ~ "Londres", # FTSE 100
    symbol == "000001.SS" ~ "Shanghaï", # SSE COMPOSITE INDEX
    symbol == "^HSI"  ~ "Hong Kong" # HANG SENG
  )) %>%
  select(-symbol)


# Valeurs de référence (pour la base 100) (les valeurs des marchés à leur clôture, le vendredi 17 janvier)
base_value_df <- tidy_markets_data %>%
  filter(date==ymd("2025-01-17")) %>%
  select(market, base_value=close)

# Transposition des valeurs journalières en base 100
markets_data_into_base100 <- tidy_markets_data %>%
  select(date, close, market) %>%
  filter(date>=ymd("2025-01-17")) %>%
  left_join(base_value_df, by = "market") %>%
  mutate(evol_base_100 = (close/base_value)*100-100) %>%
  select(date, evol_base_100, market)

# Transposition des données pour Flourish
tidy_data_to_flourish <- data.frame(date=seq(ymd("2025-01-17"), ymd("2025-04-07"), by = "1 day")) %>%
  left_join(markets_data_into_base100, by = "date") %>%
  mutate(market=ifelse(is.na(market), "remove", market)) %>%
  spread(key="market", value="evol_base_100", fill="") %>%
  select(-remove) %>%
  mutate(mois=as.character(month(date, label=T, abbr=F))) %>%
  mutate(date_axis = ifelse(day(date)==1, mois, as.character(date)),
         date_pop_up = ifelse(day(date)==1,
                              paste0(day(date), "er ", month(date, label=T, abbr=F), " ", year(date)),
                              paste0(day(date), " ", month(date, label=T, abbr=F), " ", year(date))
         )) %>%
  select(-mois)


#Ecriture des données
write_csv(tidy_data_to_flourish, "data/tidy_markets_data_to_flourish.csv")









