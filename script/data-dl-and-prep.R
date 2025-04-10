#PACKAGE USED : magrittr, dplyr, tidyr, lubridate, quantmod, readr, purrr

library(magrittr)

# Liste des valeurs boursières à télécharger

list_of_markets <- c("^GSPC", # S&P 500
                     "^GDAXI", # Dax
                     "^FCHI", # CAC 40
                     "^N225", # NIKKEI 225
                     "^FTSE", # FTSE 100
                     "000001.SS", # SSE COMPOSITE INDEX
                     "^HSI" # HANG SENG
)


# Téléchargement des valeurs

markets_data <- purrr::map_dfr(list_of_markets, function(x) {
  
  result_xts <- quantmod::getSymbols(x, src = "yahoo", from = "2022-01-01", to = lubridate::today(), auto.assign = F)
  colnames(result_xts) <- c("open", "high", "low", "close", "volume", "adjusted")
  result_df <- data.frame(date = zoo::index(result_xts), zoo::coredata(result_xts)) %>%
    dplyr::mutate(symbol=x)
  
  return(result_df)
  
})

# Tri des données de marché
tidy_markets_data <- markets_data %>%
  dplyr::mutate(market = dplyr::case_when(
    symbol == "^GSPC" ~ "New York",
    symbol == "^GDAXI" ~ "Francfort", # Dax
    symbol == "^FCHI" ~ "Paris", # CAC 40
    symbol == "^N225" ~ "Tokyo", # NIKKEI 225
    symbol == "^FTSE" ~ "Londres", # FTSE 100
    symbol == "000001.SS" ~ "Shanghaï", # SSE COMPOSITE INDEX
    symbol == "^HSI"  ~ "Hong Kong" # HANG SENG
  )) %>%
  dplyr::select(-symbol)


# Valeurs de référence (pour la base 100) (les valeurs des marchés à leur clôture, le vendredi 17 janvier)
base_value_df <- tidy_markets_data %>%
  dplyr::filter(date == lubridate::ymd("2025-01-17")) %>%
  dplyr::select(market, base_value=close)

# Transposition des valeurs journalières en base 100
markets_data_into_base100 <- tidy_markets_data %>%
  dplyr::select(date, close, market) %>%
  dplyr::filter(date>=lubridate::ymd("2025-01-17")) %>%
  dplyr::left_join(base_value_df, by = "market") %>%
  dplyr::mutate(evol_base_100 = (close/base_value)*100-100) %>%
  dplyr::select(date, evol_base_100, market)

# Transposition des données pour Flourish
tidy_data_to_flourish <- data.frame(date=seq(lubridate::ymd("2025-01-17"), lubridate::today(), by = "1 day")) %>%
  dplyr::left_join(markets_data_into_base100, by = "date") %>%
  dplyr::mutate(market=ifelse(is.na(market), "remove", market)) %>%
  tidyr::spread(key="market", value="evol_base_100", fill="") %>%
  dplyr::select(-remove) %>%
  dplyr::mutate(mois=as.character(lubridate::month(date, label=T, abbr=F))) %>%
  dplyr::mutate(date_axis = ifelse(lubridate::day(date)==1, mois, as.character(date)),
         date_pop_up = ifelse(lubridate::day(date)==1,
                              paste0(lubridate::day(date), "er ", lubridate::month(date, label=T, abbr=F), " ", lubridate::year(date)),
                              paste0(lubridate::day(date), " ", lubridate::month(date, label=T, abbr=F), " ", lubridate::year(date))
         )) %>%
  dplyr::select(-mois)


#Ecriture des données
readr::write_csv(tidy_data_to_flourish, "data/tidy_markets_data_to_flourish.csv")









