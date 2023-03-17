options(scipen = 999, digits = 15, max.print = 2000)
options(dplyr.summarise.inform = FALSE)

if(!require(pacman)) 
  install.packages("pacman", repos = "http://cran.us.r-project.org")

pacman::p_load("tidyverse", "fpp3", "fable.prophet", "ggpubr")

# Pull
tmp <- tempfile()
download.file("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1169&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=CEU5500000001&scale=left&cosd=2000-01-01&coed=2023-01-01&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Monthly&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-02-20&revision_date=2023-02-20&nd=2000-01-01", tmp)

# Transform
actual <- read.csv(tmp, col.names = c("Date", "Employed")) %>% 
  mutate(Date = as.Date(Date),
         # adjust the reported number to millions for easier reading
         Employed = Employed/1000,
         Month = yearmonth(Date)) %>% 
  dplyr::select(Month, Employed) %>% as_tsibble(index = Month)

autoplot(actual, Employed) +
  labs(title = "US employment: Financial Activities",
       y = "Number of people (millions)")

# Seasonality
actual %>% gg_season(Employed, period = "year")

# Split
train <- actual %>% 
  filter(year(Month) <= 2018)
test <- actual %>% 
  filter(year(Month) >= 2019)

# Remove full length series
rm(actual)
gc()

# Seasonal differencing
train %>% 
  gg_tsdisplay(difference(Employed, 12),
               plot_type = "partial", lag = 50) +
  labs(title = "Seasonally differenced", y = "")

# Double differenced
train %>% 
  gg_tsdisplay(difference(difference(Employed,12)),
               plot_type = "partial", lag = 50) +
  labs(title = "Double differenced",
       y = "")

# FITS
STLF = decomposition_model(STL(Employed ~ season(window = Inf)),
                           ETS(season_adjust ~ season("N")))
fits <- train %>% 
  model(
    stlf = STLF,
    ets = ETS(Employed),
    seasonal_naive_drift = SNAIVE(Employed ~ drift()),
    prophet = prophet(Employed ~ season(period = 12, order = 2,
                                        type = "multiplicative")),
    arima_search = ARIMA(Employed, greedy = F, stepwise = FALSE, approx = FALSE),
  ) %>% mutate(combination = (ets + stlf + arima_search + seasonal_naive_drift) / 4)
fits

# Forecast the means
Employment_forecasts <- fits %>% 
  forecast(h = "4 years")

# Plot the means
Employment_forecasts %>% 
  autoplot(train,
           level = NULL) +
  labs(y = "Number of People (Millions)",
       title = "US employment: Financial Activities")

# Residuals ACF comparison - just two since I am using a different method
fits %>% dplyr::select(arima_search) %>% gg_tsresiduals(lag = 56) + labs(title = "Arima search", y="")
fits %>% dplyr::select(prophet) %>% gg_tsresiduals(lag = 56) + labs(title = "Prophet", y="")
# Prophet performs poorly

# RMSE 
Employment_forecasts %>% 
  accuracy(test) %>% dplyr::select(.model, RMSE) %>% 
  arrange(RMSE)
# Combination wins

# Generate sample paths and distributions
Employment_futures <- fits %>% dplyr::select(c("ets", "stlf", "arima_search",
                                               "seasonal_naive_drift", 
                                               "combination")) %>% 
  # Generate 5000 future sample paths
  generate(h = "4 years", times = 5000) %>% 
  # Compute forecast distributions from future sample paths
  as_tibble() %>% 
  group_by(Month, .model) %>% 
  summarise(dist = distributional::dist_sample(list(.sim))) %>% 
  ungroup() %>% 
  # Create fable object
  as_fable(index = Month, key = .model,
           distribution = dist, response = "Employed")


# Match the 5000 future sample path distributions from Prophet to join with 
# Employment Futures
prophet_futures <- Employment_forecasts %>% 
  filter(.model=="prophet") %>% 
  dplyr::select(.model, Month, Employed) %>% 
  `colnames<-`(c(".model","Month","dist")) %>% 
  as_fable(index = Month, key = .model, distribution = dist, 
           response = "Employed")
# Join them all
Employment_futures <- Employment_futures %>% 
  full_join(prophet_futures,
            by = join_by(Month,.model,dist))

# Winkler test
Employment_futures %>% 
  accuracy(test, measures = interval_accuracy_measures,
           level = 95) %>% 
  arrange(winkler)

# combination wins again

# Final Forecast plot
forecast(fits, h= "4 years") %>% 
  filter(.model=='combination') %>% 
  autoplot(train) +
  autolayer(test, Employed, color = "red") +
  labs(title = "US employment: Financial Activities",
       y="Number of people (millions)",
       x = "Year/Month")
