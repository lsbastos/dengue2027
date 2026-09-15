library(tidyverse)
library(INLA)
library(geofacet)
# library(forecastID)
source("code/sprint_fun.R")

dengue <- read_csv("data/dengue.csv.gz")
chik <- read_csv("data/chikungunya.csv.gz")

# Removendo dados de 2026
dengue <- dengue |> filter(date < "2026-01-01") 
chik <- chik |> filter(date < "2026-01-01") 

# Pegando dados atualizados de 2026
dengue.update <- read_csv("data/dengue_update_2026.csv.gz")
chik.update <- read_csv("data/chikungunya_update_2026.csv.gz")

# Atualizando bases

dengue <- dengue |> bind_rows(dengue.update)
chik <- chik |> bind_rows(chik.update)


# Tyding and cleaning -----------------------------------------------------

# Removing data from 2021 and 2022 from Espirito Santo (There are notification issues)

dengue <- dengue |> 
  mutate(
    # Removendo casos do ES de 2021 e 2022 (total de casos nesses dois anos 0 e 481!)
    casos = if_else(uf == "ES" & (epiyear(date) == 2021 | epiyear(date) == 2022), NA, casos)
  ) |> drop_na(casos) 

chik <- chik |> 
  mutate(
    # Removendo casos do ES de 2021 e 2022 (total de casos nesses dois anos 0 e 481!)
    casos = if_else(uf == "ES" & (epiyear(date) == 2021 | epiyear(date) == 2022), NA, casos)
  ) |> drop_na(casos) 


dengue |> 
  group_by(date, uf) |> 
  summarise(casos = sum(casos)) |> 
  ggplot(aes(x = date, y = casos)) + 
  geom_line() +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y")

chik |> 
  group_by(date, uf) |> 
  summarise(casos = sum(casos)) |> 
  ggplot(aes(x = date, y = casos)) + 
  geom_line() +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y")


# selufs <- sort(unique(dengue$uf))
# 
# dengue <- dengue %>% 
#   filter(uf %in% selufs)


# Macroregioes ------------------------------------------------------------


# Criando as listas

dengue.tbl <- dengue %>% 
  group_by(uf, macroregional_geocode) %>% tally() %>% 
  group_by(uf) %>% 
  mutate( n = n())


macros <- unique(dengue$macroregional_geocode)


list.forecast_dengue_1 = vector(mode = "list", length = length(macros))
names(list.forecast_dengue_1) = macros

list.forecast_dengue_4 = list.forecast_dengue_3 = list.forecast_dengue_2 = list.forecast_dengue_1
list.forecast_chik_4 = list.forecast_chik_3 = list.forecast_chik_2 = list.forecast_chik_1 = list.forecast_dengue_1



# For replicability purposes
set.seed(42)


#k = 1
for(k in 1:length(macros)){
  
  # Traning data 1
  data.train_1.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_1 == T & target_1 == F) | (train_1 == F & target_1 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_1, target = target_1) %>% 
    summarise(
      cases = sum(casos), 
      .groups = "drop"
    ) 

  chik.train_1.macro.k = chik %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_1 == T & target_1 == F) | (train_1 == F & target_1 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_1, target = target_1) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 
  
  # Traning data 2
  data.train_2.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_2 == T & target_2 == F) | (train_2 == F & target_2 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_2, target = target_2) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 

  chik.train_2.macro.k = chik %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_2 == T & target_2 == F) | (train_2 == F & target_2 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_2, target = target_2) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 
  
  # Traning data 3
  data.train_3.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_3 == T & target_3 == F) | (train_3 == F & target_3 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_3, target = target_3) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 

  chik.train_3.macro.k = chik %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_3 == T & target_3 == F) | (train_3 == F & target_3 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_3, target = target_3) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 
  
  # Traning data 4
  data.train_4.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_4 == T & target_4 == F) | (train_4 == F & target_4 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_4, target = target_4) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    )
  
  data.train_4.macro.k = data.train_4.macro.k |> 
    bind_rows(
      tibble(
        Date = ymd("2027-01-03") + 7*(0:51),
        macroregional_geocode = macros[k], 
        uf = data.train_4.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )

  chik.train_4.macro.k = chik %>% 
    filter(
      macroregional_geocode == macros[k], 
      (train_4 == T & target_4 == F) | (train_4 == F & target_4 == T)
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf, train = train_4, target = target_4) %>% 
    summarise(
      cases = sum(casos), .groups = "drop"
    ) 
  
  chik.train_4.macro.k = chik.train_4.macro.k |> 
    bind_rows(
      tibble(
        Date = ymd("2027-01-03") + 7*(0:51),
        macroregional_geocode = macros[k], 
        uf = data.train_4.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )
  
  
  cat(k, data.train_1.macro.k$uf[1] , 
      data.train_1.macro.k$macroregional_geocode[1], "\n")
  
    cat("Dengue ")
  list.forecast_dengue_4[[k]]$out = forecasting_sprint(data.train_4.macro.k, DT = "2015-10-11")
  list.forecast_dengue_3[[k]]$out = forecasting_sprint(data.train_3.macro.k, DT = "2015-10-11")
  list.forecast_dengue_2[[k]]$out = forecasting_sprint(data.train_2.macro.k, DT = "2015-10-11")
  list.forecast_dengue_1[[k]]$out = forecasting_sprint(data.train_1.macro.k, DT = "2015-10-11")
  cat("Chik \n")
  list.forecast_chik_4[[k]]$out = forecasting_sprint(chik.train_4.macro.k, DT = "2015-10-11")
  list.forecast_chik_3[[k]]$out = forecasting_sprint(chik.train_3.macro.k, DT = "2015-10-11")
  list.forecast_chik_2[[k]]$out = forecasting_sprint(chik.train_2.macro.k, DT = "2015-10-11")
  list.forecast_chik_1[[k]]$out = forecasting_sprint(chik.train_1.macro.k, DT = "2015-10-11")
  
  
}


# df.forecast_dengue_1 <- tbl_total( list.forecast_dengue_1 |> 
#              map(function(x) x$out$MC) |>  bind_rows() |> filter(week < 53)
#            )


data_start_target_1 = min(list.forecast_dengue_1[[1]]$out$pred$Date)
data_start_target_2 = min(list.forecast_dengue_2[[1]]$out$pred$Date)
data_start_target_3 = min(list.forecast_dengue_3[[1]]$out$pred$Date)
data_start_target_4 = min(list.forecast_dengue_4[[1]]$out$pred$Date)


df.forecast.week_dengue_1 <- tbl_forecasting_week(list.forecast_dengue_1 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_1 + 7*(week-1)
  )

df.forecast.week_dengue_2 <- tbl_forecasting_week(list.forecast_dengue_2 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_2 + 7*(week-1)
  )

df.forecast.week_dengue_3 <- tbl_forecasting_week(list.forecast_dengue_3 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_3 + 7*(week-1)
  )

df.forecast.week_dengue_4 <- tbl_forecasting_week(list.forecast_dengue_4 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_4 + 7*(week-1)
  )


df.forecast.week_chik_1 <- tbl_forecasting_week(list.forecast_chik_1 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_1 + 7*(week-1)
  )

df.forecast.week_chik_2 <- tbl_forecasting_week(list.forecast_chik_2 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_2 + 7*(week-1)
  )

df.forecast.week_chik_3 <- tbl_forecasting_week(list.forecast_chik_3 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_3 + 7*(week-1)
  )

df.forecast.week_chik_4 <- tbl_forecasting_week(list.forecast_chik_4 |> 
                                                    map(function(x) x$out$MC) |>  
                                                    bind_rows() |> filter(week < 53)) |> 
  mutate(
    date = data_start_target_4 + 7*(week-1)
  )



df.forecast.week_dengue_1 |>  write_csv("forecasts/imdc2026/den_train_1.csv")
df.forecast.week_dengue_2 |>  write_csv("forecasts/imdc2026/den_train_2.csv")
df.forecast.week_dengue_3 |>  write_csv("forecasts/imdc2026/den_train_3.csv")
df.forecast.week_dengue_4 |>  write_csv("forecasts/imdc2026/den_train_4.csv")
df.forecast.week_dengue_4 |>   write_csv(file = "forecasts/imdc2026/den_train_4_updated.csv")


df.forecast.week_chik_1 |>  write_csv("forecasts/imdc2026/chik_train_1.csv")
df.forecast.week_chik_2 |>  write_csv("forecasts/imdc2026/chik_train_2.csv")
df.forecast.week_chik_3 |>  write_csv("forecasts/imdc2026/chik_train_3.csv")
df.forecast.week_chik_4 |>  write_csv("forecasts/imdc2026/chik_train_4.csv")
df.forecast.week_chik_4 |>  write_csv("forecasts/imdc2026/chik_train_4_updated.csv")

# 
# df.forecast <- df.forecast |> 
#   mutate(
#     values = if_else(is.na(values2), values, values2)
#   ) |> select(-values2)


# saveRDS(df.forecast, file = "forecasts/samples/sprint2025_forecast.rds")
# df.forecast = readRDS(file = "forecasts/samples/sprint2025_forecast.rds")


# Forecast por ano

tbl.total.uf.forecast <- df.forecast %>%
  group_by(uf, samples) %>%
  summarise(
    values = sum(values)
  ) %>% group_by(uf) %>%
  summarise(
    pred = median(values),
    lower_95 =  quantile(values, probs = 0.025),
    lower_90 =  quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 =  quantile(values, probs = 0.25),
    upper_50 =  quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 =  quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) %>%
  bind_rows(
    tibble(uf = "BR") %>% bind_cols(df.forecast %>%
                                      group_by(samples) %>%
                                      summarise(
                                        values = sum(values)
                                      ) %>% #group_by(uf) %>%
                                      summarise(
                                        pred = median(values),
                                        lower_95 =  quantile(values, probs = 0.025),
                                        lower_90 =  quantile(values, probs = 0.05),
                                        lower_80 = quantile(values, probs = 0.10),
                                        lower_50 =  quantile(values, probs = 0.25),
                                        upper_50 =  quantile(values, probs = 0.75),
                                        upper_80 = quantile(values, probs = 0.9),
                                        upper_90 =  quantile(values, probs = 0.95),
                                        upper_95 = quantile(values, probs = 0.975),                                      )
    )
  )




tbl.total.plot <- tbl.total.uf.forecast |> 
  left_join(dengue |> 
              filter(year(date) == 2025) |> 
              group_by(uf) |> 
              summarise(casos2025 = sum(casos))) |> 
  mutate(
    frq = pred - casos2025
  )

ggplot(tbl.total.plot, 
       aes(x=uf, y=frq)) + 
  geom_bar(aes(fill = frq < 0), stat = "identity") + 
  scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("Green", "red")) + 
  scale_x_discrete(limits = tbl.total.plot$uf)+ theme(legend.position="none")+
  labs(x = "UFs",
       y = "Differences between obversed and predictied") +
  ylab("Frequency")

write_csv(tbl.total.plot, file = "forecasts/uf.total.forecast.csv")





tbl.uf.week.forecast <- df.forecast %>% 
  group_by(uf, week, samples) %>% 
  summarise(
    values = sum(values)
  ) %>% group_by(uf, week) %>% 
  summarise(
    pred = median(values),
    lower_95 =  quantile(values, probs = 0.025),
    lower_90 =  quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 =  quantile(values, probs = 0.25),
    upper_50 =  quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 =  quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) %>% 
  bind_rows(
    tibble(uf = "BR") %>%
      bind_cols(df.forecast %>%
                  group_by(week, samples) %>%
                  summarise(
                    values = sum(values)
                  ) %>% group_by(week) %>%
                  summarise(
                    pred = median(values),
                    lower_95 =  quantile(values, probs = 0.025),
                    lower_90 =  quantile(values, probs = 0.05),
                    lower_80 = quantile(values, probs = 0.10),
                    lower_50 =  quantile(values, probs = 0.25),
                    upper_50 =  quantile(values, probs = 0.75),
                    upper_80 = quantile(values, probs = 0.9),
                    upper_90 =  quantile(values, probs = 0.95),
                    upper_95 = quantile(values, probs = 0.975),
                  )
      )
  )

tbl.uf.week.forecast <- tbl.uf.week.forecast |> 
  left_join(
    tibble(week = 1:52, date = ymd("2027-01-03") + 7*(0:51))
    # tibble(week = 1:53, date = ymd("2026-10-11") + 7*(0:52))
  ) |> select(uf, week, date, pred:upper_95)

write_csv(tbl.uf.week.forecast, file = "forecasts/uf.week.forecast.csv")


aaa <- dengue |> 
  group_by(date, uf) |> 
  summarise(casos = sum(casos)) 

g1 <- ggplot(data = aaa) + 
  geom_point(mapping = aes(x = date, y = casos)) +
  # geom_line(data = tbl.uf.week.forecast, mapping = aes(x = date, y = pred), color = "red") +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y") 


g2 <- ggplot(data = tbl.uf.week.forecast) + 
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_50, ymax = upper_50), fill = "red", alpha = 0.5) +
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_80, ymax = upper_80), fill = "red", alpha = 0.25) +
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_90, ymax = upper_90), fill = "red", alpha = 0.15) +
  geom_line( mapping = aes(x = date, y = pred), color = "red") +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y") 

teste <- aaa |> bind_rows(tbl.uf.week.forecast)


g3 <- ggplot(data = teste |> filter(date > "2020-01-01")) + 
  geom_line(mapping = aes(x = date, y = casos)) +
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_50, ymax = upper_50), fill = "red", alpha = 0.5) +
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_80, ymax = upper_80), fill = "red", alpha = 0.25) +
  geom_ribbon( mapping = aes(x = date, y = pred, ymin = lower_90, ymax = upper_90), fill = "red", alpha = 0.15) +
  geom_line( mapping = aes(x = date, y = pred), color = "red") +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y") 

g3
