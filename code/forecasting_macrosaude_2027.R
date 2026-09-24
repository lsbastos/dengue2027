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


aaa<- dengue |> 
  group_by(date, uf) |> 
  summarise(casos = sum(casos)) 

aaa |> 
  ggplot(aes(x = date, y = casos)) + 
  geom_line() +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y")


# selufs <- sort(unique(dengue$uf))
# 
# dengue <- dengue %>% 
#   filter(uf %in% selufs)

dengue.tbl <- dengue %>% 
  group_by(uf, macroregional_geocode) %>% tally() %>% 
  group_by(uf) %>% 
  mutate( n = n())


# For replicability purposes
set.seed(42)

# Macroregioes ------------------------------------------------------------


# Criando as listas

macros <- unique(dengue$macroregional_geocode)


list.dengue.forecast = vector(mode = "list", length = length(macros))
list.chik.forecast = vector(mode = "list", length = length(macros))

names(list.dengue.forecast) = macros
names(list.chik.forecast) = macros


#k = 1
for(k in 1:length(macros)){
  
  data.train.macro.k = dengue %>% 
    filter(
      macroregional_geocode == macros[k]
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = TRUE, target = FALSE, .groups = "drop"
    ) 
  
  
  data.train.macro.k = data.train.macro.k |> 
    bind_rows(
      tibble(
        # Date = ymd("2027-01-03") + 7*(0:51),
        Date = ymd("2026-10-11") + 7*(0:51),
        #### aa = "2026-10-05"; ymd(aa) |> epiweek(); ymd(aa) |> wday()
        # Date = ymd("2026-10-11") + 7*(0:52),
        macroregional_geocode = data.train.macro.k$macroregional_geocode[1], 
        uf = data.train.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )

  chik.train.macro.k = chik %>% 
    filter(
      macroregional_geocode == macros[k]
    ) %>% 
    group_by(Date = date, macroregional_geocode, uf) %>% 
    summarise(
      cases = sum(casos),
      train = TRUE, target = FALSE, .groups = "drop"
    ) 
  
  
  chik.train.macro.k = chik.train.macro.k |> 
    bind_rows(
      tibble(
        # Date = ymd("2027-01-03") + 7*(0:51),
        Date = ymd("2026-10-11") + 7*(0:51),
        #### aa = "2026-10-05"; ymd(aa) |> epiweek(); ymd(aa) |> wday()
        # Date = ymd("2026-10-11") + 7*(0:52),
        macroregional_geocode = data.train.macro.k$macroregional_geocode[1], 
        uf = data.train.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )
  
  
  list.dengue.forecast[[k]]$out = forecasting_sprint(data.train.macro.k, DT = "2015-10-11")
  list.chik.forecast[[k]]$out = forecasting_sprint(chik.train.macro.k, DT = "2015-10-11")
  
  cat(k, data.train.macro.k$uf[1] , 
      data.train.macro.k$macroregional_geocode[1], "\n")
  
}


df.dengue.forecast <- list.dengue.forecast |> 
  map(function(x) x$out$MC)  |>  bind_rows() #|> rename(values2=values)

df.chik.forecast <- list.chik.forecast |> 
  map(function(x) x$out$MC)  |> bind_rows() #|> rename(values2=values)


# df.forecast.ES <- list.forecast %>% map(function(x) x$out$MC) %>% bind_rows() |> rename(values2=values)
# 
# df.forecast <- df.forecast |> left_join(df.forecast.ES)
#
# df.forecast |> drop_na(values2) |> group_by(uf,macrocode,samples) |> 
#   summarise(old = sum(values),
#             new = sum(values2)) |> 
#   group_by(uf,macrocode) |> 
#   summarise(old = mean(old),
#             old.li = quantile(old, probs = 0.05),
#             old.ls = quantile(old, probs = 0.95),
#             new = mean(new),
#             new.li = quantile(new, probs = 0.05),
#             new.ls = quantile(new, probs = 0.95))
# 
# df.forecast <- df.forecast |> 
#   mutate(
#     values = if_else(is.na(values2), values, values2)
#   ) |> select(-values2)


# saveRDS(df.forecast, file = "forecasts/samples/sprint2025_forecast.rds")
# df.forecast = readRDS(file = "forecasts/samples/sprint2025_forecast.rds")


# Forecast por ano

tbl.total.uf.dengue.forecast <- df.dengue.forecast |> 
  group_by(uf, samples) |> 
  summarise(
    values = sum(values)
  ) |>  
  group_by(uf) |> 
  summarise(
    pred = median(values),
    lower_95 = quantile(values, probs = 0.025),
    lower_90 = quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 = quantile(values, probs = 0.25),
    upper_50 = quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 = quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) |> 
  bind_rows(
    tibble(uf = "BR")  |> 
      bind_cols(df.dengue.forecast |> 
                  group_by(samples) |> 
                  summarise(
                    values = sum(values)
                  )  |> 
                  summarise(
                    pred = median(values),
                    lower_95 = quantile(values, probs = 0.025),
                    lower_90 = quantile(values, probs = 0.05),
                    lower_80 = quantile(values, probs = 0.10),
                    lower_50 = quantile(values, probs = 0.25),
                    upper_50 = quantile(values, probs = 0.75),
                    upper_80 = quantile(values, probs = 0.9),
                    upper_90 = quantile(values, probs = 0.95),
                    upper_95 = quantile(values, probs = 0.975),                                      
                  )
      )
  )



tbl.total.uf.chik.forecast <- df.chik.forecast |> 
  group_by(uf, samples) |> 
  summarise(
    values = sum(values)
  ) |>  
  group_by(uf) |> 
  summarise(
    pred = median(values),
    lower_95 = quantile(values, probs = 0.025),
    lower_90 = quantile(values, probs = 0.05),
    lower_80 = quantile(values, probs = 0.10),
    lower_50 = quantile(values, probs = 0.25),
    upper_50 = quantile(values, probs = 0.75),
    upper_80 = quantile(values, probs = 0.9),
    upper_90 = quantile(values, probs = 0.95),
    upper_95 = quantile(values, probs = 0.975),
  ) |> 
  bind_rows(
    tibble(uf = "BR")  |> 
      bind_cols(df.chik.forecast |> 
                  group_by(samples) |> 
                  summarise(
                    values = sum(values)
                  )  |> 
                  summarise(
                    pred = median(values),
                    lower_95 = quantile(values, probs = 0.025),
                    lower_90 = quantile(values, probs = 0.05),
                    lower_80 = quantile(values, probs = 0.10),
                    lower_50 = quantile(values, probs = 0.25),
                    upper_50 = quantile(values, probs = 0.75),
                    upper_80 = quantile(values, probs = 0.9),
                    upper_90 = quantile(values, probs = 0.95),
                    upper_95 = quantile(values, probs = 0.975),                                      
                  )
      )
  )


# tbl.total.plot <- tbl.total.uf.forecast |> 
#   left_join(dengue |> 
#               filter(year(date) == 2025) |> 
#               group_by(uf) |> 
#               summarise(casos2025 = sum(casos))) |> 
#   mutate(
#     frq = pred - casos2025
#   )
# 
# ggplot(tbl.total.plot, 
#        aes(x=uf, y=frq)) + 
#   geom_bar(aes(fill = frq < 0), stat = "identity") + 
#   scale_fill_manual(guide = FALSE, breaks = c(TRUE, FALSE), values=c("Green", "red")) + 
#   scale_x_discrete(limits = tbl.total.plot$uf)+ theme(legend.position="none")+
#   labs(x = "UFs",
#        y = "Differences between obversed and predictied") +
#   ylab("Frequency")

write_csv(tbl.total.uf.chik.forecast, file = "forecasts/uf.total.chik.forecast_3rimdc.csv")
write_csv(tbl.total.uf.dengue.forecast, file = "forecasts/uf.total.dengue.forecast_3rimdc.csv")





tbl.uf.week.dengue.forecast <- df.dengue.forecast %>% 
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
      bind_cols(df.dengue.forecast %>%
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

tbl.uf.week.dengue.forecast <- tbl.uf.week.dengue.forecast |> 
  left_join(
    # tibble(week = 1:52, date = ymd("2027-01-03") + 7*(0:51))
    tibble(week = 1:52, date = ymd("2026-10-11") + 7*(0:51))
  ) |> select(uf, week, date, pred:upper_95)



tbl.uf.week.chik.forecast <- df.chik.forecast %>% 
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
      bind_cols(df.chik.forecast %>%
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

tbl.uf.week.chik.forecast <- tbl.uf.week.chik.forecast |> 
  left_join(
    # tibble(week = 1:52, date = ymd("2027-01-03") + 7*(0:51))
    tibble(week = 1:52, date = ymd("2026-10-11") + 7*(0:51))
  ) |> select(uf, week, date, pred:upper_95)


write_csv(tbl.uf.week.dengue.forecast, file = "forecasts/uf.week.dengue.forecast_3rd_imcd.csv")
write_csv(tbl.uf.week.chik.forecast, file = "forecasts/uf.week.chik.forecast_3rd_imcd.csv")


aaa <- dengue |> 
  group_by(date, uf) |> 
  summarise(casos = sum(casos)) 

g1 <- ggplot(data = aaa) + 
  geom_point(mapping = aes(x = date, y = casos)) +
  # geom_line(data = tbl.uf.week.forecast, mapping = aes(x = date, y = pred), color = "red") +
  theme_bw() + 
  facet_geo(~uf, grid = "br_states_grid1", scale = "free_y") 


g2 <- ggplot(data = tbl.uf.week.dengue.forecast) + 
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
