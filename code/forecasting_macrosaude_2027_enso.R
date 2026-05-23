library(tidyverse)
library(INLA)
library(geofacet)
# library(forecastID)
source("code/sprint_fun.R")

dengue <- read_csv("data/dengue.csv.gz")
enso <- read_csv("data/ocean_climate_oscillations.csv.gz")



# Tyding and cleaning -----------------------------------------------------

# Removing data from 2021 and 2022 from Espirito Santo (There are notification issues)

dengue <- dengue |> 
  mutate(
    # Removendo casos do ES de 2021 e 2022 (total de casos nesses dois anos 0 e 481!)
    casos = if_else(uf == "ES" & (epiyear(date) == 2021 | epiyear(date) == 2022), NA, casos)
  ) |> drop_na(casos) 


aaa <- dengue |> 
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


# enso |> 
#   filter(month(date)> 6) |> 
#   group_by(year(date)) |> 
#   summarise(
#     enso = mean(enso),
#     fase = case_when( enso > 0.5 ~ 1,
#                       enso < -.5 ~ -1,
#                       TRUE ~ 0)) |> 
#   view()

# For replicability purposes
set.seed(42)

# Macroregioes ------------------------------------------------------------


# Criando as listas

macros <- unique(dengue$macroregional_geocode)


list.forecast = vector(mode = "list", length = length(macros))
names(list.forecast) = macros

list.elnino.coefs = vector(mode = "list", length = length(macros))
names(list.elnino.coefs) = macros


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
        Date = ymd("2027-01-03") + 7*(0:51),
        #### aa = "2026-10-05"; ymd(aa) |> epiweek(); ymd(aa) |> wday()
        # Date = ymd("2026-10-11") + 7*(0:52),
        macroregional_geocode = data.train.macro.k$macroregional_geocode[1], 
        uf = data.train.macro.k$uf[1],
        cases = NA,
        train = FALSE,
        target = TRUE)
    )
  
  
  # Forescasting target 3
  aux <- forecasting.inla(dados = data.train.macro.k %>% 
                            filter(Date >= "2015-10-11"), 
                          MC =T, elnino = T)
  aux$pred$uf = data.train.macro.k$uf[1]
  aux$pred$macrocode = data.train.macro.k$macroregional_geocode[1]
  
  aux$MC$uf = data.train.macro.k$uf[1]
  aux$MC$macrocode = data.train.macro.k$macroregional_geocode[1]
  
  list.forecast[[k]]$out <- aux
  
  list.elnino.coefs[[k]] <- tibble(uf = data.train.macro.k$uf[1], 
                              macrocode = data.train.macro.k$macroregional_geocode[1],
                              aux$inla$summary.fixed[2,])
  
  cat(k, data.train.macro.k$uf[1] , 
      data.train.macro.k$macroregional_geocode[1], "\n")
  
}


coefs.test <- list.elnino.coefs |> bind_rows() 

df.forecast <- list.forecast %>%
  map(function(x) x$out$MC) %>% bind_rows() #|> rename(values2=values)

# Removing week 53
df.forecast <- df.forecast |> filter(week < 53)


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
