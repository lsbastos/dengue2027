# library(INLA)
# library(lubridate)

forecasting.inla <- function(dados,   # dados - Data containing columns (cases, date, target)
                             #Year2forecast = 2023,
                             MC = FALSE, M = 2000,
                             # Changing the first week and redefine year as seasons
                             start = 41, 
                             # make week 53 as week 52 (POG!)
                             week53 = T,
                             quantiles = c(0.05, 0.25, 0.5, 0.75, 0.9, 0.95),
                             output.only = F,
                             likelihood = "nbinomial",
                             timeRE = "rw2",
                             cyclic = T,
                             WAIC = F,
                             elnino = F){
  
  data.inla <- dados %>% ungroup() %>% 
    transmute(
      #Pop = Pop,
      Date = Date,
      week = week.season(Date, start = start),
      # week = ifelse(week == 53, 52, week),
      year = season(Date, start = start),
      cases = cases,
      target = target
    )  
  
  
  if(elnino){
    data.inla <- data.inla |> 
      mutate(
        # Strong or very strong El Nino (enso > 1) in the previous year
        # https://ggweather.com/enso/oni.htm
        elnino_prev_year = case_when(
          year == 2016 | year == 2024 | year == 2027 ~ 1,
          TRUE ~ 0
        )
      )
  }

  formula.q <- cases ~ 1 +
    f(week, model = timeRE, constr = T, cyclic = cyclic,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    ) + 
    f(year, model = "iid", constr = T,
      hyper = list(
        # Precision of unstructure random effects
        prec = list(
          prior="pc.prec",
          param=c(3, 0.01)
        )
      )
    )

  
  if(elnino){
    formula.q <- cases ~ 1 + elnino_prev_year +
      f(week, model = timeRE, constr = T, cyclic = cyclic,
        hyper = list(
          # Precision of unstructure random effects
          prec = list(
            prior="pc.prec",
            param=c(3, 0.01)
          )
        )
      ) + 
      f(year, model = "iid", constr = T,
        hyper = list(
          # Precision of unstructure random effects
          prec = list(
            prior="pc.prec",
            param=c(3, 0.01)
          )
        )
      )
    
  }
  
  # # Adding forecasting component
  # data.inla <- data.inla %>% 
  #   add_row(week = 1:52, Year = Year2forecast) %>% 
  #   mutate(year = Year - min(Year) + 1)
  
  linear.term.year.cur <- which(data.inla$target == T)
  
  
  output.mean <- inla(formula = formula.q, num.threads = 8,
                      data = data.inla %>% 
                        mutate(
                          cases = ifelse(target==F, cases, NA)
                        ),
                      control.predictor = list(link = 1, compute = T, 
                                               quantiles = quantiles),
                      family = likelihood, 
                      # offset = log(Pop / 1e5),
                      # control.family = list(
                      #   control.link = list(
                      #     model = "quantile",
                      #     quantile = 0.5
                      #     )
                      #   ),
                      # control.fixed = control.fixed(prec.intercept = 1),
                      control.compute = list(config = MC, waic = WAIC)
  )
  
  out = NULL
  
  out$inla = output.mean
  
  out$pred = data.inla %>% filter(target == T) %>% 
    bind_cols( output.mean$summary.fitted.values[linear.term.year.cur,] )
  
  if(MC){
    param.samples <- inla.posterior.sample(output.mean, n = M)
    
    samples.MC <- param.samples %>%
      map(.f = function(xxx, idx = linear.term.year.cur){
        rnbinom(
          n = idx, 
          mu = exp(xxx$latent[idx]), 
          size = xxx$hyperpar[1]
        )} ) 
    
    names(samples.MC) <- 1:M
    
    samples.MC <- samples.MC %>%   
      bind_rows(.id = "samples") %>% 
      rowid_to_column(var = "week") %>% 
      gather(key = "samples", value = "values", -week) 
    
    
    out$MC = samples.MC
  }
  
  out 
}


forecasting_sprint <- function(macrodata, DT = "2015-10-11"){

  # Forescasting target 3
  aux <- forecasting.inla(dados = macrodata %>% 
                            filter(Date >= DT), 
                          MC =T)
  aux$pred$uf = macrodata$uf[1]
  aux$pred$macrocode = macrodata$macroregional_geocode[1]
  
  aux$MC$uf = macrodata$uf[1]
  aux$MC$macrocode = macrodata$macroregional_geocode[1]
  
  aux
}

tbl_total = function(df.forecast){
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
  
  tbl.total.uf.forecast
  
} 




tbl_forecasting_week = function(df.forecast){
    tbl.total.uf.forecast <- df.forecast %>%
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
        tibble(uf = "BR") %>% bind_cols(df.forecast %>%
                                          group_by(week, samples) %>%
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
    
    tbl.total.uf.forecast
    
  } 
  
  
  

threshold.MC <- function(samples.MC){
  thresholdw.data <- samples.MC %>%  
    group_by(week) %>% 
    summarise(
      Q1 = quantile(probs = 0.25, values),
      Q2 = quantile(probs = 0.50, values),
      Q3 = quantile(probs = 0.75, values),
      P90 = quantile(probs = 0.9, values),
    )
  thresholdw.data
} 



season <- function(x, start = 41){
  if(!is.Date(x)) stop("Not Date format")
  
  ew = epiweek(x)
  ey = epiyear(x)
  
  ifelse(ew < start, paste(ey-1, ey, sep = "-"), paste(ey, ey+1, sep = "-"))
}

# season(today())

week.season  <- function(x, start = 41, week53 = T){
  if(!is.Date(x)) stop("Not Date format")
  
  ew = epiweek(x)
  
  if(week53 & any(ew==53)) ew[ew==53] = 52
  
  # 1 = start, 2 = start + 1, 3 = start + 2,...
  ifelse(ew >= start, ew - start + 1, 52 - start + 1 + ew)
}

df4plot <- function(obj, ano) {
  
  observed.tmp <- observed |> 
    filter(year.s.first == ano) |> 
    group_by(week, season, year.s.first, date) |> 
    summarise(cases = sum(cases, na.rm = TRUE)) 
  
  temp <- obj |> 
    group_by(week, samples) |> 
    summarise(values = sum(values)) |> 
    ungroup() |> 
    group_by(week) |> 
    summarise(q50 = quantile(values, probs = 0.5), 
              q75 = quantile(values, probs = 0.75),
              q90 = quantile(values, probs = 0.9),
              q100 = Inf) 
  
  tmp1 <- temp |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'maxvalues'
    )
  
  tmp2 <- temp |> 
    mutate(q100 = q90,
           q90 = q75,
           q75 = q50,
           q50 = 0) |> 
    pivot_longer(
      cols = c(q50, q75, q90, q100),
      names_to = 'quantile',
      values_to = 'minvalues'
    )
  
  tmp <- tmp1 |> 
    left_join(tmp2, by = c('week', 'quantile')) |> 
    ungroup() |> 
    left_join(observed.tmp, by = 'week') |> 
    mutate(epiweek = ifelse(week <= 12, yes = week + 40, no = week -12), 
           epiyear = ifelse(week <= 12, yes = ano, no = ano + 1),
           date = aweek::get_date(week = epiweek, year = epiyear,start = 7))
  
  tmp$quantile <- factor(tmp$quantile, 
                         levels = c('q50','q75','q90','q100'),
                         labels = c('Below the median,\ntypical','Moderately high,\nfairly typical',
                                    'Fairly high,\natypical', 'Exceptionally high,\nvery atypical'))
  return(tmp)
}
