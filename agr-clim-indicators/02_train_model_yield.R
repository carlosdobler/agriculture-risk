
country_name <- "India"



library(tidyverse)
library(stars)
library(tidymodels)



s <- 
  "/mnt/pers_disk/agr_model/{str_to_lower(country_name)}/s_f_model.rds" %>%
  str_glue() %>% 
  read_rds()

tb <- 
  s %>% 
  as_tibble() %>% 
  filter(!is.na(yield)) %>% 
  mutate(yield = if_else(yield > 10, 10, round(yield))) #%>% 
  # mutate(yield = case_when(yield < 3.33 ~ 1,
  #                          yield < 6.66 ~ 2,
  #                          TRUE ~ 3))

# tb %>%
#   ggplot(aes(x = as.numeric(yield))) +
#   geom_histogram()

table(tb$yield)

set.seed(111)
tb_f <- 
  tb %>% 
  group_by(yield) %>% 
  nest() %>% 
  mutate(d = map(data, \(df){
    
    slice_sample(df, n = 2500)
    # slice_sample(df, n = 8000)
    
  })) %>% 
  unnest(d) %>% 
  select(-data) %>% 
  select(-x, -y, -year) %>% 
  ungroup()


tb_f <- 
  tb_f %>% 
  select(-starts_with("bedd")) #%>% 
  # mutate(yield = factor(yield))

# spec <- 
#   rand_forest() %>% 
#   set_engine("randomForest") %>% 
#   set_mode("classification")

spec <- 
  rand_forest() %>% 
  set_engine("ranger", num.threads = 8) %>% 
  set_mode("regression")

wf <- 
  workflow() %>% 
  add_model(spec) %>% 
  add_formula(yield ~ .)


folds <- vfold_cv(tb_f, 5)

fit <-
  fit_resamples(wf, folds, control = control_resamples(save_pred = T))

collect_metrics(fit)

collect_predictions(fit) %>%
  ggplot(aes(x = factor(yield), y = .pred)) +
  geom_boxplot()
  # geom_point() +
  # geom_smooth() +
  # coord_equal()


fit_final <- 
  fit(wf, data = tb_f)

fit_final %>% 
  butcher::butcher() %>% 
  # write_rds("/mnt/pers_disk/model_bra_1.rds") # th: > 0
  # write_rds("/mnt/pers_disk/model_bra_2.rds") # th: > 5
  # write_rds("/mnt/pers_disk/model_bra_2_1.rds") # th: > 5, no bedd
  write_rds("/mnt/pers_disk/model_ind_2_1.rds") # th: > 5, no bedd






s %>% 
  # slice(year, 1) %>% 
  predict(fit_final, type = "prob") -> foo

foo %>% 
  select(2) %>% 
  slice(year, 15) %>% 
  as_tibble() %>% 
  ggplot(aes(x,y, fill = .pred_1)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               trans = "sqrt") +
  coord_equal()





(slice(foo, year,20) - slice(foo, year,1)) %>% 
  select(2) %>%
  as_tibble() %>% 
  ggplot(aes(x,y, fill = .pred_1)) +
  geom_raster() +
  colorspace::scale_fill_continuous_diverging(#"viridis",
    na.value = "transparent",
    # limits = c(-1,1),
    rev = T,
    oob = scales::squish,
    # trans = "sqrt"
  ) +
  coord_equal()

foo %>% 
  select(2) %>% 
  st_apply(c(1,2), mean) -> bar

bar %>% 
  as_tibble() %>% 
  ggplot(aes(x,y, fill = mean)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               trans = "sqrt"
  ) +
  coord_equal()



s %>% 
  select(yield) %>% 
  st_apply(c(1,2), mean) %>% 
  as_tibble() %>% 
  ggplot(aes(x,y, fill = mean)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               limits = c(0,10),
                                               rev = F,
                                               oob = scales::squish,
                                               trans = "sqrt"
  ) +
  coord_equal()

