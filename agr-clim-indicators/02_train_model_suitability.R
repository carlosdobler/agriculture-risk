
country_name <- "Brazil"



library(tidyverse)
library(stars)
library(tidymodels)



s <- 
  "/mnt/pers_disk/agr_model/{str_to_lower(country_name)}/s_f_model.rds" %>%
  str_glue() %>% 
  read_rds()

tb_presence_1 <- 
  s %>% 
  select(yield) %>% 
  setNames("presence") %>% 
  as_tibble() %>% 
  filter(!is.na(presence)) %>% 
  mutate(
    w = if_else(presence > 10, 10, round(presence)),
    # presence = if_else(w == 0, 0, 1)
    presence = if_else(w < 5, 0, 1)
    )

tb_absence <- 
  tb_presence_1 %>% 
  filter(presence == 0) %>% 
  select(-w)

tb_presence <- 
  tb_presence_1 %>%
  uncount(w) %>% 
  slice_sample(n = nrow(tb_absence))

tb_presence <- 
  bind_rows(tb_absence, tb_presence) %>% 
  mutate(presence = factor(presence))
  

tb_f <- 
  left_join(
    tb_presence,
    s %>% 
      select(-yield) %>% 
      as_tibble(),
    by = c("x", "y", "year")
  ) %>%
  # select(-x, -y, -year)
  select(-year)

tb_f <- 
  tb_f %>% 
  select(-starts_with("bedd"))

tb_f <- 
  tb_f %>% 
  st_as_sf(coords = c("x","y"))


# spec <- 
#   rand_forest() %>% 
#   set_engine("randomForest") %>% 
#   set_mode("classification")

spec <- 
  rand_forest() %>% 
  set_engine("ranger", num.threads = 6) %>% 
  set_mode("classification")

wf <- 
  workflow() %>% 
  add_model(spec) %>% 
  add_formula(presence ~ .)


# folds <- vfold_cv(tb_f, 5)
# 
# fit <-
#   fit_resamples(wf, folds, control = control_resamples(save_pred = T))
# 
# collect_metrics(fit)
# 
# collect_predictions(fit) %>%
#   ggplot(aes(x = yield_decile, y = .pred)) +
#   geom_point() +
#   coord_equal()

folds <- spatialsample::spatial_block_cv(tb_f)

tb_pred <- 
  folds %>% 
  mutate(.preds = map(splits, \(df){
    
    mod <- 
      fit(wf, data = analysis(df) %>% st_drop_geometry())
    
    # identify the assessment set
    holdout <- 
      assessment(df)
    
    # return the assessment set, with true and predicted price
    tibble(# geometry = holdout$geometry,
      presence = holdout$presence
    ) %>% 
      bind_cols(predict(mod, holdout, type = "prob"))
    
  }))


tb <- 
  tb_pred %>% 
  select(-splits) %>% 
  unnest(.preds)

roc_auc(tb, .pred_1, truth = presence, event_level = "second")
# India = 0.778
# Brazil = 0.838

accuracy(tb %>% 
           mutate(.pred_class = if_else(.pred_1 >= 0.5, 1, 0) %>% factor()),
         .pred_class, truth = presence)
# India = 0.716
# Brazil = 0.753



fit_final <- 
  fit(wf, data = tb_f)

fit_final %>% 
  butcher::butcher() %>% 
  # write_rds("/mnt/pers_disk/model_bra_1.rds") # th: > 0
  # write_rds("/mnt/pers_disk/model_bra_2.rds") # th: > 5
  # write_rds("/mnt/pers_disk/model_bra_2_1.rds") # th: > 5, no bedd
  write_rds("/mnt/pers_disk/model_ind_2_1.rds") # th: > 5, no bedd




# ***************

fit_final <- 
  read_rds("/mnt/pers_disk/model_ind_2_1.rds")


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



foo %>% 
  select(2) %>% 
  st_apply(c(1,2), \(x) mean(x >= 0.5)) -> baz

baz %>% 
  as_tibble() %>%
  ggplot(aes(x,y, fill = .pred_1)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               # trans = "sqrt"
  ) +
  coord_equal()

s %>% 
  select(yield) %>% 
  st_apply(c(1,2), \(x) mean(x >= 5)) %>% 
  as_tibble() %>% 
  ggplot(aes(x,y, fill = yield)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               # trans = "sqrt"
  ) +
  coord_equal()

