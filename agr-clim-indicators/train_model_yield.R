
library(tidyverse)
library(stars)
library(tidymodels)



s <- 
  "/mnt/pers_disk/agr_model/s_f_model.rds" %>% 
  read_rds()

prod_mask <- 
  s %>% 
  select(yield) %>% 
  st_apply(c(1,2), \(x) {
    
    a <- if_else(all(is.na(x)) | mean(x, na.rm = T) < 5, NA, 1)
    return(a)
    
  }, 
  .fname = "mask")

s_index <- 
  prod_mask %>% 
  mutate(lon = rep(1:158, times = 157),
         lat = rep(1:157, each = 158))


tb_index <- 
  s_index %>% 
  as_tibble() %>% 
  filter(!is.na(mask))



# ************

tb <- 
  s[,(135-1):(135+1),(97-1):(97+1)] %>% 
  as_tibble()


tb_f <- 
  tb %>% 
  select(-c(x,y,yield))
  # select(yield, year, starts_with("rr_"), starts_with("tx"), starts_with("tn_"), starts_with("rr1_"))

# tb_f %>%
#   pivot_longer(3:6) %>%
#   ggplot(aes(x = factor(year), y = value, fill = name)) +
#   geom_boxplot()

folds <- group_vfold_cv(tb_f, year, 5)

spec <- 
  rand_forest(trees = 1000, min_n = 10) %>% 
  set_engine("randomForest") %>% 
  set_mode("regression")

wf <- 
  workflow() %>% 
  add_model(spec) %>% 
  add_variables(outcomes = yield_decile, predictors = names(tb_f)[-c(1:2)])

fit <- 
  fit_resamples(wf, folds, control = control_resamples(save_pred = T))

collect_metrics(fit)

collect_predictions(fit) %>% 
  ggplot(aes(x = yield_decile, y = .pred)) +
  geom_point() +
  coord_equal()









