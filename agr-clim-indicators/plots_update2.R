
library(tidyverse)
library(stars)
library(tidymodels)


ss <- 
  "/mnt/bucket_mine/misc_data/temporary/rice_soy_2040-2060/" %>% 
  fs::dir_ls() %>% 
  map(read_ncdf, proxy = F)


c(ss[[1]], ss[[2]], along = "irr") %>% 
  st_set_dimensions("irr", values = c("full_irrigation", "no_irrigation")) %>% 
  setNames("prob") %>%
  units::drop_units() %>%
  mutate(prob = prob/100) %>% 
  st_crop(st_bbox(c(xmin = 60, xmax = 94, ymin = 5, ymax = 33), crs = 4326)) %>% 
  as_tibble() %>% 
  ggplot(aes(longitude, latitude, fill = prob)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("Rocket",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               name = "prob.",
                                               trans = "sqrt",
                                               breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                               guide = guide_colorbar(barheight = 0.6, 
                                                                      barwidth = 14,
                                                                      title.position = "top")
  ) +
  coord_equal(expand = F) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  facet_wrap(~irr, nrow = 1)



c(ss[[3]], ss[[4]], along = "irr") %>% 
  st_set_dimensions("irr", values = c("full_irrigation", "no_irrigation")) %>% 
  setNames("prob") %>%
  units::drop_units() %>%
  mutate(prob = prob/100) %>% 
  st_crop(st_bbox(c(xmin = -77, xmax = -30, ymin = -36, ymax = 7), crs = 4326)) %>% 
  as_tibble() %>% 
  ggplot(aes(longitude, latitude, fill = prob)) +
  geom_raster() +
  colorspace::scale_fill_continuous_sequential("Rocket",
                                               na.value = "transparent",
                                               limits = c(0,1),
                                               rev = F,
                                               oob = scales::squish,
                                               name = "prob.",
                                               trans = "sqrt",
                                               breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                               guide = guide_colorbar(barheight = 0.6, 
                                                                      barwidth = 14,
                                                                      title.position = "top")
  ) +
  coord_equal(expand = F) +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  facet_wrap(~irr, nrow = 1)



# *************************


s <- 
  "/mnt/pers_disk/agr_model/s_f_model.rds" %>% 
  read_rds()

fitted <- 
  "/mnt/pers_disk/model_bra_2_1.rds" %>% 
  read_rds()

s %>% 
  predict(fitted, type = "prob") -> foo

# foo %>% 
#   select(2) %>% 
#   slice(year, 15) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = .pred_1)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                na.value = "transparent",
#                                                limits = c(0,1),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                trans = "sqrt") +
#   coord_equal()



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
                                               trans = "sqrt",
                                               name = "yield potential",
                                               breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                                               guide = guide_colorbar(barheight = 0.6, 
                                                                      barwidth = 12,
                                                                      title.position = "top")
  ) +
  coord_equal(expand = F) +
  theme(legend.position = "bottom",
        axis.title = element_blank())



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
                                               # trans = "sqrt",
                                               name = "yield (t/ha)",
                                               breaks = c(0, 2, 4, 6, 8, 10),
                                               guide = guide_colorbar(barheight = 0.6, 
                                                                      barwidth = 12,
                                                                      title.position = "top")
  ) +
  coord_equal(expand = F) +
  theme(legend.position = "bottom",
        axis.title = element_blank())
