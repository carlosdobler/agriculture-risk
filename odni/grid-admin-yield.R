
country_name <- "Brazil"
crop <- "sugarcane"



library(tidyverse)
library(stars)

# template grid
grid_template <- 
  st_bbox() %>% 
  st_as_stars(dx = 0.25)



# load countries
countries <- 
  "/mnt/bucket_mine/misc_data/admin_units/ne_110m_admin_0/" %>% 
  read_sf()

# select country of interest
country <- 
  countries %>% 
  filter(ADMIN == country_name) %>%  
  mutate(a = 1) %>% 
  select(a)

# crop grid to country
grid_country <- 
  grid_template %>% 
  st_crop(st_bbox(country)) %>% 
  mutate(values = NA)



# load production map
spam <- 
  "/mnt/bucket_mine/misc_data/agriculture/spam/spam2020/production/spam2020_v1r0_global_P_SUGC_A.tif" %>% 
  read_stars()

spam_f <- 
  spam %>% 
  st_warp(grid_country) %>% 
  setNames("spam") %>% 
  mutate(spam = if_else(is.na(spam) | spam <= 0.1, 0.1, spam))



# load yield data
"gsutil cp gs://clim_data_reg_useast1/misc_data/agriculture/official_stats/brazil/brazil_sugarcane_production_2000-2023.gpkg /mnt/pers_disk/" %>% 
  system()

yield_pol <- 
  "/mnt/pers_disk/brazil_sugarcane_production_2000-2023.gpkg" %>% 
  read_sf()

yield_pol <- 
  yield_pol %>% 
  st_transform(crs = 4326)

spam_per_region <- 
  spam_f %>% 
  aggregate(yield_pol, sum) %>%
  st_as_sf() %>% 
  st_rasterize(grid_country) %>% 
  setNames("total_spam")

st_dimensions(spam_per_region) <- st_dimensions(grid_country)

w <- 
  c(spam_f, spam_per_region) %>%
  mutate(w = spam/total_spam) %>% 
  select(w)

# check
# foo <- 
#   w %>% 
#   aggregate(yield_pol, sum) %>% 
#   st_as_sf()



yield_mean <- 
  yield_pol %>% 
  st_drop_geometry() %>%
  pivot_longer(-c(1:3), names_to = "year") %>%
  mutate(value = as.numeric(value),
         value = if_else(is.na(value), 0, value)) %>% 
  group_by(CD_MICRO, Microregion) %>% 
  summarize(value = mean(value, na.rm = T))

yield_pol_mean <- 
  yield_pol %>% 
  select(1,3) %>% 
  left_join(yield_mean, by = c("CD_MICRO", "Microregion"))



yield_r_mean <- 
  yield_pol_mean %>% 
  select(value) %>% 
  st_rasterize(grid_country) %>% 
  st_warp(grid_country)


yield_f_r <- 
  c(yield_r_mean, w) %>% 
  mutate(value = value * w) %>% 
  select(value)

# # check
# foo <-
#   yield_f_r %>%
#   aggregate(yield_pol, sum) %>%
#   st_as_sf()
# 
# yield_mean %>%
#   ungroup() %>% 
#   mutate(r = foo$value)



ggplot(yield_pol_mean) +
  geom_sf(aes(fill = value), color = NA) +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               trans = "sqrt",
                                               rev = F)



yield_pol_mean %>% 
  st_rasterize(grid_country) %>% 
  as_tibble() %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               trans = "sqrt",
                                               rev = F)



yield_f_r %>% 
  as_tibble() %>%
  mutate(value = value/10) %>% 
  mutate(value = if_else(value > 3e5, 3e5, value)) %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  scale_fill_viridis_c(option = "viridis",
                       trans = "sqrt",
                       na.value = "transparent",
                       name = "tonnes",
                       breaks = c(0, 1e5, 3e5),
                       labels = c("0", "1e5", "> 3e5"),
                       guide = guide_colorbar(barheight = 0.6,      
                                              barwidth = 10,
                                              title.position = "top")) +
  coord_equal() +
  theme(legend.position = "bottom",
        axis.title = element_blank()) +
  labs(title = "Mean sugarcane production",
       subtitle = "2000 - 2023")


spam_f %>%
  setNames("value") %>% 
  c(yield_f_r) %>% 
  mutate(value = if_else(is.na(value.1), NA, value)) %>% 
  as_tibble() %>% 
  ggplot() +
  geom_raster(aes(x, y, fill = value)) +
  colorspace::scale_fill_continuous_sequential("viridis",
                                               trans = "sqrt",
                                               rev = F,
                                               na.value = "transparent") +
  coord_equal()






s_agrclim <- 
  "/mnt/bucket_mine/cmip6/nex/seasonal/GFDL-ESM4/biologically_effective_degree_days/bedd-10_seas_GFDL-ESM4_2030-2039.nc" %>% 
  read_ncdf()

s_agrclim_f <- 
  s_agrclim %>%
  st_warp(st_as_stars(st_bbox(), dx = 0.25)) %>% 
  st_crop(grid_country)


s_agrclim_seas <- 
  c(3,6,9,12) %>% 
  map(\(mon){
    
    s_agrclim_f %>% 
      filter(month(time) == mon) %>% 
      st_apply(c(1,2), mean)
    
  })

do.call(c, c(s_agrclim_seas, along = "seas")) %>% 
  st_set_dimensions(3, values = c("MAM", "JJA", "SON", "DJF")) %>% 
  as_tibble() %>% 
  ggplot(aes(x,y, fill = mean)) +
  geom_raster() +
  scale_fill_viridis_c(option = "magma",
                       na.value = "transparent",
                       name = "°C",
                       guide = guide_colorbar(barheight = 0.6,      
                                              barwidth = 10,
                                              title.position = "top")) +
  coord_equal(expand = F) +
  facet_wrap(~seas, ncol = 2) +
  theme(axis.title = element_blank(),
        legend.position = "bottom") +
  labs(title = "Projected biologically effective degree days",
       subtitle = "1930-1940")
