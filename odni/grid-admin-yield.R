
# country_name <- "Brazil"
country_name <- "India"



# *****************************************************************************

library(tidyverse)
library(stars)

source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")

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



# load yield map
spam <- 
  "/mnt/bucket_mine/misc_data/agriculture/spam/spam2020/yield/spam2020_v1r0_global_Y_SUGC_A.tif" %>% 
  read_stars()

spam_f <- 
  spam %>% 
  setNames("spam") %>% 
  mutate(spam = if_else(is.na(spam), 0, spam)) %>% 
  st_warp(grid_country, use_gdal = T, method = "average") %>% 
  setNames("spam")
  
st_dimensions(spam_f) <- st_dimensions(grid_country)


# load yield data
if(country_name == "Brazil") {
  
  "gsutil cp gs://clim_data_reg_useast1/misc_data/agriculture/official_stats/brazil/brazil_sugarcane_production_2000-2023.gpkg /mnt/pers_disk/" %>%
    system()

  "gsutil cp gs://clim_data_reg_useast1/misc_data/agriculture/official_stats/brazil/brazil_sugarcane_area_2000-2023.gpkg /mnt/pers_disk/" %>%
    system()
  
} else if (country_name == "India") {
  
  "gsutil cp gs://clim_data_reg_useast1/misc_data/agriculture/official_stats/india/india_sugarcane_area_2000-2023.gpkg /mnt/pers_disk/" %>% 
    system()
  
  "gsutil cp gs://clim_data_reg_useast1/misc_data/agriculture/official_stats/india/india_sugarcane_production_2000-2023.gpkg /mnt/pers_disk/" %>% 
    system()
  
}



pol <- 
  "/mnt/pers_disk/{str_to_lower(country_name)}_sugarcane_production_2000-2023.gpkg" %>% 
  str_glue() %>% 
  read_sf()

df_production <- 
  pol %>% 
  st_drop_geometry()

df_area <- 
  "/mnt/pers_disk/{str_to_lower(country_name)}_sugarcane_area_2000-2023.gpkg" %>% 
  str_glue() %>% 
  read_sf() %>%  
  st_drop_geometry()

pol <- 
  pol %>% 
  select(1:3) %>% 
  st_transform(crs = 4326)


# create weights

# sum per municiplaity (denominator)
spam_total <- 
  spam_f %>% 
  aggregate(pol, sum) %>%
  st_as_sf() %>% 
  st_rasterize(grid_country) %>% 
  setNames("total")

st_dimensions(spam_total) <- st_dimensions(grid_country)

w <- 
  c(spam_f, spam_total) %>%
  mutate(w = spam/total) %>% 
  select(w)

# # check: all 1?
# foo <-
#   w %>%
#   aggregate(pol, sum) %>%
#   st_as_sf()
# 
# foo$w




# calculate yield

if (country_name == "Brazil") {
  col_n <- 1:3
  col_names <- c("CD_MICRO", "State", "Microregion")
  
} else if (country_name == "India"){
  col_n <- 1:4
  col_names <- c("State", "District", "VARNAME_2")
  
}
  

df_yield <-
  full_join(
    df_production %>%
      pivot_longer(-all_of(col_n), names_to = "year", values_to = "production"),
    df_area %>%
      pivot_longer(-all_of(col_n), names_to = "year", values_to = "area"),
    by = c(col_names, "year")
  )

if (country_name == "Brazil") {
  df_yield <- 
    df_yield %>% 
    mutate(across(year:area, ~as.numeric(.x))) %>%
    mutate(yield = production/area,
           yield = if_else(is.na(yield), 0, yield)) %>%
    select(-production, -area)
  
} else if (country_name == "India") {
  df_yield <- 
    df_yield %>% 
    select(-starts_with("S.No.")) %>%
    mutate(year = str_sub(year, end = 4)) %>% 
    mutate(across(year:area, ~as.numeric(.x))) %>%
    mutate(yield = production/area,
           yield = if_else(is.na(yield) | is.infinite(yield), 0, yield)) %>%
    select(-production, -area)
  
}


# df_yield %>%
#   ggplot(aes(year, yield, group = CD_MICRO, color = CD_MICRO)) +
#   geom_line(show.legend = F)
# 
# df_production <-
#   df_production %>%
#   pivot_longer(-c(1:3), names_to = "year", values_to = "production") %>%
#   mutate(across(year:production, ~as.numeric(.x)))
# 
# df_production %>%
#   ggplot(aes(year, production, group = CD_MICRO, color = CD_MICRO)) +
#   geom_line(show.legend = F)
    


yield_r_annual <- 
  # map(2000:2023, \(yr){
  map(2000:2022, \(yr){
    
    # yr <- 2000
    
    yield_r_1yr <- 
      pol %>% 
      left_join(df_yield %>% filter(year == yr),
                by = col_names
                ) %>% 
      select(yield) %>% 
      st_rasterize(grid_country) %>% 
      st_warp(grid_country) %>% 
      
      c(w) %>% 
      mutate(yield = yield * w) %>% 
      select(yield)
    
    # # check
    # foo <-
    #   yield_r_1yr %>%
    #   aggregate(pol, sum) %>%
    #   st_as_sf()
    # 
    # df_yield %>% 
    #   filter(year == yr) %>%
    #   mutate(r = foo$yield)
    
    return(yield_r_1yr)
    
  })


yield_r_annual <- 
  yield_r_annual %>% 
  # set_names(2000:2023) %>%
  set_names(2000:2022) %>% 
  {do.call(c, c(., along = "time"))}

yield_decile_r_annual <- 
  yield_r_annual %>% 
  st_apply(c(1,2), \(x){
    
    if(all(is.na(x)) | all(x == 0)){
      rep(NA, length(x))
    } else {
      round(ecdf(x)(x),1)
    }
  
  },
  .fname = "time") %>% 
  aperm(c(2,3,1))

yield_anom_r_annual <- 
  yield_r_annual %>% 
  st_apply(c(1,2), \(x){
    
    if(all(is.na(x)) | all(x == 0)){
      rep(NA, length(x))
    } else {
      x-mean(x)
    }
    
  },
  .fname = "time") %>% 
  aperm(c(2,3,1))


dir_yield <- "/mnt/pers_disk/agr_yield"
# fs::dir_create(dir_yield)

yield_r_annual %>% 
  # st_set_dimensions(3, values = seq(as_date("2000-01-01"), as_date("2023-01-01"), by = "1 year")) %>% 
  st_set_dimensions(3, values = seq(as_date("2000-01-01"), as_date("2022-01-01"), by = "1 year")) %>%
  rt_write_nc(str_glue("{dir_yield}/yield_{str_to_lower(country_name)}_annual.nc"), daily = F)

yield_decile_r_annual %>% 
  st_set_dimensions(3, values = seq(as_date("2000-01-01"), as_date("2023-01-01"), by = "1 year")) %>% 
  rt_write_nc(str_glue("{dir_yield}/yield_{str_to_lower(country_name)}_annual_decile.nc"), daily = F)

yield_anom_r_annual %>% 
  st_set_dimensions(3, values = seq(as_date("2000-01-01"), as_date("2023-01-01"), by = "1 year")) %>% 
  rt_write_nc(str_glue("{dir_yield}/yield_{str_to_lower(country_name)}_annual_anom.nc"), daily = F)


# 
# ggplot(yield_pol_mean) +
#   geom_sf(aes(fill = value), color = NA) +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                trans = "sqrt",
#                                                rev = F)
# 
# 
# 
# yield_pol_mean %>% 
#   st_rasterize(grid_country) %>% 
#   as_tibble() %>% 
#   ggplot() +
#   geom_raster(aes(x, y, fill = value)) +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                trans = "sqrt",
#                                                rev = F)
# 
# 
# 
# yield_f_r %>% 
#   as_tibble() %>%
#   mutate(value = value/10) %>% 
#   mutate(value = if_else(value > 3e5, 3e5, value)) %>% 
#   ggplot() +
#   geom_raster(aes(x, y, fill = value)) +
#   scale_fill_viridis_c(option = "viridis",
#                        trans = "sqrt",
#                        na.value = "transparent",
#                        name = "tonnes",
#                        breaks = c(0, 1e5, 3e5),
#                        labels = c("0", "1e5", "> 3e5"),
#                        guide = guide_colorbar(barheight = 0.6,      
#                                               barwidth = 10,
#                                               title.position = "top")) +
#   coord_equal() +
#   theme(legend.position = "bottom",
#         axis.title = element_blank()) +
#   labs(title = "Mean sugarcane production",
#        subtitle = "2000 - 2023")
# 
# 
# spam_f %>%
#   setNames("value") %>% 
#   c(yield_f_r) %>% 
#   mutate(value = if_else(is.na(value.1), NA, value)) %>% 
#   as_tibble() %>% 
#   ggplot() +
#   geom_raster(aes(x, y, fill = value)) +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                trans = "sqrt",
#                                                rev = F,
#                                                na.value = "transparent") +
#   coord_equal()
# 
# 
# 
# 
# 
# 
# s_agrclim <- 
#   "/mnt/bucket_mine/cmip6/nex/seasonal/GFDL-ESM4/biologically_effective_degree_days/bedd-10_seas_GFDL-ESM4_2030-2039.nc" %>% 
#   read_ncdf()
# 
# s_agrclim_f <- 
#   s_agrclim %>%
#   st_warp(st_as_stars(st_bbox(), dx = 0.25)) %>% 
#   st_crop(grid_country)
# 
# 
# s_agrclim_seas <- 
#   c(3,6,9,12) %>% 
#   map(\(mon){
#     
#     s_agrclim_f %>% 
#       filter(month(time) == mon) %>% 
#       st_apply(c(1,2), mean)
#     
#   })
# 
# do.call(c, c(s_agrclim_seas, along = "seas")) %>% 
#   st_set_dimensions(3, values = c("MAM", "JJA", "SON", "DJF")) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = mean)) +
#   geom_raster() +
#   scale_fill_viridis_c(option = "magma",
#                        na.value = "transparent",
#                        name = "°C",
#                        guide = guide_colorbar(barheight = 0.6,      
#                                               barwidth = 10,
#                                               title.position = "top")) +
#   coord_equal(expand = F) +
#   facet_wrap(~seas, ncol = 2) +
#   theme(axis.title = element_blank(),
#         legend.position = "bottom") +
#   labs(title = "Projected biologically effective degree days",
#        subtitle = "1930-1940")
