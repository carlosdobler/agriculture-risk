
country_name <- "Brazil"

library(tidyverse)
library(stars)
library(tidymodels)
# library(furrr)
# 
# options(future.fork.enable = T)
# options(future.globals.maxSize = 1000 * 1024^2)
# options(future.rng.onMisuse = "ignore")
# 
# plan(multicore)

source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")


fitted <- 
  "/mnt/pers_disk/model_{country_name %>% str_sub(end = 3) %>% str_to_lower()}_2_1.rds" %>%
  str_glue() %>% 
  read_rds()


if (country_name == "Brazil"){
  
  br <- 
    "/mnt/bucket_mine/misc_data/admin_units/ne_110m_admin_0/ne_110m_admin_0_countries.shp" %>% 
    read_sf() %>% 
    filter(ADMIN == country_name)
  
}


models <- 
  "/mnt/bucket_mine/cmip6/nex/seasonal/" %>% 
  fs::dir_ls() %>% 
  fs::path_file()

warming_levels <- 
  "/mnt/bucket_mine/cmip6/nex/warming_levels_NEX_warming_levels_ODNI.csv" %>% 
  read_csv(name_repair = \(x) str_replace(x, " ", "_") %>% str_to_lower())


s_pred <- 
  map(models %>% set_names(), \(model){
    
    print(model)
    
    s_ind <- 
      str_glue("/mnt/pers_disk/agr_clim_ind_nex/{str_to_lower(country_name)}/s_ind_{model}.rds") %>% 
      read_rds()
    
    
    {
      # names(s_ind)[33:36] <- c("bedd_3",  "bedd_6",  "bedd_9",  "bedd_12")
      
      # ************
      
      # s <- 
      #   "/mnt/pers_disk/agr_model/s_f_model.rds" %>% 
      #   read_rds()
      # 
      # s_indd <- 
      #   s_ind %>% 
      #   filter(year %in% 2000:2019)
      # 
      # 
      # 
      # v <- names(s)[-1]
      # 
      # pp <- 
      #   imap(v %>% set_names, \(vv, i){
      #     
      #     nex <- 
      #       s_indd %>% 
      #       select(all_of(vv)) %>%
      #       st_apply(c(1,2), mean) %>% 
      #       setNames("v")
      #     
      #     era <- 
      #       s %>% 
      #       select(all_of(vv)) %>% 
      #       st_apply(c(1,2), mean) %>%
      #       setNames("v")
      #     
      #     era[is.na(nex)] <- NA
      #     
      #     bind_rows(
      #       era %>% 
      #         as_tibble() %>% 
      #         mutate(s = "era"),
      #       nex %>% 
      #         as_tibble() %>% 
      #         mutate(s = "nex")
      #     ) %>% 
      #       
      #       ggplot(aes(x,y,fill = v)) +
      #       geom_raster() +
      #       colorspace::scale_fill_continuous_sequential("viridis",
      #                                                    rev = F,
      #                                                    na.value = "transparent") +
      #       facet_wrap(~s,ncol = 2) +
      #       coord_equal() +
      #       labs(title = i)
      #     
      #   })
      }
    
    # ***********
    
    s_prob <- 
      s_ind %>% 
      predict(fitted, type = "prob") %>% 
      select(2) %>% 
      setNames("prob") %>% 
      st_set_dimensions(3, names = "time")
    
    if (country_name == "Brazil"){
      s_prob <- s_prob[br] # NOT FOR INDIA (whole subcontinent)
    }
    
    return(s_prob)
    
  })


# save to bucket
iwalk(s_pred, \(ss, i){

  f <- str_glue("/mnt/pers_disk/{str_to_lower(country_name)}_{i}_prob_yield_gt_5t.nc")

  rt_write_nc(ss %>%
                st_set_dimensions(3, values = seq(as_date("2000-01-01"), as_date("2069-01-01"), by = "1 year")),
              f,
              daily = F)

  str_glue("gsutil mv {f} gs://clim_data_reg_useast1/results/odni_sugarcane") %>%
    system()

})

# write_rds(s_pred, str_glue("/mnt/pers_disk/agr_model/s_preds_nex_{str_to_lower(country_name)}.rds"))
# s_pred <- read_rds(str_glue("/mnt/pers_disk/agr_model/s_preds_nex_{str_to_lower(country_name)}.rds"))


# *****************

cf <- 1999

s_prop_yrs_decades <- 
  names(s_pred) %>% 
  set_names() %>% 
  map(\(mod){
    
    s <- s_pred %>% pluck(mod)
    
    r <- 
      s %>% 
      st_apply(c(1,2), \(x){
        
        if(all(is.na(x))){
          
          rep(NA, 4)
          
        } else {
          
          x_ref <- mean((x[(2000-cf):(2020-cf)]) >= 0.5, na.rm = T)
          
          x_proj <- 
            warming_levels %>% 
            filter(model == mod) %>%
            pmap(\(start_year, end_year, ...){
              
              mean((x[(start_year-cf):(end_year-cf)]) >= 0.5, na.rm = T)
              
            })
          
          c(x_ref, unlist(x_proj))
          
        }
        
      }, 
      .fname = "prop") %>% 
      split("prop") %>% 
      setNames(c("2000-2020", "2020-2029", "2030-2039", "2040-2049"))
    
    return(r)
    
  })


iwalk(s_prop_yrs_decades, \(ss, i){
  
  f <- str_glue("/mnt/pers_disk/{str_to_lower(country_name)}_{i}_decadal_prop_year.nc")
  
  rt_write_nc(ss,
              f)
  
  str_glue("gsutil mv {f} gs://clim_data_reg_useast1/results/odni_sugarcane") %>%
    system()
  
})


# *****************

s_mean_prob_decades <- 
  names(s_pred) %>% 
  set_names() %>% 
  map(\(mod){
    
    s <- s_pred %>% pluck(mod)
    
    r <- 
      s %>% 
      st_apply(c(1,2), \(x){
        
        if(all(is.na(x))){
          
          rep(NA, 4)
          
        } else {
          
          x_ref <- mean(x[(2000-cf):(2020-cf)], na.rm = T)
          
          x_proj <- 
            warming_levels %>% 
            filter(model == mod) %>%
            pmap(\(start_year, end_year, ...){
              
              mean(x[(start_year-cf):(end_year-cf)], na.rm = T)
              
            })
          
          c(x_ref, unlist(x_proj))
          
        }
        
      }, 
      .fname = "prop") %>% 
      split("prop") %>% 
      setNames(c("2000-2020", "2020-2029", "2030-2039", "2040-2049"))
    
    return(r)
    
  })


iwalk(s_mean_prob_decades, \(ss, i){
  
  f <- str_glue("/mnt/pers_disk/{str_to_lower(country_name)}_{i}_decadal_mean_prob.nc")
  
  rt_write_nc(ss,
              f)
  
  str_glue("gsutil mv {f} gs://clim_data_reg_useast1/results/odni_sugarcane") %>%
    system()
  
})



# 
# # ensemble mean
# s_prop_gt5_wl_ens <- 
#   s_prop_gt5_wl %>% 
#   {do.call(c, c(., along = "model"))} %>% 
#   st_apply(c(1,2), mean)







# yield failure: old: do not run
# s_pred %>% 
#   {do.call(c, c(., along = "mod"))} %>% 
#   st_apply(c(1,2), \(x){
#     
#     if(all(is.na(x))){
#       return(c(NA,NA))
#     } else {
#       
#       m <- x[1:20,] %>% mean(na.rm = T)
#       x_anom <- x-m
#       
#       p1 <- ecdf(x_anom[21:40,])(-0.1)
#       p2 <- ecdf(x_anom[41:60,])(-0.1)
#       
#       return(c(p1,p2))
#       
#     }
#     
#   },
#   .fname = "p") %>% 
#   aperm(c(2,3,1)) -> foo
# 
# foo %>%
#   st_set_dimensions("p", values = c("2020-2040", "2040-2060")) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = .pred_1)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("Rocket",
#                                                na.value = "transparent",
#                                                limits = c(0,1),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                name = "prob.",
#                                                trans = "sqrt",
#                                                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#                                                guide = guide_colorbar(barheight = 0.6, 
#                                                                       barwidth = 14,
#                                                                       title.position = "top")
#   ) +
#   coord_equal() +
#   theme(legend.position = "bottom",
#         axis.title = element_blank()) +
#   facet_wrap(~p, nrow = 1)









# a <- 
#   foo %>%
#   select(2) %>%
#   filter(year %in% 2000:2019) %>%
#   st_apply(c(1,2), mean, .fname = "v")
# 
# a %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = v)) +
#   geom_raster() +
#   colorspace::scale_fill_binned_sequential("viridis",
#                                            na.value = "transparent",
#                                            limits = c(0,0.5),
#                                            n.breaks = 9,
#                                            rev = F,
#                                            oob = scales::squish,
#                                            # trans = "sqrt"
#   ) +
#   coord_equal()
# 
# b <- 
#   foo %>%
#   select(2) %>%
#   filter(year %in% 2040:2059) %>%
#   st_apply(c(1,2), mean, .fname = "v")
# 
# b %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = v)) +
#   geom_raster() +
#   colorspace::scale_fill_binned_sequential("viridis",
#                                            na.value = "transparent",
#                                            limits = c(0,0.5),
#                                            n.breaks = 9,
#                                            rev = F,
#                                            oob = scales::squish,
#                                            # trans = "sqrt"
#   ) +
#   coord_equal()
#   
# 
# 
# 
# (a - b) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = v)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_diverging(#"viridis",
#     na.value = "transparent",
#     # limits = c(-1,1),
#     rev = F,
#     oob = scales::squish,
#     # trans = "sqrt"
#   ) +
#   coord_equal()
# 
# 
# 
# q <- 
#   foo %>% 
#   select(2) %>% 
#   st_apply(c(1,2), \(x){
#     
#     if(all(is.na(x))){
#       rep(NA, length(x))
#     } else {
#       # ecdf(x[1:20])(x)
#       # m <- mean(x[1:20])
#       # x_anom <- x-m
#       # quantile(x_anom, 0.1)
#     }
#     
#   },
#   .fname = "year") %>%
#   aperm(c(2,3,1)) %>% 
#   st_set_dimensions(3, values = 2000:2059)
# 
# q_b <- 
#   q %>%
#   filter(year %in% 2020:2039) %>%
#   st_apply(c(1,2), \(x) mean(x <= 0.1, na.rm = T), .fname = "v")
# 
# q_c <- 
#   q %>%
#   filter(year %in% 2040:2059) %>%
#   st_apply(c(1,2), \(x) mean(x <= 0.1, na.rm = T), .fname = "v")
# 
# c(q_b, q_c, along = "p") %>%
#   st_set_dimensions("p", values = c("2020-2040", "2040-2060")) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = v)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("plasma",
#                                                na.value = "transparent",
#                                                limits = c(0,1),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                name = "prob.",
#                                                breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
#                                                guide = guide_colorbar(barheight = 0.6, 
#                                                                        barwidth = 14,
#                                                                        title.position = "top")
#   ) +
#   coord_equal() +
#   theme(legend.position = "bottom",
#         axis.title = element_blank()) +
#   facet_wrap(~p, nrow = 1)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# foo %>%
#   slice(year, 1) %>%
#   select(2) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = .pred_1)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                na.value = "transparent",
#                                                limits = c(0,1),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                trans = "sqrt"
#   ) +
#   coord_equal()
# 
# 
# 
# foo %>% 
#   select(2) %>% 
#   st_apply(c(1,2), mean) -> bar
# 
# bar %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = mean)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                na.value = "transparent",
#                                                limits = c(0,1),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                trans = "sqrt"
#   ) +
#   coord_equal()
# 
# 
# 
# s %>% 
#   select(yield) %>% 
#   st_apply(c(1,2), mean) %>% 
#   as_tibble() %>% 
#   ggplot(aes(x,y, fill = mean)) +
#   geom_raster() +
#   colorspace::scale_fill_continuous_sequential("viridis",
#                                                na.value = "transparent",
#                                                limits = c(0,10),
#                                                rev = F,
#                                                oob = scales::squish,
#                                                trans = "sqrt"
#   ) +
#   coord_equal()
