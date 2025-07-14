
country_name <- "Brazil"



library(tidyverse)
library(stars)
# library(tidymodels)
library(furrr)

options(future.fork.enable = T)
options(future.rng.onMisuse = "ignore")
plan(multicore)

source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")


dir_data <- "/mnt/pers_disk/agr_clim_ind_nex/{str_to_lower(country_name)}" %>% str_glue()
fs::dir_create(dir_data)

# dir_model <- "/mnt/pers_disk/agr_model"


s_ref <- 
  "/mnt/pers_disk/agr_yield/yield_{str_to_lower(country_name)}_annual.nc" %>%
  str_glue() %>% 
  read_ncdf() %>% 
  slice(time, 1)


tb_vars <- 
  read_csv("agr-clim-indicators/tb_vars.csv") %>% 
  .[-9,]


models <- 
  "/mnt/bucket_mine/cmip6/nex/seasonal/" %>% 
  fs::dir_ls() %>% 
  fs::path_file()
  
  
walk(models, \(model){
  
  print(model)
  
  agr_met_yrs_seas <- 
    tb_vars$var_long %>% 
    set_names(tb_vars$var_short) %>% 
    map(\(ind){
      
      print(ind)
      
      if(ind == "growing_season_length"){
        tscale <- "annual"
      } else {
        tscale <- "seasonal"
      }
      
      ff <- 
        "gsutil ls gs://clim_data_reg_useast1/cmip6/nex/{tscale}/{model}/{ind}" %>% 
        str_glue() %>% 
        system(intern = T) %>% 
        str_subset(str_flatten(2000:2069, "|"))
      
      # download
      ff %>% 
        future_walk(\(f){
          
          "gsutil cp {f} {dir_data}" %>% 
            str_glue() %>% 
            system(ignore.stdout = T, ignore.stderr = T)
          
        })
      
      ff <- 
        ff %>% 
        fs::path_file() %>% 
        {str_glue("{dir_data}/{.}")}
      
      
      # load and crop
      if (country_name == "Brazil") {
      
        nc <- 
          read_ncdf(ff[1], proxy = T) %>% 
          suppressMessages() %>% 
          rt_from_coord_to_ind(st_bbox(s_ref)[1]+360-1, 
                               st_bbox(s_ref)[2]-1,
                               st_bbox(s_ref)[3]+360+1,
                               st_bbox(s_ref)[4]+1)
          
      } else if (country_name == "India") {
        
        nc <- 
          read_ncdf(ff[1], proxy = T) %>% 
          suppressMessages() %>% 
          rt_from_coord_to_ind(st_bbox(s_ref)[1]-1, 
                               st_bbox(s_ref)[2]-1,
                               st_bbox(s_ref)[3]+1,
                               st_bbox(s_ref)[4]+1)
        
      }
      
      
      
      ss <- 
        ff %>% 
        map(\(f){
          
          s <-
            f %>% 
            read_ncdf(ncsub = cbind(start = c(nc$x_start, nc$y_start, 1),
                                    count = c(nc$x_count, nc$x_count, NA))) %>% 
            suppressMessages() %>% 
            st_warp(s_ref)
          
        }) %>% 
        do.call(c, .)
      
      # delete
      ff %>% 
        fs::file_delete()
      
      if(ind == "growing_season_length"){
        
        ss <- 
          ss %>% 
          st_set_dimensions(3, values = 2000:2069) %>% 
          setNames("v") %>% 
          units::drop_units()
        
      } else {
        
        ss <- 
          map(2000:2069, \(yr){
            
            ss %>% 
              filter(year(time) == yr) %>% 
              st_set_dimensions("time", names = "seas", values = month(st_get_dimension_values(., "time")))
            
          }) %>% 
          {do.call(c, c(., along = "year"))} %>% 
          st_set_dimensions("year", values = 2000:2069) %>% 
          setNames("v") %>% 
          units::drop_units()
        
      }
      
      
      return(ss)
      
      
    })
  
  
  agr_met_yrs_seas_nogsl <- 
    agr_met_yrs_seas %>% 
    .[-which(names(.) == "gsl")] %>%
    do.call(c, .) #%>% 
  
  agr_met_yrs_seas_nogsl <- 
    agr_met_yrs_seas_nogsl %>% 
    split("seas") %>%
    setNames(map(names(agr_met_yrs_seas_nogsl), ~str_glue("{.x}_{seq(3,12,3)}")) %>% unlist())
  
  sf <- 
    c(agr_met_yrs_seas_nogsl,
      agr_met_yrs_seas$gsl %>% adrop() %>% setNames("gsl"))
  
  write_rds(sf, str_glue("{dir_data}/s_ind_{model}.rds"))
  
})


