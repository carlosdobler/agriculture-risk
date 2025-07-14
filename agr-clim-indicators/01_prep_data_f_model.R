
country_name <- "India"



library(tidyverse)
library(stars)
library(furrr)

options(future.fork.enable = T)
plan(multicore)

source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")


dir_model <- "/mnt/pers_disk/agr_model/{str_to_lower(country_name)}" %>% str_glue()
fs::dir_create(dir_model)


# load yield

yield <- 
  "/mnt/pers_disk/agr_yield/yield_{str_to_lower(country_name)}_annual.nc" %>%
  str_glue() %>% 
  read_ncdf()

yield_decile <- 
  "/mnt/pers_disk/agr_yield/yield_{str_to_lower(country_name)}_annual_decile.nc" %>% 
  str_glue() %>% 
  read_ncdf()


# load agr met ind

agr_met <- 
  "/mnt/pers_disk/agr_clim_ind/{str_to_lower(country_name)}" %>%
  str_glue() %>% 
  fs::dir_ls() %>% 
  future_map(read_ncdf) %>%
  suppressMessages() %>% 
  map(st_warp, yield)

names(agr_met) <- names(agr_met) %>% fs::path_file() %>% str_remove(".nc")
names(agr_met)[1] <- "bedd"

# zero variance

prod_mask <- 
  yield %>% 
  st_apply(c(1,2), \(x) {
    
    a <- if_else(all(is.na(x)) | mean(x, na.rm = T) < 1, NA, 1)
    return(a)
    
  }, 
  .fname = "mask")

# ******

# s_index <- 
#   prod_mask %>% 
#   mutate(lon = rep(1:158, times = 157),
#          lat = rep(1:157, each = 158))
  

# ******


prod_mask_ncells <- 
  prod_mask %>% 
  pull() %>% 
  sum(na.rm = T)

nzv <- 
  agr_met %>% 
  set_names(fs::path_file(names(agr_met))) %>% 
  map_dbl(\(s){
    
    s[is.na(prod_mask)] <- NA
    
    s %>% 
      st_apply(c(1,2), \(x){
        
        if(all(is.na(x))){
          # c(u = NA, fr = NA)
          NA
          
        } else {
          
          ux <- unique(x)
          m <- ux[which.max(tabulate(match(x, ux)))]
          fr <- sum(x == m)/length(x)
          
          # c(u = length(ux), fr = fr)
          
          if_else(length(ux) < 10 & fr > 0.9, NA, 1)
          
        }
        
        
      },
      .fname = "nz") %>% 
      pull() %>% 
      sum(na.rm = T) %>% 
      {./prod_mask_ncells}
    
  })

agr_met <- 
  agr_met[nzv > 0.9]



# assemble

time_dim <- agr_met[[1]] %>% st_get_dimension_values("time")
# time_dim_shifted <- time_dim %>% {if_else(month(.) == 12, .+years(1), .)}

yrs <- time_dim %>% year() %>% unique()
yrs <- yrs[map_lgl(yrs, ~sum(year(time_dim) == .x) == 4)] # only full years (with 4 seasons)
yrs <- yrs %>% head(-1) # 2020-12-01 was done with only 1 season, so remove the whole year


agr_met_yrs_seas <- 
  agr_met %>% 
  map(\(s){
    
    # s[is.na(prod_mask)] <- NA
    
    map(yrs, \(yr){
      
      s %>% 
        filter(year(time) == yr) %>% 
        st_set_dimensions("time", names = "seas", values = month(st_get_dimension_values(., "time")))
      
    }) %>% 
      {do.call(c, c(., along = "year"))} %>% 
      st_set_dimensions("year", values = yrs) %>% 
      setNames("v")
    
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
  c(yield %>% 
      filter(year(time) %in% yrs) %>% 
      st_set_dimensions(3, names = "year", values = yrs),
    # yield_decile %>% 
    #   filter(year(time) %in% seq(2000,2020)) %>% 
    #   st_set_dimensions(3, names = "year", values = 2000:2020) %>% 
    #   setNames("yield_decile"),
    agr_met_yrs_seas_nogsl,
    agr_met_yrs_seas$gsl %>% adrop() %>% setNames("gsl"))

# tb_index <- 
#   s_index %>% 
#   as_tibble() %>% 
#   filter(!is.na(mask))


write_rds(sf, str_glue("{dir_model}/s_f_model.rds"))





