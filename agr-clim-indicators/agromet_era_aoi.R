
country_name <- "India"

total_period_years <- c(2000,2020)
base_years <- c(2001,2020)



# SCRIPT TO CALCULATE INDICATORS FOR AN AREA 
# BASED ON ERA5 DATA

library(tidyverse)
library(stars)
library(units)
library(furrr)

# https://github.com/pacificclimate/climdex.pcic
# https://pacificclimate.org/R/climdex.pcic_1.1-11.tar.gz
library(climdex.pcic)

options(future.fork.enable = T)
options(future.globals.maxSize = 1000*1024^2)

source("https://raw.github.com/carlosdobler/spatial-routines/master/general_tools.R")



dir_gs <- "gs://clim_data_reg_useast1/era5/daily_aggregates"

dir_data <- "/mnt/pers_disk/tmp"
dir_res <- "/mnt/pers_disk/agr_clim_ind/{str_to_lower(country_name)}" %>% str_glue()

fs::dir_create(dir_res)


# cloud directory where final results will be uploaded
# dir_res <- "gs://clim_data_reg_useast1/cmip6/era5"


var_combos <- list(list(vars_long = c("maximum_temperature", "minimum_temperature"),
                        vars_short = c("tasmax", "tasmin"),
                        vars_era = c("2m_maximum_temperature", "2m_minimum_temperature")),
                   list(vars_long = c("precipitation", "average_temperature"),
                        vars_short = c("pr", "tas"),
                        vars_era = c("total_precipitation", "2m_temperature")))
                   

tb_vars <- 
  read_csv("agr-clim-indicators/tb_vars.csv")


fn_calc_indicator <- function(s, fn, v) {
  
  # s = stars obj
  # fn = climdex function
  # v = climdex name (short)
  
  s %>% 
    st_apply(c(1,2), function(x) {
      
      if (any(is.na(x))) {
        
        rep(NA, length(time_seas))
        
      } else {
        
        fn(x)
        
      }
      
    },
    FUTURE = T,
    .fname = "time") %>% 
    aperm(c(2,3,1)) %>% 
    st_set_dimensions(3, values = time_seas) %>% 
    
    rt_write_nc(str_glue("{dir_res}/{v}.nc"), daily = F, calendar = "gregorian")
  
}





# VAR COMBO LOOP --------------------------------------------------------------


for (vc in seq_along(var_combos)) {
  
  print(str_glue("  PROCESSING INDs W VARS {str_flatten(var_combos[[vc]]$vars_long, ' + ')}"))
  
  tb_vars_sub <- 
    tb_vars %>%
    filter(str_detect(under_var, str_flatten(var_combos[[vc]]$vars_long, "|")))
  
  
  # DOWNLOAD DATA --------------------------------------------------------------
  
  plan(multicore)
  
  
  print(str_glue("    Downloading raw data..."))
  
  fs::dir_create(dir_data)
  
  # get filenames of variables to use
  ff <- 
    var_combos[[vc]]$vars_era %>% 
    map(~str_glue("gsutil ls {dir_gs}/{.x}") %>% system(intern = T)) %>% 
    map(~str_subset(.x, str_flatten(total_period_years[1]:total_period_years[2], "|")))
  
  # same length?
  length(ff[[1]]) == length(ff[[2]])
  
  
  # download all files in directories (parallel)
  
  if (vc == 2) { # only precip
    ff[[2]] <- character()
  }
  
  
  ff %>% 
    unlist(use.names = F) %>% 
    future_walk(\(f){
      
      str_glue("gsutil cp {f} {dir_data}") %>% 
        system(ignore.stdout = T, ignore.stderr = T)
      
    })
  
  ff <- 
    ff %>% 
    map(~fs::path_file(.x) %>% {str_glue("{dir_data}/{.}")})
  
  
  
  # PREP AOI ------------------------------------------------------------------
  
  s_proxy <- 
    ff[[1]][1] %>% 
    read_ncdf(proxy = T)
  
  # nc <- rt_from_coord_to_ind(s_proxy, 281, -35, 327, 8) # <---- BRAZIL
  nc <- rt_from_coord_to_ind(s_proxy, 59, 5, 94, 38) # <---- INDIA
  
  
  # PREP TIME ------------------------------------------------------------------
  
  print(str_glue("    Prep time vectors..."))
  
  # All dates
  time_vector <- 
    seq(as_date(str_glue("{total_period_years[1]}-01-01")), as_date(str_glue("{total_period_years[2]}-12-31")),  by = "1 day") %>% 
    # update time_vector
    as.character() %>% 
    PCICt::as.PCICt(cal = "gregorian")
  
  
  # table for seasonal splits
  tb_time <- 
    tibble(date = time_vector,
           yr = str_sub(date, end = 4) %>% as.numeric(),
           mon = str_sub(date, 6,7) %>% as.numeric(),
           dy = str_sub(date, 9,10) %>% as.numeric(),
           seas = case_when(mon %in% c(12, 1, 2) ~ "1", # DJF
                            mon %in% c(3:5) ~ "2", # MAM
                            mon %in% c(6:8) ~ "3", # JJA
                            mon %in% c(9:11) ~ "4"), # SON
           yr_shift = if_else(mon %in% c(1, 2), yr - 1, yr)) %>% 
    group_by(yr) %>% 
    mutate(jdy = row_number()) %>% 
    ungroup()
  
  # tb_time %>% 
  #   group_by(yr_shift, seas) %>% 
  #   summarize(n = n()) %>% 
  #   arrange(n)
  
  
  seas <- 
    str_glue("{tb_time$yr_shift}_{tb_time$seas}") %>% 
    as.vector() %>% 
    as.factor()
  
  time_seas <- 
    tb_time %>% 
    group_by(yr_shift, seas) %>% 
    mutate(s = str_glue("{first(yr_shift)}-{first(mon)}-{first(dy)}")) %>% 
    pull(s) %>% 
    as.vector() %>% 
    unique() %>% 
    PCICt::as.PCICt(cal = "gregorian")
  
  # southern hemisphere
  first_day_sh <- which(month(time_vector) == 7) %>% first() %>% {.-1}
  
  time_vector_sh <- head(time_vector, -first_day_sh) # not tail?
  
  
  time_yrs <- 
    time_vector %>% 
    year() %>% 
    unique() %>% 
    {str_glue("{.}-01-01")} %>% 
    PCICt::as.PCICt(cal = "gregorian")
  
  
  
  
  
  
  # Load data
  
  print(str_glue("      Loading data..."))
  tictoc::tic("      done")
  
  if (vc == 2) {
    
    tas <- 
      (l_s$tasmax + l_s$tasmin)/2
    
    tas <- 
      tas %>% 
      setNames("tas")
    
  }
  
  l_s <- 
    ff %>% 
    set_names(var_combos[[vc]]$vars_short) %>% 
    imap(function(ff_var, n_short){
      
      if(length(ff_var) > 0) {
        
        s <- 
          ff_var %>% 
          future_map(read_ncdf, ncsub = cbind(start = c(nc$x_start, nc$y_start, 1),
                                              count = c(nc$x_count, nc$y_count, NA))) %>%
          suppressMessages() %>% 
          map(setNames, "v")
          
        
        if (vc == 2){
          
          un <- 
            s %>% 
            map(pull) %>% 
            map(units) %>% 
            map(pluck, "numerator")
          
          s <- 
            s %>% 
            map2(un, \(s_, u_){
              
              s_ %>% 
                mutate(v = v %>% units::drop_units() %>% units::set_units(!!u_) %>% units::set_units(mm))
              
            })
          
          s <- 
            s %>% 
            map(\(s_){
              
              s_ %>% 
                units::drop_units() %>% 
                mutate(v = if_else(v < 0, 0, v)) %>% 
                setNames(n_short)
              
            })
          
        }
        
        
        s <- 
          s %>% 
          do.call(c, .)
        
        if (str_detect(ff_var[1], "temperature")) {
          
          s <- 
            s %>% 
            mutate(v = 
                     v %>% 
                     set_units(degC) %>% 
                     set_units(NULL)) %>% 
            setNames(n_short)
          
        } #else if (str_detect(ff_var[1], "precipitation")) {
        
        #   s %>%
        #     mutate(v =
        #              v %>%
        #              set_units(kg/m^2/d) %>%
        #              set_units(NULL) %>%
        #              if_else(. < 0, 0, .)) %>%
        #     setNames(variable)
        # 
        # }
        
        
        return(s)
        
      }
      
      
      
      
    })
  
  tictoc::toc()
  
  if (vc == 2) {
    
    l_s$tas <- tas
    st_dimensions(l_s$tas) <- st_dimensions(l_s$pr)
    
  }
  
  
  
  # CALCULATE INDICATORS -------------------------------------------------------
  
  print(str_glue("      Calculating indices..."))
  
  plan(sequential)
  plan(multicore, workers = 6)
  
  
  walk(tb_vars_sub$var_short, function(var_sh) {
    
    print(var_sh)
    
    # *************
    
    if (var_sh == "cfd") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          spell.length.max(x,
                                           date.factor = seas,
                                           threshold = 0,
                                           op = "<",
                                           spells.can.span.years = F)
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "csdi") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          
                          q <- get.outofbase.quantiles(tmin = x,
                                                       tmin.dates = time_vector, # dates
                                                       base.range = base_years, # baseline
                                                       temp.qtiles = 0.1, # quantile threshold
                                                       n = 5) %>% # spell duration
                            unlist() %>%
                            unname()
                          
                          threshold.exceedance.duration.index(daily.temp = x,
                                                              date.factor = seas,
                                                              jdays = tb_time$jdy,
                                                              thresholds = q,
                                                              op = "<",
                                                              spells.can.span.years = F,
                                                              max.missing.days = 1000)
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "wsdi") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          
                          q <- get.outofbase.quantiles(tmax = x,
                                                       tmax.dates = time_vector,
                                                       base.range = base_years,
                                                       temp.qtiles = 0.9,
                                                       n = 5) %>%
                            unlist() %>%
                            unname()
                          
                          threshold.exceedance.duration.index(daily.temp = x,
                                                              date.factor = seas,
                                                              jdays = tb_time$jdy,
                                                              thresholds = q,
                                                              op = ">",
                                                              spells.can.span.years = F,
                                                              max.missing.days = 1000)
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "csu") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          spell.length.max(x,
                                           date.factor = seas,
                                           threshold = 25,
                                           op = ">",
                                           spells.can.span.years = F)
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "dtr") {
      
      fn_calc_indicator(l_s$tasmax - l_s$tasmin,
                        function(x) {
                          aggregate(x, by = list(seas), mean)$x
                        },
                        var_sh)
    }
    
    
    # *************
    
    if (var_sh == "fd") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          aggregate(x < 0, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "id") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          aggregate(x < 0, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "su") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          aggregate(x > 25, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "tn") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          aggregate(x, by = list(seas), mean)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "tnn") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          aggregate(x, by = list(seas), min)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "tnx") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          aggregate(x, by = list(seas), max)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "tr") {
      
      fn_calc_indicator(l_s$tasmin,
                        function(x) {
                          aggregate(x > 20, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "tx") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          aggregate(x, by = list(seas), mean)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "txn") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          aggregate(x, by = list(seas), min)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "txx") {
      
      fn_calc_indicator(l_s$tasmax,
                        function(x) {
                          aggregate(x, by = list(seas), max)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "cdd") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          spell.length.max(x,
                                           date.factor = seas,
                                           threshold = 1,
                                           op = "<",
                                           spells.can.span.years = F)
                        },
                        var_sh)
    } 
    
    # *************
    
    if (var_sh == "cwd") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          spell.length.max(x,
                                           date.factor = seas,
                                           threshold = 1,
                                           op = ">=",
                                           spells.can.span.years = F)
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "ww") {
      
      fn_calc_indicator(c(l_s$tas, l_s$pr, along = "v"),
                        function(x) {
                          
                          q_tas <- 
                            get.outofbase.quantiles(tmin = x[,1],
                                                    tmin.dates = time_vector,
                                                    base.range = base_years,
                                                    temp.qtiles = 0.75,
                                                    n = 5) %>%
                            unlist() %>%
                            unname()
                          
                          
                          t_index <- year(time_vector) >= base_years[1] & year(time_vector) <= base_years[2]
                          q_pr <-
                            aggregate(x[,2][t_index], 
                                      by = list(yday(time_vector[t_index])), 
                                      function(x) quantile(x, 0.75))$x
                          
                          
                          tibble(tas = x[,1],
                                 pr = x[,2],
                                 time = time_vector,
                                 jdy = yday(time_vector)) %>% 
                            left_join(tibble(q_tas = q_tas) %>% mutate(jdy = row_number())) %>% 
                            left_join(tibble(q_pr = q_pr) %>% mutate(jdy = row_number())) %>% 
                            suppressMessages() %>% 
                            mutate(ww = if_else(tas > q_tas & pr >= 1 & pr > q_pr, 1, 0)) %>% 
                            pull(ww) %>% 
                            
                            aggregate(by = list(seas), sum) %>% 
                            .$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "bedd-10") {
      
      fn_calc_indicator(l_s$tas,
                        function(x) {
                          aggregate(x > 10 & x < 30, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "gsl") {
      
      # nh <- F #st_get_dimension_values(l_s$tas, "latitude")[1] >= 0
      nh <- T
      
      l_s$tas %>% 
        st_apply(c(1,2), function(x) {
          
          if (any(is.na(x))) {
            
            rep(NA, length(unique(year(time_vector))))
            
          } else {
            
            if (nh == TRUE) {
              
              growing.season.length(x, 
                                    factor(year(time_vector)), 
                                    time_vector,
                                    northern.hemisphere = T)
              
            } else {
              
              growing.season.length(tail(x, -first_day_sh), 
                                    factor(year(time_vector_sh)), 
                                    time_vector_sh,
                                    northern.hemisphere = T)
              
            }
          }
        }, 
        .fname = "time") %>% 
        aperm(c(2,3,1)) %>% 
        st_set_dimensions(3, values = time_yrs) %>% 
        rt_write_nc(str_glue("{dir_res}/{var_sh}.nc"), daily = F, calendar = "gregorian")
    }
    
    # *************
    
    if (var_sh == "r10mm") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          aggregate(x >= 10, by = list(seas), sum)$x
                        },
                        var_sh)
    }   
    
    # *************
    
    if (var_sh == "r20mm") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          aggregate(x >= 20, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "rr") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          aggregate(x, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "rr1") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          aggregate(x >= 1, by = list(seas), sum)$x
                        },
                        var_sh)
    }
    
    # *************
    
    if (var_sh == "sdii") {
      
      fn_calc_indicator(l_s$pr,
                        function(x) {
                          aggregate(x, 
                                    by = list(seas), 
                                    function(x) {
                                      if (all(x < 1)) {
                                        0
                                      } else {
                                        mean(x[x >= 1])
                                      }
                                      
                                    })$x
                        },
                        var_sh)
      
      # simple.precipitation.intensity.index(x, seas)
    }
    
    # *************
    
    if (var_sh == "tg") {
      
      fn_calc_indicator(l_s$tas,
                        function(x) {
                          aggregate(x, by = list(seas), mean)$x
                        },
                        var_sh)
    }
    
  })
  
  
  dir_data %>%
    fs::dir_delete()
  
  
}




# MOSAIC ----------------------------------------------------------------------


vars <- 
  dir_tiles %>% 
  fs::dir_ls() %>% 
  fs::path_ext_remove() %>% 
  fs::path_file() %>% 
  str_split("_", simplify = T) %>% 
  .[,1] %>% 
  unique()


for (var in vars) {
  
  print(str_glue(" "))
  print(str_glue("    Mosaicking var: {var}"))
  
  
  if (var == "gsl") {
    # annual (as opposed to seasonal)
    yrs_i <- cut(year(time_yrs), seq(1960, 2100, by = 10), right = F, labels = F)
  } else {
    yrs_i <- cut(year(time_seas), c(1959, seq(1960, 2100, by = 10)), right = F, labels = F)
  }
  
  
  
  # loop through blocks of yrs
  for (yr_i in unique(yrs_i)) {
    
    print(str_glue("      year block: {yr_i}"))
    
    pos <- 
      which(yrs_i == yr_i)
    
    if (var == "gsl") {
      time_ <- time_yrs[pos]
    } else {
      time_ <- time_seas[pos]
    }
    
    
    cols_1var <-
      
      # loop through cols
      future_map(unique(tiles$start_x), function(col_i) {
        
        r <- 
          tiles %>% 
          filter(start_x == col_i) %>% 
          left_join(st_drop_geometry(tiles_land), by = "tile_id") %>% 
          pmap(function(tile_id, start_x, end_x, start_y, end_y, cover, ...) {
            
            if (cover == FALSE) {
              
              s <- s_proxy[, start_x:end_x, start_y:end_y]
              
              empty_array <- 
                array(NA, dim = c(dim(s)[1], 
                                  dim(s)[2],
                                  last(pos) - first(pos) +1))
              
              names(dim(empty_array)) <- c("lon", "lat", "time")
              
              # format as stars
              empty_stars <- 
                st_as_stars(empty_array)
              
              st_dimensions(empty_stars)[1] <- st_dimensions(s)[1]
              st_dimensions(empty_stars)[2] <- st_dimensions(s)[2]
              empty_stars <- st_set_dimensions(empty_stars, 3, values = time_)
              
              return(empty_stars)
              
              
            } else {
              
              dir_tiles %>% 
                fs::dir_ls(regexp = str_glue("/{var}_{tile_id}.nc")) %>%
                read_ncdf(ncsub = cbind(start = c(1, 1, first(pos)),
                                        count = c(NA,NA,last(pos) - first(pos) +1))) %>% 
                suppressMessages()
            }
            
            
            
          })
        
        r <- 
          do.call(c, c(r, along = 2))
        
        st_dimensions(r)[2] <- st_dimensions(s_proxy)[2]
        
        return(r)
        
      })
    
    
    # merge all columns
    mos <- 
      do.call(c, c(cols_1var, along = 1))
    
    # fix lon dimension
    st_dimensions(mos)[1] <- st_dimensions(s_proxy)[1]
    
    
    # SAVING ------------------
    
    if (var == "gsl") {
      per <- "yr" %>% set_names("annual")
    } else {
      per <- "seas" %>% set_names("seasonal")
    }
    
    
    filename <-
      str_glue("{tempdir()}/{var}_{per}_{model}_{year(first(time_))}-{year(last(time_))}.nc")
    
    un <- tb_vars %>% filter(var_short == var) %>% pull(unit)
    var_long <- tb_vars %>% filter(var_short == var) %>% pull(var_long)
    
    write_nc(mos %>% 
               setNames("v") %>% 
               mutate(v = units::set_units(v, !!un)) %>% 
               setNames(var),
             time_seas[pos],
             filename)
    
    filename_gs <- str_glue("{dir_res}/{names(per)}/{model}/{var_long}/{fs::path_file(filename)}")
    
    str_glue("gsutil cp {filename} {filename_gs}") %>% 
      system(ignore.stdout = T, ignore.stderr = T)
    
    fs::file_delete(filename)
    
    
  } # End of block of years loop
  
  # remove tiles
  # dir_tiles %>%
  #   fs::dir_ls() %>% #regexp = str_glue("{var}_")) %>%
  #   walk(fs::file_delete)
  
} # End of vars loop

fs::dir_delete(dir_data)
fs::dir_delete(dir_tiles)

}

}









