
library(tidyverse)
library(stars)


dir_root <- "/mnt/bucket_mine/results/odni_sugarcane/"

ss <- 
  map(c("brazil", "india") %>% set_names(), \(country){
    
    fs::dir_ls(dir_root) %>% 
      str_subset(country) %>% 
      # str_subset("decadal_mean") %>% 
      str_subset("decadal_prop") %>% 
      map(read_ncdf, proxy = F) %>% 
      suppressMessages() %>% 
      {do.call(c, c(., along = "model"))}
    
  })


ss %>% 
  imap(\(s, i){
    
    # s <- ss[[2]]
    
    if (i == "brazil") {
      xlim_ <- c(-54, -34)
      ylim_ <- c(-32, -7)
    } else if (i == "india"){
      xlim_ <- c(71, 94)
      ylim_ <- c(8, 30)
    }
    
    
    s_ens <- 
      s %>%
      st_apply(c(1,2), mean, na.rm = T)
    
    tb <- 
      s_ens %>% 
      as_tibble() %>% 
      mutate(`2020-2029` = .[[4]] - .[[3]],
             `2030-2039` = .[[5]] - .[[3]],
             `2040-2049` = .[[6]] - .[[3]]) %>% 
      pivot_longer(-c(1:2))
      
    
    p1 <- 
      tb %>% 
      filter(name == "2000-2020") %>% 
      ggplot(aes(x,y, fill = value)) +
      geom_raster() +
      
      scale_fill_distiller(palette = "GnBu",
                           na.value = "transparent",
                           direction = 1,
                           trans = "sqrt",
                           name = NULL,
                           limits = c(0, 0.6),
                           oob = scales::squish,
                           breaks = seq(0, 0.6, 0.2),
                           labels = c(as.character(seq(0, 40, 20)), ">60 %"),
                           guide = guide_colorbar(barheight = 0.6, 
                                                  barwidth = 8)) +
      coord_equal(expand = F,
                  xlim = xlim_,
                  ylim = ylim_
                  ) +
      theme(axis.title = element_blank(),
            legend.position = "bottom")
    
    
    p2 <- 
      tb %>% 
      filter(name != "2000-2020") %>% 
      ggplot(aes(x,y, fill = value)) +
      geom_raster() +
      
      scale_fill_distiller(palette = "RdBu",
                           na.value = "transparent",
                           direction = 1,
                           name = NULL,
                           limits = c(-0.2, 0.2),
                           oob = scales::squish,
                           breaks = seq(-0.2, 0.2, 0.1),
                           labels = c("< -20", "-10", "0", "10", "> 20 %"),
                           guide = guide_colorbar(barheight = 0.6, 
                                                  barwidth = 12)) +
      coord_equal(expand = F,
                  xlim = xlim_,
                  ylim = ylim_
                  ) +
      theme(axis.title = element_blank(),
            legend.position = "bottom") +
      facet_wrap(~name, nrow = 1)
    
    
    patchwork::wrap_plots(p1, p2, nrow = 1)
    
    
  })




