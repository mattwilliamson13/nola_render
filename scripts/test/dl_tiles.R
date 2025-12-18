library(lidR)         # LiDAR tools
library(tidyverse)
library(sf)
library(terra)    
library(rayshader)


filepaths <- read_table("data/original/downloadlist.txt", col_names = "file_link")
filepaths_update <- read_table("data/original/downloadlist_update.txt", col_names = "file_link")

gno_filepaths <- filepaths[grepl("GreaterNewOrleans", filepaths$file_link),]
gno_updates <- filepaths_update[grepl("GreaterNewOrleans", filepaths_update$file_link),]

gno_subset <- gno_updates %>% 
  filter(!file_link %in% gno_filepaths$file_link)

dl_las_terrain <- function(destlink){
  tmp_las <- tempfile(fileext = ".laz")
  download.file(destlink, tmp_las)
  las <- readLAS(tmp_las)
  r_terrain <- rasterize_terrain(las)
  r_canopy <-  rasterize_canopy(las)
  fn <- str_sub(destlink, start = -14, end = -5)
  writeRaster(r_terrain, filename = paste0("data/processed/", fn, "terrain.tif"))
  writeRaster(r_canopy, filename = paste0("data/processed/", fn, "canopy.tif"))
unlink(tmp_las)
}

library(furrr)

safe_dl <- function(x) {
  tryCatch(
    list(
      result = dl_las_terrain(destlink = x),
      error = NULL
    ),
    error = function(e) {
      list(
        result = NULL,
        error = e
      )
    }
  )
}

plan(multisession, workers = 20)

gno_las <- future_map(
  gno_subset[[1]][101:206],
  safe_dl,
  .options = furrr_options(stdout = TRUE)
)








nola_mtx <- raster_to_matrix(complete_crop)
nola_small <- resize_matrix(nola_mtx, 0.15) 



nola_small %>% 
  sphere_shade(texture = "bw", colorintensity = 10, zscale = 3, sunangle = 45) %>%
  add_water(detect_water(nola_small,zscale = 2), color="imhof4") %>% 
  add_overlay(overlay = topo_rgb_array, alphalayer = 0.9) %>%
  add_shadow(lamb_shade(nola_small, sunaltitude = 30),0) %>%
  plot_map()




#rgb <- hist_map_proj[[1:3]]
#rgb[rgb == 0] <- NA
#hist_rgb <- as.array(rgb) / 255
#names(hist_rgb) <- c("r", "g", "b")
names(hist_map_proj) <- c("r", "g", "b")
topo_r <- resize_matrix(raster_to_matrix(hist_map_proj$r), 0.15)
topo_g <- resize_matrix(raster_to_matrix(hist_map_proj$g), 0.15)
topo_b <- resize_matrix(raster_to_matrix(hist_map_proj$b), 0.15)

#rgb_small <- resize_matrix(hist_rgb, 0.15)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))






