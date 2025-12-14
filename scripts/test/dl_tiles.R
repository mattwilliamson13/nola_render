library(lidR)         # LiDAR tools
library(tidyverse)
library(sf)
library(terra)    

library(arrow)
buildings <- open_dataset('s3://overturemaps-us-west-2/release/2025-11-19.0/theme=buildings/type=building')

nrow(buildings)

nola_bbox <- tigris::counties(state = "LA", cb = TRUE, resolution = "20m") |> 
  filter(NAME %in% c("Jefferson", "St. Charles", "Orleans", "St. Bernard")) |> 
  st_bbox() |> 
  as.vector()

nola_buildings <- buildings |>
  filter(bbox$xmin > nola_bbox[1],
         bbox$ymin > nola_bbox[2],
         bbox$xmax < nola_bbox[3],
         bbox$ymax < nola_bbox[4]) |>
  select(id, geometry, height) |> 
  collect() |>
  st_as_sf(crs = 4326)

nola_buildings_p <- nola_buildings %>% st_transform(., st_crs(gno_las[[1]]))

bldg_pts <- clip_roi(gno_las[[1]], nola_buildings_p)
bldg_pts <- bldg_pts[lengths(bldg_pts) > 0]

bldgpts <- do.call(rbind, bldg_pts)


tst_canopy <- rasterize_canopy(gno_las[[1]])
tst_bld <- rasterize_canopy(bldgpts)

filepaths <- read_table("data/original/downloadlist.txt", col_names = "file_link")
local_fp <- "/Users/mattwilliamson/Downloads/noladownload.laz"

gno_filepaths <- filepaths[grepl("GreaterNewOrleans", filepaths$file_link),]



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
  gno_filepaths[[1]][121:366],
  safe_dl,
  .options = furrr_options(stdout = TRUE)
)


ter_rasts <- list.files("data/processed/", pattern = "terrain.tif", full.names = TRUE)
terrain <- map(ter_rasts, rast)
ter_rsc <- sprc(terrain)
ter_mosaic <- mosaic(ter_rsc, fun = "min")

can_rasts <- list.files("data/processed/", pattern = "canopy.tif", full.names = TRUE)
canopy <- map(can_rasts, rast)
can_rsc <- sprc(canopy)
can_mosaic <- mosaic(can_rsc, fun = "max")


old_map <- rast("/Users/mattwilliamson/Downloads/commonwealth_wd376434p_image_georectified_primary.tif")[[1:3]]
om_ext <- ext(old_map)
om_ext_p <- project(x = om_ext, from = crs(old_map), to = crs(ter_mosaic))
om_proj <- project(old_map, crs(can_mosaic))
can_crop <- crop(ter_mosaic, om_proj)

