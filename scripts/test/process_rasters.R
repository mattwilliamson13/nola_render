library(purrr)
library(terra)    
library(rayshader)

ter_rasts <- list.files("data/processed/", pattern = "terrain.tif", full.names = TRUE)
terrain <- map(ter_rasts, rast)
ter_rsc <- sprc(terrain)
ter_mosaic <- mosaic(ter_rsc, fun = "min")

can_rasts <- list.files("data/processed/", pattern = "canopy.tif", full.names = TRUE)
canopy <- map(can_rasts, rast)
can_rsc <- sprc(canopy)
can_mosaic <- mosaic(can_rsc, fun = "max")

old_map <- rast("/Users/mattwilliamson/Downloads/commonwealth_wd376434p_image_georectified_primary.tif")[[1:4]]
om_ext <- ext(old_map)
om_ext_p <- project(x = om_ext, from = crs(old_map), to = crs(ter_mosaic))
can_crop <- crop(can_mosaic, om_ext_p)
ter_crop <- crop(ter_mosaic, om_ext_p)
complete_crop <- ter_crop
complete_crop[is.na(complete_crop)] <- 1

hist_map_proj <- project(
  old_map,
  complete_crop,
  method = "near",
  threads = TRUE# critical for maps
)

#hist_map_proj <- resample(
#  hist_map_proj,
#  complete_crop,
#  method = "near"
#)


tst_bbox <- hist_map_proj[[4]]
tst_bbox[tst_bbox == 0] <- NA
tst_bbox <- as.polygons(tst_bbox)

tst_bbox <- terra::buffer(tst_bbox, -60)
hist_map_crop <- crop(hist_map_proj, tst_bbox, mask=TRUE)
outer_elev <- crop(complete_crop, tst_bbox, mask=TRUE)
outer_elev <- outer_elev*0 + 1
nl_ext <- terra::buffer(tst_bbox, -725)

inner_elev <- crop(complete_crop, nl_ext, mask=TRUE)

e <- ext(nl_ext)

dx <- (xmax(e) - xmin(e)) * 0.07
dy <- (ymax(e) - ymin(e)) * 0.07

box <- as.polygons(ext(
  xmax(e) - 4.7*dx,
  xmax(e) - 3.1*dx,
  ymin(e) + dy,
  ymin(e) + 3*dy
), crs = crs(nl_ext))

box <- shift(box, dy = -800, dx = 450)
box_rast <- rasterize(box, inner_elev, field = 1)
inner_elev[box_rast == 1] <- 0
mod_el <- merge(inner_elev, outer_elev)
nola_mtx <- resize_matrix(raster_to_matrix(mod_el), 0.1)


names(hist_map_crop[[1:3]]) <- c("r", "g", "b")
topo_r <- resize_matrix(raster_to_matrix(hist_map_crop$r), 0.1)
topo_g <- resize_matrix(raster_to_matrix(hist_map_crop$g), 0.1)
topo_b <- resize_matrix(raster_to_matrix(hist_map_crop$b), 0.1)

#rgb_small <- resize_matrix(hist_rgb, 0.15)
topo_rgb_array <- array(0, dim = c(nrow(topo_r), ncol(topo_r), 3))
topo_rgb_array[,,1] <- topo_r/255
topo_rgb_array[,,2] <- topo_g/255
topo_rgb_array[,,3] <- topo_b/255


## the array needs to be transposed, just because.

topo_rgb_array <- aperm(topo_rgb_array, c(2,1,3))

nola_mtx %>% 
  sphere_shade(texture = , colorintensity = 10, zscale = 3, sunangle = 45) %>%
  add_overlay(overlay = topo_rgb_array, alphalayer = 0.9) %>%
  #add_water(detect_water(nola_mtx,zscale = 2), color="imhof4") %>% 
  #add_shadow(lamb_shade(nola_mtx, sunaltitude = 30),0) %>%
  add_shadow(ambient_shade(nola_mtx,zscale=1/4)) %>% 
  plot_3d(nola_mtx, theta=2.5,  phi=60, windowsize = c(1600,1600), zoom = 0.6, fov=10)


library(arrow)
buildings <- open_dataset('s3://overturemaps-us-west-2/release/2025-11-19.0/theme=buildings/type=building')
library(sf)

nola_bbox <- nl_ext
nola_bbox_p <- project(nola_bbox, "epsg:4326") %>% 
  st_as_sf() %>% 
  st_bbox()
nola_buildings <- buildings |>
  dplyr::filter(bbox$xmin > nola_bbox_p[1],
                bbox$ymin > nola_bbox_p[2],
                bbox$xmax < nola_bbox_p[3],
                bbox$ymax < nola_bbox_p[4]) |>
  dplyr::select(id, geometry, height) |> 
  dplyr::collect() |>
  st_as_sf(crs = 4326)

nola_buildings_p <- nola_buildings %>% st_transform(., st_crs(complete_crop)) %>% 
  tidyr::drop_na(height) %>% 
  dplyr::filter(height <10)

nola_cln <-st_cast(nola_buildings_p, "MULTIPOLYGON") %>% st_cast("POLYGON") 
nola_cln <- nola_cln[!tst,]
tst <- st_within(nola_cln, st_as_sf(box), sparse = FALSE)



render_buildings(nola_cln,  flat_shading  = TRUE, 
                 angle = 15 , heightmap = nola_mtx, 
                 material = "#E6F2FF", roof_material = "#E6F2FF",
                 extent = ext(nola_bbox), roof_height = 0.75, base_height = 0, data_column_top = "height",
                 alpha=0.01,
                 zscale=10, color = "#E6F2FF",
                 shadow = FALSE,
                 light_intensity = 0.3,
                 clear_previous = TRUE)
render_camera(theta=220, phi=22, zoom=0.45, fov=0)
render_snapshot()