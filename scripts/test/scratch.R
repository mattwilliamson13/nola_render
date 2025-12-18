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