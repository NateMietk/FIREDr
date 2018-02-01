# This function extracts the MODIS tiles that intersect with the shapefile area of interest
# The result will be a vector list of tiles that intersect with the spatial limiits of the
# simple features dataframe

# Variables
#   : aoi_mask = The shapefile mask where the tiles numbers are to be extracted.
#                This shapefile mask object is expected to be an sf object
#

get_tiles <- function(aoi_mask){

  require(tidyverse)

  #Download the MODIS tile grid -------------------------
  dir.create("tmp", showWarnings = FALSE)
  dir_path <- file.path("tmp")
  loc <- "https://s3-us-west-2.amazonaws.com/modis-grids/modis_grid.zip"
  dest <- paste0(dir_path, ".zip")
  download.file(loc, dest)
  unzip(dest, exdir = dir_path)
  unlink(dest)

  modis_grid <- sf::st_read( file.path(dir_path, "modis_sinusoidal_grid_world.shp"), quiet= TRUE) %>%
    dplyr::mutate(h = if_else(nchar(as.character(h)) == 1, paste0("h0", as.character(h)), paste0("h", as.character(h))),
                  v = if_else(nchar(as.character(v)) == 1, paste0("v0", as.character(v)), paste0("v", as.character(v))),
                  hv = paste0(h, v, sep = " "))
  unlink(dir_path, recursive = TRUE)

  tiles <- aoi_mask %>%
    sf::st_transform(st_crs(modis_grid)) %>%
    sf::st_intersection(modis_grid) %>%
    as.data.frame() %>%
    dplyr::select(hv) %>%
    distinct(., hv)
  return(as.vector(tiles$hv) %>%
           stringr::str_trim(., side = "both"))
}
