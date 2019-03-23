# Mosaic multiple MODIS tiles into one layer
# By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#
# This function will download all files directly from a MCD64A1 ftp site
#
## Variables
#     tiles = The vector list of hdf names from `convert_hdf_tif` output or custom vector
#     n_cores = By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#     in_dir = Location of tiles to be mosaiced
#     out_dir = The directory to house all the mosaiced tiles

mosaic_tiles <- function(year_range, n_cores = 1, in_dir, out_dir, mask, to_project, projection, projected_resolution, projection_method) {

  for (i in year_range) {

    if(!file.exists(paste0(out_dir,"/USA_", names, "_", i, ".tif"))){
      tile_files = as.vector(Sys.glob(paste0(in_dir, "/", "*", i, ".tif")))

      final <- lapply(tile_files, raster::raster) %>%
        do.call(merge, .)

      requireNamespace(snow)
      beginCluster(n_cores)

      if(to_project == TRUE) {
        final <- final %>%
          raster::projectRaster(crs = projection,
                                res = projected_resolution,
                                method = projection_method) %>%
          raster::crop(as(mask, 'Spatial')) %>%
          raster::mask(as(mask, 'Spatial'))
      } else {
        final <- final %>%
          raster::crop(as(mask, 'Spatial')) %>%
          raster::mask(as(mask, 'Spatial'))
      }
      endCluster()

      final[final < 1] <- NA
      final[final > 366] <- NA

      final_name <- paste0(out_dir,"/USA_", names, "_", i, ".tif")

      raster::writeRaster(final, final_name, format = "GTiff", overwrite=TRUE)
      gc()
    }
  }
}
