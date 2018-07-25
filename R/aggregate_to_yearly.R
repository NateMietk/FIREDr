# Aggreagte monthly files into yearly
# By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#
# This function will download all files directly from a MCD64A1 ftp site
#
## Variables
#     tiles = The vector list of hdf names from `convert_hdf_tif` output or custom vector
#     n_cores = By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#     layer_names = HDF names can be any combination of:
#                   "Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day"
#
# Future capabilities: Add custom date ranges for download

aggreate_to_yearly <- function(tiles, n_cores = 1, year_range, in_dir, out_dir, funs = 'max') {
  require(parallel)

  cl <- parallel::makeCluster(n_cores)
  registerDoParallel(cl)

  foreach (j = 1:length(tiles), .packages = c('foreach', 'tidyverse', 'raster')) %dopar% {
    foreach (i = year_range) %do% {

      tile_files = as.vector(Sys.glob(paste0(in_dir, "/", "*", i, "*", tiles[j], ".tif")))

      if(!file.exists(paste(out_dir, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep=""))){
        fire <- raster::stack(tile_files) %>%
          raster::calc(., fun = funs)

        tfilename = paste(out_dir, "/Yearly_BD_", tiles[j], "_", i, ".tif", sep="")

        raster::writeRaster(fire, tfilename, format = "GTiff", overwrite=TRUE)
      }
    }
  }
  stopCluster
  }

