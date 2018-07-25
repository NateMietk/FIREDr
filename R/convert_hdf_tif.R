# Convert the raw .HDF files into GeoTifs
# Used directly after `download_tiles`
# By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#
# This function will download all files directly from a MCD64A1 ftp site
#
# Variables
#     tiles = The vector list of hdf names from `download_tiles` output or custom vector
#     n_cores = By default this parallelized and runs on 1 core, can specify more cores with the n_cores variable
#     layer_names = HDF names can be any combination of:
#                   "Burn Date", "Burn Date Uncertainty", "QA", "First Day", "Last Day"
#

convert_hdf_tif <- function(tiles, n_cores = 1, layer_names) {
  require(parallel)

  cl <- parallel::makeCluster(n_cores)
  registerDoParallel(cl)

  foreach (j = 1:length(tiles), .packages = c('gdalUtils', 'tidyverse', 'raster')) %dopar% {

    raw_dates <- function(input_raster, output = tif_months) {

      month_range <- strsplit(input_raster, "\\_") %>%
        lapply(`[`, 2) %>%
        substr(6, 8)

      mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)

      ras <- raster(file.path(output, input_raster)) %>%
        raster::reclassify(mtrx)

      writeRaster(ras, file.path(output, input_raster), format = "GTiff", overwrite=TRUE)

    }

    # make list of all hdf files for the aoi and time range
    hdfs <- list.files(hdf_months, pattern = ".hdf",
                      recursive = TRUE)

    # split the native filename into a more readable format
    filename <- strsplit(hdfs, "\\.") %>%
      lapply(`[`, 2:3) %>%
      lapply(paste, collapse = "_") %>%
      unlist
    rm(hdfs)

    # create the final name to be written out
    outname <- paste0(names, "_", filename, ".tif")

    # make list of all hdf files and full path name
    hdfs_full = list.files(hdf_months, pattern = ".hdf",
                           recursive = TRUE, full.names = TRUE)

    for (i in 1:length(hdfs_full)) {
      if(!file.exists(paste0(tif_months, "/", outname[i]))) {

        # get the subdatasets from the hdf file
        sds <- gdalUtils::get_subdatasets(hdfs_full[i])

        for (d in 1:length(layer_names)) {

          # unpack the subdatasets based on name stored in object d
          gdalUtils::gdal_translate(sds[d], dst_dataset = paste0(tif_months, "/", outname[i]))

          raw_dates(outname[i])
        }
      }
    }
  }

  stopCluster(cl)
  }