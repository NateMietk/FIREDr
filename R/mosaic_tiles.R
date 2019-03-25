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

mosaic_tiles <- function(in_dir, out_dir, mask, n_cores = 1, timescale, fun_stat = 'max') {
  # requireNamespace('snow')
  # requireNamespace('raster')
  # requireNamespace('tidyverse')
  # requireNamespace('foreach')
  # requireNamespace('stringr')
  
  # make list of all hdf files for the aoi and time range
  hdfs <- list.files(in_dir, recursive = TRUE, full.names = TRUE)
  
  # split the native filename into a more readable format
  filename <- strsplit(basename(hdfs), "\\.") %>%
    lapply(`[`, 1) %>%
    unlist
  layer_name <- strsplit(filename, "_") %>%
    lapply(`[`, 1) %>%
    unlist
  layer_name <- unique(layer_name)
  
  if(timescale == 'months') {
    time_list <- unique(stringr::str_extract(filename, "\\d{7}")) 
  } else {
    time_list <- unique(stringr::str_extract(filename, "\\d{4}")) 
  }
  
  rename_months <- function(df) {
    case_when(df == '001' ~ '01',
              df == '032' ~ '02',
              df == '060' ~ '03',
              df == '061' ~ '03',
              df == '091' ~ '04',
              df == '092' ~ '04',
              df == '121' ~ '05',
              df == '122' ~ '05',
              df == '152' ~ '06',
              df == '153' ~ '06',
              df == '182' ~ '07',
              df == '183' ~ '07',
              df == '213' ~ '08',
              df == '214' ~ '08',
              df == '244' ~ '09',
              df == '245' ~ '09',
              df == '274' ~ '10',
              df == '275' ~ '10',
              df == '305' ~ '11',
              df == '306' ~ '11',
              df == '335' ~ '12',
              df == '336' ~ '12',
              TRUE ~ as.character(df))
  }
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  foreach::foreach (j = time_list, .packages = c('raster', 'tidyverse')) %dopar% {
    
    file_list <- list.files(in_dir, pattern = paste(j), 
                            recursive = TRUE, full.names = TRUE)
    
    if(!file.exists(file.path(out_dir, paste0(layer_name, "_", j, ".tif")))) {
      
      out_raster <- lapply(file_list, raster::raster) %>%
        do.call(merge, .)
      
      if(timescale == 'years') {
        out_raster <- raster::stack(tile_files) %>%
          raster::calc(., fun = fun_stat)
        out_filename <- j
      } else {
        out_filename <- paste0(unique(str_sub(j, 1, 4)), '_', rename_months(unique(str_sub(j, 5, 7))))
      }
      
      out_name <- file.path(out_dir, paste0(layer_name, "_", out_filename, ".tif"))
      raster::writeRaster(out_raster, out_name, format = "GTiff")
    }
    gc()
  }
  parallel::stopCluster(cl)
}