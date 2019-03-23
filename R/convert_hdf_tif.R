convert_hdf_tif <- function(tiles, in_dir, out_dir, n_cores = 1, layer_names, convert_zero_to_na = TRUE) {
  requireNamespace('parallel')
  requireNamespace('raster')
  requireNamespace('stringr')
  requireNamespace('gdalUtils')
  requireNamespace('tidyverse')
  requireNamespace('foreach')
  
  cl <- parallel::makeCluster(n_cores)
  doParallel::registerDoParallel(cl)
  
  foreach (j = 1:length(tiles), .packages = c('gdalUtils', 'tidyverse', 'raster', 'stringi')) %dopar% {
    
    raw_dates <- function(input_raster, output = out_dir) {
      
      convert_to_julian <- function(y = year, day){
        return(as.numeric(as.Date(paste(as.numeric(y), day, sep = "-"), "%Y-%j")))
      }
      
      year <- stringr::str_extract(input_raster, "\\d{4}")
      
      month_range <- strsplit(input_raster, "\\_") %>%
        lapply(`[`, 2) %>%
        substr(6, 8)
      
      if(convert_zero_to_na == TRUE) {
        mtrx <- matrix(c(-Inf, 1, NA, 367, Inf, NA), byrow=TRUE, ncol=3)
      } else {
        mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
      }
      
      ras <- raster::raster(file.path(output, input_raster)) %>%
        raster::reclassify(mtrx)  %>%
        raster::calc(., fun = function(x) convert_to_julian(day = x))
      
      writeRaster(ras, file.path(output, input_raster), format = "GTiff", overwrite=TRUE)
    }
    
    # make list of all hdf files for the aoi and time range
    hdfs <- list.files(in_dir, pattern = ".hdf",
                       recursive = TRUE)
    
    # split the native filename into a more readable format
    filename <- strsplit(hdfs, "\\.") %>%
      lapply(`[`, 2:3) %>%
      lapply(paste, collapse = "_") %>%
      unlist
    rm(hdfs)
    
    # create the final name to be written out
    outname <- paste0(layer_names, "_", filename, ".tif")
    
    # make list of all hdf files and full path name
    hdfs_full = list.files(in_dir, pattern = ".hdf",
                           recursive = TRUE, full.names = TRUE)
    for (i in 1:length(hdfs_full)) {
      if(!file.exists(paste0(out_dir, "/", outname[i]))) {
        
        # get the subdatasets from the hdf file
        sds <- gdalUtils::get_subdatasets(hdfs_full[i])
        
        for (d in 1:length(layer_names)) {
          
          # unpack the subdatasets based on name stored in object d
          gdalUtils::gdal_translate(sds[d], dst_dataset = paste0(out_dir, "/", outname[i]))
          gdalUtils::gdal_translate(sds[1], dst_dataset = paste0(out_dir, "/", outname))
          
          
          raw_dates(outname)
        }
      }
    }
  }
  
  stopCluster(cl)
}