convert_hdf_tif <- function(tiles, in_dir, out_dir, n_cores = 1, layer_names, convert_zero_to_na = TRUE, convert_date = TRUE) {
  requireNamespace('parallel')
  requireNamespace('raster')
  requireNamespace('stringr')
  requireNamespace('gdalUtils')
  requireNamespace('tidyverse')
  requireNamespace('foreach')
  
  for (j in unique(tiles)) {
    raw_dates <- function(input_raster, output = out_dir) {
      
      convert_to_julian <- function(y = year, day){
        return(as.numeric(as.Date(paste(as.numeric(y), day, sep = "-"), "%Y-%j")))
      }
      
      year <- stringr::str_extract(input_raster, "\\d{4}")
      
      if(convert_zero_to_na == TRUE) {
        mtrx <- matrix(c(-Inf, 1, NA, 367, Inf, NA), byrow=TRUE, ncol=3)
      } else {
        mtrx <- matrix(c(-Inf, 1, 0, 367, Inf, 0), byrow=TRUE, ncol=3)
      }
      
      ras <- raster::raster(input_raster)
      if(convert_zero_to_na == TRUE) {
        ras <- ras %>%
          raster::calc(., fun = function(x) convert_to_julian(day = x)) %>%
          raster::reclassify(., mtrx) 
      } else {
        ras <- ras %>%
          raster::reclassify(., mtrx) 
      }
      
      writeRaster(ras, input_raster, format = "GTiff", overwrite=TRUE)
    }
    
    # make list of all hdf files for the aoi and time range
    hdfs <- list.files(in_dir, pattern = paste(j),
                       recursive = TRUE, full.names = TRUE)
    
    # split the native filename into a more readable format
    filename <- strsplit(basename(hdfs), "\\.") %>%
      lapply(`[`, 2:3) %>%
      lapply(paste, collapse = "_") %>%
      unlist
    
    # create the final name to be written out
    outname <- paste0(layer_names, "_", filename, ".tif")
    
    cl <- parallel::makeCluster(n_cores)
    doParallel::registerDoParallel(cl)
    
    foreach (i = 1:length(hdfs), .packages = c('gdalUtils', 'foreach')) %do% {
        
      out_file <- paste0(out_dir, "/", outname[i])
      
      if(!file.exists(out_file)) {
        
        # get the subdatasets from the hdf file
        sds <- gdalUtils::get_subdatasets(hdfs[i])
        
        foreach (d = 1:length(layer_names), .packages = c('gdalUtils','tidyverse', 'raster', 'stringr')) %dopar% {
          
          # unpack the subdatasets based on name stored in object d
          gdalUtils::gdal_translate(sds[d], dst_dataset = out_file)
          
          raw_dates(input_raster = out_file)
        }
      }
    }
    parallel::stopCluster(cl)
  } 
}
