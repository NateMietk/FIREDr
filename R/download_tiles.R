# Download all MODIS MCD64A1 tiles based on the returned values from `get_tiles`
#
# This function will download all files directly from a MCD64A1 ftp site
#
## Variables
#   : tiles = The vector list of tile names from `get_tiles` output or custom vector
#     url = The URL of the ftp site or location to download file from
#     u_p = The username and password toenter the ftp site
#           Formatted as username:password
#     out_dir = The directory to house all the downloaded tiles
#
# Future capabilities: Add custom date ranges for download

download_tiles <- function(tiles, url, u_p, out_dir){
  require(tidyverse)

  for (j in 1:length(tiles)){

  filenames <- RCurl::getURL(paste0(url, tiles[j],"/"), userpwd = u_p, v=T,
                             ftp.use.epsv = FALSE)

  # write to a temporary file
  cat(filenames, file = 'tmp.txt')

  # read the temporary file as a fixed width text file
  dir_listing <- read_fwf('tmp.txt', fwf_empty('tmp.txt'))

  # give columns good names
  names(dir_listing) <- c('z1', 'z2', 'z3', 'z4', 'size_in_bytes',
                          'month_modified', 'date_modified', 'year_modified',
                          'filename')

  # filter out columns we don't care about
  dir_listing <- dplyr::select(dir_listing, -starts_with('z'))

  # iterate over the files and download each
  for (i in 1:nrow(dir_listing)) {
    output_file_name <- file.path(out_dir, dir_listing$filename[i])

    if (!file.exists(output_file_name)) {
      # download the hdf file
      download.file(paste0(url, tiles[j], "/", dir_listing$filename[i]),
                    output_file_name)

      # check size of downloaded file
      local_size <- file.info(output_file_name)$size

      # check to see if the downloaded file size is identical to the servers file size
      are_bytes_identical <- identical(as.integer(local_size), dir_listing$size_in_bytes[i])

      if (!are_bytes_identical) {
        # add warning if the downloaded file is a fragment of the source file
        warning(paste('Mismatch in file size found for', dir_listing$filename[i]))

        # write the name of the fragement to an output dataframe
        res <- res %>%
          mutate(source_file = dir_listing$filename[i])

        # delete the fragment file
        unlink(dir_listing$filename[i])
        }
      }
    }
  }
  unlink('tmp.txt')
}
