# Load the area of interest as the basis to retrive tiles
#
# This function can download files directly from a URL or
# retrieve one from an existing repository.  The resulting
# spatial features file is imported using the `sf` package
# You can learn more about package authoring with RStudio at:
#


get_mask <- function(url, dir, layer, ext, outname) {
  require(sf)
  file <- paste0(dir, "/", layer, ".", ext)

  if (!file.exists(file)) {
    dest <- paste0(dir, ".zip")
    download.file(url, destfile = dest)
    unzip(paste0(dir, ".zip"),
          exdir = dir)
    unlink(dest)
  }
  outname <- sf::st_read(dsn = dir, layer = layer)

  outname
}
