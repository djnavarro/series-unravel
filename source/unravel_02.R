
# set up ------------------------------------------------------------------

name    <- "unravel" 
version <- 2

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# import C++ functions
cpp_file <- paste0(name, "_", tidy_int_string(version, width = 2), ".cpp")
Rcpp::sourceCpp(here::here("source", cpp_file))


# art generator -----------------------------------------------------------

default_seeds <- function(version) {
  0:99 + version * 100
}

colourise_data <- function(df, pal, px) {
  df <- pal[df]
  df <- matrix(df, px, px, byrow = TRUE)
  return(df)
}

transform_data <- function(df, ncl) {
  df <- rank(abs(df))
  df <- df - min(df)
  df <- df / max(df)
  df <- as.integer(df * (ncl - 1)) + 1
  return(df)
}

write_image <- function(df, fpath, px, bg) {
  rs <- as.raster(df)
  jpeg(
    filename = fpath,
    width = px,
    height = px,
    bg = bg 
  )
  op <- par(mar = c(0,0,0,0))
  plot(rs)
  dev.off()
  par(op)
}

generate_data <- function(seed, iter, layers, px, zoom, alpha) {
  set.seed(seed)
  df <- art_data(iter, layers, px, zoom, alpha)
  return(df)
}

generate_palette <- function(seed) {
  set.seed(seed)
  p <- sample(colorir::colores$palette_name, 1L)
  pal <- colorir::colores$colour[colorir::colores$palette_name == p[1L]]
  pal <- sample(pal)
  pal <- (colorRampPalette(pal))(1024L)
  return(pal)
}

art_generator <- function(seed) {
  
  set.seed(seed)
  
  output <- output_path(name, version, seed, "jpg")
  message("generating art at ", output)
  
  layers <- 100
  iter   <- 1000 * 10^6
  px     <- 2000  
  zoom   <- 0.125
  alpha  <- 0.1

  shades <- generate_palette(seed)

  img <- generate_data(seed, iter, layers, px, zoom, alpha)
  img <- transform_data(img, ncl = length(shades))
  img <- colourise_data(img, pal = shades, px = px)
  
  write_image(img, output, px, shades[1])
  
}


# make art ----------------------------------------------------------------

seeds <- default_seeds(version)
for(s in seeds) art_generator(s)
