
# set up ------------------------------------------------------------------

name    <- "fog" 
version <- 1

# define common helper functions
source(here::here("source", "common.R"), echo = FALSE)

# import C++ functions
grow_polygon <- NULL # hack to shut the lintr up
cpp_file <- "polygon.cpp"
Rcpp::sourceCpp(here::here("source", cpp_file))

# functions ---------------------------------------------------------------

default_seeds <- function(version) {
  0:99 + version * 100
}

grow_polygon_l <- function(polygon, iterations, noise, seed = NULL) {
  if(!is.null(seed)) set.seed(seed)
  polygon <- grow_polygon(polygon, iterations, noise) |>
    tibble::as_tibble() |>
    dplyr::arrange(position) |>
    dplyr::select(x, y, seg_len)
  return(polygon)
}

grow_multipolygon_l <- function(base_shape, n, seed = NULL, ...) {
  if(!is.null(seed)) set.seed(seed)
  polygons <- list()
  for(i in 1:n) {
    polygons[[i]] <- grow_polygon_l(base_shape, ...) |>
      dplyr::mutate(id = i)
  }
  polygons <- dplyr::bind_rows(polygons)
  polygons
}

show_multipolygon <- function(polygon, fill, alpha = .02, ...) {
  ggplot2::ggplot(polygon, ggplot2::aes(x, y, group = id)) +
    ggplot2::geom_polygon(colour = NA, alpha = alpha, fill = fill, ...) + 
    ggplot2::coord_equal() + 
    ggplot2::theme_void()
}

edge_length <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

smudged_hexagon <- function(seed, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)
  
  # define hexagonal base shape
  theta <- (0:6) * pi / 3
  hexagon <- tibble::tibble(
    x = sin(theta),
    y = cos(theta),
    seg_len = edge_length(x, y, dplyr::lead(x), dplyr::lead(y))
  )
  hexagon$seg_len[7] <- 0
  base <- hexagon |> 
    grow_polygon_l(
      iterations = 60, 
      noise = noise1
    )
  
  # define intermediate-base-shapes in clusters
  polygons <- list()
  ijk <- 0
  for(i in 1:3) {
    base_i <- base |> 
      grow_polygon_l(
        iterations = 100, 
        noise = noise2
      )
    
    for(j in 1:3) {
      base_j <- base_i |> 
        grow_polygon_l(
          iterations = 50, 
          noise = noise2
        )
      
      # grow n polygons per intermediate-base
      for(k in 1:20) {
        ijk <- ijk + 1
        polygons[[ijk]] <- base_j |>
          grow_polygon_l(
            iterations = 1200, 
            noise = noise3
          ) |>
          dplyr::mutate(id = ijk)
      }
    }
  }
  
  # return as data frame
  dplyr::bind_rows(polygons)
}

generate_palette <- function(seed) {
  set.seed(seed)
  here::here("source", "palettes") |>
    fs::dir_ls() |> 
    purrr::map(~ readr::read_csv(., show_col_types = FALSE)) |> 
    dplyr::bind_rows() |> 
    dplyr::slice_sample(n = 1L) |> 
    unlist() |> 
    sample()
}

art_generator <- function(seed) {
  
  set.seed(seed)
  output <- output_path(name, version, seed, "png")
  message("generating ", output)
  
  n_hex <- 3
  shades <- generate_palette(seed)
  dat <- list()
  
  for(i in 1:n_hex) {
    dat[[i]] <- smudged_hexagon(seed = seed) |>
      dplyr::mutate(
        fill = shades[i], 
        s = dplyr::if_else(i == 1, 2, stats::runif(n = 1, min = 1, max = 2)),
        x = x * s + dplyr::if_else(i == 1, 0, stats::rnorm(1, mean = 0, sd = 1)), 
        y = y * s + dplyr::if_else(i == 1, 0, stats::rnorm(1, mean = 0, sd = 1)),
        height = -i,
        id = paste0("hex", i, "_id", id)
      )
  }
  
  dat <- dplyr::bind_rows(dat) |>
    dplyr::arrange(height) |>
    dplyr::group_by(id) |> 
    dplyr::mutate(dilution = s / (max(x) - min(x)) / (max(y) - min(y))) |>
    dplyr::ungroup() |>
    dplyr::mutate(dilution = dilution / max(dilution))
  
  pic <- dat |> 
    ggplot2::ggplot(ggplot2::aes(x, y, group = id, fill = fill, alpha = .01 * dilution)) +
    ggplot2::geom_polygon(colour = NA, show.legend = FALSE) + 
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(xlim = c(-2.5, 2.5), ylim = c(-2.5, 2.5)) + 
    ggplot2::theme_void()
  
  ggplot2::ggsave(
    filename = output, 
    plot = pic,
    width = 4000,
    height = 4000,
    units = "px",
    dpi = 300,
    bg = shades[length(shades)]
  )  

}


# make art ----------------------------------------------------------------

#seeds <- default_seeds(version) + 200
seeds <- 400
for(s in seeds) art_generator(s)

