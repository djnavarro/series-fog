
# set up ------------------------------------------------------------------

name    <- "fog" 
version <- 26

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

smudged_polygon <- function(seed, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)
  
  # define base shape
  sides <- 4
  theta <- (0:sides) * pi * 2 / sides - pi / sides

  base <- tibble::tibble(
    x = sin(theta) / cos(pi / sides), # divisor ensures width spans (-1, 1)
    y = cos(theta) / cos(pi / sides),
    seg_len = edge_length(x, y, dplyr::lead(x), dplyr::lead(y))
  )
  base$seg_len[sides + 1] <- 0

  if (sides < 100) {
    base <- base |> 
      grow_polygon_l(
        iterations = 100 - sides, 
        noise = noise1
      )
  }
   
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
          iterations = 20, 
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

expand_palette <- function(shades, to = 1024L) {
  (colorRampPalette(shades))(to)
}

append_palette <- function(shades, add, n = 1L) {
  c(shades, rep(add, n))
}

thicken_palette <- function(shades, n = 5L) {
  as.vector(t(replicate(n, shades)))
}

generate_palette <- function(seed, n) {
  set.seed(seed)
  base <- here::here("source", "palettes") |>
    fs::dir_ls() |> 
    purrr::map(~ readr::read_csv(., show_col_types = FALSE)) |> 
    dplyr::bind_rows() |> 
    dplyr::slice_sample(n = 1L) |> 
    unlist() |> 
    sample()
  c(
    sample(
      x = base[1:4], 
      size = n - 1, 
      replace = TRUE, 
      prob = c(1, 1, 1, 1)
    ), 
    base[5]
  )
}

art_generator <- function(seed) {
  
  set.seed(seed)
  output <- output_path(name, version, seed, "png")
  message("generating ", output)
  
  n_row <- 5
  n_col <- 5
  n <- n_row * n_col
  dat <- list()
  
  hex_shade <- generate_palette(seed, n = n + 1)
  hex_seed <- sample(1:10000, n)
  hex_size <- runif(n, min = .45, max = .45)
  hex_noise <- runif(n, min = 1, max = 3)
  bg <- hex_shade[n + 1]

  i <- 0
  for(r in 1:n_row) {
    cat("/")
    for(c in 1:n_col) {
      cat(".")
      if (runif(1) < 1) { 
        i <- i + 1
        dat[[i]] <- smudged_polygon(
          seed = hex_seed[i], 
          noise1 = 0, 
          noise2 = hex_noise[i], 
          noise3 = .5
        ) |>
        dplyr::mutate(
          fill = hex_shade[i], 
          s = hex_size[i],
          x = x * s + (c - 1) - (n_col - 1)/2,
          y = y * s + (r - 1) - (n_row - 1)/2,
          id = paste0("id", id, "hex", i, sep = "_")
        )
      }
    }
  }
  cat("\n")

  dat <- dplyr::bind_rows(dat) |>
    dplyr::group_by(id) |> 
    dplyr::mutate(dilution = 1 / (max(x) - min(x)) / (max(y) - min(y))) |>
    dplyr::ungroup() |>
    dplyr::mutate(dilution = dilution / max(dilution))
  
  if (runif(1) < .5) dat$x <- -dat$x
  if (runif(1) < .5) dat$y <- -dat$y

  #print(dplyr::distinct(dat, id, fill, s, dilution))
  
  edge <- (n_row + 1) / 2

  pic <- dat |> 
    ggplot2::ggplot(ggplot2::aes(
      x, y, 
      group = id, 
      fill = fill, 
      alpha = .02 * dilution)
    ) +
    ggplot2::geom_polygon(colour = NA, show.legend = FALSE) + 
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(
      xlim = c(-edge, edge), 
      ylim = c(-edge, edge)
    ) + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void()
  
  ggplot2::ggsave(
    filename = output, 
    plot = pic,
    width = 4000,
    height = 4000,
    units = "px",
    dpi = 300,
    bg = bg
  )  

}


# make art ----------------------------------------------------------------

seeds <- default_seeds(version)
for(s in seeds) art_generator(s)

