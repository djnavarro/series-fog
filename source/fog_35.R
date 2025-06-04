
# set up ------------------------------------------------------------------

name    <- "fog" 
version <- 35

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

smudged_polygon <- function(seed, shape, noise1 = 0, noise2 = 2, noise3 = 0.5) {
  set.seed(seed)

  base <- shape |> 
    grow_polygon_l(
      iterations = 120, 
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
      x = base[1:3], 
      size = n - 1, 
      replace = TRUE, 
      prob = c(4, 1, 1)
    ), 
    base[5]
  )
}

generate_lgbtiq_palette <- function(seed, n) {

  # https://www.flagcolorcodes.com/flags/pride
  queers <- list(
    rainbow   = c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088"),
    trans     = c("#5BCEFA", "#F5A9B8", "#FFFFFF"),
    bisexual  = c("#D60270", "#9B4F96", "#0038A8"),
    gay_man   = c("#078D70", "#26CEAA", "#98E8C1", "#FFFFFF", "#7BADE2", "#5049CC", "#3D1A78"),
    pansexual = c("#FF218C", "#FFD800", "#21B1FF"),
    lesbian   = c("#D52D00", "#EF7627", "#FF9A56", "#FFFFFF", "#D162A4", "#B55690", "#A30262"), 
    aromantic = c("#3DA542", "#A7D379", "#FFFFFF", "#A9A9A9", "#000000"),
    asexual   = c("#000000", "#A3A3A3", "#FFFFFF", "#800080"),
    nonbinary = c("#FCF434", "#FFFFFF", "#9C59D1", "#2C2C2C"),
    bear      = c("#613704", "#D46300", "#FDDC62", "#FDE5B7", "#FFFFFF", "#545454", "#000000"),
    intersex  = c("#FFD800", "#7902AA")
  )

  shades <- unlist(sample(queers, 1))
  origin <- gsub("[0-9]$", "", names(shades[1]))
  message(origin)
  ns <- length(shades)
  blacks <- sample(c("#000000", "#222222"), ns, TRUE)
  shades <- dplyr::if_else(runif(ns) < .7, blacks, shades)
  message(names(shades[1]))
  sample(shades, n, TRUE)
}

art_generator <- function(seed) {
  
  set.seed(seed)
  output <- output_path(name, version, seed, "png")
  message("generating ", output)
  
  n_row <- 5
  n_col <- 5
  n <- n_row * n_col
  dat <- list()
  
  sides <- 4
  theta <- (0:sides) * pi * 2 / sides
  theta <- theta - pi / sides # choose rotation (pi/sides is "squares not diamonds")

  shape <- tibble::tibble(
    x = sin(theta) / cos(pi / sides) * 1.5, # divisor ensures width spans (-1, 1) for the pi/sides rotation
    y = cos(theta) / cos(pi / sides),
    seg_len = edge_length(x, y, dplyr::lead(x), dplyr::lead(y))
  )
  shape$seg_len[sides + 1] <- 0

  scale <- 1.5

  hex_shade <- generate_lgbtiq_palette(seed, n = n + 1)
  hex_seed <- sample(1:10000, n)
  #hex_size <- runif(n, min = 1.5, max = 1.5)
  hex_noise <- runif(n, min = 1, max = 2)
  bg <- "#000000"

  i <- 0
  for(r in 1:n_row) {
    for(c in 1:n_col) {
      if (r %in% c(1, n_row) & c %in% c(1, n_col)) {
        is_corner <- 1
      } else {
        is_corner <- 0
      }
      if (runif(1) < .90 | is_corner) { 
        i <- i + 1
        dat[[i]] <- smudged_polygon(
          seed = hex_seed[i], 
          shape = shape,
          noise1 = 0, 
          noise2 = hex_noise[i], 
          noise3 = .5
        ) |>
        dplyr::mutate(
          fill = hex_shade[i], 
          x = x * scale + (c - 1) * 2 - (n_col - 1),
          y = y * scale + (r - 1) - (n_row - 1)/2,
          id = paste0("id", id, "hex", i, sep = "_")
        )
      }
    }
  }
  
  dat <- dplyr::bind_rows(dat) |>
    dplyr::group_by(id) |> 
    dplyr::mutate(dilution = 1 / (max(x) - min(x)) / (max(y) - min(y))) |>
    dplyr::ungroup() |>
    dplyr::mutate(dilution = dilution / max(dilution))
  
  if (runif(1) < .5) dat$x <- -dat$x
  if (runif(1) < .5) dat$y <- -dat$y

  #print(dplyr::distinct(dat, id, fill, s, dilution))
  
  y_edge <- (n_row + 2) / 2
  x_edge <- (n_col + 1) 

  pic <- dat |> 
    ggplot2::ggplot(ggplot2::aes(
      x, y, 
      group = id, 
      fill = fill, 
      alpha = .03 * dilution)
    ) +
    ggplot2::geom_polygon(colour = NA, show.legend = FALSE) + 
    ggplot2::scale_fill_identity() +
    ggplot2::scale_alpha_identity() +
    ggplot2::coord_equal(
      xlim = c(-x_edge, x_edge), 
      ylim = c(-y_edge, y_edge)
    ) + 
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::theme_void()
  
  pixels_wide <- 3000
  pixels_high <- pixels_wide * (y_edge / x_edge)

  ggplot2::ggsave(
    filename = output, 
    plot = pic,
    width = pixels_wide,
    height = pixels_high,
    units = "px",
    dpi = 300,
    bg = bg
  )  

}


# make art ----------------------------------------------------------------

seeds <- default_seeds(version)
for(s in seeds) art_generator(s)

