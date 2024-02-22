source(here::here("sketches.R"))

# data frame with one row per blob
set.seed(3L)
palette <- c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088")
n_blobs <- 200L
values <- tibble::tibble(
  x = rnorm(n_blobs, sd = 1.5),
  y = rnorm(n_blobs, sd = 1.5),
  n = 500L,
  fill = sample(palette, n_blobs, replace = TRUE),
  radius = runif(n_blobs),
  range = runif(n_blobs, min = 0, max = .5),
  color = fill
)

# list of blobs to draw
blobs <- purrr::pmap(values, blob)

# create a sketch from the blobs and then draw the sketch
blobs |> sketch() |> draw(xlim = c(-2, 2), ylim = c(-2, 2))

