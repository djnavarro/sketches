source(here::here("sketches.R"))

# data frame with one row per blob
set.seed(3L)
palette <- c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088")
n_ribbons <- 200L
values <- tibble::tibble(
  x = rnorm(n_ribbons, sd = 1.5),
  y = rnorm(n_ribbons, sd = 1.5),
  xend = x + 1,
  yend = y,
  width = 1,
  n = 500L,
  fill = sample(palette, n_ribbons, replace = TRUE),
  color = fill
)

# list of blobs to draw
ribbons <- purrr::pmap(values, ribbon)

# create a sketch from the ribbons and then draw the sketch
ribbons |> sketch() |> draw(xlim = c(-2, 2), ylim = c(-2, 2))

