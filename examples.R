source(here::here("sketches.R"))

# data frame with one row per blob
values <- tibble::tibble(
  x = cos(seq(0, pi * 5/3, length.out = 6)),
  y = sin(seq(0, pi * 5/3, length.out = 6)),
  n = 500L,
  fill = c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088"),
  color = fill
)

# list of blobs to draw
blobs <- purrr::pmap(values, blob)

# create a sketch from the blobs and then draw the sketch
blobs |> sketch() |> draw()

