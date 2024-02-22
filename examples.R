source(here::here("sketches.R"))

style_data <- tibble::tibble(
  fill = c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088"),
  color = fill
)

blobs_data <- tibble::tibble(
  style = purrr::pmap(style_data, style),
  x = cos(seq(0, pi * 5/3, length.out = 6)),
  y = sin(seq(0, pi * 5/3, length.out = 6)),
  n = 500L
)

blobs <- purrr::pmap(blobs_data, blob)

s <- sketch(shapes = blobs)
draw(s)
