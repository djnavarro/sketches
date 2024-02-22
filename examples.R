source(here::here("sketches.R"))

dat <- tibble::tibble(
  fill = c("#e50000", "#ff8d00", "#ffee00", "#028121", "#004cff", "#770088"),
  color = fill,
  x_center = cos(seq(0, pi * 5/3, length.out = 6)),
  y_center = sin(seq(0, pi * 5/3, length.out = 6)),
  n = 500
)

sketch(shapes = purrr::pmap(dat, blob)) |> draw()
