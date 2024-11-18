source(here::here("sketches.R"))

make_sketch <- function(seed) {

  # data frame with one row per twist
  set.seed(seed)
  palettes <- readr::read_csv("palette_02.csv", show_col_types = FALSE)
  row <- sample.int(nrow(palettes), 1)
  palette <- unlist(palettes[row, ])
  n_twists <- 400L
  values <- tibble::tibble(
    x = rnorm(n_twists, sd = 2),
    y = rnorm(n_twists, sd = 2),
    xend = x + 1,
    yend = y + rnorm(n_twists, sd = 1),
    width = runif(n_twists, min = .1, max = .3),
    smooth = 6L,
    n = 100L,
    fill = sample(palette, n_twists, replace = TRUE),
    color = fill
  )

  # list of blobs to draw
  twists <- purrr::pmap(values, twist)

  # create a sketch from the ribbons and then draw the sketch
  seed_str <- stringr::str_pad(seed, width = 4, pad = "0")
  png(
    filename = here::here(
      "image_04",
      paste0("sketch_04_", seed_str, ".png")
    ),
    width = 2000,
    height = 2000,
    units = "px",
    bg = palette[1]
  )
  twists |>
    sketch() |>
    draw(xlim = c(-2, 2), ylim = c(-2, 2))
  dev.off()

}

for(s in 1:50) {
  message("seed ", s)
  make_sketch(s)
}
