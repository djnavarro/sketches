
library(S7)

# drawable classes --------------------------------------------------------

# drawable is the parent class for all shape classes, and is used as a container
# for the aesthetic properties of the object to be drawn; ironically, a plain
# drawable object won't show up as anything in a plot because it has no
# coordinates
drawable <- new_class(
  name = "drawable",
  properties = list(
    color     = new_property(class_character, default = "black"),
    linewidth = new_property(class_numeric, default = 1),
    fill      = new_property(class_character, default = NA_character_)
  )
)

# shapes are the simplest kind of drawables that do anything interesting when
# plotted: they are drawables that have x and y coordinates that define the
# vertices of a polygon when rendered
shape <- new_class(
  name = "shape",
  parent = drawable,
  package = "sketches",
  properties = list(
    x = class_numeric,
    y = class_numeric
  ),
  validator = function(self) {
    if (length(self@x) != length(self@y)) {
      "x and y must be the same length"
    }
  }
)

# circles inherit from drawables, and are defined by a centroid,
# a radius, and the number of points n used to draw the shape:
# the x and y coordinates associated with the circle are computed
# properties
circle <- new_class(
  name = "circle",
  parent = drawable,
  properties = list(

    # user-defined properties
    x_center = new_property(class_numeric, default = 0),
    y_center = new_property(class_numeric, default = 0),
    radius   = new_property(class_numeric, default = 1),
    n        = new_property(class_numeric, default = 100),

    # computed properties
    x = new_property(class_numeric, getter = function(self) {
      angle <- seq(0, 2 * pi, length.out = self@n)
      self@x_center + self@radius * cos(angle)
    }),
    y = new_property(class_numeric, getter = function(self) {
      angle <- seq(0, 2 * pi, length.out = self@n)
      self@y_center + self@radius * sin(angle)
    })
  )
)

# blobs are essentially "circles with a non-constant radius", where the
# irregularity in the radius is a smoothly varying distortion created with perlin
# noise
blob <- new_class(
  name = "blob",
  parent = drawable,
  properties = list(

    # user specified properties
    x_center   = new_property(class_numeric, default = 0),
    y_center   = new_property(class_numeric, default = 0),
    radius_min = new_property(class_numeric, default = 0.9),
    radius_max = new_property(class_numeric, default = 1.1),
    n          = new_property(class_numeric, default = 100),
    frequency  = new_property(class_numeric, default = 1),
    octaves    = new_property(class_integer, default = 2L),
    seed       = new_property(class_numeric, default = 1),

    # computed properties
    radius = new_property(class_numeric, getter = function(self) {
      angle <- seq(0, 2*pi, length.out = self@n)
      raw_radius <- ambient::fracture(
        noise = ambient::gen_simplex,
        fractal = ambient::fbm,
        x = self@x_center + cos(angle) * (self@radius_min + self@radius_max) / 2,
        y = self@y_center + sin(angle) * (self@radius_min + self@radius_max) / 2,
        frequency = self@frequency,
        seed = self@seed,
        octaves = self@octaves
      )
      ambient::normalize(
        raw_radius,
        from = c(-0.5, 0.5),
        to = c(self@radius_min, self@radius_max)
      )
    }),
    x = new_property(class_numeric, getter = function(self) {
      angle <- seq(0, 2 * pi, length.out = self@n)
      self@x_center + self@radius * cos(angle)
    }),
    y = new_property(class_numeric, getter = function(self) {
      angle <- seq(0, 2 * pi, length.out = self@n)
      self@y_center + self@radius * sin(angle)
    })
  )
)



# sketch class ------------------------------------------------------------

# a sketch is a list of drawables
sketch <- new_class(
  name = "sketch",
  properties = list(
    shapes = new_property(class = class_list, default = list())
  ),
  validator = function(self) {
    if (!all(purrr::map_lgl(self@shapes, \(d) inherits(d, "drawable")))) {
      "shapes must be a list of drawable-classed objects"
    }
  }
)

# ggplot2 style "addition" operator TODO: not happy with this tbh
`+.sketch` <- function(e1, e2) {
  e1@shapes <- c(e1@shapes, e2)
  e1
}



# draw generic and methods ------------------------------------------------


# s7 generic function used for plotting
draw <- new_generic("draw", dispatch_args = "object")

# draw method for any simple drawable
method(draw, drawable) <- function(object, xlim = NULL, ylim = NULL, ...) {

  # check is required because only the child classes have coordinates
  if (!prop_exists(object, "x") | !prop_exists(object, "y")) {
    rlang::warn("Drawables without x and y coordinates are ignored by draw()")
    return(invisible(NULL))
  }

  # plotting area is a single viewport with equal-axis scaling
  if (is.null(xlim)) xlim <- range(object@x)
  if (is.null(ylim)) ylim <- range(object@x)
  x_width <- xlim[2] - xlim[1]
  y_width <- ylim[2] - ylim[1]
  vp <- grid::viewport(
    xscale = xlim,
    yscale = ylim,
    width  = grid::unit(min(1, x_width / y_width), "snpc"),
    height = grid::unit(min(1, y_width / x_width), "snpc"),
  )

  # shapes are always polygon grobs
  grob <- grid::polygonGrob(
    x = object@x,
    y = object@y,
    gp = grid::gpar(
      col = object@color,
      fill = object@fill,
      lwd = object@linewidth
    ),
    vp = vp,
    default.units = "native"
  )

  # draw the grob
  grid::grid.newpage()
  grid::grid.draw(grob)
}

method(draw, sketch) <- function(object, xlim = NULL, ylim = NULL, ...) {

  # set default axis limits
  if (is.null(xlim)) {
    xlim <- c(
      min(purrr::map_dbl(object@shapes, \(s, id) min(s@x))),
      max(purrr::map_dbl(object@shapes, \(s, id) max(s@x)))
    )
  }
  if (is.null(ylim)) {
    ylim <- c(
      min(purrr::map_dbl(object@shapes, \(s) min(s@y))),
      max(purrr::map_dbl(object@shapes, \(s) max(s@y)))
    )
  }

  # plotting area is a single viewport with equal-axis scaling
  x_width <- xlim[2] - xlim[1]
  y_width <- ylim[2] - ylim[1]
  vp <- grid::viewport(
    xscale = xlim,
    yscale = ylim,
    width  = grid::unit(min(1, x_width / y_width), "snpc"),
    height = grid::unit(min(1, y_width / x_width), "snpc"),
  )

  # draw the grobs
  grid::grid.newpage()
  for(s in object@shapes) {
    grob <- grid::polygonGrob(
      x = s@x,
      y = s@y,
      gp = grid::gpar(
        col = s@color,
        fill = s@fill,
        lwd = s@linewidth
      ),
      vp = vp,
      default.units = "native"
    )
    grid::grid.draw(grob)
  }
}


# catchall method for non-drawables
method(draw, class_any) <- function(object, ...) {
  rlang::warn("Non-drawable objects ignored by draw()")
  return(invisible(NULL))
}


# convert methods ---------------------------------------------------------

# convert any drawable to a named list of properties
method(convert, list(drawable, class_list)) <- function(from, to) {
  labels <- prop_names(from)
  values <- lapply(labels, \(x) prop(from, x))
  names(values) <- labels
  values
}

# convert any drawable to a plain shape class
method(convert, list(drawable, shape)) <- function(from, to) {

  # extract coordinates
  x <- if (prop_exists(from, "x")) from@x else numeric(0L)
  y <- if (prop_exists(from, "y")) from@y else numeric(0L)

  # extract aesthetic properties
  d <- convert(from, drawable) # strips non-aesthetic properties
  l <- convert(d, class_list)

  do.call(shape, c(l, list(x = x, y = y)))
}

