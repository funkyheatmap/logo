library(funkyheatmap)
library(ggplot2)
library(tibble)

df <- data.frame(
  row.names = c("A", "B", "C", "D", "E"),
  a = c(.7, .3, NA, .2, 1),
  b = c(.8, 1, NA, .5, .2),
  c = c(.6, .8, NA, .65, .45),
  d = c(1, .8, NA, 1, .75),
  e = c(.2, .4, NA, .8, 1.),
  es = c(1, .8, NA, 1, 1),
  str = c("", "", "funkyheatmap", "", "")
)
column_info <- tribble(
  ~id, ~geom, ~palette, ~options,
  "a", "funkyrect", "a", list(),
  "b", "funkyrect", "b", list(),
  "c", "funkyrect", "c", list(),
  "str", "text", NA, list(width = 1, overlay = TRUE, size = 4),
  "d", "funkyrect", "d", list(),
  "e", "rect", "e", list(id_size = "es", id_color = "e")
)
palettes <- list(
  "a" = "Grays",
  "b" = "Greens",
  "c" = "Blues",
  "d" = "YlOrBr",
  "e" = "Reds"
)
legends <- list(
  list(palette = "a", enabled = FALSE),
  list(palette = "b", enabled = FALSE),
  list(palette = "c", enabled = FALSE),
  list(palette = "d", enabled = FALSE),
  list(palette = "e", enabled = FALSE)
)
g <- funky_heatmap(
  df,
  column_info = column_info,
  palettes = palettes,
  legends = legends,
  position_args = position_arguments(
    row_space = .05,
    col_space = .05,
    row_bigspace = .05,
  ),
  scale_column = FALSE,
)

# ggsave("src/attempt1/funkyheatmap.pdf", g, width = g$width, height = g$height)

# add hex border
generate_hexagon <- function(center_x, center_y, size, angle = 0) {
  angles <- seq(angle, angle + 2 * pi, length.out = 7)[-7] # 6 angles for hexagon
  x <- center_x + size * cos(angles)
  y <- center_y + size * sin(angles)
  data.frame(x, y)
}
df <- generate_hexagon(2.5 + 0.15, -(3 + .15), 2.75, 0)

h <- g +
  geom_polygon(aes(x = x, y = y), df, fill = NA, color = "black")

ggsave("src/attempt1/funkyheatmap.pdf", h, width = g$width, height = g$height)
ggsave("src/attempt1/funkyheatmap.svg", h, width = g$width, height = g$height)
