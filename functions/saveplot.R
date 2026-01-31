save_png <- function(name, plot = NULL, width = 8, height = 6, dpi = 150) {
  dir.create(make_path(VERSION), showWarnings = FALSE, recursive = TRUE)
  if (is.null(plot)) {
     plot = ggplot2::get_last_plot() 
  }
  ggplot2::ggsave(
    plot = plot,
    filename = file.path(make_path(VERSION), sprintf("%s.png", name)),
    width = width, height = height, dpi = dpi
  )
}
