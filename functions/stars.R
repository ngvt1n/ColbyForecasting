
bind_attrs <- function(x, ...) {
  
  #' bind_attrs generic
  #'
  #' @param x An object
  #' @param ... Arguments passed to or from other methods
  #' @return a `stars` object
  
  UseMethod("bind_attrs", x)
}


bind_attrs.default <- function(x, ...){
  
  #' Bind one or more `stars` objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x a stars object
  #' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
  #'   silently removed first. Ignored if `x` is a list.
  #' @return `stars` objects
  
  stop("first argument must be either list or a `stars` object")
}


bind_attrs.stars <- function(x, ...){
  
  #' Bind one or more `stars` objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x a stars object
  #' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
  #'   silently removed first.
  #' @return `stars` objects
  
  x = list(x, ...)
  x = x[!sapply(x, is.null)]
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = NA_integer_)))
}


bind_attrs.list <- function(x, ...){
  
  #' Bind a list of \code{stars} objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x list of `stars` objects
  #' @param ... ignored
  #' @return `stars` objects
  
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = NA_integer_)))
}

#########
bind_bands <- function(x, ...) {
  
  #' bind_bands generic
  #'
  #' @param x An object
  #' @param ... Arguments passed to or from other methods
  #' @return a `stars` object
  
  UseMethod("bind_bands", x)
}


bind_bands.default <- function(x, ...){
  
  #' Bind one or more `stars` objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x a stars object
  #' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
  #'   silently removed first. Ignored if `x` is a list.
  #' @param along named list indicating the name of the time dimension and its values
  #' @return `stars` objects
  
  stop("first argument must be either list or a `stars` object")
}


bind_bands.stars <- function(x, ..., along = list(month = month.abb)){
  
  #' Bind one or more `stars` objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x a stars object
  #' @param ... one or more `stars` objects to bind to `x` as additional attributes.  Any NULL elements are
  #'   silently removed first.
  #' @param along named list indicating the name of the time dimension and its values
  #' @return `stars` objects
  
  x = list(x, ...)
  x = x[!sapply(x, is.null)]
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = along)))
}


bind_bands.list <- function(x, ..., along = list(month = month.abb)){
  
  #' Bind a list of \code{stars} objects by attribute.
  #'
  #' @seealso [stars issue 440](https://github.com/r-spatial/stars/issues/440#issuecomment-877627732)
  #'
  #' @param x list of `stars` objects
  #' @param ... ignored
  #' @param along named list indicating the name of the time dimension and its values
  #' @return `stars` objects
  
  if (length(x) == 0) stop("input has zero length")
  do.call(c, append(x, list(along = along)))
}


plot_stars_months = function(x,
                             colors = c("magma",
                                        "inferno",
                                        "plasma",
                                        "viridis",
                                        "cividis",
                                        "rocket",
                                        "mako",
                                        "turbo")[1],
                             coast = read_coastline(),
                             coast_color = "white"){
  
  #' Plot a single stars attribute (variable) over many months
  #' 
  #' @param x stars object with one or more months along the 'month' dimension
  #' @param colors str the name of the color table to use.  See 
  #'  deatils here https://ggplot2-book.org/scales-colour.html#sec-colour-continuous
  #' @param coast sf coast line or NULL to skip
  #' @return a ggplot2 object
  gg = ggplot2::ggplot() +
    stars::geom_stars(data = x[1]) + 
    ggplot2::scale_fill_viridis_c(option = colors[1], limits = c(0,1)) + 
    ggplot2::coord_sf(crs = sf::st_crs(x)) + 
    ggplot2::facet_wrap(~month)
  if (!is.null(coast)) {
    gg = gg + 
      ggplot2::geom_sf(data = sf::st_geometry(coast), color = coast_color)
  }
  gg
}


