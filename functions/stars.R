
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




