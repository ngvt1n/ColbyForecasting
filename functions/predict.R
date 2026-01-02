
predict_table = function(wflow, newdata, type = "prob", threshold = 0.5, ...){
  
  #' Predict a classification from a table of new data
  #' 
  #' @param wflow a workflow object
  #' @param newdata table of new data including a `class` variable
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a table of predictions with the orginal class label
  
  lvls = levels(newdata$class)
  predict(wflow, newdata, type = type, ...) |>
    dplyr::mutate(.pred = if_else(.pred_presence >= threshold, lvls[1], lvls[2]) |>
                    factor(levels = lvls),
                  class = newdata$class)
}


predict_stars = function(x, newdata, 
                         wids = dplyr::pull(x, dplyr::all_of("wflow_id")),
                         type = "prob", 
                         threshold = 0.5, 
                         ...){
  
  #' Predict a classification from stars object
  #' 
  #' @param x a workflow set table
  #' @param newdata stars data
  #' @param wids str one or more workflow ids
  #' @param typ chr the type of prediction, by default "prob" - see `?predict.model_fit`
  #' @param threshold num the aribitray threshold used to define the outcome
  #' @param ... other arguments for `predict.model_fit`
  #' @return a stars object
  
  # a little function to accept a model name and return the
  # appropriate value for the `type` argument to predict
  get_type_var = function(name){
    switch(name,
           "logistic_reg" = "response",
           "rand_forest" = "response",
           "boost_tree" = "response",
           "maxnet" = "cloglog",
           "maxent" = "cloglog",
           stop("name not known: ", name))
  } 
  
  lvls = c("presence", "background")
  d = names(stars::st_dimensions(newdata))
  months = stars::st_get_dimension_values(newdata, 3)
  along = list(months) |>
    rlang::set_names(d[3])
  ss = x |>
    dplyr::filter(.data$wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        
        mtype = model_fit_spec(row)
        #cat("mtype", mtype, "\n")
        model = row$.workflow[[1]] |>
          workflows::extract_fit_engine()
        
        s = lapply(seq_along(months),
                   function(imonth){
                     thismonth = dplyr::slice(newdata, "month", imonth)
                     
                     if (mtype == "boost_tree")  {
                       pred = predict(model, 
                                 thismonth |> as.data.frame(add_coordinates = FALSE),
                                 type = get_type_var(mtype))
                       p = thismonth[1]
                       p[[1]][] <- pred
                     } else if (mtype == "rand_forest") {
                       p = predict(thismonth, model, type = get_type_var(mtype))[1]
                     } else if (mtype == "logistic_reg"){
                       p = predict(thismonth, model, type = get_type_var(mtype))
                     } else if (mtype %in% c("maxent", "maxnet") ){
                       p = predict(thismonth, model, type = get_type_var(mtype))
                     } else {
                       stop("model type not known: ", mtype)
                     }
                      p |>
                       rlang::set_names(row$wflow_id)
                   }) |>
          bind_bands(along = along)
      }
    ) |>
    bind_attrs()
}


write_prediction = function(x, filename = "prediction.tif",
                            attr = ".pred_presence"){
  #' Save a prediction (or a time series of predictions)
  #' 
  #' @param x stars object with one or more time layers
  #' @param filename chr, the name of the file to write
  #' @param attr chr or num, either the positional index or attribute name to write.
  #' @return the input object x (suitable for piping)
  
  stopifnot(inherits(x, "stars"))
  
  stars::write_stars(x[attr], filename)
  
}


read_prediction = function(x, filename = "prediction.tif",
                            attr = ".pred_presence"){
  #' Read a prediction (or a time series of predictions)
  #' 
  #' @param filename chr, the name of the file to write
  #' @param attr chr or num, either the positional index or attribute name to write.
  #' @return the input object x (suitable for piping)
  
  stopifnot(inherits(x, "stars"))
  
  stars::write_stars(x[attr], filename)
  
}

read_prediction = function(filename, attr = '.pred_presence', time = NULL){
  #' Read a prediction (or a time series of predictions)
  #' 
  #' @param file chr, the name of the file to write
  #' @param attr chr the name to apply to the attribute
  #' @param time any type, an optional "time" to apply the the third dimension for a 
  #'   mutil-layer array
  #' @return stars object
  
  x = stars::read_stars(filename) |>
    setNames(attr)
  if (length(dim(x) > 2) %% !is.null(time)) {
    x = stars::st_set_dimensions(x, 3, values = time, names = "time")
  }
  return(x)
}