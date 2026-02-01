
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


get_response_type = function(name){
  
  #' A little function to accept a model name and return the
  #' appropriate value for the `type` argument to predict
  #'
  #' @param name str, the model specification name
  #' @return str, the appropriate value for response type
  switch(name,
         "logistic_reg" = "response",
         "rand_forest" = "response",
         "boost_tree" = "response",
         "maxnet" = "cloglog",
         "maxent" = "cloglog",
         stop("name not known: ", name))
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
                                 type = get_response_type(mtype))
                       p = thismonth[1]
                       p[[1]][] <- pred
                     } else if (mtype == "rand_forest") {
                       p = predict(thismonth, model, type = get_response_type(mtype))[1]
                     } else if (mtype == "logistic_reg"){
                       p = predict(thismonth, model, type = get_response_type(mtype))
                     } else if (mtype %in% c("maxent", "maxnet") ){
                       p = predict(thismonth, model, type = get_response_type(mtype))
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

threshold_prediction = function(x, threshold = 0.5){
  
  #' Threshold a prediction map
  #' 
  #' @param x stars, a stars prediction map (habitat suitability index)
  #' @param threshold numeric, values in x at or above this value are considered
  #'   to be predicted presences
  #' @return a stars object with habitat suitability index converted to a binary class
  
  x |>
    dplyr::mutate(dplyr::across(dplyr::everything(),
                               ~factor(.x >= threshold[1],
                                       levels = c(FALSE, TRUE),
                                       labels = c("abscence", "presence"),
                                       exclude = NULL)))
}

plot_prediction = function(x,
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
  #'  details here https://ggplot2-book.org/scales-colour.html#sec-colour-continuous.
  #'  This is ignored is the data type is not numeric.
  #' @param coast sf coast line or NULL to skip
  #' @return a ggplot2 object
  
  if (inherits(x[[1]][1], "numeric")){
    cat("numeric\n")
    gg = ggplot2::ggplot() +
      stars::geom_stars(data = x[1]) + 
      ggplot2::scale_fill_viridis_c(option = colors[1], 
                                    limits = c(0,1), 
                                    na.value = "grey50") + 
      ggplot2::facet_wrap(~month)
  } else {
    gg = ggplot2::ggplot() +
      stars::geom_stars(data = x[1]) + 
      ggplot2::scale_fill_viridis_d(
                                    na.value = "grey50", 
                                    na.translate = FALSE,
      ) +
      ggplot2::facet_wrap(~month)
  }
  if (!is.null(coast)) {
    gg = gg + 
      ggplot2::geom_sf(data = sf::st_geometry(coast), color = coast_color)
  } else {
    gg = gg + 
      ggplot2::coord_sf(crs = sf::st_crs(x))
  }
  gg
}

plot_overall_prediction = function(
  #' Compose multiple prediction plots together, requires the patchwork package
  #' 
  #' @param title Overall title
  #' @param model Model to use
  #' @return a patchwork object
                                   title,
                                   model,
                                   nc = nowcast, 
                                   fc1 = forecast_4.5_2055,
                                   fc2 = forecast_4.5_2075, 
                                   fc3 = forecast_8.5_2055, 
                                   fc4 = forecast_8.5_2075
) {
  p0 = plot_prediction(nc[model]) + ggtitle("Nowcast")
  p5 = model_fit_pdp(model_fits, wid = model, title = "Partial Dependence Curve")
  p1 = plot_prediction(fc1[model]) + ggtitle("RCP4.5 2055 Forecast")
  p2 = plot_prediction(fc2[model]) + ggtitle("RCP4.5 2075 Forecast")
  p3 = plot_prediction(fc3[model]) + ggtitle("RCP8.5 2055 Forecast")
  p4 = plot_prediction(fc4[model]) + ggtitle("RCP8.5 2075 Forecast")
  overall = p0 + p5 + p1 + p2 + p3 + p4 + 
    patchwork::plot_annotation(
      title = title
    ) + 
    patchwork::plot_layout(
      guides = "collect",
      axis_titles = "collect",
      axes = "collect",
      design = "
      12
      34
      56
      "
    )
  save_png(title, overall, width = 16, height = 18)
}

write_prediction = function(x, 
                            scientificname = "Mola mola",
                            version = "v1",
                            year = "CURRENT",
                            scenario = "CURRENT",
                            path = data_path("predictions")){
  
  #' Save a prediction to file
  #' 
  #' @param x stars object with one or more time layers
  #' @param scientificname chr, the name of the species
  #' @param version chr, the version identifier
  #' @param year chr, one of CURRENT, 2055 or 2075
  #' @param scenario chr, one of CURRENT, RCP45 or RCP85
  #' @param path chr the data path
  #' @return the input object x invisibly (also suitable for piping)
  
  stopifnot(inherits(x, "stars"))
  path = make_path(path)
  
  filename = sprintf("%s-%s-%s-%s.rds",
                     gsub(" ", "_", scientificname, fixed = TRUE),
                     version[1],
                     year[1],
                     scenario[1])
  saveRDS(x, file.path(path, filename))
  invisible(x)
}


read_prediction = function(x,
                           scientificname = "Mola mola",
                           version = "v1",
                           year = "CURRENT",
                           scenario = "CURRENT",
                           path = data_path("predictions")){
  #' Read a prediction from file
  #' 
  #' @param x stars object with one or more time layers
  #' @param scientificname chr, the name of the species
  #' @param version chr, the version identifier
  #' @param year chr, one of CURRENT, 2055 or 2075
  #' @param scenario chr, one of CURRENT, RCP45 or RCP85
  #' @param path chr the data path
  #' @return the input object x (suitable for piping)
  
  filename = file.path(path,
                       sprintf("%s-%s-%s-%s.rds",
                               gsub(" ", "_", scientificname, fixed = TRUE),
                               version[1],
                               year[1],
                               scenario[1]))
  if (!file.exists(filename)) stop("file not found: ", filename)
  readRDS(filename)
}
