workflowset_selectomatic = function(wflow, splitdata,
                                    filename = "workflowset",
                                    path = data_path("models")){
  
  #' Selects best hyperparams, transfers those to the final model and then 
  #' performs a final fit and testing suite for each wflow in the set.
  #' 
  #' Note that this wraps a number of important steps into one.  We provided this
  #' in the interest of speed, but the details are worthy of investogating.
  #' 
  #' @param wflow a workflow set with one or more workflows
  #' @param splitdata rsplit object with training and testing data
  #' @param filename str or NULL, a name for the file or NULL to not write
  #' @param path str or NULL, if not NULL then write each fitted workflow to file
  #' @return a tibble with one or more  "last_fit" model objects
  
  # fish out the best metrics per workflow
  m = metrics_table(wflow, what = "best")
  
  models = wflow$wflow_id |>
    sapply(
      function(wid){
        # get the workflow
        w = workflowsets::extract_workflow(wflow, wid)
        # transfer the "best" hyperparameter values to each workflow
        these_metrics = dplyr::filter(m, .data$wflow_id == wid)
        tune::finalize_workflow(w, these_metrics) |>
          # now fit with all of the training data (and testing data for measuring)
          tune::last_fit(splitdata, metrics = tidysdm::sdm_metric_set(yardstick::accuracy))
      }, simplify = FALSE) |> 
    dplyr::bind_rows() |>
    dplyr::mutate(wflow_id =  wflow$wflow_id, .before = 1)
  
  if (!is.null(path) && !is.null(filename)){
      models = write_model_fit(models, filename = filename, path = path)
  }

  return(models)
}


model_fit_metrics = function(x = read_model_fit(),
                             wids = dplyr::pull(x, dplyr::all_of("wflow_id"))){
  
  #' Given a table of model fits extract the metrics into a tidy table
  #' 
  #' @param x table of model fits
  #' @param ids str, the workflow(s) to operate upon, by default all of them
  #' @return a table of metrics

  x |>
    dplyr::filter(.data$wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        row$.metrics[[1]] |>
          dplyr::select(-dplyr::any_of(c(".estimator", ".config"))) |>
          tidyr::pivot_wider(names_from = .metric, values_from = .estimate) |>
          dplyr::mutate(wflow_id = row |> pull(1), .before = 1) 
      }) |>
    dplyr::bind_rows()
}

model_fit_accuracy = function(x = read_model_fit(),
                              wids = dplyr::pull(x, dplyr::all_of("wflow_id"))){
   x |>
    dplyr::filter(wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        r = row$.predictions[[1]] |>
          yardstick::accuracy(class, .pred_class) |>
          dplyr::mutate(wflow_id = row |> pull(1), .before = 1) 
      }) |>
    dplyr::bind_rows()
}

model_fit_roc_auc = function(x = read_model_fit(),
                             wids = dplyr::pull(x, dplyr::all_of("wflow_id"))){
  
  #' Given a table of model fits, plot the ROC curves with the AUC shown
  #' 
  #' @param x table of model fits
  #' @param wids str, the workflow(s) to operate upon, by default all of them
  #' @return faceted ggplot object
  
  pp = x |>
    dplyr::filter(wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
    function(row, key){
      r = row$.predictions[[1]] |>
        yardstick::roc_curve(class, .pred_presence) |>
        dplyr::mutate(wflow_id = row |> pull(1), .before = 1) 
    }) |>
    dplyr::bind_rows()
  
  auc = x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        r = row$.predictions[[1]] |>
          roc_auc(class, .pred_presence) |>
          dplyr::mutate(wflow_id = row |> pull(1), 
                        x = 0.5, 
                        y = 0.1,
                        label = sprintf("AUC: %0.3f", .data$.estimate),
                        .before = 1) 
      }) |>
    dplyr::bind_rows()
  
  ggplot(data = pp, 
         mapping = ggplot2::aes(x = 1 - specificity, y = sensitivity)) +
    geom_path() +
    geom_abline(lty = 3) +
    coord_equal() +
    theme_bw() + 
    labs(x = "False Positive Rate (Specificity)",
         y = "True Positive Rate (Sensitivity)") + 
    ggplot2::geom_text(data = auc,
                       mapping = ggplot2::aes(x = x, y = y, 
                                              label = label,
                                              hjust = "left", 
                                              vjust = "top")) +
    ggplot2::facet_wrap(~wflow_id)
  
}

model_fit_confmat = function(x = read_model_fit(),
                             wids = dplyr::pull(x, dplyr::all_of("wflow_id"))){
  
  #' Given a table of model fits, plot the confusion matrices
  #' 
  #' Borrows heavily from the [yardstick package](https://github.com/tidymodels/yardstick/blob/51761c4e7a34e960949c75aeb2952ef02c408106/R/conf_mat.R#L421)
  #' 
  #' @param x table of model fits
  #' @param wids str, the workflow(s) to operate upon, by default all of them 
  #' @return faceted ggplot object 
  
  acc = model_fit_accuracy(x, wids = wids)
  
  cm_as_tibble = function(cm){
    x = as.data.frame.table(cm$table) |>
      rlang::set_names(c("Prediction", "Truth", "Freq"))
    lvls = levels(x$Prediction)
    dplyr::as_tibble(x) |>
      dplyr::mutate(Prediction = factor(.data$Prediction, levels = rev(lvls)),
                    Truth = factor(.data$Truth, levels = lvls))
  }
  
  
  pp = x |>
    dplyr::filter(wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        wid = row |> dplyr::pull(1)
        row$.predictions[[1]] |>
          yardstick::conf_mat(class, .pred_class) |>
          cm_as_tibble() |>
          dplyr::mutate(wflow_id = wid,
                        .before = 1) |>
          dplyr::mutate(.accuracy = filter(acc, wflow_id == wid) |>
                          dplyr::pull(dplyr::all_of(".estimate")))
        
      }) |> 
    dplyr::bind_rows() |>
    dplyr::mutate(title = sprintf("%s (acc = %0.3f)", .data$wflow_id, .data$.accuracy))
  
  ggplot2::ggplot(data = pp,
                  mapping = ggplot2::aes(x = Truth,
                                         y = Prediction,
                                         fill = Freq ) ) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_gradient(low = "grey90",
                               high = "grey40") +
  ggplot2::theme(
    panel.background = ggplot2::element_blank(),
    legend.position = "none" ) +
  ggplot2::geom_text(
    mapping = ggplot2::aes(label = Freq) ) +

  ggplot2::facet_wrap(~title)
}



model_fit_pdp = function(x,
                         wid = dplyr::pull(x, dplyr::all_of("wflow_id"))[1],
                         ...){
  
  #' Create a partial dependence plot for a model fit
  #' 
  #' @param x table of model fits
  #' @param wid str the id of the workflow to plot (wflow_id)
  #' @param ... other arguments for the [partial_dependence_plot] function
  #' @return ggplot object
  
  thisx = x |>
    dplyr::filter(wflow_id == wid)
  thisdata =  thisx$splits[[1]] |>
    rsample::training() |>
    dplyr::select(-class) |>
    sf::st_drop_geometry()
  if (model_fit_spec(thisx) == "boost_tree"){
    thisdata = as.matrix(thisdata)
  }
  pd = partial_dependence(
         object = workflows::extract_fit_engine(thisx$.workflow[[1]]), 
         v = model_fit_varnames(thisx), 
         data = thisdata,
         which_pred = "presence",
         prob = TRUE) 
  plot(pd, ...)
}

model_fit_vi = function(x,
                        wids = dplyr::pull(x, dplyr::all_of("wflow_id")),
                        scale = TRUE,
                        ...){
  
  #' Compute variable importance
  #' 
  #' @param x table of model fits
  #' @param id str the id of the workflow to plot (wflow_id)
  #' @param scale logical, if TRUE scale values 0-1 by model fit
  #' @param ... other arguments for the [vip::vi] function
  #' @return a table if variable importance values
  
  
  x |>
    dplyr::filter(wflow_id %in% wids) |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        if (model_fit_spec(row) == "maxent"){
          r = NULL
        } else {
          r = row$.workflow[[1]] |> 
            workflows::extract_fit_parsnip() |>
            vip::vi(scale = scale, ...) |>
            dplyr::mutate(wflow_id = row$wflow_id, .before = 1)
        }
        r
      })  |>
    dplyr::bind_rows() |>
    tidyr::pivot_wider(names_from = "Variable",
                       values_from = "Importance")
}


model_fit_vip = function(x, ...){
  #' Make a heat map showing relative variable importance
  #' 
  #' If glm is part of the group then POS and NEG are 
  #' treated separately.
  #' 
  #' @param x table of model fits
  #' @param ... other arguments for `model_fit_vi`
  #' @return ggplot heat map
  
  v = model_fit_vi(x, ...)
  
  if ("Sign" %in% names(v)){
    v = v |>
      dplyr::mutate(wflow_id = if_else(is.na(.data$Sign),
                                       .data$wflow_id,
                                       paste(.data$wflow_id, .data$Sign))) |>
      dplyr::select(-Sign)
  }
  lvls = rev(v$wflow_id)
  vl = tidyr::pivot_longer(v,
                           -dplyr::any_of("wflow_id"),
                           names_to = "Covariate",
                           values_to = "Importance",
                           values_transform = function(x) x/100) |>
    dplyr::mutate(wflow_id = factor(wflow_id, levels = lvls))
  
  ggplot2::ggplot(data = vl,
                  mapping = ggplot2::aes(x = Covariate,
                                         y = wflow_id,
                                         fill = Importance)) +
    ggplot2::geom_tile() +
    ggplot2::labs(y = "Workflow ID", 
                  x = "Covariate")
  
  
  
}

model_fit_varnames = function(x,
                              wid = dplyr::pull(x, dplyr::all_of("wflow_id"))[1]){

  #' Retrieve the predictior variable names for a workflow
  #' @param x table of model fits
  #' @param wid str the id of the workflow to plot (wflow_id)
  #' @return string vector of predictor names
  
  thisx = x |>
    dplyr::filter(wflow_id == wid) 
  thisx$.workflow[[1]] |>
    workflowsets::extract_preprocessor() |>
      summary() |>
      dplyr::filter(role == "predictor") |>
      dplyr::pull(var = 1)
}

model_fit_spec <- function(x){
  
  #' Retrieve the model specifications for one or more model fits
  #' 
  #' @param x workflow set table
  #' @return str vector of first element of class attribute of each workflowset
  
  x |>
    dplyr::rowwise() |>
    dplyr::group_map(
      function(row, key){
        workflows::extract_spec_parsnip(row$.workflow[[1]]) |> 
          attr("class") |>
          getElement(1)
      }) |>
    unlist()
}


write_model_fit = function(x,
                        filename = "model_fits",
                        path = data_path("models"),
                        packed = FALSE){
  
  #' Write a workflowset fit object to file
  #' 
  #' @param x workflowset object
  #' @param filename str a name for the file
  #' @param path str or NULL, if not NULL then write each fitted workflow to file
  #' @param packed logical, if TRUE then butcher the models before saving
  #' @return the input workflowset

  if (packed){
    orig_x = x
    for (i in seq_len(nrow(x))) {
      x[i,]$.workflow[[1]] <- bundle::bundle(x[i,]$.workflow[[1]])
    }
    saveRDS(x, file.path(path, sprintf("%s.rds", filename)))
    x = orig_x
  } else {
    saveRDS(x, file.path(path, sprintf("%s.rds", filename)))
  }
  
  x
}

read_model_fit = function(filename = "model_fits",
                          path = data_path("models"),
                          packed = FALSE){
  
  #' Read a workflowset fit
  #' 
  #' @param filename str a name for the file
  #' @param path str or NULL, if not NULL then write each fitted workflow to file
  #' @param packed logical, if TRUE unbundle the workflows
  #' @return a named list with one or more "last_fit" model objects
  #' 
  x = readRDS(file.path(path, sprintf("%s.rds", filename)))
  if (packed){
    for (i in seq_len(nrow(x))) {
      x[i,]$.workflow[[1]] <- bundle::unbundle(x[i,]$.workflow[[1]])
    }
  }
  x
}
