## -------------------------------------------------------------------
source("./setup.R")
set.seed(1235)
VERSION = "v6B"

## ----load_prerequisites-----------------------------------------------
coast = read_coastline()
present = brickman_database() |>
  dplyr::filter(scenario == "PRESENT", interval == "mon") |> read_brickman()
mask = brickman_database() |>
  filter(scenario == "STATIC", var == "mask") |> read_brickman()
keep = filter_collinear(present, method = "cor_caret", cutoff = 0.65)
keep = c("depth", "month", keep)

## ----write_cfg------------------------------------------------------
cfg = list(
  version = VERSION,
  scientificname = SPECIES,
  method_background = "bias",
  number_background = "average of observations per month",
  thinning = "false",
  metrics = sdm_metric_set(),
  keep_vars =  keep)


write_configuration(cfg)          

## ----load obs-------------------------------------------------------
obs = read_Tlongicornis(filter_individualCount = FALSE)

## ----plot_dataset---------------------------------------
ggplot() +
  geom_sf(data = obs, 
          mapping = aes(col=paste(str_wrap(dataset_title, width = 20), "\n")),
          alpha =  0.4) +
  geom_sf(data = coast, col = "orange")  + 
  labs(x = "Longitude", y = "Latitude", title = "All", color = "Dataset of origin") +   
  theme_bw() +  # <- make a simple white background
  ggtitle("Records collected and dataset of origin")
save_png("1_datasets")


## ----thin_observations----------------------------------------------
thinned_obs = sapply(month.abb,
                     function(mon){ 
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       # thin_by_dist(temp_x, 20000)
                       temp_x
                     }, simplify = FALSE) |>
  dplyr::bind_rows() 


## ----bias_map-------------------------------------------------------
bias_map = rasterize_point_density(obs, mask) # <-- note the original observations

ggplot() +
  geom_stars(data = bias_map, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent") +
  geom_sf(data = coast, col = "orange") + 
  labs(x = "Longitude", y = "Latitude", title = "Bias map using all observations")
save_png("1_bias_map")


## ----all_observations_count-----------------------------------------
all_counts = count(st_drop_geometry(thinned_obs), month) # counting is faster without spatial baggage


## ----n_background_per_month-----------------------------------------
nback_avg = mean(all_counts$n) |>
  round()
nback_avg


## ----sample_background----------------------------------------------
obsbkg <- sapply(month.abb,
                 function(mon) {
                   sample_background(
                     thinned_obs |> filter(month == mon),
                     bias_map,
                     method = "bias",  # <-- it needs to know it's a bias map
                     return_pres = TRUE, # <-- give me the obs back, too
                     n = nback_avg) |>   # <-- how many points
                     mutate(month = mon, .before = 1)
                 }, simplify = FALSE) |>
  bind_rows() |>
  mutate(month = factor(month, levels = month.abb))


## ----plot_presence_background---------------------------------------
ggplot() +
  geom_sf(data = obsbkg, 
          mapping = aes(col=class),
          alpha =  0.2) +
  geom_sf(data = coast, col = "orange")  + 
  labs(x = "Longitude", y = "Latitude", title = "All") +   
  theme_bw() +  # <- make a simple white background
  scale_fill_okabe_ito()  +  # <-- colorblind friendly for N Record
  ggtitle("Spatial distribution of presence and background points")
save_png("1_presence_background")


## ----present_model_input--------------------------------------------
model_input = obsbkg


## -------------------------------------------------------------------
present = read_brickman(add = c("depth"))


## ----extract--------------------------------------------------------
variables = extract_brickman(present, model_input, form = "wide")
variables


## ----wrangle_variables----------------------------------------------
variables = variables |>
  mutate(class = model_input$class) |>    # the $ extracts a column 
  select(-.id)                            # the minus means "deselect" or "drop"
variables


## ----write_model_input----------------------------------------------
write_model_input(variables, scientificname = SPECIES, version = VERSION)

## ----load_data------------------------------------------------------
model_input = read_model_input(scientificname = SPECIES, 
                               version = VERSION,
                               log_me = c("depth", "Xbtm")) |>
  dplyr::mutate(month = month_as_number(.data$month)) |>
  select(all_of(c("class", cfg$keep_vars)))


## ----plot_pres_vs_bg, warning = FALSE-------------------------------
plot_pres_vs_bg(model_input, "class")
save_png("1_presence_vs_background")


## ----initial_split--------------------------------------------------
model_input_split = spatial_initial_split(model_input, 
                                          prop = 1 / 5,     # 20% for testing
                                          strategy = spatial_block_cv) # see ?spatial_block_cv
model_input_split


## ----initial_split_plot---------------------------------------------
# autoplot(model_input_split)


## ----cv_training----------------------------------------------------
tr_data = training(model_input_split)
cv_tr_data <- spatial_block_cv(tr_data,
                               v = 5,     
                               repeats = 5,
                               cellsize = grid_cellsize(model_input),
                               offset = grid_offset(model_input) + 0.00001
)
autoplot(cv_tr_data) + 
  ggtitle("Training and testing data cross validation splits across repeats")
save_png("2_cv_training_splits")


## ----recipe---------------------------------------------------------
one_row_of_training_data = dplyr::slice(tr_data,1)
rec = recipe(one_row_of_training_data, formula = class ~ .)


## ----recipe_summary-------------------------------------------------
summary(rec)


## ----make_workflow--------------------------------------------------
wflow = workflow_set(
  
  preproc = list(default = rec), # not much happening in our preprocessor
  
  models = list(                 # but we have 4 models to add
    
    # very simple - nothing to tune
    glm = logistic_reg(
      mode = "classification") |>
      set_engine("glm"),
    
    # two knobs to tune
    rf = rand_forest(
      mtry = tune(),
      trees = tune(),
      mode = "classification") |>
      set_engine("ranger", 
                 importance = "impurity"),
    
    # so many things to tune!
    btree = boost_tree(
      mtry = tune(), 
      trees = tune(), 
      tree_depth = tune(), 
      learn_rate = tune(), 
      loss_reduction = tune(), 
      stop_iter = tune(),
      mode = "classification") |>
      set_engine("xgboost"),
    
    # just two again
    maxent = maxent(
      feature_classes = tune(),
      regularization_multiplier = tune(),
      mode = "classification") |>
      set_engine("maxnet")
  )
)


## ----metrics--------------------------------------------------------
metrics = cfg$metrics
metrics


## ----fit, warning = FALSE-------------------------------------------
wflow <- wflow |>
  workflow_map("tune_grid",
               resamples = cv_tr_data, 
               grid = 3,
               metrics = metrics, 
               verbose = TRUE)

## ----plot_wflow-------------------------------------------
autoplot(wflow) + 
  ggtitle("Performance of models across parameter combinations")
save_png("2_workflow_performance")

## ----select_best----------------------------------------------------
model_fits = workflowset_selectomatic(wflow, model_input_split,
                                      filename = sprintf("%s-%s-model_fits", SPECIES, VERSION),
                                      path = data_path("models"))

## ----model_fit_metrics----------------------------------------------
model_fit_metrics(model_fits) |> 
  print() |> 
  capture.output() |> sapply(function(x) { paste("# ", x)}) |> write_lines(file = sprintf("make_all_Tlongicornis_%s.R", VERSION), append = TRUE)

## ----model_fit_confmat----------------------------------------------
model_fit_confmat(model_fits) +
  ggtitle("Models confusion matrices")
save_png("2_models_confusion_matrices")


## ----model_fit_roc_auc----------------------------------------------
model_fit_roc_auc(model_fits) + 
  ggtitle("Models performance by AUC")
save_png("2_models_roc_auc")

## ----model_fit_vip--------------------------------------------------
model_fit_varimp_plot(model_fits) +
  ggtitle("Permutation importance of covariates for each models")
save_png("2_models_variable_importance")


## ----random_forest--------------------------------------------------
rf = model_fits |>
  filter(wflow_id == "default_rf")


## -------------------------------------------------------------------
# autoplot(wflow, metric = "tss")


## ----rf_preds-------------------------------------------------------
rf$.predictions[[1]]


## ----rf_workflow----------------------------------------------------
rf$.workflow[[1]]


## ----load_workflow--------------------------------------------------
model_fits = read_model_fit(filename = sprintf("%s-%s-model_fits", SPECIES, VERSION))
model_fits


## ----nowcast--------------------------------------------------------
db = brickman_database()
present = read_brickman(db |>  filter(scenario == "PRESENT",  interval == "mon"),
                        add = c("depth", "month")) |>
  mutate(depth = log10(depth)) |>
  select(all_of(cfg$keep_vars))
nowcast = predict_stars(model_fits, present)
nowcast

## ----load_2075_RCP85, warning = FALSE-------------------------------
covars_rcp85_2055 = read_brickman(db |> filter(scenario == "RCP85", 
                                               year == 2055, 
                                               interval == "mon"),
                                  add = c("depth", "month")) |>
  mutate(depth = log10(depth)) |>
  select(all_of(cfg$keep_vars))

covars_rcp45_2055 = read_brickman(db |> filter(scenario == "RCP45", 
                                               year == 2055, 
                                               interval == "mon"),
                                  add = c("depth", "month")) |>
mutate(depth = log10(depth)) |>
select(all_of(cfg$keep_vars))

covars_rcp85_2075 = read_brickman(db |> filter(scenario == "RCP85", 
                                               year == 2075, 
                                               interval == "mon"),
                                  add = c("depth", "month")) |>
mutate(depth = log10(depth)) |>
select(all_of(cfg$keep_vars))

covars_rcp45_2075 = read_brickman(db |> filter(scenario == "RCP45", 
                                               year == 2075, 
                                               interval == "mon"),
                                  add = c("depth", "month")) |>
mutate(depth = log10(depth)) |>
select(all_of(cfg$keep_vars))



## ----forecast-------------------------------------------------------
forecast_4.5_2055 = predict_stars(model_fits, covars_rcp45_2055)
forecast_4.5_2075 = predict_stars(model_fits, covars_rcp45_2075)
forecast_8.5_2055 = predict_stars(model_fits, covars_rcp85_2055)
forecast_8.5_2075 = predict_stars(model_fits, covars_rcp85_2075)


plot_overall_prediction("Overall GLM Predictions", "default_glm")
plot_overall_prediction("Overall RF Predictions", "default_rf")
plot_overall_prediction("Overall Boosted Tree Predictions", "default_btree")
plot_overall_prediction("Overall MaxEnt Predictions", "default_maxent")

plot_difference(nowcast['default_glm'], forecast_8.5_2075['default_glm']) + ggtitle("GLM prediction of changes from nowcast to RCP8.5 2075")
save_png("glm_now-2075-85")
plot_difference(nowcast['default_rf'], forecast_8.5_2075['default_rf']) + ggtitle("RF prediction of changes from nowcast to RCP8.5 2075")
save_png("rf_now-2075-85")
plot_difference(nowcast['default_btree'], forecast_8.5_2075['default_btree']) + ggtitle("Boosted Tree prediction of changes from nowcast to RCP8.5 2075")
save_png("btree_now-2075-85")
plot_difference(nowcast['default_maxent'], forecast_8.5_2075['default_maxent']) + ggtitle("MaxEnt prediction of changes from nowcast to RCP8.5 2075")
save_png("maxent_now-2075-85")

comp = plot_prediction(nowcast['default_glm']) +
  plot_prediction(nowcast['default_rf']) + 
  plot_prediction(nowcast['default_btree']) + 
  plot_prediction(nowcast['default_maxent']) + 
  plot_annotation(title = "Models Nowcast Prediction Comparison")
save_png("comparison", comp, width = 16, height = 12)

#  # A tibble: 4 × 5
#    wflow_id       accuracy boyce_cont roc_auc tss_max
#    <chr>             <dbl>      <dbl>   <dbl>   <dbl>
#  1 default_glm       0.647      0.912   0.722   0.357
#  2 default_rf        0.588      0.613   0.690   0.345
#  3 default_btree     0.672      0.431   0.714   0.374
#  4 default_maxent    0.652      0.935   0.734   0.380