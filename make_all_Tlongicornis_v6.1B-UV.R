## -------------------------------------------------------------------
source("./setup.R")
set.seed(1235)
VERSION = "v6.1B-UV"

## ----load_prerequisites-----------------------------------------------
coast = read_coastline()
present = brickman_database() |>
  dplyr::filter(scenario == "PRESENT", interval == "mon") |> read_brickman()
mask = brickman_database() |>
  filter(scenario == "STATIC", var == "mask") |> read_brickman()
keep = filter_collinear(present, method = "cor_caret", cutoff = 0.65)
keep = c("depth", "month", "SSS", "Sbtm", "Tbtm",  "MLD",   "SST")

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


## ----thin_observations----------------------------------------------
thinned_obs = sapply(month.abb,
                     function(mon){ 
                       temp_x = obs |> filter(month == mon)
                       if(nrow(temp_x) == 0) return(NULL)
                       thin_by_dist(temp_x, 20000)
                     }, simplify = FALSE) |>
  dplyr::bind_rows() 


## ----bias_map-------------------------------------------------------
bias_map = rasterize_point_density(obs, mask) # <-- note the original observations

ggplot() +
  geom_stars(data = bias_map, aes(fill = count)) +
  scale_fill_viridis_b(na.value = "transparent") +
  geom_sf(data = coast, col = "orange") + 
  labs(x = "Longitude", y = "Latitude", title = "Bias map using all observations")


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


## ----model_fit_roc_auc----------------------------------------------
model_fit_roc_auc(model_fits) + 
  ggtitle("Models performance by AUC")

## ----model_fit_vip--------------------------------------------------
model_fit_varimp_plot(model_fits) +
  ggtitle("Permutation importance of covariates for each models") + 
  scale_fill_viridis_c(option="magma")


## ----random_forest--------------------------------------------------
rf = model_fits |>
  filter(wflow_id == "default_rf")


## -------------------------------------------------------------------
# autoplot(wflow, metric = "tss")


## ----rf_preds-------------------------------------------------------
rf$.predictions[[1]]


## ----rf_workflow----------------------------------------------------
rf$.workflow[[1]]


## ----pd_plot--------------------------------------------------
model_fit_pdp(model_fits, wid = "default_glm", title = "Generalized Linear Model")
model_fit_pdp(model_fits, wid = "default_rf", title = "Random Forest")
model_fit_pdp(model_fits, wid = "default_btree", title = "Boosted Tree")
model_fit_pdp(model_fits, wid = "default_maxent", title = "Max Entropy")


## ----load_workflow--------------------------------------------------
model_fits = read_model_fit(filename = sprintf("%s-%s-model_fits", SPECIES, VERSION))
model_fits


## ----nowcast--------------------------------------------------------
db = brickman_database()
present = read_brickman(db |>  filter(scenario == "PRESENT",  interval == "mon"),
                        add = c("depth", "month")) |>
  select(all_of(cfg$keep_vars))
nowcast = predict_stars(model_fits, present)
nowcast


## ----plot_nowcast_maxent, warning = FALSE---------------------------
plot_prediction(nowcast['default_glm']) +    ggtitle("GLM Nowcast")
plot_prediction(nowcast['default_rf']) +     ggtitle("RF Nowcast")
plot_prediction(nowcast['default_btree']) +  ggtitle("Btree Nowcast")
plot_prediction(nowcast['default_maxent']) + ggtitle("Maxent Nowcast")


## ----plot_class_labels, warning = FALSE-----------------------------
# pa_nowcast = threshold_prediction(nowcast)
# plot_prediction(pa_nowcast['default_btree'])


## ----load_2075_RCP85, warning = FALSE-------------------------------
covars_rcp85_2075 = read_brickman(db |> filter(scenario == "RCP85", 
                                               year == 2075, 
                                               interval == "mon"),
                                  add = c("depth", "month")) |>
  select(all_of(cfg$keep_vars))



## ----forecast-------------------------------------------------------
forecast_2075 = predict_stars(model_fits, covars_rcp85_2075)
forecast_2075


## ----plot_forecast, warning = FALSE---------------------------------
plot_prediction(forecast_2075['default_glm']) + ggtitle("GLM RCP8.5 2075 Forecast")
plot_prediction(forecast_2075['default_rf']) + ggtitle("RF RCP8.5 2075 Forecast")
plot_prediction(forecast_2075['default_btree']) + ggtitle("Btree RCP8.5 2075 Forecast")
plot_prediction(forecast_2075['default_maxent']) + ggtitle("Maxent RCP8.5 2075 Forecast")


## ----save_pred------------------------------------------------------
# make sure the output directory exists
path = make_path("predictions")

write_prediction(nowcast,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "CURRENT",
                 scenario = "CURRENT")
write_prediction(forecast_2075,
                 scientificname = cfg$scientificname,
                 version = cfg$version,
                 year = "2075",
                 scenario = "RCP85")

# ----copy_plots------
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)
plots.png.paths <- plots.png.paths["empty" |> grepl(plots.png.paths) |> not()]
file.copy(from=plots.png.paths, to=sprintf("./%s/%s", make_path(VERSION), basename(plots.png.paths)), overwrite=TRUE)




#  # A tibble: 4 × 5
#    wflow_id       accuracy boyce_cont roc_auc tss_max
#    <chr>             <dbl>      <dbl>   <dbl>   <dbl>
#  1 default_glm       0.537      0.282   0.557  0.119 
#  2 default_rf        0.545      0.453   0.577  0.158 
#  3 default_btree     0.553      0.438   0.566  0.156 
#  4 default_maxent    0.518      0.340   0.516  0.0702
