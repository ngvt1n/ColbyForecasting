SPECIES = "Temora longicornis"
obs = read_model_input(
  scientificname = SPECIES)

db = brickman_database() |>
  dplyr::filter(scenario == "RCP45", 
                year == 2055,
                interval == "mon") |>
     read_brickman()

x = group_by(obs, month, class) |> 
  group_map(~ .x |> slice(1) , .keep=TRUE) |> 
  bind_rows() |> 
  print()

wide_values = extract_brickman(db, x, form = "wide") |>
  select(month, class, MLD, Sbtm, SSS, SST) |> 
  print()


# Homework:
# For each month select at random one 
# presence and one background point 
# (so, that will be 2 x 12 = 24 points!) 
# from your model input data. 
# Then select three (3) variables 
# in the Brickman present monthly data set, 
# and build a single table 
# that has the three variables 
# for the 24 points. 