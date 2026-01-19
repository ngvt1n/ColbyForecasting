DB = brickman_database() |>
  print()

brickman_variables("all")

db = DB |>
  dplyr::filter(scenario == "RCP45", 
                year == 2055,
                interval == "mon")
x = read_brickman(db)
x

buoys = gom_buoys()
buoys

wide_values = extract_brickman(x, buoys, form = "wide")

m1 = wide_values |> filter(.id == "p1")

m1$month = as.factor(m1$month)

ggplot(data = m1, 
       mapping = aes(
         x = month,
         y = SST,
       )) + 
  geom_point() +
  ggtitle("RCP4.5 2055 SST at buoy M01 ")