# This is an example of a assignment script
#
# Assignment Script
#
# Use the [Brickman tutorial](https://github.com/BigelowLab/ColbyForecasting/wiki/Brickman) 
# to extract data from the location of Buoy N01 for current conditions. Extract
# both `SST` and `Tbtm` for every month and then determine the temperature
# difference `dT`. Make a plot of `dT` (y-axis) as a function of `month` (x-axis).  

# always start with setting things up
source("setup.R")

# load the buoys, the coastline and the database of covariate data
buoys = gom_buoys()
coast = read_coastline()
db = brickman_database()

# filter the buoys to pull out just N01
buoys = buoys |> 
  filter(id == "N01")  # note the double '=='

# filter the database to just the current scenario monthlies
db = db |> 
  filter(scenario == "PRESENT", interval == "mon")

# read the covariates
covars = read_brickman(db)

# extract the variables from the covars
# we want to compare two variables, we could do it with the
# long form, but the wide form might be better
x = extract_brickman(covars, buoys, form = "wide")

# add a column (called mutating which sounds vaguely gross) to the table
# the new colum is dT = SST - Tbtm
x = x |> 
  mutate(dT = SST - Tbtm)

# now make a plot of dT as a function of month
ggplot(data = x,
       mapping = aes(x = month, y = dT)) +
  geom_point() + 
  labs(title = "Temp difference at buoy N01")

# whoops!  That's with the months in alphabetical order.  To control the order
# make month into a factor and specify the order.  The built in value, `month.abb`
# has the month abbreviations in chronological order. Another mutate should do it.
# This time the mutate simple modifies a column instead of adding a new one.
x = x |>
  mutate(month = factor(month, levels = month.abb))

# now plot!
ggplot(data = x,
       mapping = aes(x = month, y = dT)) +
  geom_point() + 
  labs(y = "dT (C)", 
       title = "Temp difference at buoy N01")

# tahdah!

# bonus... save the image
ggsave("images/N01_dT.png")
