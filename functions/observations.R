
read_observations = function(scientificname = "Temora longicornis",
                             minimum_year = 1970, 
                             filtered_basisOfRecord = "PreservedSpecimen",
                             filter_individualCount = TRUE,
                             ...){
  
  #' Read raw OBIS data and then filter it
  #' 
  #' @param scientificname chr, the name of the species to read
  #' @param minimum_year num, the earliest year of observation to accept or 
  #'   set to NULL to skip
  #' @param filtered_basisOfRecord chr, the name of filtered basisOfRecord
  #' @param filter_individualCount logical, whether to filter NA individualCount
  #' @param ... other arguments passed to `read_obis()`
  #' @return a filtered table of observations
  
  # Happy coding!
  
  # read in the raw data
  x = fetch_obis(scientificname, ...) |>
    dplyr::mutate(month = factor(month, levels = month.abb)) |>
    filter(!is.na(eventDate))
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  if (!is.null(filtered_basisOfRecord)){
    x = x |>
      filter(basisOfRecord != filtered_basisOfRecord)
  }
  
  if (filter_individualCount){
    x = x |>
      filter(!is.na(individualCount))
  }
  
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  mask = read_brickman(db)
  hitOrMiss = extract_brickman(mask, x)
  
  x = x |>
    filter(!is.na(hitOrMiss$value))
  
  return(x)
}
