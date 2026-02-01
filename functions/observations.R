
read_observations = function(scientificname = "Temora longicornis",
                             minimum_year = 1970, 
                             filtered_basisOfRecord = c(
                               "PreservedSpecimen",
                               "materialSample", 
                               "MaterialSample", 
                               "NomenclaturalChecklist"
                             ),
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
    filter(!is.na(eventDate)) |> 
    filter(dropped == FALSE)
  
  # if the user provided a non-NULL filter by year
  if (!is.null(minimum_year)){
    x = x |>
      filter(year >= minimum_year)
  }
  
  if (!is.null(filtered_basisOfRecord)){
    x = x |>
      filter(!(basisOfRecord %in% filtered_basisOfRecord))
  }
  
  if (filter_individualCount){
    x = x |>
      filter(!is.na(individualCount))
  }
  
  db = brickman_database() |>
    filter(scenario == "STATIC", var == "mask")
  mask = read_brickman(db)
  hitOrMiss = extract_brickman(mask, x)

  dataset_titles = x |> 
    count(dataset_id) |> 
    _$dataset_id |> 
    sapply(fetch_dataset_title) |> print()

  x = x |> 
    mutate(dataset_title = dataset_titles[dataset_id] |> unname())
  
  x = x |>
    filter(!is.na(hitOrMiss$value))
  
  return(x)
}


read_Tlongicornis = function(scientificname = "Temora longicornis",
                             ...){
  return(read_observations(scientificname, ...))

}


read_Dbrightwelli = function(scientificname = "Ditylum brightwellii",
                             ...){
  return(read_observations(scientificname, ...))
}
