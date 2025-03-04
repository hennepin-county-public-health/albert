#' List Databricks volumes
#'
#' Use SQL queries to list Databricks volumes, their permission scheme, and the corresponding data lake location. Returns a tibble.
#' @param catalogue the catalogue that includes the volumes; points to health_fs by default
#' @param schema the schema that includes the volumes; points to health_fs by default
#' @param path all or part of a specific data lake file path that the user wishes find the corresponding volumes for
#' @export

#Primary user is identifying the volume that contains particular data, but it could also be used to create a vector of volumes that could be looped through.
#When supplied, the "path" argument will filter the volumes to only those that include the specified string. For instance, Trusted/PublicHealth/Reference, PublicHealth, Reference could all be supplied to filter the results
list_volumes <- function(catalogue = "prd_ph_source", schema = "health_fs", path = NULL){
  volumes <- SparkR::sql(str_c("SHOW VOLUMES IN ", catalogue, ".", schema)) |>
    SparkR::collect() |>
    tibble::as_tibble() |>
    dplyr::rename(data_lake = database, volume = volume_name) |>
    tidyr::separate_wider_delim(volume, delim = "_", names = c("level", "area", "directory", "subdirectory"), too_few = "align_start", cols_remove = FALSE) |>
    dplyr::mutate(
      area = dplyr::case_when(
        area == "ph" ~ "PublicHealth",
        area == "np" ~ "Northpoint",
        area == "me" ~ "MedicalExaminer"),
      #opioids too?
      dplyr::across(c(level, directory, subdirectory), stringr::str_to_title),
      data_lake_path = paste(level, area, directory, subdirectory, sep = "/"),
      data_lake_path = stringr::str_remove(data_lake_path, "/NA$")) |>
    dplyr::select(data_lake, volume, data_lake_path, level, area, directory, subdirectory)
  
  if (!is.null(path)){
    volumes <- volumes |> dplyr::filter(stringr::str_detect(data_lake_path, path))
  }
  
  #This code will get information about which groups have read permissions on each volume to add to the table
  permissions <- purrr::map(volumes$volume, function(vol){
    temp <- SparkR::sql(str_c("SHOW GRANTS ON VOLUME ", catalogue, ".", schema, ".", vol)) |>
      SparkR::collect() |>
      tibble::as_tibble() |>
      dplyr::filter(ActionType == "READ VOLUME") |>
      dplyr::mutate(volume = vol) |>
      dplyr::summarise(read_permissions = paste(Principal, collapse = ", "), .by = volume)
  }) |>
    purrr::list_rbind()
  
  final <- volumes |>
    dplyr::left_join(permissions, by = "volume")
  
  return(final)
}