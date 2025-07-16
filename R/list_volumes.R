#' List Databricks volumes
#'
#' Use SQL queries to list Databricks volumes, their permission scheme, and the corresponding data lake location. Returns a tibble.
#' @param catalog the catalog that includes the volumes; likely prd_ph_source
#' @param schema the schema that includes the volumes; raw, refined, or trusted. Default is all three.
#' @param parent_folder optional filter; the parent folder the volume maps to
#' @export

#Primary user is identifying the volume that contains particular data, but it could also be used to create a vector of volumes that could be looped through.
#When supplied, the "path" argument will filter the volumes to only those that include the specified string. For instance, Trusted/PublicHealth/Reference, PublicHealth, Reference could all be supplied to filter the results
list_volumes <- function(catalog = "prd_ph_source", schema = c("raw", "refined", "trusted"), parent_folder = NULL){

  #Ensure schema values are lowercase, and a valid level
  schemas <- purrr::map_chr(schema, ~tolower(.))

  purrr::walk(schemas, function(s){
    if (!s %in% c("raw", "refined", "trusted")){
      stop(paste0(s, " schema/level not found. Valid values are raw, refined, and trusted."))
    }
  })

  #SQL query to get volumes in catalog and schema
  volumes <- purrr::map(schemas, function(s){
    #Get volumes in catalog and schema
    volumes <- SparkR::sql(paste0("SHOW VOLUMES IN ", catalog, ".", s)) |>
      SparkR::collect() |>
      tibble::as_tibble() |>
      dplyr::rename(level = database, volume = volume_name)}) |>
    purrr::list_rbind() |>
    dplyr::arrange(level, volume)

  #SQL query
  permissions <- purrr::map(1:nrow(volumes), function(row){

    temp <- volumes[row, ]

    permissions <- SparkR::sql(paste0("SHOW GRANTS ON VOLUME ", catalog, ".", temp$level, ".`", temp$volume, "`")) |>
      SparkR::collect() |>
      tibble::as_tibble() |>
      dplyr::filter(ActionType == "READ VOLUME") |>
      dplyr::distinct(Principal, .keep_all = TRUE) |>
      dplyr::mutate(volume = stringr::str_remove(ObjectKey, paste0(catalog, ".", temp$level, ".")),
                    level = stringr::str_remove(ObjectKey, paste0(catalog, ".")),
                    level = stringr::str_remove(level, "\\..*$")) |>
      dplyr::summarise(read_permissions = paste(Principal, collapse = ", "), .by = c(volume, level))
  }) |>
    purrr::list_rbind()

  #If parent folder is provided, filter volumes to match
  if (!is.null(parent_folder)){

    #Convert to kebab-case if necessary
    parent_folder <- parent_folder |>
      stringr::str_replace_all("([a-z])([A-Z])", "\\1 \\2") |>
      stringr::str_replace_all("\\s", "-") |>
      stringr::str_to_lower()

    volumes <- volumes |>
      filter(str_detect(volume, parent_folder))

    if (nrow(volumes) == 0){
      stop("Parent folder filter returned no volumes. Is folder name correct?")
    }
  }

  final <- volumes |>
    dplyr::left_join(permissions, by = c("volume", "level"))

  return(final)
}
