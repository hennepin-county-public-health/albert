#' Read data lake files
#'
#' `read_csv_adls`, `read_parquet_adls`, `read_excel_adls`, and `read_raw_adls` offer functionality to read Azure data lake files. Use ... to pass arguments to the underlying read functions. See SharePoint documentation for more information about how each function works and required setup. These functions are only intended to work with trusted files. Code that transforms raw or refined files should be stored in Databricks.
#' @param path the path to a file
#' @param endpoint the data lake storage account. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @param container the data lake container. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @param return_excel FALSE by default; switch to TRUE in `read_adls_raw` if returning raw output from an Excel file.
#' @export
#'

read_raw_adls <- function(path, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), return_excel = FALSE){

  #Errors if environmental variables are not found
  if (endpoint == ""){
    stop("'endpoint' environmental variable not found. Verify that it's been successfully created.")
  }

  if (container == ""){
    stop("'container' environmental variable not found. Verify that it's been successfully created.")
  }

  #Error if not accessing trusted file
  if (!(str_detect(tolower(path), "^[^/]+/trusted/") | str_detect(tolower(path), "^trusted/"))){
    stop("This function is only intended to read trusted data. Verify the specified file is in trusted.")
  }

  #Functions are only intended to read from trusted for reporting

  #Make sure Azure CLI is found, try to find if not
  az_locate()

  #Make sure auth_token exists, generate if not
  az_adls_token()

  azure_endpoint <- AzureStor::storage_endpoint(
    endpoint,
    token = .az_state$token
  )

  container <- AzureStor::storage_container(azure_endpoint, container)

  tryCatch({
    data_raw <- AzureStor::storage_download(
      container,
      src = path,
      dest = NULL
    )
  },
  error = function(e){

    if (grepl("Not Found \\(HTTP 404\\)", e$message)){
      stop("File not found. Did you specify the correct file path?")
    } else if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to download file due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  })

  con <- rawConnection(data_raw)

  if (return_excel == TRUE){
    return(data_raw)
  } else {
    return(con)
  }
}

#' @rdname read_raw_adls
#' @export
read_csv_adls <- function(path, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), ...){

  #Make sure file is a csv
  file_type <- sub(".*(\\.[A-Za-z]+)$", "\\1", path) |>
    tolower()

  if (file_type != ".csv" ){
    stop("Error reading file: is it a csv file?. See other read_adls functions for additional options.")
  }

  con <- read_raw_adls(path, endpoint, container)

  #Return dataframe from csv file
  temp <- readr::read_csv(con, ...)

  return(temp)

}

#' @rdname read_raw_adls
#' @export
read_parquet_adls <- function(path, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), ...){

  #Make sure file is a parquet
  file_type <- sub(".*(\\.[A-Za-z]+)$", "\\1", path) |>
    tolower()

  if (file_type != ".parquet" ){
    stop("Error reading file: is it a parquet file?. See other read_adls functions for additional options.")
  }

  con <- read_raw_adls(path, endpoint, container)

  #Return dataframe from csv file
  temp <- arrow::read_parquet(con, ...)

  return(temp)

}

#' @rdname read_raw_adls
#' @export
read_excel_adls <- function(path, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), ...){

  #Make sure file is a parquet
  file_type <- sub(".*(\\.[A-Za-z]+)$", "\\1", path) |>
    tolower()

  if (!file_type %in% c(".xlsx", ".xls")){
    stop("Error reading file: is it an Excel file?. See other read_adls functions for additional options.")
  }

  con <- read_raw_adls(path, endpoint, container, return_excel = TRUE)

  #Return dataframe from csv file
  tmp <- tempfile(fileext = file_type)
  writeBin(con, tmp)
  temp <- readxl::read_excel(tmp, ...)
  unlink(tmp)

  return(temp)

}
