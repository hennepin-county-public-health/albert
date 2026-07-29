#' Write data lake files
#'
#' `write_csv_adls` and `write_parquet_adls` write  Azure data lake files. Use ... to pass arguments to the underlying write functions. See SharePoint documentation for more information about how each function works and required setup. These functions are only intended to work with trusted files. Code that transforms raw or refined files should be stored in Databricks.
#' @param x a dataframe to be written
#' @param path the file path
#' @param endpoint the data lake storage account. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @param container the data lake container. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @export
#'

write_csv_adls <- function(x, file, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), ...){

  #Errors if environmental variables are not found
  if (endpoint == ""){
    stop("'endpoint' environmental variable not found. Verify that it's been successfully created.")
  }

  if (container == ""){
    stop("'container' environmental variable not found. Verify that it's been successfully created.")
  }

  #Error if not accessing trusted file
  if (!(str_detect(tolower(file), "^[^/]+/trusted/") | str_detect(tolower(file), "^trusted/"))){
    stop("This function is only intended to write trusted data. Verify the file is being written to trusted.")
  }

  #Make sure file is acceptable. Stop/warn if not
  file_type <- sub(".*(\\.[A-Za-z]+)$", "\\1", file) |>
    tolower()

  if (file_type != ".csv"){
    stop("Failed writing file type. Are you writing a file with the .csv extension?")
  }

  #Make sure Azure CLI is found, try to find if not
  az_locate()

  #Make sure auth_token exists, generate if not
  az_adls_token()

  azure_endpoint <- AzureStor::storage_endpoint(
    endpoint,
    token = .az_state$token
  )

  container <- AzureStor::storage_container(azure_endpoint, container)

  #Test if folder exists. If not, warn when writing
  folder <- sub("/[^/]*$", "", file)

  tryCatch({
    folder_exists <- AzureStor::storage_dir_exists(container, folder)
  },
  error = function(e){

    if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to write file to specified location due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  })

  if (folder_exists == FALSE){
    warning("Data is being written to a folder that didn't previously exist.", immediate. = TRUE)
  }

  #Rather than writing directly from memory, writing from a tmp file seems preferred
  tmp <- tempfile(fileext = file_type)

  readr::write_csv(x, tmp, ...)

  tryCatch({
    upload_res <- AzureStor::storage_upload(
      container,
      src = tmp,
      dest = file
    )
  },
  error = function(e){

    if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to write file to specified location due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  }
  )

  unlink(tmp)
}

#' @rdname write_csv_adls
#' @export
write_parquet_adls <- function(x, file, endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), ...){

  if (endpoint == ""){
    stop("'endpoint' environmental variable not found. Verify that it's been successfully created.")
  }

  if (container == ""){
    stop("'container' environmental variable not found. Verify that it's been successfully created.")
  }

  #Error if not accessing trusted file
  if (!(str_detect(tolower(file), "^[^/]+/trusted/") | str_detect(tolower(file), "^trusted/"))){
    stop("This function is only intended to write trusted data. Verify the file is being written to trusted.")
  }

  #Make sure file is acceptable. Stop/warn if not
  file_type <- sub(".*(\\.[A-Za-z]+)$", "\\1", file) |>
    tolower()

  if (file_type != ".parquet"){
    stop("Failed writing file type. Are you writing a file with the .parquet extension?")
  }

  #Make sure Azure CLI is found, try to find if not
  az_locate()

  #Make sure auth_token exists, generate if not
  az_adls_token()

  azure_endpoint <- AzureStor::storage_endpoint(
    endpoint,
    token = .az_state$token
  )

  container <- AzureStor::storage_container(azure_endpoint, container)

  #Test if folder exists. If not, warn when writing
  folder <- sub("/[^/]*$", "", file)

  tryCatch({
    folder_exists <- AzureStor::storage_dir_exists(container, folder)
  },
  error = function(e){

    if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to write file to specified location due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  })

  if (folder_exists == FALSE){
    warning("Data is being written to a folder that didn't previously exist.", immediate. = TRUE)
  }

  #Rather than writing directly from memory, writing from a tmp file seems preferred
  tmp <- tempfile(fileext = file_type)

  arrow::write_parquet(x, tmp, ...)

  tryCatch({
    upload_res <- AzureStor::storage_upload(
      container,
      src = tmp,
      dest = file
    )
  },
  error = function(e){

    if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to write file to specified location due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  })

  unlink(tmp)
}
