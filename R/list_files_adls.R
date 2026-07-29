#' List data lake files
#'
#' List files in an Azure data lake. See SharePoint documentation for more information about how this function works and required setup. This function is intended to only work with trusted files.
#' @param path a directory path
#' @param endpoint the data lake storage account. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @param container the data lake container. Pulls from R environmental variable by default. See setup documentation with information about how to set.
#' @param recursive logical. Should the listing recurse into directories?
#' @param include_dirs logical. Should subdirectory names be included?
#' @export
#'

list_files_adls <- function(path = "", endpoint = Sys.getenv("ADLS_ENDPOINT"), container = Sys.getenv("ADLS_CONTAINER"), recursive = FALSE, include_dirs = FALSE){

  #Errors if environmental variables are not found
  if (endpoint == ""){
    stop("'endpoint' environmental variable not found. Verify that it's been successfully created.")
  }

  if (container == ""){
    stop("'container' environmental variable not found. Verify that it's been successfully created.")
  }

  if (!(str_detect(tolower(path), "^[^/]+/trusted/") | str_detect(tolower(path), "^trusted/"))){
    stop("This function is only intended to work with trusted data. Verify the specified file path is in trusted.")
  }

  path_clean <- ifelse(grepl("^/", path), path, paste0("/", path))

  #Handle improper argument values
  if (!recursive %in% c(TRUE, FALSE)){
    stop("Valid values of recursive are TRUE and FALSE.")
  }

  if (!include_dirs %in% c(TRUE, FALSE)){
    stop("Valid values of include_dirs are TRUE and FALSE.")
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

  tryCatch({
    file_list <- AzureStor::list_storage_files(container, dir = path, recursive = recursive)
  },
  error = function(e){

    if (grepl("Forbidden \\(HTTP 403\\)", e$message)){
      stop("Unable to list files at specified path due to insufficient permissions.")
    } else {
      stop(e$message)
    }
  })

  if (nrow(file_list) == 0){
    warning("No files returned. Ensure the specified path is correct and you have sufficient permissions.", immediate. = TRUE)
    return("")
  }

  if (include_dirs == TRUE){

    temp <- file_list$name

  } else if (include_dirs == FALSE){

    temp <- filter(file_list, isdir == FALSE)$name
  }
  return(temp)
}
