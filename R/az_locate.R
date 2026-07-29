#' Azure authentication
#'
#' This function will ensure Azure CLI can be found via the command line. If not, it will attempt to find and connect. It runs all data lake functions to ensure the necessary authentication is possible.
#' @param path_manual default is NULL; if supplied, will check for Azure CLI at that path in addition to the default ones. Not generally necessary.
#' @export
#'

az_locate <- function(path_manual = NULL){

  #Create env, if necessary, to store azure/auth token states
  az_env <- exists('.az_state')#, inherits = FALSE)
  if (az_env == FALSE){
    .az_state <<- new.env(parent = emptyenv())
  }

  #Test if path is available
  tryCatch(
    .az_state$version <<- system(
      'az --version',
      intern = TRUE
    ),
    error = function(e){
      .az_state$version <<- NULL
    }
  )

  #If not, try to find  Azure CLI
  if (is.null(.az_state$version)){

    potential_locations <- c("C:/Program Files/Microsoft SDKs/Azure/CLI2/wbin/az.cmd", #expected
                             "C:/Program Files (x86)/Microsoft SDKs/Azure/CLI2/wbin/az.cmd",
                             "C:/Program Files/AzureCLI/wbin/az.cmd",
                             "C:/Program Files (x86)/AzureCLI/wbin/az.cmd")

    #Add manual path if supplied via argument.
    if (!is.null(path_manual)){
      potential_locations <- c(path_manual, potential_locations)
    }

    for (pl in potential_locations){
      if (file.exists(pl)){

        .az_state$path <<- pl
        break

      }
      else if (!file.exists(pl) & pl == potential_locations[length(potential_locations)]){

        .az_state$path <- NULL
        stop("Microsoft Azure CLI (x64) not found at any expected path. Please ensure it has been downloaded from the Software Center. \nIf this error persists, manually identify the path to 'az.cmd' on your computer and supply it via the 'path_manual' argument.")

      }
    }

    #If path is identified, add it as an environmental variable to user's Windows account
    #Using PS based on rec - no consistent with other terminal code but works unlike the other approach
    if (!is.null(.az_state$path)){

      ps_command <- sprintf('[Environment]::SetEnvironmentVariable("Path", [Environment]::GetEnvironmentVariable("Path", "User") + ";%s", "User")',
                            .az_state$path)

      system2("powershell", args = c("-Command", shQuote(ps_command)))

      return("Azure CLI path added to Windows environmental variable.")
      #could always add extra code here that re-runs the initial test to ensure the CLI can be found.
    }
  }
}
