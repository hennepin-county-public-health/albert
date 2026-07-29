#' Azure authentication
#'
#' Generate a manual, non-refreshable Azure authentication token with user's account credentials. It expires after an hour and a half. The first time a user runs this function they'll be prompted to sign in via a pop-up window. Afterward, it will happen automatically. This requires Azure CLI to be installed and findable.
#' @param mode silent, token, or debug. "silent" is the default and is used inside other functions. "token" will return the actual token. "debug" returns useful information for troubleshooting token issues.
#' @export
#'

az_adls_token <- function(mode = "silent"){

  if (!mode %in% c("silent", "token", "debug")){
    stop("Valid mode values are silent, token, and debug.")
  }

  #Create env, if necessary, to store token state
  az_env <- exists('.az_state')#, inherits = FALSE)
  if (az_env == FALSE){
    .az_state <<- new.env(parent = emptyenv())
  }

  #First, check whether .az_state indicates the user is logged on
  #No means either, 1) user has never logged in or 2) they are logged in but .az_state was just created (first use in current session)
  if (is.null(.az_state$logged_in)){
    acct_info <- suppressWarnings(system("az account show", intern = TRUE)) #if logged in, add that value to the env (below)
    .az_state$acct_info <<- acct_info

    if (any(acct_info == "ERROR: Please run 'az login' to setup account.")){ #if not logged in, log in
      log_in <- system("az login", intern = TRUE)
      .az_state$login_res <<- log_in

      if (sum(grepl("tenantDefaultDomain", log_in)) > 0){
        .az_state$logged_in <<- TRUE #if log in was successful, save for session and proceed
        acct_info <- suppressWarnings(system("az account show", intern = TRUE))
        .az_state$acct_info <<- acct_info
      }
    } else if (sum(grepl("tenantDefaultDomain", acct_info)) > 0){
      .az_state$logged_in <<- TRUE #if already logged in, save for session and proceed
    } else {
      warning('Azure CLI login not successful. Use `az_adls_token(mode = "debug")` for troubleshooting.')
    }
  }

  #Now, check whether new token should be generated (it expires after 1.5 hours)
  now <- Sys.time()

  if (is.null(.az_state$expires_on) || now >= .az_state$expires_on - 60){
    message("Generating new Azure CLI token.")

    #Generate new token
    cli_token <- system(
      'az account get-access-token --resource https://storage.azure.com/ --query accessToken -o tsv',
      intern = TRUE
    )

    az_token <- AzureAuth::AzureManualToken$new(cli_token)

    .az_state$token_raw <<- cli_token
    .az_state$token <<- az_token
    .az_state$expires_on <<- as.POSIXct(as.numeric(az_token$credentials$expires_on), tz = "America/Chicago")
  }

  if (mode == "debug"){

    temp <- list(
      acct_info = .az_state$acct_info,
      login = .az_state$login_res,
      token_raw = .az_state$token_raw,
      token_azure = .az_state$token
    )

    return(temp)
  }

  if (mode == "token"){

    return(.az_state$token)

  }
}
