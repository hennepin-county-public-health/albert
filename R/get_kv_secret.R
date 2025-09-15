#' Get secret.
#'
#' This function will get secret values from a Databricks secret scope/key vault. While the base Databricks utility will get the secret from a specified scope, this function will cycle through all scopes to attempt find an accessible version for the user. It's intended to make code more flexible between users with access to different key vaults.
#' @param secret_name the name (key) of a secret
#' @returns a string that cannot be directly printed.
#' @export

get_kv_secret <- function(secret_name) {
  scopes <- purrr::map_chr(dbutils.secrets.listScopes(), \(n) return(n$name))

  for (s in seq_along(scopes)) {
    scope <- scopes[s]
    tryCatch({
      secret <- dbutils.secrets.get(scope = scope, key = secret_name)
      message(paste("Accessing", secret_name, "from", scope))
      return(secret)
    }, error = function(e) {
      if (s == max(seq_along(scopes))) {
        stop("Secret not found in accessible Key Vaults.")
      } else {
        invisible()
      }
    })
  }
}
