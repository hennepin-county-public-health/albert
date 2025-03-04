#' Edit Schema of Arrow data
#'
#' Clean the schema of an arrow dataset to remove "null" types which lead to errors. A typical workflow for this function is:
#' @param arrow_dataset data in Arrow environment
#' @export

clean_schema <- function(arrow_dataset){

  initial_schema <- arrow_dataset$schema #get default schema

  schema_cols <- initial_schema$num_fields #number of columns

  schema_cleaned <- schema(
    map(1:schema_cols, function(x){

      cname <- initial_schema[[x]]$name #column name
      ctype <- capture.output(initial_schema[[x]]$type)[2] #default column type

      if (ctype == "null" | ctype == "string"){ #adjust column types
        return(Field$create(name = cname, type = string()))
      } else if (ctype == "int64") {
        return(Field$create(name = cname, type = int64()))
      } else if (ctype == "double"){
        return(Field$create(name = cname, type = double()))
      } else {
        return(Field$create(name = cname, type = string()))
      }
    })
  )
  return(schema_cleaned)
}
