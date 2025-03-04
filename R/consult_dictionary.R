#' Set csv column types
#'
#' Reference column types specified in a data dictionary to create a column specification (.cols) for read_csv's col_types.
#' @param dictionary a dataframe with information from a data dictionary
#' @param column_names a column containing expected data columns
#' @param na_accept a column containing expected type for each expected column
#' @param date_format a single date format or a vector of multiple date formats used to read in date columns. If a vector is supplied, its length must equal the number of date columns.
#' @param datetime_format a single datetime format or a vector of multiple datetime formats used to read in datetime columns. If a vector is supplied, its length must equal the number of datetime columns.
#' @export

consult_dictionary <- function(dictionary, column_names, column_types, date_format = "%m/%d/%Y", datetime_format = "%m/%d/%Y %I:%M:%S %p"){
  
  dict <- dictionary |>
    select(variable = all_of(column_names), type = all_of(column_types)) |>
    mutate(type = str_to_lower(type))
  
  #The number of date(time) formats should either be 1 or equal to the number of columns
  dlen <- nrow(filter(dict, type == "date"))
  dtlen <- nrow(filter(dict, type == "datetime"))
  dflen <- length(date_format)
  dftlen <- length(datetime_format)
  
  if (dflen != 1 & dflen != dlen){
    stop(str_c("date_format length should either be 1 or ", dlen, ", not ", dflen))
  }
  
  if (dftlen != 1 & dftlen != dtlen){
    stop(str_c("datetime_format length should either be 1 or ", dtlen, ", not ", dftlen))
  }
  
  #Create base list of columns
  col_types <- cols()
  date_count <- 0
  datetime_count <- 0
  
  #Add type information for each column
  for (x in 1:nrow(dict)){
    
    temp <- dict |> slice(x)
    
    #there's got to be a better way.. but ifelse/casewhen don't work for this
    if (temp$type == "character"){
      col_types$cols[[temp$variable]] <- col_character()
    } else if (temp$type == "numeric"){
      col_types$cols[[temp$variable]] <- col_number()
    } else if (temp$type == "logical"){
      col_types$cols[[temp$variable]] <- col_logical()
    } else if (temp$type == "date" & length(date_format) == 1){
      col_types$cols[[temp$variable]] <- col_date(format = date_format)
    } else if (temp$type == "date" & length(date_format > 1)){
      date_count <- date_count + 1
      col_types$cols[[temp$variable]] <- col_date(format = date_format[date_count])
    } else if (temp$type == "datetime" & length(datetime_format) == 1){
      col_types$cols[[temp$variable]] <- col_datetime(format = datetime_format)
    } else if (temp$type == "datetime" & length(datetime_format) > 1){
      datetime_count <- datetime_count + 1
      col_types$cols[[temp$variable]] <- col_datetime(format = datetime_format[datetime_count])
    } else {
      stop(str_c('Column type "', temp$type, '" not accepted. Accepted types are: character, date, datetime, logical, and numeric.'))
    }
    
  }
  return(col_types)
}
