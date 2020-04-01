#' Algorithmia Wrapper for GEKS
#'
#' @param input_json an input json. See ?GEKS for the details of the JSON
#' @return see ?GEKS for return details. This function will retun the exact same
#' results, except in json format rather than a list
#' @export
#' @import jsonlite
GEKS_algorithmia <- function(input_json){
  # This is a wrapper for the algorithmia implementation of the GEKS code
  # It takes a json and unpacks it and then calls the function as normal
  # It then wraps the output back up into a json to return it

  input_list <- fromJSON(input_json)

  geks_op <- GEKS(times = input_list$times,
                  price = input_list$price,
                  id = input_list$id,
                  window_length = input_list$window_length,
                  weight = input_list$weight,
                  splice_pos = input_list$splice_pos,
                  num_cores = NULL)

  toJSON(geks_op)

}
