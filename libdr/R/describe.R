#' Produce descriptive analysis of categorical variables
#' @param vars Character vector giving the names of categorical variables
#' @param data Data frame (or tibble) holding the data of interest
#' @export
describe_cat <- function(vars, data){
  if (tibble::is_tibble(data)) {
    data <- as.data.frame(data)
  }
  for (i in 1:length(vars)) {
    var <- vars[i]
    print(table(data[, var], useNA = "always"))
    print(prop.table(table(data[, var], useNA = "always")))
  }
}
