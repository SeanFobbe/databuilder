#' Replaces empty elements in vector with NA
#'
#' @param x A vector. 


vec_empty_NA <- function(x){
    gsub("^$",
         "NA",
         x)}
