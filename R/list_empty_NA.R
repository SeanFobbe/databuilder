#' empty.list.NA: Replaces empty elements in list with 'NA'. Vectorized.
#'
#' @param x A list.
#'
#' @return The list that was passed, where empty elements are replaced with 'NA'.

replace_NA <- function(x){
    
    if (length(x) == 0){
        
        NA_character_
        
    }else{
        
        paste(x,
              collapse = " ")

    }
    
}

list_empty_NA <- function(x){lapply(x,
                                    replace_NA)}
