#'## year.iso: Transform Two-Digit Years to Four-Digit Years

#' This function transforms two-digit years (YY) to four-digit years compliant with ISO-8601 (YYYY). It is based on the assumption that two-digit years above a certain boundary year belong to the reference century and years at or below that boundary belong to the following century.


#' @param x A vector of two-digit years.
#' @param boundary The boundary year. Defaults to 50 (= 1950).
#' @param century The reference century. Defaults to the 1900s (20th century).
#'
#' 
#' @return A vector of four-digit years.
#'
#' 
#' @examples
#' year_iso(91)
#' year_iso(c(13, 45, 36, 57, 68))


#' @export


year_iso <- function(x,
                     boundary = 50,
                     century = 1900){

    century.next <- century + 100
    
    data.table::fifelse(x > boundary,
                        century + x,
                        century.next + x,
                        na = NA)
    
}
