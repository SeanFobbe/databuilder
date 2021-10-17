#' Extract Links from HTML
#'
#' Extracts hyperlinks from one or more HTML documents by reading a HTML markup, selecting all <a> nodes and returning their 'href' attributes as a vector. Robust implementation that returns 'NA' on error. Vectorized.
#' 
#' @param x Character. A vector of filenames or URLs.
#' @param NA.omit Logical. Whether to return results with or without NAs included. Defaults to FALSE.

#' @return A vector of hyperlinks (for a single HTML document) or a named list of vectors (for a vector of document names).


extract_links <- function(x,
                          NA.omit = FALSE){
    
    links <- tryCatch({
        
        html <- xml2::read_html(x)
        
        nodes <- rvest::html_elements(html,
                                      "a")
        
        rvest::html_attr(nodes,
                         'href')},
        
        error = function(cond) {
            
            return(NA)}
        
        )

    if(NA.omit == TRUE){

        links <- links[!is.na(links)]
            
    }

    return(links)
    
}


extract_links <- Vectorize(extract_links)
