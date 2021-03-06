#' Fast Frequency Tables

#' Creates frequency tables for an arbitrary number of variables. It can return them as a list, write them to an arbitrary folder on disk as CSV files (with an optional prefix or return kable tables that are designed to work well with render() and LaTeX. It is based on data.table and is therefore capable of quickly processing massive data sets.
#'
#' To show the kable output in render() you must add the following chunk option: "results = 'asis'"


#' @param x A data.frame or data.table.

#' @param varlist (Optional) Character. A vector of variable names to construct tables for. Defaults to all variables.

#' @param sumrow (Optional) Logical. Whether to add a summary row. Defaults to TRUE.

#' @param out.list (Optional) Logical. Whether to output the frequency tables as a list. Defaults to TRUE. Returns NULL otherwise.

#' @param out.kable (Optional) Logical. Whether to return kable tables. Defaults to FALSE.

#' @param out.csv (Optional) Logical. Whether to write CSV files (one per variable) to disk. Defaults to FALSE.

#' @param out.dir (Optional) Character. The target directory for writing CSV files. Defaults to the current R working directory.

#' @param prefix (Optional) A string to be prefixed to the filename of each CSV file. Default is not to add a string and just to output the variable name as the name of the CSV file.

#' @param align (Optional) Alignment of table columns passed to kable. Default is "r". Note that your options passed must work for a five-column layout.



#' @return Returns a list of frequency tables for each variable by default. Different out.* arguments can add CSV and kable output.




ffreqtable <- function(x,
                       varlist = names(x),
                       sumrow = TRUE,
                       out.list = TRUE,
                       out.kable = FALSE,
                       out.csv = FALSE,
                       out.dir = "./",
                       prefix = "",
                       align = "r"){

    
    ## Test if x is a suitable object
    
    if((is.vector(x) == FALSE) && (is.data.frame(x) == FALSE) && (is.data.table(x) == FALSE)){
        stop("ffreqtable only accepts data.table, data.frame or vectors as input.")
        }

    
    ## Check if x is vector or data.frame and coerce to data.table 

    if(is.data.frame(x) == TRUE){
        
        setDT(x)
        
    }else if(is.vector(x)){
        
        x <- as.data.table(x)
        
    }

    
    ## Begin List
    freqtable.list <- vector("list",
                             length(varlist))

    ## Separately declare Variable due to NSE notes in R CMD check
    N <- NULL  
    
    ## Calculate Frequency Table
    for (i in seq_along(varlist)){
        
        varname <- varlist[i]
        
        freqtable <- x[,
                       .N,
                       keyby = c(paste0(varname))]
        
        freqtable[, c("exact_pct",
                      "rounded_pct",
                      "cumul_pct") := {
                          exact_pct  <-  N/sum(N)*100
                          rounded_pct <- round(exact_pct, 2)
                          cumul_pct <- round(cumsum(exact_pct), 2)
                          list(exact_pct,
                               rounded_pct,
                               cumul_pct)
                      }
                  ]

        ## Calculate Summary Row
        if (sumrow == TRUE){
            colsums <-  cbind("Total",
                              freqtable[, lapply(.SD, function(x){round(sum(x))}),
                                        .SDcols = c("N",
                                                    "exact_pct",
                                                    "rounded_pct")
                                        ], round(max(freqtable$cumul_pct)))
            
            colnames(colsums)[c(1,5)] <- c(varname, "cumul_pct")
            freqtable <- rbind(freqtable, colsums)
        }
        
        ## Add Frequency Table to List
        freqtable.list[[i]] <- freqtable

        ## Write CSV
        if (out.csv == TRUE){
            
            data.table::fwrite(freqtable,
                               paste0(out.dir,
                                      prefix,
                                      varname,
                                      ".csv"),
                               na = "NA")

        }

        ## Output Kable
        if (out.kable == TRUE){

            cat("\n------------------------------------------------\n")
            cat(paste0("Frequency Table for Variable:   ", varname, "\n"))
            cat("------------------------------------------------\n")
            cat(paste0("\n ",
                       x[, .N, keyby=c(paste0(varname))][,.N],
                       " unique value(s) detected.\n\n"))

            kable <- knitr::kable(freqtable,
                                  format = "latex",
                                  align = align,
                                  booktabs = TRUE,
                                  longtable = TRUE)
            
            print(kableExtra::kable_styling(kable,
                                            latex_options = "repeat_header"))
        }
    }

    ## Return List of Frequency Tables
    if (out.list == TRUE){
        return(freqtable.list)
    }
}
