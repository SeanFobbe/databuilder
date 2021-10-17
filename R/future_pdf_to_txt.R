#'## Parallelized Extraction of Text from PDF Documents

#' Extracts text from PDF files and writes the result to disk as TXT files. Parallel implementation with the future package. Resulting TXT files have the same filename as the original document (only the extension is modified).
#'
#' Please note that you must declare your own future evaluation strategy. By default the function will be evaluated sequentially. Use plan(multisession, workers = n) on Windows or plan(multicore, workers = n) on Linux/Mac, where n stands for the number of CPU cores you wish to use. Due to the need to read/write to the disk the function may not work properly on high-performance clusters.


#' @param x A vector of PDF filenames.
#'
#' 
#' @return A set of TXT files on disk with the same basename as the original PDF files. Invisible return in R session.



pdf_to_txt <- function(x){
    
    ## Extract text layer from PDF
    pdf.extracted <- pdftools::pdf_text(x)

    ## TXT filename
    txtname <- gsub("\\.pdf",
                    "\\.txt",
                    x,
                    ignore.case = TRUE)
    
    ## Write TXT to Disk
    write.table(pdf.extracted,
                txtname,
                quote = FALSE,
                row.names = FALSE,
                col.names = FALSE)
    
}



future_pdf_to_txt <- function(x){

    ## Timestamp: Begin
    begin.extract <- Sys.time()

    ## Intro message
    message(paste("Processing",
                  length(x),
                  "files. Begin at:",
                  begin.extract))

    ## Perform conversion from PDF to TXT
    invisible(future.apply::future_lapply(x,
                                          pdf_to_txt,
                                          future.seed = TRUE))


    ## Construct full list of TXT names
    txt.names <- gsub("\\.pdf",
                      "\\.txt",
                      x,
                      ignore.case = TRUE)

    ## Check list of TXT files in folder
    txt.results <- list.files(pattern = "\\.txt")

    ## Compare full list to files in folder
    txt.missing <- setdiff(txtnames,
                           txtresults)
    
    ## Timestamp: End
    end.extract <- Sys.time()

    ## Duration
    duration.extract <- end.extract - begin.extract

    ## Outro message
    message(paste0("Successfully processed ",
                   length(x) - length(txt.missing),
                   " files. ",
                   length(txt.missing),
                   " files failed. Runtime was ",
                   round(duration.extract,
                         digits = 2),
                   " ",
                   attributes(duration.extract)$units,
                   ". Ended at: ",
                   end.extract))


}
