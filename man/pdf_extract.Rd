% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_extract.R
\name{pdf_extract}
\alias{pdf_extract}
\title{\subsection{Parallelized Conversion of PDF to TXT}{

Extracts text from PDF files and writes the result to disk as TXT files. Parallel implementation with the future package. Resulting TXT files have the same filename as the original document (only the extension is modified).
}}
\usage{
pdf_extract(x, outputdir = NULL, quiet = TRUE)
}
\arguments{
\item{x}{A vector of PDF filenames.}

\item{quiet}{Supress messages.}
}
\value{
A set of TXT files on disk with the same basename as the original PDF files. Invisible return in R session.
}
\description{
Please note that you must declare your own future evaluation strategy prior to using the function to enable parallelization. By default the function will be evaluated sequentially. On Windows, use future::plan(multisession, workers = n), on Linux/Mac, use future::plan(multicore, workers = n), where n stands for the number of CPU cores you wish to use. Due to the need to read from the disk the function may not work properly on high-performance clusters.
}
