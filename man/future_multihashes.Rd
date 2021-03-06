% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/future_multihashes.R
\name{future_multihashes}
\alias{future_multihashes}
\title{Parallel Computation of SHA2 and SHA3 Hashes
This function parallelizes computation of both SHA2-256 and SHA3-512 hashes for an arbitrary number of files. It returns a data frame of file names, SHA2-256 hashes and SHA3-512 hashes. The function requires the existence of the openssl library (RPM) on the system.}
\usage{
future_multihashes(x)
}
\arguments{
\item{x}{A vector of filenames. Should be located in the working directory.}
}
\description{
Please note that you must declare your own future evaluation strategy prior to using the function to enable parallelization. By default the function will be evaluated sequentially. On Windows, use future::plan(multisession, workers = n), on Linux/Mac, use future::plan(multicore, workers = n), where n stands for the number of CPU cores you wish to use. Due to the need to read/write to the disk the function may not work properly on high-performance clusters.
}
