% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auxinf_autr.R
\name{auxinf_autr}
\alias{auxinf_autr}
\title{Function with information about scientific partnerships}
\usage{
auxinf_autr(input_date, input_tam)
}
\arguments{
\item{input_date}{is a dataframe with the scientific production database}

\item{input_tam}{is the length of the dataframe to be generated with the main articles, authors and journals}
}
\value{
The function returns a list containing 5 dataframes with information about the number of authors who wrote the articles and the number of articles written by each author, both in the general database and in the list of main references.
}
\description{
This function generates quantitative information about the partnerships of researchers in the development of scientific production on the topic covered. The function returns a list containing 5 dataframes with information about the number of authors who wrote the articles and the number of articles written by each author, both in the general database and in the list of main references.
}
\examples{

file_db <- system.file("extdata","example_database.csv",package = "bibliorefer")
separator <- ","
input_date <- example_database(file_db, separator)
input_tam <- 100
extra_inf <- auxinf_autr(input_date,input_tam)
extra_inf


}
\references{
Digitalization as a way forward: A bibliometric analysis of 20 Years of servitization research Zhou, C.,  Song, W. Journal of Cleaner Production, 300, 2021.
}
