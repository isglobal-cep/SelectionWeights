% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\name{generate_fake_data}
\alias{generate_fake_data}
\title{Generate a random dataset}
\usage{
generate_fake_data(num_rows, num_numeric_cols, num_factor_cols, id_str)
}
\arguments{
\item{num_rows}{The number of rows to add. A positive integer.}

\item{num_numeric_cols}{The number of numerical variables to add. A positive integer.}

\item{num_factor_cols}{The number of categorical variables to add. A positive integer.}

\item{id_str}{The string indicating IDs. A string.}
}
\value{
A random dataset. A tibble.
}
\description{
Generate a random dataset
}
\examples{
dat <- generate_fake_data(10, 5, 3, "HelixID")
}
