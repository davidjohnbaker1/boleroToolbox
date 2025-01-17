% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_transactions_table.R
\name{create_transactions_table}
\alias{create_transactions_table}
\title{Create Transactions Table}
\usage{
create_transactions_table(df, org, lookup_vector)
}
\arguments{
\item{df}{Raw data from org at transaction level}

\item{org}{Three letter code used to check correct lookup table used}

\item{lookup_vector}{Named vector from global environment in app that provides old and used names}
}
\value{
Transactions table
}
\description{
Create Transactions Table
}
