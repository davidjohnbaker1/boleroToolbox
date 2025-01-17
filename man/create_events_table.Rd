% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_events_table.R
\name{create_events_table}
\alias{create_events_table}
\title{Create Events Table}
\usage{
create_events_table(df, org, lookup_vector)
}
\arguments{
\item{df}{Raw data from org at transaction level}

\item{org}{Three letter code used to check correct lookup table used}

\item{lookup_vector}{Named vector from global environment in app that provides old and used names}
}
\value{
Events Table
}
\description{
Create Events Table
}
