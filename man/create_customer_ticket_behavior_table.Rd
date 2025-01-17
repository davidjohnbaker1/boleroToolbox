% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_customer_ticket_behavior_table.R
\name{create_customer_ticket_behavior_table}
\alias{create_customer_ticket_behavior_table}
\title{Create Customer Ticket Behavior Table}
\usage{
create_customer_ticket_behavior_table(
  transaction_df,
  org,
  recent_patron_max = 90,
  current_patron_max = 270,
  expiring_patron_max = 365,
  lapsed_patron_max = 1095
)
}
\arguments{
\item{transaction_df}{Result of \code{boleroToolbox::create_transactions_table()}}

\item{org}{Three letter code used to check correct lookup table used}

\item{recent_patron_max}{Maximum days allowed for patron to be considered recent (90 default)}

\item{current_patron_max}{Maximum days allowed for current patron (270 default)}

\item{expiring_patron_max}{Maximum days allowed for expiring patron (1 Year default)}

\item{lapsed_patron_max}{Maximum days allowed for lapsed patron (3 Years default)}
}
\value{
Table of all transactions with ticket behavior metrics
}
\description{
Create Customer Ticket Behavior Table
}
