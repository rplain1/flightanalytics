% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrival_curve.R
\name{get_pgds_passenger_bag_factors}
\alias{get_pgds_passenger_bag_factors}
\title{Get PGDS Passenger and Bag Factors}
\source{
\href{https://iabsc.org/wp-content/uploads/2021/04/Planning-Guidelines-and-Design-Standards-for-Checked-Baggage-Inspection-Systems-V7.0.pdf}{Planning Guidelines and Design Standards for Checked Baggage Inspection Systems V7.0}
}
\usage{
get_pgds_passenger_bag_factors()
}
\value{
A tibble containing the following columns:
\describe{
  \item{airline}{Name of the airline operator.}
  \item{carrier}{Carrier code of the airline operator.}
  \item{load_factor}{Load factor for the airline, representing the percentage of seats filled.}
  \item{check_bag_factor}{Percentage of parties checking bags pre-gate.}
  \item{avg_num_bags}{Average number of checked bags per passenger.}
}
}
\description{
This function creates a data frame containing passenger and baggage factors for various airlines.
The data includes load factors, the percentage of parties checking bags pre-gate, and the average
number of checked bags per passenger. The data is sourced from the Planning Guidelines and Design
Standards for Checked Baggage Inspection Systems V7.0.
}
\examples{
\dontrun{
get_pgds_passenger_bag_factors()
}
}
