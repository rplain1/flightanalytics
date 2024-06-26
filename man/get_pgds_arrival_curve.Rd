% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/arrival_curve.R
\name{get_pgds_arrival_curve}
\alias{get_pgds_arrival_curve}
\title{Get Passenger and Baggage Arrival Curve Data}
\source{
\href{https://iabsc.org/wp-content/uploads/2021/04/Planning-Guidelines-and-Design-Standards-for-Checked-Baggage-Inspection-Systems-V7.0.pdf}{Planning Guidelines and Design Standards for Checked Baggage Inspection Systems V7.0}
}
\usage{
get_pgds_arrival_curve()
}
\value{
A data frame with the following columns:
\itemize{
  \item \code{minutes_prior}: Minutes prior to arrival time.
  \item \code{peak_domestic_8am}: Arrival curve data for peak domestic flights at 8 AM.
  \item \code{off_peak_domestic}: Arrival curve data for off-peak domestic flights.
  \item \code{international}: Arrival curve data for international flights.
}
}
\description{
This function returns a data frame containing arrival curve data for passengers and baggage.
}
\examples{
get_pgds_arrival_curve()

}
