#' Get Passenger and Baggage Arrival Curve Data
#'
#' This function returns a data frame containing arrival curve data for passengers and baggage.
#'
#' @return A data frame with the following columns:
#' \itemize{
#'   \item \code{minutes_prior}: Minutes prior to arrival time.
#'   \item \code{peak_domestic_8am}: Arrival curve data for peak domestic flights at 8 AM.
#'   \item \code{off_peak_domestic}: Arrival curve data for off-peak domestic flights.
#'   \item \code{international}: Arrival curve data for international flights.
#' }
#'
#' @examples
#' get_pgds_arrival_curve()
#'
#' @export
get_pgds_arrival_curve <- function() {
  tibble::tibble(
    minutes_prior = rev(c(">240", "240", "230", "220", "210", "200", "190", "180", "170", "160", "150", "140", "130", "120", "110", "100", "90", "80", "70", "60", "50", "40", "30", "20", "10")),
    peak_domestic_8am = c(0.80, 0.26, 0.42, 1.10, 3.08, 6.71, 10.34, 12.87, 13.54,
                          12.79, 11.21, 8.70, 6.13, 4.11, 2.66, 1.69, 1.10, 0.72,
                          0.46, 0.32, 0.22, 0.15, 0.11, 0.08, 0.41),
    off_peak_domestic = c(0.06, 0.30, 0.48, 0.98, 2.10, 4.03, 6.19, 8.16, 9.59,
                          10.25, 10.08, 9.25, 7.95, 6.44, 5.09, 3.94, 3.06, 2.36,
                          1.83, 1.43, 1.14, 0.92, 0.74, 0.62, 3.01),
    international = c(0.22, 0.11, 0.15, 0.28, 0.61, 1.32, 3.08, 5.13, 7.37,
                      8.93, 10.28, 10.69, 9.75, 8.40, 7.12, 5.74, 4.75, 3.81,
                      2.92, 2.17, 1.62, 1.19, 0.90, 0.71, 2.77)
  )
}


