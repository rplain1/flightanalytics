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
#' @source \href{https://iabsc.org/wp-content/uploads/2021/04/Planning-Guidelines-and-Design-Standards-for-Checked-Baggage-Inspection-Systems-V7.0.pdf}{Planning Guidelines and Design Standards for Checked Baggage Inspection Systems V7.0}
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

#' Clean Arrival Curve Data
#'
#' This function cleans the arrival curve data by extracting numeric values from the 'minutes_prior' column,
#' grouping the data by minute intervals, summarizing the values, and scaling them to a percentage format.
#'
#' @param arrival_curve A data frame containing arrival curve data with a 'minutes_prior' column.
#' @return A cleaned data frame with the following columns:
#'   \itemize{
#'     \item \code{minutes_prior}: Minutes prior to arrival time.
#'     \item Other columns representing aggregated and scaled values.
#'   }
#'
#' @details
#' This function extracts numeric values from the 'minutes_prior' column using regular expressions,
#' groups the data by minute intervals, sums the values within each interval, and scales the summed values
#' to a percentage format by dividing them by 100.
#'
#' @examples
#' arrival_curve <- data.frame(minutes_prior = c("10", "20", "30"),
#'                             value1 = c(5, 10, 15),
#'                             value2 = c(20, 30, 40))
#' clean_arrival_curve(arrival_curve)
#'
#' @export
#' @importFrom rlang .data
clean_arrival_curve <- function(arrival_curve) {

  if (!("minutes_prior" %in% colnames(arrival_curve))) {
    stop("Error: 'minutes_prior' column not found in the input data frame.")
  }

  arrival_curve |>
    dplyr::mutate(minutes_prior = stringr::str_extract(.data$minutes_prior, '\\d+') |> as.numeric()) |>
    dplyr::group_by(.data$minutes_prior) |>
    dplyr::summarise(
      dplyr::across(dplyr::everything(), sum)
    ) |>
    tidyr::pivot_longer(cols = -.data$minutes_prior, values_to = 'value') |>
    dplyr::mutate(value = .data$value / 100)

}

#' Create KDE Samples from Arrival Curve
#'
#' This function takes the output from clean_arrival_curve(get_pgds_arrival_curve()) and creates a KDE sample to join to the flight schedule.
#'
#' @param arrival_curve_output A data frame resulting from clean_arrival_curve(get_pgds_arrival_curve()).
#' @return A data frame with KDE samples and additional metadata columns.
#' @export
#' @importFrom rlang .data
create_arrival_curve_kde <- function(arrival_curve_output) {

  arrival_curve_kde <- arrival_curve_output |>
    dplyr::group_by(.data$name) |>
    tidyr::nest() |>
    dplyr::ungroup() |>
    # Sample 1000 values from 10-minute increments
    dplyr::mutate(
      samples = purrr::map(.x = .data$data, ~sample(.x$minutes_prior, 1000, replace = TRUE, prob = .x$value))
    ) |>
    # Apply KDE function
    dplyr::mutate(
      .kde = purrr::map(.x = .data$samples, ~ks::kde(.x))
    ) |>
    dplyr::mutate(
      peak = .data$name == 'peak_domestic_8am',
      domestic = !stringr::str_detect(.data$name, 'international')
    )

  return(arrival_curve_kde)
}


#' Get PGDS Passenger and Bag Factors
#'
#' This function creates a data frame containing passenger and baggage factors for various airlines.
#' The data includes load factors, the percentage of parties checking bags pre-gate, and the average
#' number of checked bags per passenger. The data is sourced from the Planning Guidelines and Design
#' Standards for Checked Baggage Inspection Systems V7.0.
#'
#' @return A tibble containing the following columns:
#' \describe{
#'   \item{airline}{Name of the airline operator.}
#'   \item{carrier}{Carrier code of the airline operator.}
#'   \item{load_factor}{Load factor for the airline, representing the percentage of seats filled.}
#'   \item{check_bag_factor}{Percentage of parties checking bags pre-gate.}
#'   \item{avg_num_bags}{Average number of checked bags per passenger.}
#' }
#' @source \href{https://iabsc.org/wp-content/uploads/2021/04/Planning-Guidelines-and-Design-Standards-for-Checked-Baggage-Inspection-Systems-V7.0.pdf}{Planning Guidelines and Design Standards for Checked Baggage Inspection Systems V7.0}
#' @importFrom tibble tibble
#' @importFrom dplyr mutate across contains
#' @export
#'
#' @examples
#' \dontrun{
#' get_pgds_passenger_bag_factors()
#' }
get_pgds_passenger_bag_factors <- function() {

  # Define the data
  operator_name <- c("Continental Airlines", "Alaska Airlines", "America West Airlines (domestic destinations)",
                     "United Airlines", "XX Airlines", "SkyWest Airlines", "American Airlines",
                     "JetBlue Airways", "Delta Air Lines", "America West Airlines (Mexican destinations)",
                     "Aloha Airlines", "Horizon Air", "Mesa Airlines", "ATA Airlines",
                     "United Express/SkyWest Airlines")
  operator_code <- c("CO", "AS", "HP", "UA", "XX", "OO", "AA", "B6", "DL", "HP", "AQ", "QX", "YV", "TZ", "A296")
  load_factor <- c(96, 98, 83, 85, 77, 91, 98, 90, 89, 83, 85, 60, 85, 85, 91)

  percent_of_parties_checking_pre_gate <- c(75, 80, 84, 45, 34, 79, 90, 90, 92, 100, 97, 77, 51, 64, 66)
  average_number_of_checked_bags_per_passenger <- c(0.79, 0.71, 0.68, 0.87, 0.92, 0.91, 0.71, 0.90, 0.98, 1.30, 1.30, 0.95, 0.96, 1.23, 0.87)

  # Create the data frame
  operator_data <- tibble::tibble(
    airline = operator_name,
    carrier = operator_code,
    load_factor = load_factor,
    check_bag_factor = percent_of_parties_checking_pre_gate,
    avg_num_bags = average_number_of_checked_bags_per_passenger
  ) |>
    dplyr::mutate(dplyr::across(dplyr::contains('factor'), \(x) x/100))

  operator_data

}

#' Clean and join nycflights13 datasets
#'
#' This function joins flights, planes, and airports data from the nycflights13 package.
#' It can also accept data frames as arguments for more flexibility.
#'
#' @param flights A data frame containing flight data. If NULL, the function uses `nycflights13::flights`.
#' @param planes A data frame containing planes data. If NULL, the function uses `nycflights13::planes`.
#' @param airports A data frame containing airports data. If NULL, the function uses `nycflights13::airports`.
#' @return A data frame with joined flights, planes, and airports data, including columns indicating if the airport is domestic US and flight type.
#' @export
#' @importFrom rlang .data
clean_and_join_nycflights <- function(flights = NULL, planes = NULL, airports = NULL) {

  # Ensure necessary packages are loaded
  if (!requireNamespace("nycflights13", quietly = TRUE)) {
    stop("The package 'nycflights13' is required but not installed.")
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The package 'dplyr' is required but not installed.")
  }

  # Load default datasets from nycflights13 package if not provided
  if (is.null(flights)) {
    flights <- nycflights13::flights
  }
  if (is.null(planes)) {
    planes <- nycflights13::planes
  }
  if (is.null(airports)) {
    airports <- nycflights13::airports
  }

  # Define latitude and longitude ranges for the contiguous United States
  lat_min <- 24.396308
  lat_max <- 49.384358
  lon_min <- -125.0
  lon_max <- -66.93457

  # Add column to indicate if airport is in the domestic United States
  airports <- airports |>
    dplyr::mutate(is_domestic_us = (.data$lat >= lat_min & .data$lat <= lat_max & .data$lon >= lon_min & .data$lon <= lon_max))

  # Join the datasets
  flights |>
    dplyr::mutate(dep_dttm = .data$time_hour + lubridate::minutes(.data$minute))
    dplyr::left_join(planes |> dplyr::select(.data$tailnum, .data$seats), by = "tailnum") |>
    dplyr::left_join(airports, by = c("origin" = "faa"), suffix = c("", "_origin")) |>
    dplyr::left_join(airports, by = c("dest" = "faa"), suffix = c("", "_dest")) |>
    dplyr::mutate(flight_type = dplyr::case_when(
      .data$is_domestic_us_dest == FALSE ~ 'international',
      .data$sched_dep_time <= 800 ~ 'peak_domestic_8am',
      TRUE ~ 'off_peak_domestic'
    )) |>
    dplyr::group_by(.data$carrier) |>
    dplyr::mutate(seats = dplyr::if_else(is.na(.data$seats), stats::median(.data$seats, na.rm = TRUE), .data$seats)) |>
    dplyr::ungroup()


}










