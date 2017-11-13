#'Reads CSV file into R
#'
#'This is a function that takes a filename as a parameter which is supposed
#'to be a CSV file and reads that file into R and then converts it to a
#'tibble dataframe.
#'
#'@param filename A file path of a CSV file to be converted
#'
#'@return This function returns a tibble dataframe from a CSV file.
#'If the file path is wrong or the file does not exist then an error message
#'will return.
#'
#'@importFrom readr read_csv
#'@importFrom dplyr tbl_df
#'
#'@examples
#'acc_2015 <- fars_read("path/accident_2015.csv")
#'acc_2014 <- fars_read("path/accident_2014.csv")
#'acc_2013 <- fars_read("path/accident_2013.csv")
#'
#'@export
fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#'Creates filename for CSV file
#'
#'This is a function that takes a year parameter and creates a filename for
#'Fatality Analysis Reporting System data.
#'
#'@param year is an integer that is a year reported by the Fatality Analysis Reporting System
#'
#'@return This function returns a tibble filepath for a CSV file containing data for the
#'Fatality Analysis Reporting System
#'
#'@examples
#'make_filename(2013)
#'make_filename(2014)
#'make_filename(2015)
#'
#'@export
make_filename <- function(year) {
  year <- as.integer(year)
  file <- sprintf("accident_%d.csv.bz2", year)
  system.file("extdata", file, package = "fars2")
}

#'This function return a list of dataframes
#'
#'Applied accross multiple or single number of CSV files based on what
#'is specified in the 'years' paramater; this function reads in CSV file(s)
#'and converts them into a tibble dataframe and then selects only the 'MONTH'
#'and 'year' columns.
#'
#'@param years is a vector of integers or single integer representing
#'years reported on by the Fatality Analysis Reporting System.
#'
#'@return This function returns a list of tibble dataframes containing
#'the month and year varibles from the accident files from the
#'Fatality Analysis Reporting System. The dataframes are compliled
#'based on the years specified in a vector from the 'years' parameter.
#'If only one year is provided that a simple tibble dataframe is returned.
#'
#'
#'@importFrom dplyr %>% mutate select
#'
#'@examples
#'acc_years <- fars_read_years(c(2013, 2014, 2015))
#'
#'@export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate(dat, year = year) %>%
        dplyr::select(MONTH, year)
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#'This function summarizes a dataframe from CSV file
#'
#'This function summarizes the number of accidents reported by the Fatality
#'Analysis Reporting System by year and month.
#'
#'@param years is a vector of integers or single integer representing
#'years reported on by the Fatality Analysis Reporting System.
#'
#'@return This function returns a dataframe that summarizes the three
#'dataframe objects from the list created by the 'fars_read_years' function.
#'The dataframe contains four columns and twelve rows. The 'MONTH' column
#'has the month number and the three year columns contain the number of
#'fatalities in the corresponding month.
#'
#'
#'
#'@importFrom dplyr %>% bind_rows group_by summarize
#'@importFrom tidyr spread
#'
#'@examples
#'acc_years <- fars_summarize_years(c(2013, 2014, 2015))
#'
#'@export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by(year, MONTH) %>%
    dplyr::summarize(n = n()) %>%
    tidyr::spread(year, n)
}

#'This function produces a map of accidents in a single state
#'
#'@param state.num is an integer indicating which state's data will be
#'filtered and then mapped. Parameter can be any integer 1-56.
#'@param year is a single integer representing years reported on by
#'the Fatality Analysis Reporting System. Parameter can years 2013-2015.
#'
#'@return This function returns a map showing the location of accidents in
#'a single state from a single year based on data from the the Fatality
#'Analysis Reporting System.
#'
#'@importFrom dplyr filter
#'@importFrom maps map
#'@importFrom graphics points
#'
#'@examples
#'acc_years <- fars_map_state(10, 2015)
#'
#'@export
fars_map_state <- function(state.num, year) {
  filename <- make_filename(year)
  data <- fars_read(filename)
  state.num <- as.integer(state.num)

  if(!(state.num %in% unique(data$STATE)))
    stop("invalid STATE number: ", state.num)
  data.sub <- dplyr::filter(data, STATE == state.num)
  if(nrow(data.sub) == 0L) {
    message("no accidents to plot")
    return(invisible(NULL))
  }
  is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
  is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
  with(data.sub, {
    maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
              xlim = range(LONGITUD, na.rm = TRUE))
    graphics::points(LONGITUD, LATITUDE, pch = 46)
  })
}





