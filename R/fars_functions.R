# Mastering Software Development in R Specialization: Course 3 - Building R Packages
# Week 2 - Peer-graded Assignment: Documenting Code
# ..................................................................................


#' \code{fars_read} function reads a csv file as a dataframe
#' the function takes a string argurment which is the name of the data filename
#' if it does not exist, it will generate an error like this: "File 'filename' does not exist"
#' otherwise, the function will recognise the data and will read the filename as a dataframe
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A string character that represents a data filename in csv format to be read
#'
#' @return A data frame read from the filename argument using the dplyr package. If it does not exists, it will throw an error
#'
#' @examples
#' \dontrun{fars_read("accident_2015.csv.bz2")}
#'
#' @export
fars_read <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        data <- suppressMessages({
                readr::read_csv(filename, progress = FALSE)
        })
        dplyr::tbl_df(data)
}





#' \code{make_filename} function allows to create a filename
#' the function takes a numeric or string argument (a year) and a csv filename is created
#' based on that year. the function guarantees that the argument be converted to as integer
#'
#' @param year A numeric or string argument which is the year for the future filename
#'
#' @return A combination of text and the year argument which yields the csv filename as a text type
#'
#' @examples
#' \dontrun{make_filename(2015)}
#'
#' @export
make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}





#' \code{fars_read_years} function allows to read a vector of years
#' the function uses the lapply function and the two functions created above
#' to create a new csv filename for each year and then test if the filename exists
#' then by using the dplyr package only 2 columns will be visible: month and year
#' if the year is invalid, it will return "NULL"
#'
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#'
#' @param years A vector of years to be read
#'
#' @return returns a list of dataframes with only the month and year columns
#'
#' @examples
#' \dontrun{fars_read_years(c(2013, 2014, 2015))}
#'
#' @export
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





#' \code{fars_summarize_years} function allows to summarise the data by years
#' the function takes a list of vectors (years) which are passed in the third function above
#' then dataframes are binded to be summarised the number of cases per year
#' from a longitudinal format to wide format is converted by using the spead function
#'
#' @importFrom dplyr bind_rows group_by summarize
#' @importFrom tidyr spread
#' @importFrom magrittr %>%
#'
#' @param years A vector of years to be read
#'
#' @return returns a dataframe with the number of cases summarised by year in a wide format
#'
#' @examples
#' \dontrun{fars_summarize_years(c(2013, 2014, 2015))}
#'
#' @export
fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by(year, MONTH) %>%
                dplyr::summarize(n = n()) %>%
                tidyr::spread(year, n)
}





#' \code{fars_map_state} function allows to plot cases on a map based on a state number and specfic year data
#' the function takes two arguments: a state number and year which serve as inputs in the first 2 functions
#' created above. to make sure the remaining functions work appropiately, state number is converted as integer
#' then, if the state number is not included in the data, an error will appear as "invalid STATE number xxxxx"
#' otherwise the data will be filtered with that state number. in case the filtered data does not have any cases
#' a message will appear that no accidents can be ploted. If there cases, longitude and latitude data are assigned
#' to cases with NULL longitude and latitude. finally, a map with cases ploted is created based on the state
#' and year specified as arguments
#'
#' @importFrom dplyr filter
#' @importFrom maps map
#' @importFrom graphics points
#'
#' @param state.num A state number to plot cases in a map
#' @param year A year of data to plot cases in a map
#'
#' @return a graphic that shows the map of cases for a specific state number and year
#'
#' @examples
#' \dontrun{fars_map_state(1, 2015)}
#'
#' @export
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
