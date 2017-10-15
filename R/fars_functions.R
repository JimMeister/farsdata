#' ----1-----
#' fars_read: Reads a csv file.
#'
#' @description The function reads a csv file defined by \code{filename} argument and returns
#'              a tibble. If the path is incorrect the function will end and return an error.
#'
#' @param filename Path to the csv file.
#'
#' @return The function returns a tibble based on the specified csv file.
#'
#' @examples
#'
#' \dontrun{
#' tb_accident_2015 <- fars_read("./data/accident_2015.csv.bz2")
#' }
#'
#' @import readr
#' @import dplyr
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

#' ----2-----
#' make_filename: Creates a filename.
#'
#' @description The function creates a filename for a .csv.bz2 file based on the \code{year}
#'              argument in a form "accident_<year>.csv.bz2". It requires a numerical or
#'              integer input otherwise ends and returns an error.
#'
#' @param year A numerical input which is defined as integer indicating a year of a data set.
#'
#' @return Returns a character variable in a format "accident_<year>.csv.bz2".
#'
#' @examples
#' \dontrun{
#' makefilename(2016)
#' }

make_filename <- function(year) {
        year <- as.integer(year)
        sprintf("accident_%d.csv.bz2", year)
}

#' ----3-----
#' fars_read_years: Reads month and year from accident files
#'
#' @description The function accepts a vector of years and returns a list of data
#'              frames with month and year columns based on data in "accident_<year>.csv.bz2
#'              files.
#'
#' @param years A vector or list of years in numeric or integer format.
#'
#' @return Returns a list of tibbles with the same number of rows
#'         as the data in "accident_<year>.csv.bz2" files and two columns - month &
#'         year. Returns NULL and a warning if the file does not exist.
#'
#'
#' @import dplyr

fars_read_years <- function(years) {
        lapply(years, function(year) {
                file <- make_filename(year)
                tryCatch({
                        dat <- fars_read(file)
                        dplyr::mutate_(dat, year = ~year) %>%
                             dplyr::select_("MONTH", "year")
                }, error = function(e) {
                        warning("invalid year: ", year)
                        return(NULL)
                })
        })
}

#' ----4-----
#' fars_summarize_years: Counts number of accidents per month & year.
#'
#' @description Based on the user-selected years, the function summarises the number of accidents
#'              in the US on a monthly basis. The accident files need to be in the working
#'              directory.
#'
#' @param years A vector or list of years (numeric or integer) that will be
#' searched in the data
#'
#' @return Returns a tibble in a wide format (pivot) with months in rows and defined
#'         years in columns, using as values the number of accidents. Returns a warning for
#'         every input year that does not exist in the datasets.
#'
#' @examples
#' \dontrun{
#' fars_summarize_years(2015:2016)
#' }
#'
#' @import dplyr
#' @import tidyr
#'
#' @export

fars_summarize_years <- function(years) {
        dat_list <- fars_read_years(years)
        dplyr::bind_rows(dat_list) %>%
                dplyr::group_by_("year", "MONTH") %>%
                dplyr::summarize_(n = ~n()) %>%
                tidyr::spread_("year", "n")
}

#' ----5-----
#' fars_map_state: Creates a visualisation of the accidents on a US state map.
#'
#' @description The function accepts a state ID and year and returns a map visualisation of the accidents.
#'              The state ID has to be an integer or numerical.
#'
#' @param state.num The ID of a US state as specified in the FARS data pack.
#'
#' @param year The selected year the visualisation will be based on.
#'
#' @return Returns a graph of the accidents based on the \code{state.num} and
#'         \code{year} inputs. Returns an error if the state and/ or year do not exist in the
#'         data set.
#'
#'
#' @import dplyr
#' @import maps
#' @import graphics
#'
#' @export

fars_map_state <- function(state.num, year) {
        filename <- make_filename(year)
        data <- fars_read(filename)
        state.num <- as.integer(state.num)

        if(!(state.num %in% unique(data$STATE)))
                stop("invalid STATE number: ", state.num)
        data.sub <- dplyr::filter_(data, ~STATE == state.num)
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
