############################################################################
# Function for selecting cases requested
############################################################################

#' Filter function
#'
#' @param data 'data.frame' of Sentencing Guidelines public data set.
#' @param filters a named list of all arguments passed to the function.
#'
#' @return A filtered data frame.
#' @keywords internal
#' @name filter_function

requested_cases <- function(data, filters) {
  df1 <- data

  for (name in names(filters)) {
    value <- filters[[name]]

    if (is.null(value)) next

    if (stringr::str_ends(name, "_min")) {
      var <- stringr::str_remove(name, "_min$")
      df1 <- df1 %>%
        dplyr::filter(.data[[var]] >= value)

    } else if (stringr::str_ends(name, "_max")) {
      var <- stringr::str_remove(name, "_max$")
      df1 <- df1 %>%
        dplyr::filter(.data[[var]] <= value)

    } else if (length(value) > 1) {
      df1 <- df1 %>%
        dplyr::filter(.data[[name]] %in% value)

    } else if (name == "reason") {
      df1 <- df1 %>%
        dplyr::mutate(reason_match = dplyr::if_else(
          reason1 == value | reason2 == value | reason3 == value | reason4 == value,
          1, 0
        )) %>%
        dplyr::filter(reason_match == 1) %>%
        dplyr::select(-reason_match)

    } else if (name == "preason") {
      df1 <- df1 %>%
        dplyr::mutate(preason_match = dplyr::if_else(
          preason1 == value | preason2 == value | preason3 == value,
          1, 0
        )) %>%
        dplyr::filter(preason_match == 1) %>%
        dplyr::select(-preason_match)

    } else if (name == "any_of") {

      for (cond in value) {
        df1 <- df1 %>%
          dplyr::filter(
            dplyr::if_any(
              dplyr::all_of(cond$vars),
              ~ .x %in% cond$value
            )
          )
      }

    } else {
      df1 <- df1 %>% dplyr::filter(.data[[name]] == value)
    }
  }

  return(df1)
}
