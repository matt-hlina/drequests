############################################################################
# Function used to identify the number of non-public cases removed from data
############################################################################

#' Non-public function
#'
#' @param data a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return The number of non-public cases.
#' @keywords internal
#' @name non_public_function
#'

non_public_cases <- function(data, filters) {
  df1 <- data

  for (name in names(filters)) {
    value <- filters[[name]]

    if (is.null(value)) next


    if (name == "any_of") {

      for (cond in value) {
        df1 <- df1 %>%
          dplyr::filter(
            dplyr::if_any(
              dplyr::all_of(cond$vars),
              ~ .x %in% cond$value
            )
          )
      }

    } else if (stringr::str_ends(name, "_min")) {
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


    } else {
      df1 <- df1 %>% dplyr::filter(.data[[name]] == value)
    }
  }

  # for selecting non-public cases only
  if ("not_public" %in% names(df1)) {
    df1 <- df1 %>%
      dplyr::filter(not_public != 0)
  }

  else if (!"not_public" %in% names(df1)) {
    print("The variable not_public was not found in the data. Use data where
          non-public data can be identified.")
  }

  print("Non-public cases:")
  print(nrow(df1))

}



