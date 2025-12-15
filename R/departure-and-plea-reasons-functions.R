############################################################################
# Departure and plea reasons functions
############################################################################

#' Departure reasons function
#'
#' @param data_frame a filtered 'data.frame' of the departure reasons for
#' departures within the data set.
#' @param dep_type the type of departure to filter the data by: mit_disp, mit_dur,
#' agg_disp, or agg_dur.
#'
#' @return a 'data.frame' of the number of reasons and percent composition of departure
#' reasons for the data set.
#' @keywords internal
#' @name dep_reasons_function
#'

dep_reasons <- function(data_frame,
                        dep_type = NULL) {

  if(dep_type == "mit_disp") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$dispdep == 2)
  }

  else if(dep_type == "mit_dur") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$durdep == 2)
  }

  else if(dep_type == "agg_disp") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$dispdep == 1)
  }

  else if(dep_type == "agg_dur") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$durdep == 1)
  }

  # Number of departure cases
  dep_cases <- data_frame %>%
    dplyr::summarize(N = n())

  # Determine data filters and convert dep reason1-4 to long format
  dep_reasons_long <- data_frame %>%
    dplyr::select(reason1,
                  reason2,
                  reason3,
                  reason4) %>%
    dplyr::mutate(dplyr::across(
      c(
        reason1, reason2, reason3, reason4
        ), as_factor
    )) %>%
    tidyr::pivot_longer(
      cols = starts_with("reason"),
      names_to = "reason_number",
      values_to = "reason"
    ) %>%
    dplyr::filter(!is.na(reason), # to remove any NA values and remove reasons2-4 where reason1 == 0
                  !(reason_number == "reason2" & reason == 0),
                  !(reason_number == "reason3" & reason == 0),
                  !(reason_number == "reason4" & reason == 0))

  # Determine the number of each reason and percent composition
  dep_reasons <- dep_reasons_long %>%
    dplyr::group_by(reason) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / dep_cases$N * 100, 1), nsmall = 1), "%")),
                  dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(desc(N))

  return(dep_reasons)
}

#' Departure plea reasons function
#'
#' @param data_frame a filtered 'data.frame' of the departure plea reasons for
#' departures within the data set.
#' @param dep_type the type of departure to filter the data by: mit_disp, mit_dur,
#' agg_disp, or agg_dur.
#'
#' @return a 'data.frame' of the number of reasons and percent composition of departure
#' plea reasons for the data set.
#' @keywords internal
#' @name dep_plea_reasons_function
#'

dep_plea_reasons <- function(data_frame,
                             dep_type = NULL) {

  if(dep_type == "mit_disp") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$dispdep == 2)
  }

  else if(dep_type == "mit_dur") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$durdep == 2)
  }

  else if(dep_type == "agg_disp") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$dispdep == 1)
  }

  else if(dep_type == "agg_dur") {
    data_frame <- data_frame %>% dplyr::filter(data_frame$durdep == 1)
  }

  # Number of departure cases
  dep_cases <- data_frame %>%
    dplyr::summarize(N = n())

  disp_plea_df <- data_frame %>%
    dplyr::mutate(unknown = dplyr::if_else(preason1 == 0 | preason2 == 0 | preason3 == 0,
                                           1, 0),
                  accepts = dplyr::if_else(preason1 == 440 | preason2 == 440 | preason3 == 440,
                                           1, 0),
                  pros_objects = dplyr::if_else(preason1 == 441 | preason2 == 441 | preason3 == 441,
                                                1, 0),
                  pros_not_object = dplyr::if_else(preason1 == 442 | preason2 == 442 | preason3 == 442,
                                                   1, 0),
                  plea_deal = dplyr::if_else(preason1 == 470 | preason2 == 470 | preason3 == 470,
                                             1, 0),
                  pros_motion = dplyr::if_else(preason1 == 599 | preason2 == 599 | preason3 == 599,
                                               1, 0),
                  pr_accepts = dplyr::if_else(accepts == 1 | pros_not_object == 1 | plea_deal == 1 |
                                                pros_motion == 1, 1, 0),
                  pr_objects = dplyr::if_else(pros_objects == 1, 1, 0),
                  pr_unknown = dplyr::if_else(unknown == 1 & accepts == 0 & pros_objects == 0 &
                                                pros_not_object == 0 & plea_deal == 0 & pros_motion == 0, 1, 0),
                  plea_reasons_grp = dplyr::case_when(
                    pr_accepts == 1 ~ "Prosecutor did not Object/Plea Negotiation",
                    pr_objects == 1 ~ "Prosecutor Objects",
                    pr_unknown == 1 ~ "Plea Reason Unknown",
                    TRUE ~ "-99999999999999999999"),
                  plea_reasons_grp = factor(
                    plea_reasons_grp,
                    levels = c(
                      "Prosecutor did not Object/Plea Negotiation",
                      "Prosecutor Objects",
                      "Plea Reason Unknown",
                      "-99999999999999999999")
                  )
    ) %>%
    dplyr::group_by(plea_reasons_grp) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / dep_cases$N * 100, 1), nsmall = 1), "%")),
                  dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(plea_reasons_grp)

  return(disp_plea_df)
}


