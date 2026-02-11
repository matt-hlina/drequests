############################################################################
# CHS table functions
############################################################################

#' Table functions
#'
#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by Criminal
#' History Score.
#' @keywords internal
#' @name table_functions
#'
#'

total_cases_by_chs <- function(data_frame) {

  total_cases <- data_frame %>%
    dplyr::group_by(history) %>%
    dplyr::summarize(N = n(), .groups = "drop") %>%
    dplyr::mutate(history = as.character(history)) %>%
    tidyr::complete(history = as.character(0:6), fill = list(N = 0)) %>%
    dplyr::mutate(
      history = factor(history, levels = as.character(0:6)),
      N = as.factor(N)
    ) %>%
    dplyr::arrange(history)

  # Total cases
  total_case <- data_frame %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(
      history = "Total",
      N = as.factor(N)) %>%
    dplyr::select(history, N)

  # Data frame of 100% eight times
  one_hundred_percent <- data.frame(
    history = c("0", "1", "2", "3", "4", "5", "6", "Total"),
    N = rep(paste0(format(100.0, nsmall = 1), "%"), 8)
  )

  # Combine total cases for table
  table_total_cases <- dplyr::bind_rows(total_cases,
                                        total_case,
                                        one_hundred_percent) %>%
    dplyr::arrange(history) %>%
    dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(N, "%"), 1, 0))

  return(table_total_cases)
}


#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by Criminal
#' History Score and presumptive disposition.
#'
#' @keywords internal
#' @name table_functions

pres_disp_cases <- function(data_frame) {

    print("Rows received:")
    print(nrow(data_frame))


  pres_disp <- data_frame %>%
    dplyr::count(history, presumpt) %>%
    dplyr::mutate(
      history = factor(history, levels = as.character(0:6)),
      presumpt = if_else(presumpt == 1, "Stay", "Commit"),
      presumpt = factor(presumpt, levels = c("Stay", "Commit"))
    ) %>%
    tidyr::complete(history, presumpt, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = presumpt, values_from = n, values_fill = 0)

  # Totals row (count & percentage)
  total_pres_disp <- data.frame(
    history = c("Total", "Total"),
    Stay = c(
      as.character(sum(pres_disp$Stay)),
      paste0(format(round(sum(pres_disp$Stay) / (sum(pres_disp$Stay) + sum(pres_disp$Commit)) * 100, 1), nsmall = 1), "%")
    ),
    Commit = c(
      as.character(sum(pres_disp$Commit)),
      paste0(format(round(sum(pres_disp$Commit) / (sum(pres_disp$Stay) + sum(pres_disp$Commit)) * 100, 1), nsmall = 1), "%")
    )
  )

  # Percent rows per history
  percent_pres_disp <- pres_disp %>%
    dplyr::mutate(
      Stay = tidyr::replace_na(Stay, 0),
      Commit = tidyr::replace_na(Commit, 0),
      total = Stay + Commit,
      Stay = dplyr::if_else(total == 0, "0.0%", paste0(format(round(Stay / total * 100, 1), nsmall = 1), "%")),
      Commit = dplyr::if_else(total == 0, "0.0%", paste0(format(round(Commit / total * 100, 1), nsmall = 1), "%"))
    ) %>%
    dplyr::select(history, Stay, Commit) %>%
    dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN%", "0.0%", .)))

  pres_disp <- pres_disp %>%
    dplyr::mutate(
      Stay = as.character(Stay),
      Commit = as.character(Commit)
    )

  # Final table
  table_pres_disp <- dplyr::bind_rows(
    pres_disp,
    total_pres_disp,
    percent_pres_disp
  ) %>%
    dplyr::arrange(history) %>%
    dplyr::mutate(
      cases = dplyr::if_else(!stringr::str_detect(Stay, "%") &
                               !stringr::str_detect(Commit, "%"), 1, 0)
    )

  return(list(table_pres_disp = table_pres_disp, pres_disp = pres_disp))
}


#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#' @param pres_disp 'data.frame' of the total cases separated by presumptive disposition.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by Criminal
#' History Score and dispositional departures. Percentages are calculated using presumptive
#' stays as the denominator for aggravated dispositional departures and presumptive
#' commits as the denominator for mitigated dispositional departures.
#'
#' @keywords internal
#' @name table_functions

  disp_dep_cases <- function(data_frame, pres_disp) {

    disp_dep <- data_frame %>%
      dplyr::filter(dispdep %in% c(0, 1, 2)) %>%
      dplyr::mutate(
        history = as.factor(history),
        history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
        dispdep = case_when(
          dispdep == 0 ~ "None",
          dispdep == 1 ~ "Aggravated",
          dispdep == 2 ~ "Mitigated",
          TRUE ~ "99999"
        ),
        dispdep = factor(dispdep, levels = c("None", "Aggravated", "Mitigated"))
      ) %>%
      dplyr::count(history, dispdep) %>%
      tidyr::complete(history, dispdep, fill = list(n = 0)) %>%
      tidyr::pivot_wider(names_from = dispdep, values_from = n, values_fill = 0)

    pres_disp <- pres_disp %>%
      dplyr::mutate(dplyr::across(c(Stay, Commit), ~ as.numeric(as.character(.))))

    total_disp_dep_cases <- data.frame(
      history = c("Total", "Total"),
      None = c(as.character(sum(disp_dep$None)),
               as.character(paste0(format(round(sum(disp_dep$None) /
                                                  (sum(disp_dep$None) +
                                                     sum(disp_dep$Aggravated) +
                                                     sum(disp_dep$Mitigated)) * 100, 1),
                                          nsmall = 1), "%"))),
      Aggravated = c(as.character(sum(disp_dep$Aggravated)),
                     as.character(paste0(format(round(sum(disp_dep$Aggravated) /
                                                        sum(pres_disp$Stay) * 100, 1),
                                                nsmall = 1), "%"))),
      Mitigated = c(as.character(sum(disp_dep$Mitigated)),
                    as.character(paste0(format(round(sum(disp_dep$Mitigated) /
                                                       sum(pres_disp$Commit) * 100, 1),
                                               nsmall = 1), "%")))
    ) %>%
    dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN%", "0.0%", .)))

    disp_dep_percentages <- disp_dep %>%
      dplyr::mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                    aggravated_percent = paste0(format(round(Aggravated / pres_disp$Stay * 100, 1), nsmall = 1), "%"),
                    mitigated_percent = paste0(format(round(Mitigated / pres_disp$Commit * 100, 1), nsmall = 1), "%")) %>%
      dplyr::select(history,
                    "None" = none_percent,
                    "Aggravated" = aggravated_percent,
                    "Mitigated" = mitigated_percent) %>%
      dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN%", "0.0%", .)))

    disp_dep <- disp_dep %>%
      dplyr::mutate(None = as.factor(None),
                    Aggravated = as.factor(Aggravated),
                    Mitigated = as.factor(Mitigated))

    table_disp_dep <- dplyr::bind_rows(disp_dep,
                                       total_disp_dep_cases,
                                       disp_dep_percentages) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .))) %>%
      dplyr::arrange(history) %>%
      dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(None, "%") &
                                             !stringr::str_detect(Aggravated, "%") &
                                             !stringr::str_detect(Mitigated, "%"),
                                           1, 0)) %>%
      dplyr::rename(none_disp = None, agg_disp = Aggravated, mit_disp = Mitigated)

    return(table_disp_dep)

    }

  #' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
  #'
  #' @return a 'data.frame' of the total cases for the data set grouped by Criminal
  #' History Score and durational departures. Percentages are calculated using
  #' prison sentences only.
  #'
  #' @keywords internal
  #' @name table_functions

  dur_dep_cases <- function(data_frame) {
    dur_dep <- data_frame %>%
      dplyr::filter(prison == 100, durdep %in% c(0, 1, 2)) %>%
      dplyr::mutate(
        durdep = dplyr::recode(as.numeric(durdep), `0` = "None", `1` = "Aggravated", `2` = "Mitigated"),
        durdep = factor(durdep, levels = c("None", "Aggravated", "Mitigated")),
        history = as.factor(history),
        history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6"))
      ) %>%
      dplyr::count(history, durdep) %>%
      tidyr::complete(history, durdep, fill = list(n = 0)) %>%
      tidyr::pivot_wider(names_from = durdep, values_from = n, values_fill = 0)

    dur_dep <- dur_dep %>%
      dplyr::mutate(dplyr::across(c(None, Aggravated, Mitigated), ~ as.numeric(as.character(.))))

    total_dur_dep_cases <- data.frame(
      history = c("Total", "Total"),
      None = c(as.character(sum(dur_dep$None)),
               as.character(paste0(format(round(sum(dur_dep$None) /
                                                  (sum(dur_dep$None) +
                                                     sum(dur_dep$Aggravated) +
                                                     sum(dur_dep$Mitigated)) * 100, 1),
                                          nsmall = 1), "%"))),
      Aggravated = c(as.character(sum(dur_dep$Aggravated)),
                     as.character(paste0(format(round(sum(dur_dep$Aggravated) /
                                                        (sum(dur_dep$None) +
                                                           sum(dur_dep$Aggravated) +
                                                           sum(dur_dep$Mitigated)) * 100, 1),
                                                nsmall = 1), "%"))),
      Mitigated = c(as.character(sum(dur_dep$Mitigated)),
                    as.character(paste0(format(round(sum(dur_dep$Mitigated) /
                                                       (sum(dur_dep$None) +
                                                          sum(dur_dep$Aggravated) +
                                                          sum(dur_dep$Mitigated)) * 100, 1),
                                               nsmall = 1), "%")))
    ) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .)))

    dur_dep_percentages <- dur_dep %>%
      dplyr::mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                    aggravated_percent = paste0(format(round(Aggravated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                    mitigated_percent = paste0(format(round(Mitigated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%")) %>%
      dplyr::select(history,
                    "None" = none_percent,
                    "Aggravated" = aggravated_percent,
                    "Mitigated" = mitigated_percent) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .)))

    dur_dep <- dur_dep %>%
      dplyr::mutate(None = as.factor(None),
                    Aggravated = as.factor(Aggravated),
                    Mitigated = as.factor(Mitigated))

    table_dur_dep <- dplyr::bind_rows(dur_dep,
                                      total_dur_dep_cases,
                                      dur_dep_percentages) %>%
      dplyr::arrange(history) %>%
      dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(None, "%") &
                                             !stringr::str_detect(Aggravated, "%") &
                                             !stringr::str_detect(Mitigated, "%"),
                                           1, 0)) %>%
      dplyr::rename(none_dur = None,
                    agg_dur = Aggravated,
                    mit_dur = Mitigated) %>%
      dplyr::mutate(dplyr::across(dplyr::ends_with("_percent"), ~ replace(., is.na(.), "0.0%")))

    return(table_dur_dep)
  }


  #' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
  #'
  #' @return a 'data.frame' of the average prison duration sentenced for each Criminal History Score.
  #'
  #' @keywords internal
  #' @name table_functions

  prison_duration <- function(data_frame) {
    pris_dur <- data_frame %>%
      dplyr::filter(
        prison == 100
      ) %>%
      dplyr::group_by(history) %>%
      dplyr::mutate(
        history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
      ) %>%
      dplyr::summarize(avg_dur = format(round(mean(confine), 1))) %>%
      tidyr::complete(history) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~replace_na(.x, '0')),
             cases = 1,
             dplyr::across(dplyr::everything(), ~as.character(.x)))

    all_pris_dur <- data_frame %>%
      filter(
        prison == 100
      ) %>%
      summarize(avg_dur = format(round(mean(confine), 1))) %>%
      mutate(history = "Total",
             across(everything(), ~replace_na(.x, '0')),
             cases = 1,
             across(everything(), ~as.character(.x)))

    table_pris_dur <- pris_dur %>%
      bind_rows(all_pris_dur) %>%
      dplyr::mutate(cases = as.numeric(cases))

    return(table_pris_dur)
  }


  #' @param table_total_cases a 'data.frame' of the total cases for the data set grouped by Criminal
  #' History Score.
  #' @param table_pres_disp a 'data.frame' of the total cases for the data set grouped by Criminal
  #' History Score and presumptive disposition.
  #' @param table_disp_dep a 'data.frame' of the total cases for the data set grouped by Criminal
  #' History Score and dispositional departures. Percentages are calculated using presumptive
  #' stays as the denominator for aggravated dispositional departures and presumptive
  #' commits as the denominator for mitigated dispositional departures.
  #' @param table_dur_dep a 'data.frame' of the total cases for the data set grouped by Criminal
  #' History Score and durational departures. Percentages are calculated using
  #' prison sentences only.
  #' @param table_pris_dur a 'data.frame' of the average prison duration sentenced for each
  #' Criminal History Score.
  #'
  #' @return A 'data.frame' all data request table columns combined into one table.
  #'
  #' @keywords internal
  #' @name table_functions

  final_chs_table <- function(table_total_cases, table_pres_disp, table_disp_dep,
                              table_dur_dep, table_pris_dur) {

    final_table <- table_total_cases %>%
      dplyr::left_join(table_pres_disp, by = c("history", "cases")) %>%
      dplyr::left_join(table_disp_dep, by = c("history", "cases")) %>%
      dplyr::left_join(table_dur_dep, by = c("history", "cases")) %>%
      dplyr::left_join(table_pris_dur, by = c("history", "cases")) %>%
      dplyr::select(history,
                    N,
                    Stay,
                    Commit,
                    none_disp,
                    agg_disp,
                    mit_disp,
                    none_dur,
                    agg_dur,
                    mit_dur,
                    avg_dur) %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), ~replace_na(.x, "")))

    return(final_table)

  }



