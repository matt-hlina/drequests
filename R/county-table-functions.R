############################################################################
# County table functions
############################################################################

#' Table functions
#'
#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by county.
#' @keywords internal
#' @name table_functions
#'
#'

total_cases_by_county <- function(data_frame) {

  total_cases <- data_frame %>%
    dplyr::group_by(county) %>%
    dplyr::summarize(N = n(), .groups = "drop") %>%
    dplyr::mutate(percent = paste0(format(round(N / sum(N) * 100, 1), nsmall = 1), "%"),
                  county = haven::as_factor(county)
    )

  # Total cases
  total_case <- data_frame %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(
      county = "zTotal",
      percent = paste0(format(round(N / sum(N) * 100, 1), nsmall = 1), "%")) %>%
    dplyr::select(county,
                  N,
                  percent)

  # Combine total cases for table
  table_total_cases <- dplyr::bind_rows(total_cases,
                                        total_case) %>%
    dplyr::arrange(county)

  return(table_total_cases)
}


#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by county and presumptive disposition.
#'
#' @keywords internal
#' @name table_functions

pres_disp_cases_by_county <- function(data_frame) {

  print("Rows received:")
  print(nrow(data_frame))


  pres_disp_county <- data_frame %>%
    dplyr::count(county, presumpt) %>%
    dplyr::mutate(
      presumpt = as_factor(presumpt)
    ) %>%
    tidyr::pivot_wider(
      names_from = presumpt,
      values_from = n,
      values_fill = 0
    ) %>%
    dplyr::mutate(county = haven::as_factor(county)) %>%
    dplyr::mutate(stay_percent = if_else(Stay == 0 & Commit == 0,
                                         "-",
                                         as.character(paste0(format(round(Stay / (Stay + Commit) * 100, 1), nsmall = 1), "%"))),
                  commit_percent = if_else(Stay == 0 & Commit == 0,
                                           "-",
                                           as.character(paste0(format(round(Commit / (Stay + Commit) * 100, 1), nsmall = 1), "%")))
    ) %>%
    dplyr::select(county, Stay, stay_percent, Commit, commit_percent)

  # Totals row (count & percentage)
  total_pres_disp <- data.frame(
    county = c("zTotal"),
    Stay = sum(pres_disp_county$Stay),
    stay_percent = as.character(paste0(format(round(sum(pres_disp_county$Stay) /
                                                      (sum(pres_disp_county$Stay) +
                                                         sum(pres_disp_county$Commit)) * 100, 1),
                                              nsmall = 1), "%")),
    Commit = sum(pres_disp_county$Commit),
    commit_percent = as.character(paste0(format(round(sum(pres_disp_county$Commit) /
                                                        (sum(pres_disp_county$Stay) +
                                                           sum(pres_disp_county$Commit)) * 100, 1),
                                                nsmall = 1), "%"))
  )

  # Final table
  table_pres_disp <- dplyr::bind_rows(
    pres_disp_county,
    total_pres_disp
  ) %>%
    dplyr::arrange(county)


  return(list(table_pres_disp = table_pres_disp, pres_disp_county = pres_disp_county))
}


#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#' @param pres_disp 'data.frame' of the total cases separated by presumptive disposition.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by county
#' and dispositional departures. Percentages are calculated using presumptive
#' stays as the denominator for aggravated dispositional departures and presumptive
#' commits as the denominator for mitigated dispositional departures.
#'
#' @keywords internal
#' @name table_functions

disp_dep_cases_by_county <- function(data_frame, pres_disp) {

  disp_dep <- data_frame %>%
    dplyr::count(county, dispdep) %>%
    dplyr::mutate(
      dispdep = haven::as_factor(dispdep),
      dispdep = factor(dispdep, levels = c("None", "Aggravated", "Mitigated"))  # enforce all possible values
    ) %>%
    tidyr::complete(county, dispdep, fill = list(n = 0)) %>%  # add missing combinations
    tidyr::pivot_wider(
      names_from = dispdep,
      values_from = n,
      values_fill = 0
    ) %>%
    dplyr::mutate(
      county = as_factor(county)) %>%
    dplyr::mutate(none_disp_percent = if_else(None == 0 & Aggravated == 0 & Mitigated == 0,
                                              "-",
                                              as.character(paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1),
                                                                         nsmall = 1), "%"))),
                  agg_disp_percent = if_else(Aggravated == 0 & pres_disp$Stay == 0,
                                             "-",
                                             as.character(paste0(format(round(Aggravated / pres_disp$Stay * 100, 1),
                                                                        nsmall = 1), "%"))),
                  mit_disp_percent = if_else(Mitigated == 0 & pres_disp$Commit == 0,
                                             "-",
                                             as.character(paste0(format(round(Mitigated / pres_disp$Commit * 100, 1),
                                                                        nsmall = 1), "%")))
    ) %>%
    dplyr::select(county, None, none_disp_percent, Aggravated, agg_disp_percent, Mitigated, mit_disp_percent)

  ####################################
  total_disp_dep_cases <- data.frame(
    county = c("zTotal"),
    None = sum(disp_dep$None),
    none_disp_percent = as.character(paste0(format(round(sum(disp_dep$None) /
                                                           (sum(disp_dep$None) +
                                                              sum(disp_dep$Aggravated) +
                                                              sum(disp_dep$Mitigated)) * 100, 1),
                                                   nsmall = 1), "%")),
    Aggravated = sum(disp_dep$Aggravated),
    agg_disp_percent = as.character(paste0(format(round(sum(disp_dep$Aggravated) /
                                                          sum(pres_disp$Stay) * 100, 1),
                                                  nsmall = 1), "%")),
    Mitigated = sum(disp_dep$Mitigated),
    mit_disp_percent = as.character(paste0(format(round(sum(disp_dep$Mitigated) /
                                                          sum(pres_disp$Commit) * 100, 1),
                                                  nsmall = 1), "%"))
  )

  ###########################################
  table_disp_dep <- dplyr::bind_rows(disp_dep,
                                     total_disp_dep_cases) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .))) %>%
    dplyr::arrange(county) %>%
    dplyr::rename(none_disp = None, agg_disp = Aggravated, mit_disp = Mitigated)


  return(table_disp_dep)

}

#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the total cases for the data set grouped by county
#' and durational departures. Percentages are calculated using prison sentences only.
#'
#' @keywords internal
#' @name table_functions

dur_dep_cases_by_county <- function(data_frame, table) {

  counties_to_include <- table %>%
    filter(county != "zTotal") %>%
    select(county)

  #########################
  dur_dep <- data_frame %>%
    dplyr::filter(prison == 100, durdep %in% c(0, 1, 2)) %>%
    dplyr::mutate(
      county = haven::as_factor(county),
      durdep = haven::as_factor(durdep)
    ) %>%
    dplyr::count(county, durdep) %>%
    tidyr::complete(county = counties_to_include$county,
                    durdep,
                    fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = durdep, values_from = n, values_fill = 0) %>%
    dplyr::mutate(none_dur_percent = if_else(None == 0 & Aggravated == 0 & Mitigated == 0,
                                             "",
                                             paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1),
                                                           nsmall = 1), "%")),
                  agg_dur_percent = if_else(None == 0 & Aggravated == 0 & Mitigated == 0,
                                            "",
                                            paste0(format(round(Aggravated / (None + Aggravated + Mitigated) * 100, 1),
                                                          nsmall = 1), "%")),
                  mit_dur_percent = if_else(None == 0 & Aggravated == 0 & Mitigated == 0,
                                            "",
                                            paste0(format(round(Mitigated / (None + Aggravated + Mitigated) * 100, 1),
                                                          nsmall = 1), "%"))
    ) %>%
    dplyr::mutate(across(ends_with("_percent"),
                         ~ ifelse(is.nan(as.numeric(gsub("%", "", .))), "0.0%", .))) %>%
    dplyr::select(county, None, none_dur_percent, Aggravated, agg_dur_percent,
                  Mitigated, mit_dur_percent)

  ##################################################
  total_dur_dep_cases <- data.frame(
    county = c("zTotal"),
    None = sum(dur_dep_cases$None),
    none_dur_percent = as.character(paste0(format(round(sum(dur_dep_cases$None) /
                                                          (sum(dur_dep_cases$None) +
                                                             sum(dur_dep_cases$Aggravated) +
                                                             sum(dur_dep_cases$Mitigated)) * 100, 1),
                                                  nsmall = 1), "%")),
    Aggravated = sum(dur_dep_cases$Aggravated),
    agg_dur_percent = as.character(paste0(format(round(sum(dur_dep_cases$Aggravated) /
                                                         (sum(dur_dep_cases$None) +
                                                            sum(dur_dep_cases$Aggravated) +
                                                            sum(dur_dep_cases$Mitigated)) * 100, 1),
                                                 nsmall = 1), "%")),
    Mitigated = sum(dur_dep_cases$Mitigated),
    mit_dur_percent = as.character(paste0(format(round(sum(dur_dep_cases$Mitigated) /
                                                         (sum(dur_dep_cases$None) +
                                                            sum(dur_dep_cases$Aggravated) +
                                                            sum(dur_dep_cases$Mitigated)) * 100, 1),
                                                 nsmall = 1), "%"))
  )

  table_dur_dep <- dplyr::bind_rows(dur_dep,
                                    total_dur_dep_cases) %>%
    dplyr::arrange(county) %>%
    dplyr::mutate(across(ends_with("_percent"),
                         ~ ifelse(. == "", "-", .))) %>%
    dplyr::rename(none_dur = None, agg_dur = Aggravated, mit_dur = Mitigated)

  return(table_dur_dep)
}


#' @param data_frame a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return a 'data.frame' of the average prison duration sentenced for each county.
#'
#' @keywords internal
#' @name table_functions

prison_duration_by_county <- function(data_frame, table) {


  counties_to_include <- table %>%
    filter(county != "zTotal") %>%
    select(county)

  ##########################
  pris_dur <- data_frame %>%
    dplyr::filter(
      prison == 100
    ) %>%
    dplyr::mutate(
      county = haven::as_factor(county)
    ) %>%
    dplyr::group_by(county) %>%
    dplyr::summarize(avg_dur = format(round(mean(confine), 1))
    ) %>%
    tidyr::complete(county = counties_to_include$county) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~replace_na(.x, '0')))

  all_pris_dur <- data_frame %>%
    filter(
      prison == 100
    ) %>%
    summarize(avg_dur = format(round(mean(confine), 1))) %>%
    mutate(county = "zTotal",
           across(everything(), ~replace_na(.x, '0')))

  table_pris_dur <- pris_dur %>%
    bind_rows(all_pris_dur)

  return(table_pris_dur)
}


#' @param table_total_cases a 'data.frame' of the total cases for the data set grouped by county.
#' @param table_pres_disp a 'data.frame' of the total cases for the data set grouped by county
#' and presumptive disposition.
#' @param table_disp_dep a 'data.frame' of the total cases for the data set grouped by county
#' and dispositional departures. Percentages are calculated using presumptive
#' stays as the denominator for aggravated dispositional departures and presumptive
#' commits as the denominator for mitigated dispositional departures.
#' @param table_dur_dep a 'data.frame' of the total cases for the data set grouped by county
#' and durational departures. Percentages are calculated using prison sentences only.
#' @param table_pris_dur a 'data.frame' of the average prison duration sentenced for each
#' county.
#'
#' @return A 'data.frame' all data request table columns combined into one table.
#'
#' @keywords internal
#' @name table_functions

final_table_by_county <- function(table_total_cases, table_pres_disp, table_disp_dep,
                                  table_dur_dep, table_pris_dur) {

  final_table <- table_total_cases %>%
    dplyr::left_join(table_pres_disp, by = c("county")) %>%
    dplyr::left_join(table_disp_dep, by = c("county")) %>%
    dplyr::left_join(table_dur_dep, by = c("county")) %>%
    dplyr::left_join(table_pris_dur, by = c("county")) %>%
    dplyr::select(county,
                  N,
                  Stay,
                  stay_percent,
                  Commit,
                  commit_percent,
                  none_disp,
                  none_disp_percent,
                  agg_disp,
                  agg_disp_percent,
                  mit_disp,
                  mit_disp_percent,
                  none_dur,
                  none_dur_percent,
                  agg_dur,
                  agg_dur_percent,
                  mit_dur,
                  mit_dur_percent,
                  avg_dur) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~replace_na(.x, "")))

  return(final_table)

}
