############################################################################
# Data request function for when sentencing info is asked to be grouped by CHS
############################################################################

#' CHS data request function
#'
#' @param data 'data.frame' of Sentencing Guidelines public data set.
#' @param case_list_name name of the case list file.
#' @param case_list_path pathway where the case list will be saved.
#' @param output_name name of the report output file.
#' @param output_path pathway where the report output will be saved.
#' @param ... represents any variable within the data set. When a variable is used
#' as an argument, the definition of the argument will be used to filter the data.
#' Four arguments that are derivatives of variables exist: variablename_min,
#' variablename_max, reason, and preason. variablename_min and _max are used for
#' selecting a range of data, (i.e., sentyear) and reason and preason are used for
#' selecting departure reasons and plea reasons.
#'
#' @import dplyr
#' @import forcats
#' @import tibble
#' @export
#'
#' @name chs_data_request_function

chs_data_request <- function(data,
                             case_list_name,
                             case_list_path,
                             output_name,
                             output_path,
                             ...) {

  filters <- list(...)

  ###########################################################################
  # Everything below is pipeline
  ###########################################################################

  # Create case list #

  # filter for cases of interest to the requester
  df1 <- requested_cases(data, filters) # function found in filter-function.R

  # select only the necessary variables for the final case list
  data_set <- case_list(data, df1) # function found in final-case-list-function.R

  # save the case list under the directed pathway with the directed name
  create_excel(data_set, case_list_name, case_list_path)

  ########################################################################
  # Create report tables #
  # Total cases by CHS
  total_cases_by_chs <- df1 %>%
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
  total_case <- df1 %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(
      history = "Total",
      N = as.factor(N)) %>%
    dplyr::select(history, N)

  # Data frame of 100% seven times
  one_hundred_percent <- data.frame(
    history = c("0", "1", "2", "3", "4", "5", "6", "Total"),
    N = rep(paste0(format(100.0, nsmall = 1), "%"), 8)
  )

  # Combine total cases for table
  table_total_cases <- dplyr::bind_rows(total_cases_by_chs,
                                        total_case,
                                        one_hundred_percent) %>%
    dplyr::arrange(history) %>%
    dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(N, "%"), 1, 0))

  # Presumptive Disposition
  pres_disp_cases <- df1 %>%
    dplyr::count(history, presumpt) %>%
    dplyr:: mutate(
      history = as.factor(history),
      history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
      presumpt = as.factor(presumpt),
      presumpt = factor(presumpt, levels = c("Stay", "Commit"))
    ) %>%
    tidyr::complete(history, presumpt, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = presumpt, values_from = n, values_fill = 0)

  total_pres_disp_cases <- data.frame(
    history = c("Total", "Total"),
    Stay = c(as.character(sum(pres_disp_cases$Stay)),
             as.character(paste0(format(round(sum(pres_disp_cases$Stay) /
                                                (sum(pres_disp_cases$Stay) + sum(pres_disp_cases$Commit)) * 100, 1),
                                        nsmall = 1), "%"))),
    Commit = c(as.character(sum(pres_disp_cases$Commit)),
               as.character(paste0(format(round(sum(pres_disp_cases$Commit) /
                                                  (sum(pres_disp_cases$Stay) + sum(pres_disp_cases$Commit)) * 100, 1),
                                          nsmall = 1), "%")))
  )

  pres_disp_percentages <- pres_disp_cases %>%
    dplyr::mutate(
      Stay = dplyr::if_else(is.na(Stay), 0, Stay),
      Commit = dplyr::if_else(is.na(Commit), 0, Commit),
      total = Stay + Commit,
      stay_percent = dplyr::if_else(total == 0, "0.0%", paste0(format(round(Stay / total * 100, 1), nsmall = 1), "%")),
      commit_percent = dplyr::if_else(total == 0, "0.0%", paste0(format(round(Commit / total * 100, 1), nsmall = 1), "%"))
    ) %>%
    dplyr::select(history, "Stay" = stay_percent, "Commit" = commit_percent) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .)))

  pres_disp_cases <- pres_disp_cases %>%
    dplyr::mutate(Stay = as.factor(Stay), Commit = as.factor(Commit))

  table_pres_disp <- dplyr::bind_rows(pres_disp_cases,
                                      total_pres_disp_cases,
                                      pres_disp_percentages) %>%
    dplyr::arrange(history) %>%
    dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(Stay, "%") & !stringr::str_detect(Commit, "%"), 1, 0))

  # Dispositional Departures
  dis_dep_cases <- df1 %>%
    dplyr::filter(dispdep %in% c(0, 1, 2)) %>%
    dplyr::mutate(
      history = as.factor(history),
      history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
      dispdep = as.factor(dispdep),
      dispdep = factor(dispdep, levels = c("None", "Aggravated", "Mitigated"))
    ) %>%
    dplyr::count(history, dispdep) %>%
    tidyr::complete(history, dispdep, fill = list(n = 0)) %>%
    tidyr::pivot_wider(names_from = dispdep, values_from = n, values_fill = 0)

  pres_disp_cases <- pres_disp_cases %>%
    dplyr::mutate(dplyr::across(c(Stay, Commit), ~ as.numeric(as.character(.))))

  total_disp_dep_cases <- data.frame(
    history = c("Total", "Total"),
    None = c(as.character(sum(dis_dep_cases$None)),
             as.character(paste0(format(round(sum(dis_dep_cases$None) /
                                                (sum(dis_dep_cases$None) +
                                                   sum(dis_dep_cases$Aggravated) +
                                                   sum(dis_dep_cases$Mitigated)) * 100, 1),
                                        nsmall = 1), "%"))),
    Aggravated = c(as.character(sum(dis_dep_cases$Aggravated)),
                   as.character(paste0(format(round(sum(dis_dep_cases$Aggravated) /
                                                      sum(pres_disp_cases$Stay) * 100, 1),
                                              nsmall = 1), "%"))),
    Mitigated = c(as.character(sum(dis_dep_cases$Mitigated)),
                  as.character(paste0(format(round(sum(dis_dep_cases$Mitigated) /
                                                     sum(pres_disp_cases$Commit) * 100, 1),
                                             nsmall = 1), "%")))
  )

  dis_dep_percentages <- dis_dep_cases %>%
    dplyr::mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                  aggravated_percent = paste0(format(round(Aggravated / pres_disp_cases$Stay * 100, 1), nsmall = 1), "%"),
                  mitigated_percent = paste0(format(round(Mitigated / pres_disp_cases$Commit * 100, 1), nsmall = 1), "%")) %>%
    dplyr::select(history,
                  "None" = none_percent,
                  "Aggravated" = aggravated_percent,
                  "Mitigated" = mitigated_percent) %>%
    dplyr::mutate(dplyr::across(everything(), ~ gsub("NaN%", "0.0%", .)))

  dis_dep_cases <- dis_dep_cases %>%
    dplyr::mutate(None = as.factor(None),
                  Aggravated = as.factor(Aggravated),
                  Mitigated = as.factor(Mitigated))

  table_disp_dep <- dplyr::bind_rows(dis_dep_cases,
                                      total_disp_dep_cases,
                                      dis_dep_percentages) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .))) %>%
    dplyr::arrange(history) %>%
    dplyr::mutate(cases = dplyr::if_else(!stringr::str_detect(None, "%") &
                                         !stringr::str_detect(Aggravated, "%") &
                                         !stringr::str_detect(Mitigated, "%"),
                           1, 0)) %>%
    dplyr::rename(none_disp = None, agg_disp = Aggravated, mit_disp = Mitigated)

  # Durational Departures
  dur_dep_cases <- df1 %>%
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

  dur_dep_cases <- dur_dep_cases %>%
    dplyr::mutate(dplyr::across(c(None, Aggravated, Mitigated), ~ as.numeric(as.character(.))))

  total_dur_dep_cases <- data.frame(
    history = c("Total", "Total"),
    None = c(as.character(sum(dur_dep_cases$None)),
             as.character(paste0(format(round(sum(dur_dep_cases$None) /
                                                (sum(dur_dep_cases$None) +
                                                   sum(dur_dep_cases$Aggravated) +
                                                   sum(dur_dep_cases$Mitigated)) * 100, 1),
                                        nsmall = 1), "%"))),
    Aggravated = c(as.character(sum(dur_dep_cases$Aggravated)),
                   as.character(paste0(format(round(sum(dur_dep_cases$Aggravated) /
                                                      (sum(dur_dep_cases$None) +
                                                         sum(dur_dep_cases$Aggravated) +
                                                         sum(dur_dep_cases$Mitigated)) * 100, 1),
                                              nsmall = 1), "%"))),
    Mitigated = c(as.character(sum(dur_dep_cases$Mitigated)),
                  as.character(paste0(format(round(sum(dur_dep_cases$Mitigated) /
                                                     (sum(dur_dep_cases$None) +
                                                        sum(dur_dep_cases$Aggravated) +
                                                        sum(dur_dep_cases$Mitigated)) * 100, 1),
                                             nsmall = 1), "%")))
  ) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .)))

  dur_dep_percentages <- dur_dep_cases %>%
    dplyr::mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                  aggravated_percent = paste0(format(round(Aggravated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
                  mitigated_percent = paste0(format(round(Mitigated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%")) %>%
    dplyr::select(history,
                  "None" = none_percent,
                  "Aggravated" = aggravated_percent,
                  "Mitigated" = mitigated_percent) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ gsub("NaN%", "0.0%", .)))

  dur_dep_cases <- dur_dep_cases %>%
    dplyr::mutate(None = as.factor(None),
                  Aggravated = as.factor(Aggravated),
                  Mitigated = as.factor(Mitigated))

  table_dur_dep <- dplyr::bind_rows(dur_dep_cases,
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

  # Combine all into one final table
  final_chs_table <- table_total_cases %>%
    dplyr::left_join(table_pres_disp, by = c("history", "cases")) %>%
    dplyr::left_join(table_disp_dep, by = c("history", "cases")) %>%
    dplyr::left_join(table_dur_dep, by = c("history", "cases")) %>%
    dplyr::select(history,
                  N,
                  Stay,
                  Commit,
                  none_disp,
                  agg_disp,
                  mit_disp,
                  none_dur,
                  agg_dur,
                  mit_dur)

  ################################################################
  # Departure and plea reasons template
  ################################################################


  #####################################################
  # Mitigated dispositional departures
  #####################################################
  # Departure reasons

  # Number of mitigated dispositional departure cases
  mdd_cases <- df1 %>%
    dplyr::filter(dispdep == 2,
    ) %>%
    dplyr::summarize(N = n())

  # Determine data filters and convert dep reason1-4 to long format
  mit_disp_dep_reasons_long <- df1 %>%
    dplyr::filter(dispdep == 2
    ) %>%
    dplyr::select(reason1,
           reason2,
           reason3,
           reason4) %>%
    dplyr::mutate(
      reason1 = as.factor(reason1),
      reason2 = as.factor(reason2),
      reason3 = as.factor(reason3),
      reason4 = as.factor(reason4)) %>%
    tidyr::pivot_longer(
      cols = starts_with("reason"),
      names_to = "reason_number",
      values_to = "reason"
    ) %>%
    dplyr::filter(!is.na(reason),
           !(reason_number == "reason2" & reason == 0),
           !(reason_number == "reason3" & reason == 0),
           !(reason_number == "reason4" & reason == 0))

  # Determine the number of each reason and percent composition
  mdd_reasons <- mit_disp_dep_reasons_long %>%
    dplyr::group_by(reason) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / mdd_cases$N * 100, 1), nsmall = 1), "%")),
           dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(desc(N))

  ################################################################
  # Departure plea reasons

  mit_disp_plea_df <- df1 %>%
    dplyr::filter(dispdep == 2) %>%
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
               "Other Reasons",
               "Other Reason",
               "-99999999999999999999")
           )
    ) %>%
    dplyr::group_by(plea_reasons_grp) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / mdd_cases$N * 100, 1), nsmall = 1), "%")),
           dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(plea_reasons_grp)

  ################################################################
  # Durational departure reasons
  ################################################################

  # Mitigated durational departures
  ################################################################
  # Departure reasons

  # Number of mitigated durational departure cases
  mitigated_dur_dep_cases <- df1 %>%
    dplyr::filter(durdep == 2,
           prison == 100
    ) %>%
    dplyr::summarize(N = n())

  # Determine data filters and convert dep reason1-4 to long format
  dur_dep_reasons_long <- df1 %>%
    dplyr::filter(durdep == 2,
           prison == 100
    ) %>%
    dplyr::select(reason1,
           reason2,
           reason3,
           reason4) %>%
    dplyr::mutate(
      reason1 = as.factor(reason1),
      reason2 = as.factor(reason2),
      reason3 = as.factor(reason3),
      reason4 = as.factor(reason4)) %>%
    tidyr::pivot_longer(
      cols = starts_with("reason"),
      names_to = "reason_number",
      values_to = "reason"
    ) %>%
    dplyr::filter(!is.na(reason))

  # Determine the number of each reason and percent composition
  mit_dur_dep_reasons <- dur_dep_reasons_long %>%
    dplyr::filter(reason != 0) %>%
    dplyr::group_by(reason) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / sum(mitigated_dur_dep_cases$N) * 100, 1), nsmall = 1), "%")),
           dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(desc(N))

  ################################################################
  # Departure plea reasons

  mit_dur_plea_df <- df1 %>%
    dplyr::filter(durdep == 2,
           prison == 100) %>%
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
           pr_other = dplyr::if_else(unknown != 1 &
                                accepts != 1 &
                                pros_objects != 1 &
                                pros_not_object != 1 &
                                plea_deal != 1 &
                                pros_motion != 1, 1, 0),
           plea_reasons_grp = dplyr::case_when(
             pr_accepts == 1 ~ "Prosecutor did not Object/Plea Negotiation",
             pr_objects == 1 ~ "Prosecutor Objects",
             pr_unknown == 1 ~ "Plea Reason Unknown",
             pr_other == 1 ~ "Other Reason",
             TRUE ~ "-99999999999999999999"),
           plea_reasons_grp = factor(
             plea_reasons_grp,
             levels = c(
               "Prosecutor did not Object/Plea Negotiation",
               "Prosecutor Objects",
               "Plea Reason Unknown",
               "Other Reasons",
               "-99999999999999999999")
           )
    ) %>%
    dplyr::group_by(plea_reasons_grp) %>%
    dplyr::summarize(N = n()) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percent = as.factor(paste0(format(round(N / mitigated_dur_dep_cases$N * 100, 1), nsmall = 1), "%")),
           dplyr::across(c(percent), ~ trimws(.))) %>%
    dplyr::arrange(plea_reasons_grp)

  ###########################################################################
  # Return final result in Excel file
  ###########################################################################
  report_table <- list(
    "table" = final_chs_table,
    "mit-disp-dep-reas" = mdd_reasons,
    "mit-disp-dep-plea-reas" = mit_disp_plea_df,
    "mit-dur-dep-reas" = mit_dur_dep_reasons,
    "mit-dur-dep-plea-reas" = mit_dur_plea_df
  )


  # save the report table output under the directed pathway with the directed name
  create_excel(report_table, output_name, output_path)

}





