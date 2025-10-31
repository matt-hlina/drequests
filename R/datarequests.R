#' Check data request
#' @import dplyr
#' @import forcats
#' @import rstudioapi
#' @import stringr
#' @import tibble
#' @import tidyr
#' @import writexl
#'
#' @export
#'
#' @param data 'data.frame' of Sentencing Guidelines public data set.
#' @param case_list_path pathway where the case list will be saved.
#' @param case_list_name name of the case list file.
#' @param output_path pathway where the report output will be saved.
#' @param output_name name of the report output file.
#' @param ... represents any variable within the data set. When a variable is used
#' as an argument, the definition of the argument will be used to filter the data.
#' Four arguments that are derivatives of variables exist: variablename_min,
#' variablename_max, reason, and preason. variablename_min and _max are used for
#' selecting a range of data, (i.e., sentyear) and reason and preason are used for
#' selecting departure reasons and plea reasons.

chs_data_request <- function(data,
                             case_list_path,
                             case_list_name,
                             output_path,
                             output_name,
                             ...) {

  filters <- list(...)
  df1 <- data

  for (name in names(filters)) {
    value <- filters[[name]]

    if (is.null(value)) next

    if (str_ends(name, "_min")) {
      var <- str_remove(name, "_min$")
      df1 <- df1 %>% filter(.data[[var]] >= value)

    } else if (str_ends(name, "_max")) {
      var <- str_remove(name, "_max$")
      df1 <- df1 %>% filter(.data[[var]] <= value)

    } else if (length(value) > 1) {
      df1 <- df1 %>% filter(.data[[name]] %in% value)

    } else if (name == "reason") {
      df1 <- df1 %>%
        mutate(reason_match = if_else(
          reason1 == value | reason2 == value | reason3 == value | reason4 == value,
          1, 0
        )) %>%
        filter(reason_match == 1) %>%
        select(-reason_match)

    } else if (name == "preason") {
      df1 <- df1 %>%
        mutate(preason_match = if_else(
          preason1 == value | preason2 == value | preason3 == value,
          1, 0
        )) %>%
        filter(preason_match == 1) %>%
        select(-preason_match)

    } else {
      df1 <- df1 %>% filter(.data[[name]] == value)
    }
  }

  ###########################################################################
  # Everything below is pipeline
  ###########################################################################

  # Create case list #
  data_set <- df1 %>%
    mutate(county = as_factor(county),
           Agecat = as_factor(Agecat),
           sex = as_factor(sex),
           race = as_factor(race),
           severity = as_factor(severity),
           severity = case_when(
             severity == "51" ~ "D1",
             severity == "52" ~ "D2",
             severity == "53" ~ "D3",
             severity == "54" ~ "D4",
             severity == "55" ~ "D5",
             severity == "56" ~ "D6",
             severity == "57" ~ "D7",
             severity == "58" ~ "D8",
             severity == "59" ~ "D9",
             TRUE ~ severity),
           typecust = as_factor(typecust),
           presumpt = as_factor(presumpt),
           plea = as_factor(plea),
           inctype = as_factor(inctype),
           stayexec = as_factor(stayexec),
           impose = as_factor(impose),
           condconf = as_factor(condconf),
           consec = as_factor(consec),
           dispdep = as_factor(dispdep),
           durdep = as_factor(durdep),
           cnsdep = as_factor(cnsdep),
           reason1 = as_factor(reason1),
           reason2 = as_factor(reason2),
           reason3 = as_factor(reason3),
           reason4 = as_factor(reason4),
           preason1 = as_factor(preason1),
           preason2 = as_factor(preason2),
           preason3 = as_factor(preason3),
           Offense = as_factor(Offense),
    ) %>%
    arrange(district,
            county,
            jlname,
            sentyear) %>%
    select("Conviction Statute" = convstat,
           "Offense Name" = Offense_Title,
           "Year Sentenced" = sentyear,
           "District" = district,
           "County" = county,
           "Judge Last Name" = jlname,
           "Judge First Name" = jfname,
           "Case Number" = dcnum,
           "Count Number" = count,
           "Attempt" = attempt,
           "Conspiracy" = conspir,
           "Age Category" = Agecat,
           "Sex" = sex,
           "Race" = race,
           "Offense Date" = doff,
           "Severity Level" = severity,
           "Type of Custody Offender on at Time of Offense" = typecust,
           "Total Criminal History Points" = histall,
           "Criminal History Score" = history,
           "Presumptive Disposition" = presumpt,
           "Presumptive Duration (months)" = time,
           "Lower Range" = Mintime,
           "Upper Range" = Maxtime,
           "Plea" = plea,
           "Pronounced Incarceration Type" = inctype,
           "Pronounced Confinement (months)" = confine,
           "Stay of Execution" = stayexec,
           "Stay of Imposition" = impose,
           "Length of Stay" = staylnth,
           "Conditional Confinement (days)" = condconf,
           "Received Consecutive Sentence" = consec,
           "Total Consecutive Sentence (months)" = aggsentc,
           "Dispositional Departure" = dispdep,
           "Durational Departure" = durdep,
           "Consecutive Departure" = cnsdep,
           "Departure Reason 1" = reason1,
           "Departure Reason 2" = reason2,
           "Departure Reason 3" = reason3,
           "Departure Reason 4" = reason4,
           "Plea Reason 1" = preason1,
           "Plea Reason 2" = preason2,
           "Plea Reason 3" = preason3,
           "Penalty Statute" = penaltystat
    )

  excel_file <- file.path(case_list_path, case_list_name)

  write_xlsx(data_set, path = excel_file, col_names = TRUE)

  file_show(path = excel_file)

  ########################################################################
  # Create report tables #
  # Total cases by CHS
  total_cases_by_chs <- df1 %>%
    group_by(history) %>%
    summarize(N = n(), .groups = "drop") %>%
    mutate(history = as.character(history)) %>%
    complete(history = as.character(0:6), fill = list(N = 0)) %>%
    mutate(
      history = factor(history, levels = as.character(0:6)),
      N = as_factor(N)
    ) %>%
    arrange(history)

  # Total cases
  total_case <- df1 %>%
    summarize(N = n()) %>%
    mutate(history = "Total",
           N = as_factor(N)) %>%
    select(history, N)

  # Data frame of 100% seven times
  one_hundred_percent <- data.frame(
    history = c("0", "1", "2", "3", "4", "5", "6", "Total"),
    N = rep(paste0(format(100.0, nsmall = 1), "%"), 8)
  )

  # Combine total cases for table
  table_total_cases <- bind_rows(total_cases_by_chs,
                                 total_case,
                                 one_hundred_percent) %>%
    arrange(history) %>%
    mutate(cases = if_else(!str_detect(N, "%"), 1, 0))

  # Presumptive Disposition
  pres_disp_cases <- df1 %>%
    count(history, presumpt) %>%
    mutate(
      history = as_factor(history),
      history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
      presumpt = as_factor(presumpt),
      presumpt = factor(presumpt, levels = c("Stay", "Commit"))
    ) %>%
    complete(history, presumpt, fill = list(n = 0)) %>%
    pivot_wider(names_from = presumpt, values_from = n, values_fill = 0)

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
    mutate(
      Stay = if_else(is.na(Stay), 0, Stay),
      Commit = if_else(is.na(Commit), 0, Commit),
      total = Stay + Commit,
      stay_percent = if_else(total == 0, "0.0%", paste0(format(round(Stay / total * 100, 1), nsmall = 1), "%")),
      commit_percent = if_else(total == 0, "0.0%", paste0(format(round(Commit / total * 100, 1), nsmall = 1), "%"))
    ) %>%
    select(history, "Stay" = stay_percent, "Commit" = commit_percent) %>%
    mutate(across(everything(), ~ gsub("NaN%", "0.0%", .)))

  pres_disp_cases <- pres_disp_cases %>%
    mutate(Stay = as_factor(Stay), Commit = as_factor(Commit))

  table_pres_disp <- bind_rows(pres_disp_cases,
                               total_pres_disp_cases,
                               pres_disp_percentages) %>%
    arrange(history) %>%
    mutate(cases = if_else(!str_detect(Stay, "%") & !str_detect(Commit, "%"), 1, 0))

  # Dispositional Departures
  dis_dep_cases <- df1 %>%
    filter(dispdep %in% c(0, 1, 2)) %>%
    mutate(
      history = as_factor(history),
      history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6")),
      dispdep = as_factor(dispdep),
      dispdep = factor(dispdep, levels = c("None", "Aggravated", "Mitigated"))
    ) %>%
    count(history, dispdep) %>%
    complete(history, dispdep, fill = list(n = 0)) %>%
    pivot_wider(names_from = dispdep, values_from = n, values_fill = 0)

  pres_disp_cases <- pres_disp_cases %>%
    mutate(across(c(Stay, Commit), ~ as.numeric(as.character(.))))

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
    mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
           aggravated_percent = paste0(format(round(Aggravated / pres_disp_cases$Stay * 100, 1), nsmall = 1), "%"),
           mitigated_percent = paste0(format(round(Mitigated / pres_disp_cases$Commit * 100, 1), nsmall = 1), "%")) %>%
    select(history,
           "None" = none_percent,
           "Aggravated" = aggravated_percent,
           "Mitigated" = mitigated_percent) %>%
    mutate(across(everything(), ~ gsub("NaN%", "0.0%", .)))

  dis_dep_cases <- dis_dep_cases %>%
    mutate(None = as_factor(None),
           Aggravated = as_factor(Aggravated),
           Mitigated = as_factor(Mitigated))

  table_disp_dep <- bind_rows(dis_dep_cases,
                              total_disp_dep_cases,
                              dis_dep_percentages) %>%
    mutate(across(everything(), ~ gsub("NaN%", "0.0%", .))) %>%
    arrange(history) %>%
    mutate(cases = if_else(!str_detect(None, "%") &
                             !str_detect(Aggravated, "%") &
                             !str_detect(Mitigated, "%"),
                           1, 0)) %>%
    rename(none_disp = None, agg_disp = Aggravated, mit_disp = Mitigated)

  # Durational Departures
  dur_dep_cases <- df1 %>%
    filter(prison == 100, durdep %in% c(0, 1, 2)) %>%
    mutate(
      durdep = recode(as.numeric(durdep), `0` = "None", `1` = "Aggravated", `2` = "Mitigated"),
      durdep = factor(durdep, levels = c("None", "Aggravated", "Mitigated")),
      history = as_factor(history),
      history = factor(history, levels = c("0", "1", "2", "3", "4", "5", "6"))
    ) %>%
    count(history, durdep) %>%
    complete(history, durdep, fill = list(n = 0)) %>%
    pivot_wider(names_from = durdep, values_from = n, values_fill = 0)

  dur_dep_cases <- dur_dep_cases %>%
    mutate(across(c(None, Aggravated, Mitigated), ~ as.numeric(as.character(.))))

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
    mutate(across(everything(), ~ gsub("NaN%", "0.0%", .)))

  dur_dep_percentages <- dur_dep_cases %>%
    mutate(none_percent = paste0(format(round(None / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
           aggravated_percent = paste0(format(round(Aggravated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%"),
           mitigated_percent = paste0(format(round(Mitigated / (None + Aggravated + Mitigated) * 100, 1), nsmall = 1), "%")) %>%
    select(history,
           "None" = none_percent,
           "Aggravated" = aggravated_percent,
           "Mitigated" = mitigated_percent) %>%
    mutate(across(everything(), ~ gsub("NaN%", "0.0%", .)))

  dur_dep_cases <- dur_dep_cases %>%
    mutate(None = as_factor(None),
           Aggravated = as_factor(Aggravated),
           Mitigated = as_factor(Mitigated))

  table_dur_dep <- bind_rows(dur_dep_cases,
                             total_dur_dep_cases,
                             dur_dep_percentages) %>%
    arrange(history) %>%
    mutate(cases = if_else(!str_detect(None, "%") &
                             !str_detect(Aggravated, "%") &
                             !str_detect(Mitigated, "%"),
                           1, 0)) %>%
    rename(none_dur = None,
           agg_dur = Aggravated,
           mit_dur = Mitigated) %>%
    mutate(across(ends_with("_percent"), ~ replace(., is.na(.), "0.0%")))

  # Combine all into one final table
  final_chs_table <- table_total_cases %>%
    left_join(table_pres_disp, by = c("history", "cases")) %>%
    left_join(table_disp_dep, by = c("history", "cases")) %>%
    left_join(table_dur_dep, by = c("history", "cases")) %>%
    select(history,
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
    filter(dispdep == 2,
    ) %>%
    summarize(N = n())

  # Determine data filters and convert dep reason1-4 to long format
  mit_disp_dep_reasons_long <- df1 %>%
    filter(dispdep == 2
    ) %>%
    select(reason1,
           reason2,
           reason3,
           reason4) %>%
    mutate(
      reason1 = as_factor(reason1),
      reason2 = as_factor(reason2),
      reason3 = as_factor(reason3),
      reason4 = as_factor(reason4)) %>%
    pivot_longer(
      cols = starts_with("reason"),
      names_to = "reason_number",
      values_to = "reason"
    ) %>%
    filter(!is.na(reason),
           !(reason_number == "reason2" & reason == 0),
           !(reason_number == "reason3" & reason == 0),
           !(reason_number == "reason4" & reason == 0))

  # Determine the number of each reason and percent composition
  mdd_reasons <- mit_disp_dep_reasons_long %>%
    group_by(reason) %>%
    summarize(N = n()) %>%
    mutate(percent = as_factor(paste0(format(round(N / mdd_cases$N * 100, 1), nsmall = 1), "%")),
           across(c(percent), ~ trimws(.))) %>%
    arrange(desc(N))

  ################################################################
  # Departure plea reasons

  mit_disp_plea_df <- df1 %>%
    filter(dispdep == 2) %>%
    mutate(unknown = if_else(preason1 == 0 | preason2 == 0 | preason3 == 0,
                             1, 0),
           accepts = if_else(preason1 == 440 | preason2 == 440 | preason3 == 440,
                             1, 0),
           pros_objects = if_else(preason1 == 441 | preason2 == 441 | preason3 == 441,
                                  1, 0),
           pros_not_object = if_else(preason1 == 442 | preason2 == 442 | preason3 == 442,
                                     1, 0),
           plea_deal = if_else(preason1 == 470 | preason2 == 470 | preason3 == 470,
                               1, 0),
           pros_motion = if_else(preason1 == 599 | preason2 == 599 | preason3 == 599,
                                 1, 0),
           pr_accepts = if_else(accepts == 1 | pros_not_object == 1 | plea_deal == 1 |
                                  pros_motion == 1, 1, 0),
           pr_objects = if_else(pros_objects == 1, 1, 0),
           pr_unknown = if_else(unknown == 1 & accepts == 0 & pros_objects == 0 &
                                  pros_not_object == 0 & plea_deal == 0 & pros_motion == 0, 1, 0),
           plea_reasons_grp = case_when(
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
               "-99999999999999999999")
           )
    ) %>%
    group_by(plea_reasons_grp) %>%
    summarize(N = n()) %>%
    ungroup() %>%
    mutate(percent = as_factor(paste0(format(round(N / mdd_cases$N * 100, 1), nsmall = 1), "%")),
           across(c(percent), ~ trimws(.))) %>%
    arrange(plea_reasons_grp)

  ################################################################
  # Durational departure reasons
  ################################################################

  # Mitigated durational departures
  ################################################################
  # Departure reasons

  # Number of mitigated durational departure cases
  mitigated_dur_dep_cases <- df1 %>%
    filter(durdep == 2,
           prison == 100
    ) %>%
    summarize(N = n())

  # Determine data filters and convert dep reason1-4 to long format
  dur_dep_reasons_long <- df1 %>%
    filter(durdep == 2,
           prison == 100
    ) %>%
    select(reason1,
           reason2,
           reason3,
           reason4) %>%
    mutate(
      reason1 = as_factor(reason1),
      reason2 = as_factor(reason2),
      reason3 = as_factor(reason3),
      reason4 = as_factor(reason4)) %>%
    pivot_longer(
      cols = starts_with("reason"),
      names_to = "reason_number",
      values_to = "reason"
    ) %>%
    filter(!is.na(reason))

  # Determine the number of each reason and percent composition
  mit_dur_dep_reasons <- dur_dep_reasons_long %>%
    filter(reason != 0) %>%
    group_by(reason) %>%
    summarize(N = n()) %>%
    mutate(percent = as_factor(paste0(format(round(N / sum(mitigated_dur_dep_cases$N) * 100, 1), nsmall = 1), "%")),
           across(c(percent), ~ trimws(.))) %>%
    arrange(desc(N))

  ################################################################
  # Departure plea reasons

  mit_dur_plea_df <- df1 %>%
    filter(durdep == 2,
           prison == 100) %>%
    mutate(unknown = if_else(preason1 == 0 | preason2 == 0 | preason3 == 0,
                             1, 0),
           accepts = if_else(preason1 == 440 | preason2 == 440 | preason3 == 440,
                             1, 0),
           pros_objects = if_else(preason1 == 441 | preason2 == 441 | preason3 == 441,
                                  1, 0),
           pros_not_object = if_else(preason1 == 442 | preason2 == 442 | preason3 == 442,
                                     1, 0),
           plea_deal = if_else(preason1 == 470 | preason2 == 470 | preason3 == 470,
                               1, 0),
           pros_motion = if_else(preason1 == 599 | preason2 == 599 | preason3 == 599,
                                 1, 0),
           pr_accepts = if_else(accepts == 1 | pros_not_object == 1 | plea_deal == 1 |
                                  pros_motion == 1, 1, 0),
           pr_objects = if_else(pros_objects == 1, 1, 0),
           pr_unknown = if_else(unknown == 1 & accepts == 0 & pros_objects == 0 &
                                  pros_not_object == 0 & plea_deal == 0 & pros_motion == 0, 1, 0),
           pr_other = if_else(unknown != 1 &
                                accepts != 1 &
                                pros_objects != 1 &
                                pros_not_object != 1 &
                                plea_deal != 1 &
                                pros_motion != 1, 1, 0),
           plea_reasons_grp = case_when(
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
    group_by(plea_reasons_grp) %>%
    summarize(N = n()) %>%
    ungroup() %>%
    mutate(percent = as_factor(paste0(format(round(N / mitigated_dur_dep_cases$N * 100, 1), nsmall = 1), "%")),
           across(c(percent), ~ trimws(.))) %>%
    arrange(plea_reasons_grp)

  ###########################################################################
  # Return final result in Excel file
  ###########################################################################
  excel_list <- list(
    "table" = final_chs_table,
    "mit-disp-dep-reas" = mdd_reasons,
    "mit-disp-dep-plea-reas" = mit_disp_plea_df,
    "mit-dur-dep-reas" = mit_dur_dep_reasons,
    "mit-dur-dep-plea-reas" = mit_dur_plea_df
  )

  excel_file <- file.path(output_path, output_name)

  write_xlsx(excel_list, path = excel_file, col_names = TRUE)

  file_show(path = excel_file)
}








