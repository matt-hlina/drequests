############################################################################
# Function used to select variables of interest for case list
############################################################################

#' Cosmetic function
#'
#' @param data 'data.frame' of Sentencing Guidelines public data set.
#' @param df1 a filtered 'data.frame' of the cases requested in the data request.
#'
#' @return A filtered data frame with fewer variables.
#' @keywords internal
#' @name comsetic_function
#'

case_list <- function(data, df1) {
  data_set <- df1 %>%
    dplyr::mutate(dplyr::across(
      c(
        county, Agecat, sex, race, severity, typecust, presumpt, plea,
        inctype, stayexec, impose, condconf, consec, dispdep, durdep,
        cnsdep, reason1, reason2, reason3, reason4,
        preason1, preason2, preason3, Offense
      ),
      as.factor
    ),
    severity = dplyr::case_when(
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
    ) %>%
    dplyr::arrange(district, county, jlname, sentyear) %>%
    dplyr::select(
      "Conviction Statute" = convstat,
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
}

