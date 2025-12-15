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
#' @param mit_disp represents mitigated dispositional departure. used for filtering
#' in departure reasons and plea reasons function.
#' @param mit_dur represents mitigated durational departure. used for filtering
#' in departure reasons and plea reasons function.
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
#' @name datarequest

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
  data_set <- case_list(df1) # function found in final-case-list-function.R

  # save the case list under the directed pathway with the directed name
  create_excel(data_set, case_list_name, case_list_path)

  ########################################################################
  # Create report tables #
  # Total cases by CHS
  table_total_cases <- total_cases_by_chs(df1)

  # Cases by Presumptive Disposition
  out <- pres_disp_cases(df1)

  table_pres_disp <- out$table_pres_disp
  pres_disp_df <- out$pres_disp

  # Cases by Dispositional Departures
  table_disp_dep <- disp_dep_cases(df1, pres_disp_df)

  # Cases by Durational Departures
  table_dur_dep <- dur_dep_cases(df1)

  # Combine all into one final table
  final_chs_table <- final_chs_table(table_total_cases, table_pres_disp, table_disp_dep, table_dur_dep)

  ################################################################
  # Departure reasons
  ################################################################

  # Mitigated dispositional departure reasons
  dep_reasons(df1, mit_disp)

  ############################################
  # Mitigated durational departure reasons
  dep_reasons(df1, mit_dur)


  ################################################################
  # Departure plea reasons
  ################################################################

  # Mitigated dispositional departure reasons
  dep_plea_reasons(df1, mit_disp)

  ############################################
  # Mitigated durational departure reasons
  dep_plea_reasons(df1, mit_dur)


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





