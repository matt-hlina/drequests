################################################################################
# Function used to export the case list to Excel and open the file once exported
################################################################################

#' Excel function
#'
#' @param data_set any 'data.frame'.
#' @param name name of the file.
#' @param path path where the file will be saved.
#'
#' @return a named excel file saved in the path provided and opened.
#' @keywords internal
#' @name excel_function
#'

create_excel <- function(data_set, name, path) {
  excel_file <- file.path(path, name)
  writexl::write_xlsx(data_set, path = excel_file, col_names = TRUE)
  file.show(path = excel_file)
}
