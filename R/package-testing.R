pd0123 <- haven::read_sav("J:/SYS/Public_Data/0_CURRENT Public Files/PublicFile01_23_RevisedOct2025.sav")


library(dplyr)

chs_data_request(data = pd0123,
                            case_list_path = "I:/Requests/2025-function-testing/case-lists",
                            case_list_name = 'this-is-a-test-case-list.xlsx',
                            output_path = "I:/Requests/2025-function-testing/report-output",
                            output_name = 'this-is-a-test-of-report-output.xlsx',

                            sentyear_min = 2019,
                            statute = 609222100)
