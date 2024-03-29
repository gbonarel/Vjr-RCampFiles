rm(list=ls())
#---------------------------------------------------------------------
path_root="."
path_data=file.path(path_root, "data")
path_libs=file.path(path_root, "libs")
path_output=file.path(path_root, "output")
#---------------------------------------------------------------------
report_name=paste0(
  "rd_leases_report_", 
  stringr::str_replace_all(Sys.Date(), "-", "_"),
  ".xlsx"
)
load(file.path(path_data, "biotech.rda"))
#---------------------------------------------------------------------
source(file.path(path_libs, "rd_leases.R")) # replace blank for zeros not NAs
data %>% 
  rd.leases.adjustments.f %>%
  # mutate_all(na_if,"") %>%
  writexl::write_xlsx(path=file.path(path_output, report_name)) # NA becomes blanks
#---------------------------------------------------------------------
rm(path_root, path_data, path_libs, path_output)
rm(data, report_name)
rm(rd.leases.adjustments.f)
