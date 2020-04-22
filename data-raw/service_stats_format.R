
library(dplyr)


#missing = FLASE if missing not allowed
service_stats_format <- list(
  "division_numeric_code" = list(
    "type" = "value",
    "valid" = fpemdata::divisions$division_numeric_code %>% unique(),
    "missing" = FALSE,
    "required" = TRUE
  ),
  "name" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "year" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "emu" = list(
    "basic" = TRUE,
    "required" = TRUE
  ),
  "ss_type" = list(
    "basic" = TRUE,
    "required" = TRUE
  )
)

usethis::use_data(service_stats_format, overwrite = TRUE)