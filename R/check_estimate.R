check_estimate <- function(x,
                           y,
                           division_numeric_code,
                           is_in_union,
                           indicator) {
  low <- x - .05
  up <- x + .05
  if(any(y < low | y > up)) {
    if (!dir.exists("output")) dir.create("output")
    cat(paste(division_numeric_code, is_in_union, indicator, "has > 5% difference"), sep = "", append = TRUE, file = "output/automatic_estimate_check.txt", fill = TRUE)
  }
}


