subpopulation_labels <- function(contraceptive_use) {
  ifelse(
    contraceptive_use$age_group_bias == "+",
    "+",
    ifelse(
      contraceptive_use$age_group_bias == "-",
      "-",
      ifelse(
        contraceptive_use$age_group_bias == "?",
        "A",
        ifelse(
          contraceptive_use$has_traditional_method_bias == "Y",
          "F",
          ifelse(
            contraceptive_use$modern_method_bias == "-",
            "S-",
            ifelse(contraceptive_use$modern_method_bias == "+",
                   "S+",
                   "")
          )
        )
      )
    )
  )
}