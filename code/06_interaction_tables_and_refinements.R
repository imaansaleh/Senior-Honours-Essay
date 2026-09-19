# Make Master Tables Smaller

# Age x Access

library(modelsummary)
library(tinytable)

access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  age_55_plus = "Age 55+",
  "access_smartphone_binary:age_55_plus" = "Smartphone × Age 55+",
  "access_computer_binary:age_55_plus" = "Computer × Age 55+",
  "access_highspeed_binary:age_55_plus" = "High-speed × Age 55+"
)

models_access <- list(
  "Online Purchase" = interaction_access_purchase,
  "Online Banking" = interaction_access_banking,
  "Online Investing" = interaction_access_investing,
  "Online Tax Filing" = interaction_access_tax
)

tab_access <- modelsummary(
  models_access,
  coef_map = access_terms,
  coef_omit = "Intercept",
  stars = c("*" = .05, "**" = .01, "***" = .001),
  statistic = "({std.error})",
  fmt = 3,
  gof_map = data.frame(
    raw = c("nobs", "r.squared"),
    clean = c("Observations", "R²"),
    fmt = c(0, 3)
  ),
  title = "Age Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = c(
    "Standard errors in parentheses.",
    "All regressions control for digital skills, income, education, geography, labour status, internet quality, gender, and demographic characteristics."
  ),
  output = "tinytable"
)

tab_access <- tab_access |>
  format_tt(j = 1:5, fontsize = 9) |>
  theme_tt("compact") |>
  style_tt(i = 0, bold = TRUE) |>
  style_tt(j = 1, align = "l") |>
  style_tt(j = 2:5, align = "c")

save_tt(tab_access, "MASTER_ACCESS_INTERACTION_TABLE_REFORMATTED.html")



# Professor Suggestions:

# Minority x Access

# level 1

make_minority_access_table <- function(model, outcome_name = "Outcome") {

  b <- coef(model)

  get_coef <- function(name) {
    if (name %in% names(b)) {
      return(as.numeric(b[name]))
    } else {
      return(NA_real_)
    }
  }

  vm   <- get_coef("visible_minority_dummy")
  sm   <- get_coef("access_smartphone_binary")
  comp <- get_coef("access_computer_binary")
  hs   <- get_coef("access_highspeed_binary")

  sm_vm   <- get_coef("access_smartphone_binary:visible_minority_dummy")
  comp_vm <- get_coef("access_computer_binary:visible_minority_dummy")
  hs_vm   <- get_coef("access_highspeed_binary:visible_minority_dummy")

  result <- data.frame(
    Outcome = outcome_name,
    Group = c(
      "Non-visible minority, no access",
      "Non-visible minority, smartphone access",
      "Non-visible minority, computer access",
      "Non-visible minority, high-speed access",
      "Visible minority, no access",
      "Visible minority, smartphone access",
      "Visible minority, computer access",
      "Visible minority, high-speed access"
    ),
    Effect = c(
      0,
      sm,
      comp,
      hs,
      vm,
      vm + sm + sm_vm,
      vm + comp + comp_vm,
      vm + hs + hs_vm
    )
  )

  result$Effect <- round(result$Effect, 3)

  return(result)
}

# line 2

minority_access_purchase_table <- make_minority_access_table(
  interaction_minority_access_purchase,
  "Online Purchase"
)

minority_access_purchase_table

# line 3
minority_access_investing_table <- make_minority_access_table(
  interaction_minority_access_investing,
  "Online Investing"
)

minority_access_investing_table

# line 4
minority_access_banking_table <- make_minority_access_table(
  interaction_minority_access_banking,
  "Online Banking"
)

minority_access_banking_table

# line 5
minority_access_tax_table <- make_minority_access_table(
  interaction_minority_access_tax,
  "Online Tax Filing"
)

minority_access_tax_table

# line 6

minority_access_all <- rbind(
  minority_access_purchase_table,
  minority_access_banking_table,
  minority_access_investing_table,
  minority_access_tax_table
)

minority_access_all

# line 7

library(tidyr)

minority_access_wide <- minority_access_all %>%
  pivot_wider(
    names_from = Outcome,
    values_from = Effect
  )

minority_access_wide

# line 8

library(modelsummary)

datasummary_df(
  minority_access_wide,
  title = "Visible Minority × Access (Group Effects)",
  output = "MINORITY_ACCESS_FRIENDLY_TABLE.html"
)

getwd()

# fix table

install.packages(c("sandwich", "gt", "dplyr", "tidyr"))

library(sandwich)
library(gt)
library(dplyr)
library(tidyr)

make_group_row <- function(model, coef_names, label, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  est_str <- paste0(sprintf("%.3f", est), stars)
  se_str  <- paste0("(", sprintf("%.3f", se), ")")

  data.frame(
    Group = label,
    Estimate = est_str,
    SE = se_str,
    stringsAsFactors = FALSE
  )
}

make_minority_access_friendly <- function(model, outcome_name) {

  rows <- bind_rows(

    data.frame(
      Group = "Non-visible minority, no access",
      Estimate = "0.000",
      SE = "",
      stringsAsFactors = FALSE
    ),

    make_group_row(model,
                   c("access_smartphone_binary"),
                   "Non-visible minority, smartphone access"),

    make_group_row(model,
                   c("access_computer_binary"),
                   "Non-visible minority, computer access"),

    make_group_row(model,
                   c("access_highspeed_binary"),
                   "Non-visible minority, high-speed access"),

    make_group_row(model,
                   c("visible_minority_dummy"),
                   "Visible minority, no access"),

    make_group_row(model,
                   c("visible_minority_dummy",
                     "access_smartphone_binary",
                     "access_smartphone_binary:visible_minority_dummy"),
                   "Visible minority, smartphone access"),

    make_group_row(model,
                   c("visible_minority_dummy",
                     "access_computer_binary",
                     "access_computer_binary:visible_minority_dummy"),
                   "Visible minority, computer access"),

    make_group_row(model,
                   c("visible_minority_dummy",
                     "access_highspeed_binary",
                     "access_highspeed_binary:visible_minority_dummy"),
                   "Visible minority, high-speed access")
  )

  rows <- rows %>%
    mutate(
      Outcome = outcome_name
    )

  return(rows)
}

# new

friendly_purchase <- make_minority_access_friendly(
  interaction_minority_access_purchase,
  "Online Purchase"
)



friendly_banking <- make_minority_access_friendly(
  interaction_minority_access_banking,
  "Online Banking"
)



friendly_investing <- make_minority_access_friendly(
  interaction_minority_access_investing,
  "Online Investing"
)


friendly_tax <- make_minority_access_friendly(
  interaction_minority_access_tax,
  "Online Tax Filing"
)


friendly_all <- bind_rows(
  friendly_purchase,
  friendly_banking,
  friendly_investing,
  friendly_tax
)

friendly_est <- friendly_all %>%
  select(Group, Outcome, Estimate) %>%
  pivot_wider(names_from = Outcome, values_from = Estimate)

friendly_se <- friendly_all %>%
  select(Group, Outcome, SE) %>%
  pivot_wider(names_from = Outcome, values_from = SE)





friendly_table <- bind_rows(
  friendly_est %>% slice(1),
  friendly_se %>% slice(1),
  friendly_est %>% slice(2),
  friendly_se %>% slice(2),
  friendly_est %>% slice(3),
  friendly_se %>% slice(3),
  friendly_est %>% slice(4),
  friendly_se %>% slice(4),
  friendly_est %>% slice(5),
  friendly_se %>% slice(5),
  friendly_est %>% slice(6),
  friendly_se %>% slice(6),
  friendly_est %>% slice(7),
  friendly_se %>% slice(7),
  friendly_est %>% slice(8),
  friendly_se %>% slice(8)
)

r2_row <- data.frame(
  Group = "R²",
  "Online Purchase" = as.character(sprintf("%.3f", summary(interaction_minority_access_purchase)$r.squared)),
  "Online Banking" = as.character(sprintf("%.3f", summary(interaction_minority_access_banking)$r.squared)),
  "Online Investing" = as.character(sprintf("%.3f", summary(interaction_minority_access_investing)$r.squared)),
  "Online Tax Filing" = as.character(sprintf("%.3f", summary(interaction_minority_access_tax)$r.squared)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


adjr2_row <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = as.character(sprintf("%.3f", summary(interaction_minority_access_purchase)$adj.r.squared)),
  "Online Banking" = as.character(sprintf("%.3f", summary(interaction_minority_access_banking)$adj.r.squared)),
  "Online Investing" = as.character(sprintf("%.3f", summary(interaction_minority_access_investing)$adj.r.squared)),
  "Online Tax Filing" = as.character(sprintf("%.3f", summary(interaction_minority_access_tax)$adj.r.squared)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

n_row <- data.frame(
  Group = "Observations",
  "Online Purchase" = as.character(nobs(interaction_minority_access_purchase)),
  "Online Banking" = as.character(nobs(interaction_minority_access_banking)),
  "Online Investing" = as.character(nobs(interaction_minority_access_investing)),
  "Online Tax Filing" = as.character(nobs(interaction_minority_access_tax)),
  check.names = FALSE,
  stringsAsFactors = FALSE
)



friendly_table_final <- bind_rows(
  friendly_table,
  n_row,
  r2_row,
  adjr2_row
)

friendly_table_final


gt_table <- friendly_table_final %>%
  gt() %>%
  tab_header(
    title = "Visible Minority × Digital Access: Group Effects"
  ) %>%
  tab_source_note(
    source_note = md("+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001")
  ) %>%
  tab_source_note(
    source_note = md("Non visible minorities are the reference group. All regressions control for digital skills, income, education, gender, age, geography, labour status, internet quality and demographic characteristics.")
  )


gtsave(
  gt_table,
  "Tables/Interactions/Visible Minority/MINORITY_ACCESS_FRIENDLY_TABLE.html"
)

getwd()

gtsave(
  gt_table,
  "~/Desktop/MINORITY_ACCESS_FRIENDLY_TABLE.html"
)




# minority x access re-do

# 1

library(sandwich)
library(dplyr)
library(tidyr)
library(gt)

# 2

make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}




# 3

make_minority_access_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:8,
    Group = c(
      "Non-visible minority without access (reference)",
      "with smartphone access",
      "with computer access",
      "with high speed internet access",
      "Visible minority without access",
      "with smartphone access",
      "with computer access",
      "with high speed internet access"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("access_smartphone_binary")),
      make_group_cell(model, c("access_computer_binary")),
      make_group_cell(model, c("access_highspeed_binary")),
      make_group_cell(model, c("visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "access_smartphone_binary",
                               "access_smartphone_binary:visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "access_computer_binary",
                               "access_computer_binary:visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "access_highspeed_binary",
                               "access_highspeed_binary:visible_minority_dummy"))
    ),
    stringsAsFactors = FALSE
  )
}



# 4

prof_access_purchase <- make_minority_access_prof_table(
  interaction_minority_access_purchase,
  "Online Purchase"
)



prof_access_banking <- make_minority_access_prof_table(
  interaction_minority_access_banking,
  "Online Banking"
)



prof_access_investing <- make_minority_access_prof_table(
  interaction_minority_access_investing,
  "Online Investing"
)


prof_access_tax <- make_minority_access_prof_table(
  interaction_minority_access_tax,
  "Online Tax Filing"
)


prof_access_all <- bind_rows(
  prof_access_purchase,
  prof_access_banking,
  prof_access_investing,
  prof_access_tax
)


prof_access_wide <- prof_access_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 5

prof_access_wide <- prof_access_wide %>%
  select(-Order)



# 6

r2_row <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_minority_access_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_minority_access_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_minority_access_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_minority_access_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)



adjr2_row <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_minority_access_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_minority_access_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_minority_access_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_minority_access_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 7

prof_access_final <- bind_rows(
  prof_access_wide,
  r2_row,
  adjr2_row
)



# 8

gt_access <- prof_access_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%

  tab_header(
    title = "Visible Minority × Digital Access: Group Effects"
  ) %>%

  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%

  cols_align(
    align = "left",
    columns = "Group"
  ) %>%

  # FIXED: remove md() so + shows correctly
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%

  tab_source_note(
    source_note = "Non visible minorities are the reference group. All regressions control for digital skills, income, education, gender, age, geography, labour status, internet quality and demographic characteristics."
  )



# 9

gtsave(
  gt_access,
  "Tables/Interactions/Visible Minority/MINORITY_ACCESS_FRIENDLY_TABLE.html"
)






# MinorityxSkills

# 1

make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2

make_minority_skill_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:10,
    Group = c(
      "Non-visible minority without skills (reference)",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills",
      "Visible minority without skills",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("skill_files")),
      make_group_cell(model, c("skill_word")),
      make_group_cell(model, c("skill_presentation")),
      make_group_cell(model, c("skill_spreadsheet")),
      make_group_cell(model, c("visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "skill_files",
                               "skill_files:visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "skill_word",
                               "skill_word:visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "skill_presentation",
                               "skill_presentation:visible_minority_dummy")),
      make_group_cell(model, c("visible_minority_dummy",
                               "skill_spreadsheet",
                               "skill_spreadsheet:visible_minority_dummy"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3


prof_skill_purchase <- make_minority_skill_prof_table(
  interaction_minority_skill_purchase,
  "Online Purchase"
)

prof_skill_banking <- make_minority_skill_prof_table(
  interaction_minority_skill_banking,
  "Online Banking"
)

prof_skill_investing <- make_minority_skill_prof_table(
  interaction_minority_skill_investing,
  "Online Investing"
)

prof_skill_tax <- make_minority_skill_prof_table(
  interaction_minority_skill_tax,
  "Online Tax Filing"
)

# 4

prof_skill_all <- bind_rows(
  prof_skill_purchase,
  prof_skill_banking,
  prof_skill_investing,
  prof_skill_tax
)


# 5

prof_skill_wide <- prof_skill_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6

prof_skill_wide <- prof_skill_wide %>%
  select(-Order)


# 7

r2_row_skill <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_minority_skill_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_minority_skill_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_minority_skill_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_minority_skill_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_skill <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_minority_skill_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_minority_skill_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_minority_skill_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_minority_skill_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 8

prof_skill_final <- bind_rows(
  prof_skill_wide,
  r2_row_skill,
  adjr2_row_skill
)


# 9

gt_skill <- prof_skill_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Visible Minority × Digital Skills: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Non visible minorities are the reference group. All regressions control for digital access, income, education, gender, age, geography, labour status, internet quality and demographic characteristics."
  )

# 10

gtsave(
  gt_skill,
  "Tables/Interactions/Visible Minority/MINORITY_SKILL_FRIENDLY_TABLE.html"
)

# Age x Access

# 1
make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}

# 2

make_age_access_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:8,
    Group = c(
      "Under age 55 without access (reference)",
      "with smartphone access",
      "with computer access",
      "with high-speed internet",
      "Age 55+ without access",
      "with smartphone access",
      "with computer access",
      "with high-speed internet"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("access_smartphone_binary")),
      make_group_cell(model, c("access_computer_binary")),
      make_group_cell(model, c("access_highspeed_binary")),
      make_group_cell(model, c("age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "access_smartphone_binary",
                               "access_smartphone_binary:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "access_computer_binary",
                               "access_computer_binary:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "access_highspeed_binary",
                               "access_highspeed_binary:age_55_plus"))
    ),
    stringsAsFactors = FALSE
  )
}

# 3

prof_age_access_purchase <- make_age_access_prof_table(
  interaction_access_purchase,
  "Online Purchase"
)

prof_age_access_banking <- make_age_access_prof_table(
  interaction_access_banking,
  "Online Banking"
)

prof_age_access_investing <- make_age_access_prof_table(
  interaction_access_investing,
  "Online Investing"
)

prof_age_access_tax <- make_age_access_prof_table(
  interaction_access_tax,
  "Online Tax Filing"
)

# 4

prof_age_access_all <- bind_rows(
  prof_age_access_purchase,
  prof_age_access_banking,
  prof_age_access_investing,
  prof_age_access_tax
)

# 5

prof_age_access_wide <- prof_age_access_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)

# 6

prof_age_access_wide <- prof_age_access_wide %>%
  select(-Order)

# 7

r2_row_age_access <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_access_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_access_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_access_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_access_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_age_access <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_access_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_access_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_access_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_access_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 8

prof_age_access_final <- bind_rows(
  prof_age_access_wide,
  r2_row_age_access,
  adjr2_row_age_access
)


# 9

gt_age_access <- prof_age_access_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Age 55+ × Digital Access: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Individuals under age 55 are the reference group. All regressions control for digital skills, income, education, gender, geography, labour status, internet quality and demographic characteristics."
  )

# 10
gtsave(
  gt_age_access,
  "Tables/Interactions/Age/AGE_ACCESS_FRIENDLY_TABLE.html"
)


# AgexSkill

# 1

make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2

make_age_skill_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:10,
    Group = c(
      "Under age 55 without skills (reference)",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills",
      "Age 55+ without skills",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("skill_files")),
      make_group_cell(model, c("skill_word")),
      make_group_cell(model, c("skill_presentation")),
      make_group_cell(model, c("skill_spreadsheet")),
      make_group_cell(model, c("age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "skill_files",
                               "skill_files:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "skill_word",
                               "skill_word:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "skill_presentation",
                               "skill_presentation:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "skill_spreadsheet",
                               "skill_spreadsheet:age_55_plus"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3

prof_age_skill_purchase <- make_age_skill_prof_table(
  interaction_age_purchase,
  "Online Purchase"
)

prof_age_skill_banking <- make_age_skill_prof_table(
  interaction_skill_banking,
  "Online Banking"
)

prof_age_skill_investing <- make_age_skill_prof_table(
  interaction_skill_investing,
  "Online Investing"
)

prof_age_skill_tax <- make_age_skill_prof_table(
  interaction_skill_tax,
  "Online Tax Filing"
)


# 4

prof_age_skill_all <- bind_rows(
  prof_age_skill_purchase,
  prof_age_skill_banking,
  prof_age_skill_investing,
  prof_age_skill_tax
)


# 5

prof_age_skill_wide <- prof_age_skill_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)

# 6

prof_age_skill_wide <- prof_age_skill_wide %>%
  select(-Order)

# 7

r2_row_age_skill <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_age_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_skill_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_skill_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_skill_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_age_skill <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_age_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_skill_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_skill_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_skill_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 8

prof_age_skill_final <- bind_rows(
  prof_age_skill_wide,
  r2_row_age_skill,
  adjr2_row_age_skill
)

# 9

gt_age_skill <- prof_age_skill_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Age 55+ × Digital Skills: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Individuals under age 55 are the reference group. All regressions control for digital access, income, education, gender, geography, labour status, internet quality and demographic characteristics."
  )


# 10

gtsave(
  gt_age_skill,
  "Tables/Interactions/Age/AGE_SKILL_FRIENDLY_TABLE.html"
)


# AgexEducation

# 1

make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}

# 2

make_age_education_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:6,
    Group = c(
      "Under age 55 with university education (reference)",
      "with high school or less",
      "with some post-secondary",
      "Age 55+ with university education",
      "with high school or less",
      "with some post-secondary"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("highschool_dummy")),
      make_group_cell(model, c("some_postsec_dummy")),
      make_group_cell(model, c("age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "highschool_dummy",
                               "highschool_dummy:age_55_plus")),
      make_group_cell(model, c("age_55_plus",
                               "some_postsec_dummy",
                               "some_postsec_dummy:age_55_plus"))
    ),
    stringsAsFactors = FALSE
  )
}

# 3

prof_age_education_purchase <- make_age_education_prof_table(
  interaction_education_purchase,
  "Online Purchase"
)

prof_age_education_banking <- make_age_education_prof_table(
  interaction_education_banking,
  "Online Banking"
)

prof_age_education_investing <- make_age_education_prof_table(
  interaction_education_investing,
  "Online Investing"
)

prof_age_education_tax <- make_age_education_prof_table(
  interaction_education_tax,
  "Online Tax Filing"
)


# 4

prof_age_education_all <- bind_rows(
  prof_age_education_purchase,
  prof_age_education_banking,
  prof_age_education_investing,
  prof_age_education_tax
)


# 5

prof_age_education_wide <- prof_age_education_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6

prof_age_education_wide <- prof_age_education_wide %>%
  select(-Order)


# 7

r2_row_age_education <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_education_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_education_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_education_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_education_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_age_education <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_education_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_education_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_education_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_education_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 8

prof_age_education_final <- bind_rows(
  prof_age_education_wide,
  r2_row_age_education,
  adjr2_row_age_education
)


# 9

gt_age_education <- prof_age_education_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Age 55+ × Education: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Individuals under age 55 with university education are the reference group. All regressions control for digital skills, digital access, income, geography, labour status, internet quality, gender and demographic characteristics."
  )


# 10

gtsave(
  gt_age_education,
  "Tables/Interactions/Age/AGE_EDUCATION_FRIENDLY_TABLE.html"
)

# gender x Access

# 1
make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2
make_gender_access_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:8,
    Group = c(
      "Male without access (reference)",
      "with smartphone access",
      "with computer access",
      "with high-speed internet",
      "Female without access",
      "with smartphone access",
      "with computer access",
      "with high-speed internet"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("access_smartphone_binary")),
      make_group_cell(model, c("access_computer_binary")),
      make_group_cell(model, c("access_highspeed_binary")),
      make_group_cell(model, c("factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "access_smartphone_binary",
                               "access_smartphone_binary:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "access_computer_binary",
                               "access_computer_binary:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "access_highspeed_binary",
                               "access_highspeed_binary:factor(gender)Female"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3
prof_gender_access_purchase <- make_gender_access_prof_table(
  interaction_gender_access_purchase,
  "Online Purchase"
)

prof_gender_access_banking <- make_gender_access_prof_table(
  interaction_gender_access_banking,
  "Online Banking"
)

prof_gender_access_investing <- make_gender_access_prof_table(
  interaction_gender_access_investing,
  "Online Investing"
)

prof_gender_access_tax <- make_gender_access_prof_table(
  interaction_gender_access_tax,
  "Online Tax Filing"
)


# 4
prof_gender_access_all <- bind_rows(
  prof_gender_access_purchase,
  prof_gender_access_banking,
  prof_gender_access_investing,
  prof_gender_access_tax
)


# 5
prof_gender_access_wide <- prof_gender_access_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6
prof_gender_access_wide <- prof_gender_access_wide %>%
  select(-Order)


# 7
r2_row_gender_access <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_access_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_access_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_access_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_access_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_gender_access <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_access_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_access_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_access_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_access_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 8
prof_gender_access_final <- bind_rows(
  prof_gender_access_wide,
  r2_row_gender_access,
  adjr2_row_gender_access
)


# 9
gt_gender_access <- prof_gender_access_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Gender × Digital Access: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Males are the reference group. All regressions control for digital skills, income, education, age, geography, labour status, internet quality and demographic characteristics."
  )


# 10
gtsave(
  gt_gender_access,
  "Tables/Interactions/Gender/GENDER_ACCESS_FRIENDLY_TABLE.html"
)


# GenderxEducation

# 1
make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2
make_gender_education_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:6,
    Group = c(
      "Male with university education (reference)",
      "with high school or less",
      "with some post-secondary",
      "Female with university education",
      "with high school or less",
      "with some post-secondary"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("highschool_dummy")),
      make_group_cell(model, c("some_postsec_dummy")),
      make_group_cell(model, c("factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "highschool_dummy",
                               "highschool_dummy:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "some_postsec_dummy",
                               "some_postsec_dummy:factor(gender)Female"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3
prof_gender_education_purchase <- make_gender_education_prof_table(
  interaction_gender_education_purchase,
  "Online Purchase"
)

prof_gender_education_banking <- make_gender_education_prof_table(
  interaction_gender_education_banking,
  "Online Banking"
)

prof_gender_education_investing <- make_gender_education_prof_table(
  interaction_gender_education_investing,
  "Online Investing"
)

prof_gender_education_tax <- make_gender_education_prof_table(
  interaction_gender_education_tax,
  "Online Tax Filing"
)


# 4
prof_gender_education_all <- bind_rows(
  prof_gender_education_purchase,
  prof_gender_education_banking,
  prof_gender_education_investing,
  prof_gender_education_tax
)


# 5
prof_gender_education_wide <- prof_gender_education_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6
prof_gender_education_wide <- prof_gender_education_wide %>%
  select(-Order)


# 7
r2_row_gender_education <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_gender_education <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 8
prof_gender_education_final <- bind_rows(
  prof_gender_education_wide,
  r2_row_gender_education,
  adjr2_row_gender_education
)


# 9
gt_gender_education <- prof_gender_education_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Gender × Education: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Males with university education are the reference group. All regressions control for digital skills, digital access, income, age, geography, labour status, internet quality and demographic characteristics."
  )


# 10
gtsave(
  gt_gender_education,
  "Tables/Interactions/Gender/GENDER_EDUCATION_FRIENDLY_TABLE.html"
)


# GenderxSkills

# 1
make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2
make_gender_skill_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:10,
    Group = c(
      "Male without skills (reference)",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills",
      "Female without skills",
      "with file management skills",
      "with word processing skills",
      "with presentation skills",
      "with spreadsheet skills"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("skill_files")),
      make_group_cell(model, c("skill_word")),
      make_group_cell(model, c("skill_presentation")),
      make_group_cell(model, c("skill_spreadsheet")),
      make_group_cell(model, c("factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "skill_files",
                               "skill_files:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "skill_word",
                               "skill_word:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "skill_presentation",
                               "skill_presentation:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "skill_spreadsheet",
                               "skill_spreadsheet:factor(gender)Female"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3
prof_gender_skill_purchase <- make_gender_skill_prof_table(
  interaction_gender_purchase,
  "Online Purchase"
)

prof_gender_skill_banking <- make_gender_skill_prof_table(
  interaction_gender_banking,
  "Online Banking"
)

prof_gender_skill_investing <- make_gender_skill_prof_table(
  interaction_gender_investing,
  "Online Investing"
)

prof_gender_skill_tax <- make_gender_skill_prof_table(
  interaction_gender_tax,
  "Online Tax Filing"
)


# 4
prof_gender_skill_all <- bind_rows(
  prof_gender_skill_purchase,
  prof_gender_skill_banking,
  prof_gender_skill_investing,
  prof_gender_skill_tax
)


# 5
prof_gender_skill_wide <- prof_gender_skill_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6
prof_gender_skill_wide <- prof_gender_skill_wide %>%
  select(-Order)


# 7
r2_row_gender_skill <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_gender_skill <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 8
prof_gender_skill_final <- bind_rows(
  prof_gender_skill_wide,
  r2_row_gender_skill,
  adjr2_row_gender_skill
)


# 9
gt_gender_skill <- prof_gender_skill_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Gender × Digital Skills: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Males are the reference group. All regressions control for digital access, income, education, age, geography, labour status, internet quality and demographic characteristics."
  )


# 10
gtsave(
  gt_gender_skill,
  "Tables/Interactions/Gender/GENDER_SKILL_FRIENDLY_TABLE.html"
)


# Clean OLS

# 1

cius_work <- cius_work %>%
  mutate(
    highschool_dummy = case_when(
      education == 1 ~ 1,
      education %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    some_postsec_dummy = case_when(
      education == 2 ~ 1,
      education %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(cius_work$education, cius_work$highschool_dummy, useNA = "ifany")
table(cius_work$education, cius_work$some_postsec_dummy, useNA = "ifany")

# 2

cius_work$income_quintile <- factor(cius_work$income_quintile)
cius_work$income_quintile <- relevel(cius_work$income_quintile, ref = "1")
levels(cius_work$income_quintile)


# 3

model_purchase_ols <- lm(
  dfs_purchase ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    gender +
    immigrant_dummy + visible_minority_dummy + disability_dummy + indigenous_dummy +
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,
  data = cius_work
)

model_banking_ols <- lm(
  dfs_banking ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    gender +
    immigrant_dummy + visible_minority_dummy + disability_dummy + indigenous_dummy +
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,
  data = cius_work
)

model_investing_ols <- lm(
  dfs_investing ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    gender +
    immigrant_dummy + visible_minority_dummy + disability_dummy + indigenous_dummy +
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,
  data = cius_work
)

model_tax_ols <- lm(
  dfs_tax ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    gender +
    immigrant_dummy + visible_minority_dummy + disability_dummy + indigenous_dummy +
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,
  data = cius_work
)

# 4


# Re-do Gender x Edu

cius_work$education_factor <- factor(
  cius_work$education,
  levels = c(1, 2, 3),
  labels = c("High school or less", "Some post-secondary", "University")
)

cius_work$education_factor <- relevel(
  cius_work$education_factor,
  ref = "High school or less"
)

levels(cius_work$education_factor)


# 2

interaction_gender_education_purchase <- lm(
  dfs_purchase ~
    education_factor * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_education_banking <- lm(
  dfs_banking ~
    education_factor * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_education_investing <- lm(
  dfs_investing ~
    education_factor * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_education_tax <- lm(
  dfs_tax ~
    education_factor * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)


# 3

gender_education_terms <- c(
  "education_factorSome post-secondary" = "Some post-secondary",
  "education_factorUniversity" = "University",
  "factor(gender)Female" = "Female",
  "education_factorSome post-secondary:factor(gender)Female" = "Some post-secondary × Female",
  "education_factorUniversity:factor(gender)Female" = "University × Female"
)

models_gender_education <- list(
  "Online Purchase" = interaction_gender_education_purchase,
  "Online Banking" = interaction_gender_education_banking,
  "Online Investing" = interaction_gender_education_investing,
  "Online Tax Filing" = interaction_gender_education_tax
)

modelsummary(
  models_gender_education,
  coef_map = gender_education_terms,
  stars = TRUE,
  title = "Gender Heterogeneity in the Effect of Education on Digital Financial Services Use",
  notes = "High school or less is the reference education group. All regressions control for digital skills, digital access, income, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_GENDER_EDUCATION_TABLE.html"
)



# 1
make_group_cell <- function(model, coef_names, vcov_type = "HC1") {

  b <- coef(model)
  V <- sandwich::vcovHC(model, type = vcov_type)

  cvec <- rep(0, length(b))
  names(cvec) <- names(b)

  for (nm in coef_names) {
    if (nm %in% names(cvec)) {
      cvec[nm] <- cvec[nm] + 1
    }
  }

  est <- sum(cvec * b, na.rm = TRUE)
  se  <- sqrt(as.numeric(t(cvec) %*% V %*% cvec))
  z   <- est / se
  p   <- 2 * pnorm(abs(z), lower.tail = FALSE)

  stars <- ifelse(p < 0.001, "***",
                  ifelse(p < 0.01,  "**",
                         ifelse(p < 0.05,  "*",
                                ifelse(p < 0.1,   "+", ""))))

  paste0(sprintf("%.3f", est), stars, "<br>(", sprintf("%.3f", se), ")")
}


# 2
make_gender_education_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:6,
    Group = c(
      "Male with high school or less (reference)",
      "with some post-secondary",
      "with university education",
      "Female with high school or less",
      "with some post-secondary",
      "with university education"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",
      make_group_cell(model, c("education_factorSome post-secondary")),
      make_group_cell(model, c("education_factorUniversity")),
      make_group_cell(model, c("factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "education_factorSome post-secondary",
                               "education_factorSome post-secondary:factor(gender)Female")),
      make_group_cell(model, c("factor(gender)Female",
                               "education_factorUniversity",
                               "education_factorUniversity:factor(gender)Female"))
    ),
    stringsAsFactors = FALSE
  )
}


# 3
prof_gender_education_purchase <- make_gender_education_prof_table(
  interaction_gender_education_purchase,
  "Online Purchase"
)

prof_gender_education_banking <- make_gender_education_prof_table(
  interaction_gender_education_banking,
  "Online Banking"
)

prof_gender_education_investing <- make_gender_education_prof_table(
  interaction_gender_education_investing,
  "Online Investing"
)

prof_gender_education_tax <- make_gender_education_prof_table(
  interaction_gender_education_tax,
  "Online Tax Filing"
)


# 4
prof_gender_education_all <- bind_rows(
  prof_gender_education_purchase,
  prof_gender_education_banking,
  prof_gender_education_investing,
  prof_gender_education_tax
)


# 5
prof_gender_education_wide <- prof_gender_education_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 6
prof_gender_education_wide <- prof_gender_education_wide %>%
  select(-Order)


# 7
r2_row_gender_education <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_gender_education <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)


# 8
prof_gender_education_final <- bind_rows(
  prof_gender_education_wide,
  r2_row_gender_education,
  adjr2_row_gender_education
)


# 9
gt_gender_education <- prof_gender_education_final %>%
  gt() %>%
  fmt_markdown(columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")) %>%
  tab_header(
    title = "Gender × Education: Group Effects"
  ) %>%
  cols_align(
    align = "center",
    columns = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing")
  ) %>%
  cols_align(
    align = "left",
    columns = "Group"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Males with high school or less are the reference group. All regressions control for digital skills, digital access, income, age, geography, labour status, internet quality and demographic characteristics."
  )


# 10
gtsave(
  gt_gender_education,
  "Tables/Interactions/Gender/GENDER_EDUCATION_FRIENDLY_TABLE.html"
)

