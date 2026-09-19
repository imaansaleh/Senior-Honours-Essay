# Sample Characteristics table

library(dplyr)
library(tibble)
library(gt)

sample_characteristics_table <- tibble(
  Variable = c(
    "Percent online purchase",
    "Percent online banking",
    "Percent online investing",
    "Percent online tax filing",
    "Percent smartphone access",
    "Percent computer access",
    "Percent high-speed internet",
    "Percent file management skills",
    "Percent word processing skills",
    "Percent presentation skills",
    "Percent spreadsheet skills",
    "Percent immigrant",
    "Percent visible minority",
    "Percent disability",
    "Percent Indigenous",
    "Percent rural",
    "Internet quality",
    "Income quintile",
    "  Q1",
    "  Q2",
    "  Q3",
    "  Q4",
    "  Q5",
    "Education",
    "  High school or less",
    "  Some post-secondary",
    "  University",
    "Gender",
    "  Female",
    "  Male",
    "Age group",
    "  15–24",
    "  25–34",
    "  35–44",
    "  45–54",
    "  55–64",
    "  65+",
    "Province group",
    "  Ontario",
    "  Atlantic",
    "  Prairies",
    "  Quebec",
    "  British Columbia",
    "N"
  ),
  Statistic = c(
    sprintf("%.1f", mean(sample_table_data$`Online purchase`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Online banking`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Online investing`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Online tax filing`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Smartphone access`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Computer access`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`High-speed internet`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`File management skills`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Word processing skills`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Presentation skills`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Spreadsheet skills`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Immigrant, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Visible minority`, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Disability, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Indigenous, na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Rural, na.rm = TRUE) * 100),
    sprintf("%.3f (%.3f)",
            mean(sample_table_data$`Internet quality`, na.rm = TRUE),
            sd(sample_table_data$`Internet quality`, na.rm = TRUE)),
    "",
    sprintf("%.1f", mean(sample_table_data$`Income quintile` == "Q1", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Income quintile` == "Q2", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Income quintile` == "Q3", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Income quintile` == "Q4", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Income quintile` == "Q5", na.rm = TRUE) * 100),
    "",
    sprintf("%.1f", mean(sample_table_data$Education == "High school or less", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Education == "Some post-secondary", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Education == "University", na.rm = TRUE) * 100),
    "",
    sprintf("%.1f", mean(sample_table_data$Gender == "Female", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$Gender == "Male", na.rm = TRUE) * 100),
    "",
    sprintf("%.1f", mean(sample_table_data$`Age group` == "15–24", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Age group` == "25–34", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Age group` == "35–44", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Age group` == "45–54", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Age group` == "55–64", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Age group` == "65+", na.rm = TRUE) * 100),
    "",
    sprintf("%.1f", mean(sample_table_data$`Province group` == "Ontario", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Province group` == "Atlantic", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Province group` == "Prairies", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Province group` == "Quebec", na.rm = TRUE) * 100),
    sprintf("%.1f", mean(sample_table_data$`Province group` == "British Columbia", na.rm = TRUE) * 100),
    format(nrow(sample_table_data), big.mark = ",")
  )
)

gt_sample <- sample_characteristics_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Sample Characteristics"
  ) %>%
  cols_label(
    Variable = "Variable",
    Statistic = "Mean (SD)/Percent"
  ) %>%
  cols_align(
    align = "left",
    columns = Variable
  ) %>%
  cols_align(
    align = "center",
    columns = Statistic
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(
      rows = Variable %in% c("Income quintile", "Education", "Gender", "Age group", "Province group", "N")
    )
  ) %>%
  tab_options(
    table.font.size = px(11),
    heading.title.font.size = px(13),
    column_labels.font.size = px(11),
    data_row.padding = px(1),
    column_labels.padding = px(2),
    heading.padding = px(2),
    table.width = pct(100)
  )

gtsave(gt_sample, "sample_characteristics_table.html")

# Fix gender x edu

cius_work <- cius_work %>%
  mutate(
    highschool_dummy = ifelse(education == 1, 1, 0),
    some_postsec_dummy = ifelse(education == 2, 1, 0)
    # university = omitted group
  )

# 2

cius_work$gender <- factor(cius_work$gender,
                           levels = c("Male", "Female"))




# 3

levels(cius_work$gender)

# 4
cius_work <- cius_work %>%
  mutate(
    education_group = case_when(
      education == 1 ~ "High school or less",
      education == 2 ~ "Some post-secondary",
      education == 3 ~ "University",
      TRUE ~ NA_character_
    )
  )
cius_work$education_group <- factor(
  cius_work$education_group,
  levels = c("University", "Some post-secondary", "High school or less")
)

# 5
levels(cius_work$education_group)
table(cius_work$education_group, useNA = "ifany")

table(cius_work$education, useNA = "ifany")

# 6

cius_work <- cius_work %>%
  filter(education %in% c(1, 2, 3))

table(cius_work$education_group, useNA = "ifany")


# Re-do Gender x Edu with Male + University as omitted group

library(dplyr)
library(tidyr)
library(modelsummary)
library(sandwich)
library(gt)


# 1
cius_work$education_factor <- factor(
  cius_work$education,
  levels = c(1, 2, 3),
  labels = c("High school or less", "Some post-secondary", "University")
)

# CHANGE THE REFERENCE GROUP HERE
cius_work$education_factor <- relevel(
  cius_work$education_factor,
  ref = "University"
)

levels(cius_work$education_factor)

# 2
interaction_gender_education_purchase_uni <- lm(
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

interaction_gender_education_banking_uni <- lm(
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

interaction_gender_education_investing_uni <- lm(
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

interaction_gender_education_tax_uni <- lm(
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

# 4
# SAME TABLE STYLE AS YOUR CURRENT ONE
# but now the omitted group is Male with university education

make_gender_education_prof_table_uni <- function(model, outcome_name) {

  data.frame(
    Order = 1:6,
    Group = c(
      "Male with university education (reference)",
      "with some post-secondary",
      "with high school or less",
      "Female with university education",
      "with some post-secondary",
      "with high school or less"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",

      # Male with some post-secondary
      make_group_cell(model, c("education_factorSome post-secondary")),

      # Male with high school or less
      make_group_cell(model, c("education_factorHigh school or less")),

      # Female with university education
      make_group_cell(model, c("factor(gender)Female")),

      # Female with some post-secondary
      make_group_cell(model, c(
        "factor(gender)Female",
        "education_factorSome post-secondary",
        "education_factorSome post-secondary:factor(gender)Female"
      )),

      # Female with high school or less
      make_group_cell(model, c(
        "factor(gender)Female",
        "education_factorHigh school or less",
        "education_factorHigh school or less:factor(gender)Female"
      ))
    ),
    stringsAsFactors = FALSE
  )
}

# 5
prof_gender_education_purchase_uni <- make_gender_education_prof_table_uni(
  interaction_gender_education_purchase_uni,
  "Online Purchase"
)

prof_gender_education_banking_uni <- make_gender_education_prof_table_uni(
  interaction_gender_education_banking_uni,
  "Online Banking"
)

prof_gender_education_investing_uni <- make_gender_education_prof_table_uni(
  interaction_gender_education_investing_uni,
  "Online Investing"
)

prof_gender_education_tax_uni <- make_gender_education_prof_table_uni(
  interaction_gender_education_tax_uni,
  "Online Tax Filing"
)

# 6
prof_gender_education_all_uni <- bind_rows(
  prof_gender_education_purchase_uni,
  prof_gender_education_banking_uni,
  prof_gender_education_investing_uni,
  prof_gender_education_tax_uni
)

# 7
prof_gender_education_wide_uni <- prof_gender_education_all_uni %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order)


# 8
prof_gender_education_wide_uni <- prof_gender_education_wide_uni %>%
  select(-Order)

# 9
r2_row_gender_education_uni <- data.frame(
  Group = "R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase_uni)$r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking_uni)$r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing_uni)$r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax_uni)$r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

adjr2_row_gender_education_uni <- data.frame(
  Group = "Adjusted R²",
  "Online Purchase" = sprintf("%.3f", summary(interaction_gender_education_purchase_uni)$adj.r.squared),
  "Online Banking" = sprintf("%.3f", summary(interaction_gender_education_banking_uni)$adj.r.squared),
  "Online Investing" = sprintf("%.3f", summary(interaction_gender_education_investing_uni)$adj.r.squared),
  "Online Tax Filing" = sprintf("%.3f", summary(interaction_gender_education_tax_uni)$adj.r.squared),
  check.names = FALSE,
  stringsAsFactors = FALSE
)

# 10
prof_gender_education_final_uni <- bind_rows(
  prof_gender_education_wide_uni,
  r2_row_gender_education_uni,
  adjr2_row_gender_education_uni
)

# 11
gt_gender_education_uni <- prof_gender_education_final_uni %>%
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

# 12
gtsave(
  gt_gender_education_uni,
  "Tables/Interactions/Gender/GENDER_EDUCATION_FRIENDLY_TABLE_UNIVERSITY_REF.html"
)


# Fix OLS

# 1

cius_work <- cius_work %>%
  mutate(
    quality_occasional = case_when(
      quality_basic == 2 ~ 1,                 # occasional problems
      quality_basic %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    quality_frequent = case_when(
      quality_basic == 1 ~ 1,                 # frequent problems
      quality_basic %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(cius_work$quality_basic, cius_work$quality_occasional, useNA = "ifany")
table(cius_work$quality_basic, cius_work$quality_frequent, useNA = "ifany")

# 2 Online purchase

model_purchase_clean <- lm(
  dfs_purchase ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income
    income_quintile +

    # Education
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)


# 3 Online banking

model_banking_clean <- lm(
  dfs_banking ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income
    income_quintile +

    # Education
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 4 online investing

model_investing_clean <- lm(
  dfs_investing ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income
    income_quintile +

    # Education
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 5 online tax filing

model_tax_clean <- lm(
  dfs_tax ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income
    income_quintile +

    # Education
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 6

models <- list(
  "Online Purchase" = model_purchase_clean,
  "Online Banking" = model_banking_clean,
  "Online Investing" = model_investing_clean,
  "Online Tax Filing" = model_tax_clean
)


# 7

modelsummary(
  models,

  vcov = "HC1",

  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",

    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet",

    "quality_occasional" = "Internet quality: occasional problems",
    "quality_frequent" = "Internet quality: frequent problems",

    "income_quintile2" = "Income Q2",
    "income_quintile3" = "Income Q3",
    "income_quintile4" = "Income Q4",
    "income_quintile5" = "Income Q5",

    "some_postsec_dummy" = "Some post-secondary",
    "university_dummy" = "University",

    "rural_dummy" = "Rural",
    "factor(province_group)Atlantic" = "Atlantic",
    "factor(province_group)Prairies" = "Prairies",
    "factor(province_group)Quebec" = "Quebec",
    "factor(province_group)British Columbia" = "British Columbia",

    "not_employed_dummy" = "Not employed",
    "genderFemale" = "Female",

    "immigrant_dummy" = "Immigrant",
    "visible_minority_dummy" = "Visible minority",
    "disability_dummy" = "Disability",
    "indigenous_dummy" = "Indigenous",

    "age_15_24" = "Age 15–24",
    "age_35_44" = "Age 35–44",
    "age_45_54" = "Age 45–54",
    "age_55_64" = "Age 55–64",
    "age_65_plus" = "Age 65+"
  ),

  coef_omit = "Intercept",

  stars = c("*" = .05, "**" = .01, "***" = .001),
  fmt = 3,

  gof_map = c("nobs", "r.squared", "adj.r.squared"),

  notes = c(
    "Linear probability models with HC1 robust standard errors.",
    "Reference groups: Income Q1, High school or less, Male, Ontario, Urban, Employed, Age 25–34, and no internet problems."
  ),

  output = "ALL_MODELS_TABLE_UPDATED_QUALITY.html"
)








# re-do

library(dplyr)
library(modelsummary)
library(sandwich)

# Make sure income baseline is Q1
cius_work$income_quintile <- factor(cius_work$income_quintile)
cius_work$income_quintile <- relevel(cius_work$income_quintile, ref = "1")

# Make sure gender baseline is Male
cius_work$gender <- factor(cius_work$gender, levels = c("Male", "Female"))

# Education dummies with UNIVERSITY omitted
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

# Internet quality dummies with NO PROBLEMS omitted
cius_work <- cius_work %>%
  mutate(
    quality_occasional = case_when(
      quality_basic == 2 ~ 1,                 # occasional problems
      quality_basic %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    quality_frequent = case_when(
      quality_basic == 1 ~ 1,                 # frequent problems
      quality_basic %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

# Quick checks
table(cius_work$education, cius_work$highschool_dummy, useNA = "ifany")
table(cius_work$education, cius_work$some_postsec_dummy, useNA = "ifany")
table(cius_work$quality_basic, cius_work$quality_occasional, useNA = "ifany")
table(cius_work$quality_basic, cius_work$quality_frequent, useNA = "ifany")
levels(cius_work$income_quintile)
levels(cius_work$gender)


# 2

model_purchase_clean <- lm(
  dfs_purchase ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income (reference = Q1)
    income_quintile +

    # Education (reference = university)
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (reference = male)
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age (reference = 25–34)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)


# 3

model_banking_clean <- lm(
  dfs_banking ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income (reference = Q1)
    income_quintile +

    # Education (reference = university)
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (reference = male)
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age (reference = 25–34)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 4

model_investing_clean <- lm(
  dfs_investing ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income (reference = Q1)
    income_quintile +

    # Education (reference = university)
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (reference = male)
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age (reference = 25–34)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 5

model_tax_clean <- lm(
  dfs_tax ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +

    # Internet quality (reference = no problems)
    quality_occasional + quality_frequent +

    # Income (reference = Q1)
    income_quintile +

    # Education (reference = university)
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy + factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (reference = male)
    gender +

    # Identity
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy +

    # Age (reference = 25–34)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

# 6

models <- list(
  "Online Purchase" = model_purchase_clean,
  "Online Banking" = model_banking_clean,
  "Online Investing" = model_investing_clean,
  "Online Tax Filing" = model_tax_clean
)

# 7

library(gt)

tab <- modelsummary(
  models,

  vcov = "HC1",

  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",
    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet",
    "quality_occasional" = "Internet quality: occasional problems",
    "quality_frequent" = "Internet quality: frequent problems",
    "income_quintile2" = "Income Q2",
    "income_quintile3" = "Income Q3",
    "income_quintile4" = "Income Q4",
    "income_quintile5" = "Income Q5",
    "highschool_dummy" = "High school or less",
    "some_postsec_dummy" = "Some post-secondary",
    "rural_dummy" = "Rural",
    "factor(province_group)Atlantic" = "Atlantic",
    "factor(province_group)Prairies" = "Prairies",
    "factor(province_group)Quebec" = "Quebec",
    "factor(province_group)British Columbia" = "British Columbia",
    "not_employed_dummy" = "Not employed",
    "genderFemale" = "Female",
    "immigrant_dummy" = "Immigrant",
    "visible_minority_dummy" = "Visible minority",
    "disability_dummy" = "Disability",
    "indigenous_dummy" = "Indigenous",
    "age_15_24" = "Age 15–24",
    "age_35_44" = "Age 35–44",
    "age_45_54" = "Age 45–54",
    "age_55_64" = "Age 55–64",
    "age_65_plus" = "Age 65+"
  ),

  coef_omit = "Intercept",
  stars = c("+" = .1, "*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  gof_map = c("nobs", "r.squared", "adj.r.squared"),

  notes = c(
    "Linear probability models with HC1 robust standard errors.",
    "Reference groups: No internet problems, Income Q1, University, Ontario, Urban, Employed, Male, Non-immigrant, Not a visible minority, No disability, Non-Indigenous, and Age 25–34."
  ),

  output = "gt"
)

# 🔥 FORCE CLEAN BLACK TEXT
tab <- tab %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.size = "14px"
  )
tab <- tab %>%
  tab_style(
    style = cell_borders(sides = "bottom", color = "transparent"),
    locations = cells_body()
  )

gtsave(tab, "ALL_MODELS_TABLE_UPDATED_QUALITY.html")







# Testing gender x edu with binary quality variable


# 1

library(dplyr)
library(sandwich)
library(tidyr)
library(gt)

# Education factor with UNIVERSITY as reference
cius_work$education_factor <- factor(
  cius_work$education,
  levels = c(1, 2, 3),
  labels = c("High school or less", "Some post-secondary", "University")
)

cius_work$education_factor <- relevel(
  cius_work$education_factor,
  ref = "University"
)

# Gender factor with MALE as reference
cius_work$gender <- factor(cius_work$gender, levels = c("Male", "Female"))

levels(cius_work$education_factor)
levels(cius_work$gender)

# 2

interaction_gender_education_purchase <- lm(
  dfs_purchase ~
    education_factor * gender +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_occasional + quality_frequent +
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
    education_factor * gender +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_occasional + quality_frequent +
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
    education_factor * gender +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_occasional + quality_frequent +
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
    education_factor * gender +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_occasional + quality_frequent +
    income_quintile +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# 3

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

# 4

make_gender_education_prof_table <- function(model, outcome_name) {

  data.frame(
    Order = 1:6,
    Group = c(
      "Male with university education (reference)",
      "with some post-secondary",
      "with high school or less",
      "Female with university education",
      "with some post-secondary",
      "with high school or less"
    ),
    Outcome = outcome_name,
    Cell = c(
      "0.000",

      # Male with some post-secondary
      make_group_cell(model, c("education_factorSome post-secondary")),

      # Male with high school or less
      make_group_cell(model, c("education_factorHigh school or less")),

      # Female with university education
      make_group_cell(model, c("genderFemale")),

      # Female with some post-secondary
      make_group_cell(model, c(
        "genderFemale",
        "education_factorSome post-secondary",
        "education_factorSome post-secondary:genderFemale"
      )),

      # Female with high school or less
      make_group_cell(model, c(
        "genderFemale",
        "education_factorHigh school or less",
        "education_factorHigh school or less:genderFemale"
      ))
    ),
    stringsAsFactors = FALSE
  )
}

# 5

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

# 6

prof_gender_education_all <- bind_rows(
  prof_gender_education_purchase,
  prof_gender_education_banking,
  prof_gender_education_investing,
  prof_gender_education_tax
)

prof_gender_education_wide <- prof_gender_education_all %>%
  pivot_wider(
    id_cols = c(Order, Group),
    names_from = Outcome,
    values_from = Cell
  ) %>%
  arrange(Order) %>%
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

prof_gender_education_final <- bind_rows(
  prof_gender_education_wide,
  r2_row_gender_education,
  adjr2_row_gender_education
)

# 8

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
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.size = "14px"
  ) %>%
  tab_source_note(
    source_note = "+ p < 0.1, * p < 0.05, ** p < 0.01, *** p < 0.001"
  ) %>%
  tab_source_note(
    source_note = "Males with university education are the reference group. All regressions control for digital skills, digital access, internet quality, income, age, geography, labour status and demographic characteristics."
  )

# 9

gtsave(
  gt_gender_education,
  "GENDER_EDUCATION_UPDATED_QUALITY.html"
)

getwd()


# Fix sample characteristics table

library(dplyr)
library(gt)

# --------------------------------------------------
# 1. Make sure the quality dummies already exist
# --------------------------------------------------

cius_work <- cius_work %>%
  mutate(
    quality_occasional = case_when(
      quality_basic == 2 ~ 1,
      quality_basic %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    ),

    quality_frequent = case_when(
      quality_basic == 1 ~ 1,
      quality_basic %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    )
  )

# --------------------------------------------------
# 2. Create summary values
# --------------------------------------------------

sample_table <- data.frame(
  Variable = c(
    "Percent online purchase",
    "Percent online banking",
    "Percent online investing",
    "Percent online tax filing",
    "Percent smartphone access",
    "Percent computer access",
    "Percent high-speed internet",
    "Percent file management skills",
    "Percent word processing skills",
    "Percent presentation skills",
    "Percent spreadsheet skills",
    "Percent immigrant",
    "Percent visible minority",
    "Percent disability",
    "Percent Indigenous",
    "Percent rural",
    "Percent internet problems (occasional)",
    "Percent internet problems (frequent)",

    "Income quintile",
    "Q1",
    "Q2",
    "Q3",
    "Q4",
    "Q5",

    "Education",
    "High school or less",
    "Some post-secondary",
    "University",

    "Gender",
    "Female",
    "Male",

    "Age group",
    "15–24",
    "25–34",
    "35–44",
    "45–54",
    "55–64",
    "65+",

    "Province group",
    "Ontario",
    "Atlantic",
    "Prairies",
    "Quebec",
    "British Columbia",

    "N"
  ),

  `Mean (SD)/Percent` = c(
    round(mean(cius_work$dfs_purchase, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_banking, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_investing, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_tax, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_smartphone_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_computer_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_highspeed_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_files, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_word, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_presentation, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_spreadsheet, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$immigrant_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$visible_minority_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$disability_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$indigenous_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$rural_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$quality_occasional, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$quality_frequent, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$income_quintile == "1", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "2", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "3", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "4", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "5", na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$education == 1, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$education == 2, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$education == 3, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$gender == "Female", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$gender == "Male", na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$age_group == 1, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 2, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 3, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 4, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 5, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 6, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$province_group == "Ontario", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Atlantic", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Prairies", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Quebec", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "British Columbia", na.rm = TRUE) * 100, 1),

    format(nrow(cius_work), big.mark = ",")
  ),
  stringsAsFactors = FALSE
)

# --------------------------------------------------
# 3. Identify section header rows
# --------------------------------------------------

section_rows <- which(sample_table$Variable %in% c(
  "Income quintile",
  "Education",
  "Gender",
  "Age group",
  "Province group",
  "N"
))

# --------------------------------------------------
# 4. Make the gt table
# --------------------------------------------------

gt_sample_table <- sample_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Sample Characteristics"
  ) %>%
  cols_label(
    Variable = "Variable",
    `Mean (SD)/Percent` = "Mean (SD)/Percent"
  ) %>%
  cols_align(
    align = "left",
    columns = "Variable"
  ) %>%
  cols_align(
    align = "center",
    columns = "Mean (SD)/Percent"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = section_rows)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = sample_table$Variable == "N")
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.size = "14px",
    data_row.padding = px(4)
  )




# New Sample Characteristics

library(dplyr)
library(gt)


# 1. Create internet quality dummies

cius_work <- cius_work %>%
  mutate(
    quality_frequent = case_when(
      quality_basic == 1 ~ 1,
      quality_basic %in% c(2, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    quality_occasional = case_when(
      quality_basic == 2 ~ 1,
      quality_basic %in% c(1, 3) ~ 0,
      TRUE ~ NA_real_
    ),
    quality_none = case_when(
      quality_basic == 3 ~ 1,
      quality_basic %in% c(1, 2) ~ 0,
      TRUE ~ NA_real_
    )
  )

# 2. Build sample characteristics table

sample_table <- data.frame(
  Variable = c(
    "Percent online purchase",
    "Percent online banking",
    "Percent online investing",
    "Percent online tax filing",
    "Percent smartphone access",
    "Percent computer access",
    "Percent high-speed internet",
    "Percent file management skills",
    "Percent word processing skills",
    "Percent presentation skills",
    "Percent spreadsheet skills",
    "Percent immigrant",
    "Percent visible minority",
    "Percent disability",
    "Percent Indigenous",
    "Percent rural",
    "Percent internet problems (frequent)",
    "Percent internet problems (occasional)",
    "Percent no internet problems",

    "Income quintile",
    "Q1",
    "Q2",
    "Q3",
    "Q4",
    "Q5",

    "Education",
    "High school or less",
    "Some post-secondary",
    "University",

    "Gender",
    "Female",
    "Male",

    "Age group",
    "15–24",
    "25–34",
    "35–44",
    "45–54",
    "55–64",
    "65+",

    "Province group",
    "Ontario",
    "Atlantic",
    "Prairies",
    "Quebec",
    "British Columbia",

    "N"
  ),

  `Mean (SD)/Percent` = c(
    round(mean(cius_work$dfs_purchase, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_banking, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_investing, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$dfs_tax, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_smartphone_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_computer_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$access_highspeed_binary, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_files, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_word, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_presentation, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$skill_spreadsheet, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$immigrant_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$visible_minority_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$disability_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$indigenous_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$rural_dummy, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$quality_frequent, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$quality_occasional, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$quality_none, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$income_quintile == "1", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "2", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "3", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "4", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$income_quintile == "5", na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$education == 1, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$education == 2, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$education == 3, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$gender == "Female", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$gender == "Male", na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$age_group == 1, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 2, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 3, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 4, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 5, na.rm = TRUE) * 100, 1),
    round(mean(cius_work$age_group == 6, na.rm = TRUE) * 100, 1),

    "",
    round(mean(cius_work$province_group == "Ontario", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Atlantic", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Prairies", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "Quebec", na.rm = TRUE) * 100, 1),
    round(mean(cius_work$province_group == "British Columbia", na.rm = TRUE) * 100, 1),

    format(nrow(cius_work), big.mark = ",")
  ),

  stringsAsFactors = FALSE,
  check.names = FALSE
)

# --------------------------------------------------
# 3. Section header rows
# --------------------------------------------------

section_rows <- which(sample_table$Variable %in% c(
  "Income quintile",
  "Education",
  "Gender",
  "Age group",
  "Province group",
  "N"
))

# --------------------------------------------------
# 4. Make gt table
# --------------------------------------------------

gt_sample_table <- sample_table %>%
  gt() %>%
  tab_header(
    title = "Table 1. Sample Characteristics"
  ) %>%
  cols_label(
    Variable = "Variable",
    `Mean (SD)/Percent` = "Mean (SD)/Percent"
  ) %>%
  cols_align(
    align = "left",
    columns = "Variable"
  ) %>%
  cols_align(
    align = "center",
    columns = "Mean (SD)/Percent"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(rows = section_rows)
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.size = "14px",
    data_row.padding = px(4)
  )

# --------------------------------------------------
# 5. Save and open
# --------------------------------------------------

gtsave(gt_sample_table, "TABLE_1_SAMPLE_CHARACTERISTICS_UPDATED.html")
browseURL("TABLE_1_SAMPLE_CHARACTERISTICS_UPDATED.html")


# Calculate included and excluded observations by DFS outcome

n_total <- nrow(cius_work)
n_total

included_purchase <- sum(!is.na(cius_work$dfs_purchase))
included_banking  <- sum(!is.na(cius_work$dfs_banking))
included_investing <- sum(!is.na(cius_work$dfs_investing))
included_tax <- sum(!is.na(cius_work$dfs_tax))

excluded_purchase <- sum(is.na(cius_work$dfs_purchase))
excluded_banking  <- sum(is.na(cius_work$dfs_banking))
excluded_investing <- sum(is.na(cius_work$dfs_investing))
excluded_tax <- sum(is.na(cius_work$dfs_tax))




included_share_purchase <- included_purchase / n_total
excluded_share_purchase <- excluded_purchase / n_total

included_share_banking <- included_banking / n_total
excluded_share_banking <- excluded_banking / n_total

included_share_investing <- included_investing / n_total
excluded_share_investing <- excluded_investing / n_total

included_share_tax <- included_tax / n_total
excluded_share_tax <- excluded_tax / n_total



# Create inclusion/exclusion summary table

dfs_sample_summary <- data.frame(
  Service = c("Online Purchase", "Online Banking", "Online Investing", "Online Tax Filing"),
  Included = c(included_purchase, included_banking, included_investing, included_tax),
  Excluded = c(excluded_purchase, excluded_banking, excluded_investing, excluded_tax),
  Included_Share = c(included_share_purchase, included_share_banking, included_share_investing, included_share_tax),
  Excluded_Share = c(excluded_share_purchase, excluded_share_banking, excluded_share_investing, excluded_share_tax)
)

dfs_sample_summary

library(dplyr)
library(gt)

# --------------------------------------------------
# 1. Total base sample after universe restrictions
# --------------------------------------------------

n_total <- nrow(cius_work)

# --------------------------------------------------
# 2. Create summary table
# --------------------------------------------------

dfs_sample_summary <- data.frame(
  Service = c(
    "Online Purchase",
    "Online Banking",
    "Online Investing",
    "Online Tax Filing"
  ),

  Included = c(
    sum(!is.na(cius_work$dfs_purchase)),
    sum(!is.na(cius_work$dfs_banking)),
    sum(!is.na(cius_work$dfs_investing)),
    sum(!is.na(cius_work$dfs_tax))
  ),

  Excluded = c(
    sum(is.na(cius_work$dfs_purchase)),
    sum(is.na(cius_work$dfs_banking)),
    sum(is.na(cius_work$dfs_investing)),
    sum(is.na(cius_work$dfs_tax))
  ),

  stringsAsFactors = FALSE
) %>%
  mutate(
    `Base Sample` = n_total,
    `Included Share (%)` = Included / `Base Sample` * 100,
    `Excluded Share (%)` = Excluded / `Base Sample` * 100
  ) %>%
  select(Service, `Base Sample`, Included, Excluded, `Included Share (%)`, `Excluded Share (%)`)

# --------------------------------------------------
# 3. Make clean HTML table
# --------------------------------------------------

gt_dfs_sample_summary <- dfs_sample_summary %>%
  gt() %>%
  tab_header(
    title = "Inclusion and Exclusion by Digital Financial Service Outcome"
  ) %>%
  cols_label(
    Service = "Digital financial service",
    `Base Sample` = "Base sample",
    Included = "Included",
    Excluded = "Excluded",
    `Included Share (%)` = "Included share (%)",
    `Excluded Share (%)` = "Excluded share (%)"
  ) %>%
  cols_align(
    align = "left",
    columns = Service
  ) %>%
  cols_align(
    align = "center",
    columns = c(`Base Sample`, Included, Excluded, `Included Share (%)`, `Excluded Share (%)`)
  ) %>%
  fmt_number(
    columns = c(`Base Sample`, Included, Excluded),
    decimals = 0,
    sep_mark = ","
  ) %>%
  fmt_number(
    columns = c(`Included Share (%)`, `Excluded Share (%)`),
    decimals = 3
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_body()
  ) %>%
  tab_style(
    style = cell_text(color = "black"),
    locations = cells_column_labels(everything())
  ) %>%
  tab_options(
    table.background.color = "white",
    heading.background.color = "white",
    column_labels.background.color = "white",
    table.font.size = "14px",
    data_row.padding = px(5)
  ) %>%
  tab_source_note(
    source_note = "Included observations have valid binary responses (0/1); excluded observations are missing or non-response categories recoded as NA."
  )

# --------------------------------------------------
# 4. Save as HTML and open
# --------------------------------------------------

gtsave(gt_dfs_sample_summary, "DFS_INCLUSION_EXCLUSION_TABLE.html")
browseURL("DFS_INCLUSION_EXCLUSION_TABLE.html")
