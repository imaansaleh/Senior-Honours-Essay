# 4. INTERACTION ANALYSIS

# Create age-group indicators for interaction analysis
cius_work <- cius_work %>%
  mutate(
    age_55_plus = case_when(
      age_group %in% c(5, 6) ~ 1,
      age_group %in% c(1, 2, 3, 4) ~ 0,
      TRUE ~ NA_real_
    ),

    age_under_55 = case_when(
      age_group %in% c(1, 2, 3, 4) ~ 1,
      age_group %in% c(5, 6) ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$age_group, cius_work$age_55_plus, useNA="ifany")
table(cius_work$age_group, cius_work$age_under_55, useNA="ifany")

# Age x Skill - Purchase Interaction

interaction_age_purchase <- lm(
  dfs_purchase ~

    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * age_55_plus +

    access_smartphone_binary +
    access_computer_binary +
    access_highspeed_binary +
    quality_basic_ord +

    factor(income_quintile) +

    highschool_dummy +
    some_postsec_dummy +

    rural_dummy +
    factor(province_group) +

    not_employed_dummy +

    # Include gender control
    factor(gender) +

    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy,

  data = cius_work
)
modelsummary(interaction_age_purchase)

coef_labels <- c(

  # Skills
  skill_files = "File management skill",
  skill_word = "Word processing skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",

  # Age
  age_55_plus = "Age 55+",

  # Interactions
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+",

  # Access
  access_smartphone_binary = "Access to smartphone",
  access_computer_binary = "Access to computer",
  access_highspeed_binary = "Access to high-speed internet",
  quality_basic_ord = "Internet quality",

  # Income
  "factor(income_quintile)2" = "Income quintile 2",
  "factor(income_quintile)3" = "Income quintile 3",
  "factor(income_quintile)4" = "Income quintile 4",
  "factor(income_quintile)5" = "Income quintile 5",

  # Education
  highschool_dummy = "High school or less",
  some_postsec_dummy = "Some post-secondary",

  # Geography
  rural_dummy = "Rural",
  "factor(province_group)Atlantic" = "Atlantic",
  "factor(province_group)Prairies" = "Prairies",
  "factor(province_group)Quebec" = "Quebec",
  "factor(province_group)British Columbia" = "British Columbia",

  # Labour
  not_employed_dummy = "Not employed",

  # Gender
  "factor(gender)Female" = "Female",

  # Identity
  immigrant_dummy = "Immigrant",
  visible_minority_dummy = "Visible minority",
  disability_dummy = "Disability",
  indigenous_dummy = "Indigenous"
)

library(modelsummary)
modelsummary(
  interaction_age_purchase,
  coef_map = coef_labels,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log",
  title = "Interaction Model: Age and Digital Skills on Online Purchase"
)

modelsummary(
  interaction_purchase,
  coef_map = coef_labels,
  stars = TRUE,
  gof_omit = "AIC|BIC|Log",
  title = "Interaction Model: Age and Digital Skills on Online Purchase",
  output = "interaction_age_purchase.html"
)

# Generate table focused on interaction terms
library(modelsummary)

main_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  age_55_plus = "Age 55+",
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+"
)
modelsummary(
  interaction_purchase,
  coef_map = main_terms,
  stars = TRUE,
  title = "Age Interaction Effects on Online Purchase",
  notes = "Model includes additional controls for income, education, geography, labour status, gender, and identity characteristics.",
  output = "interaction_purchase_mainterms.html"
)

# Age x Skill - Online Banking Interaction

interaction_skill_banking <- lm(
  dfs_banking ~

    # SKILL INTERACTIONS (MAIN MECHANISM)
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * age_55_plus +

    # ACCESS (controls)
    access_smartphone_binary +
    access_computer_binary +
    access_highspeed_binary +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME (lowest quintile baseline)
    income_quintile +

    # EDUCATION (university baseline)
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR MARKET
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)

summary(interaction_skill_banking)

library(modelsummary)

banking_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  age_55_plus = "Age 55+",
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+"
)

modelsummary(
  interaction_skill_banking,
  coef_map = banking_terms,
  stars = TRUE,
  title = "Age Interaction Effects on Online Banking",
  notes = "All models control for income, education, geography, labour status, access, internet quality, gender and demographic characteristics.",
  output = "interaction_age_banking.html"
)

# Age x Skills - Online Investing
interaction_skill_investing <- lm(
  dfs_investing ~

    # SKILL INTERACTIONS
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * age_55_plus +

    # ACCESS (controls)
    access_smartphone_binary +
    access_computer_binary +
    access_highspeed_binary +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)
investing_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  age_55_plus = "Age 55+",
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+"
)

modelsummary(
  interaction_skill_investing,
  coef_map = investing_terms,
  stars = TRUE,
  title = "Age Interaction Effects on Online Investing",
  notes = "Controls for income, education, geography, labour status, access, internet quality, gender and demographic characteristics.",
  output = "interaction_age_investing.html"
)

# Age x Skill - Online Tax
interaction_skill_tax <- lm(
  dfs_tax ~

    # SKILL INTERACTIONS
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * age_55_plus +

    # ACCESS (controls)
    access_smartphone_binary +
    access_computer_binary +
    access_highspeed_binary +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)
tax_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  age_55_plus = "Age 55+",
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+"
)

modelsummary(
  interaction_skill_tax,
  coef_map = tax_terms,
  stars = TRUE,
  title = "Age Interaction Effects on Online Tax Filing",
  notes = "Controls for income, education, geography, labour status, access, internet quality, gender and demographic characteristics.",
  output = "interaction_age_tax.html"
)

# Combine Age x Skills - all DFS into 1 table

library(modelsummary)

interaction_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  age_55_plus = "Age 55+",
  "skill_files:age_55_plus" = "File skill × Age 55+",
  "skill_word:age_55_plus" = "Word skill × Age 55+",
  "skill_presentation:age_55_plus" = "Presentation skill × Age 55+",
  "skill_spreadsheet:age_55_plus" = "Spreadsheet skill × Age 55+"
)

models_interaction <- list(
  "Online Purchase" = interaction_age_purchase,
  "Online Banking" = interaction_skill_banking,
  "Online Investing" = interaction_skill_investing,
  "Online Tax Filing" = interaction_skill_tax
)

modelsummary(
  models_interaction,
  coef_map = interaction_terms,
  stars = TRUE,
  title = "Age Heterogeneity in the Effect of Digital Skills on Digital Financial Services Use",
  notes = "All regressions control for income, education, geography, labour status, access, internet quality, gender and demographic characteristics.",
  output = "MASTER_INTERACTION_TABLE.html"
)


# Interaction - Age x Access

# Age x Access - Purchase
interaction_access_purchase <- lm(
  dfs_purchase ~

    # ACCESS INTERACTIONS
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * age_55_plus +

    # SKILLS (controls)
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)
access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  age_55_plus = "Age 55+",
  "access_smartphone_binary:age_55_plus" = "Smartphone × Age 55+",
  "access_computer_binary:age_55_plus" = "Computer × Age 55+",
  "access_highspeed_binary:age_55_plus" = "High-speed × Age 55+"
)

modelsummary(
  interaction_access_purchase,
  coef_map = access_terms,
  stars = TRUE,
  title = "Age Interaction Effects of Digital Access on Online Purchasing",
  notes = "Controls for digital skills, income, education, geography, labour status, internet quality, gender and demographic characteristics.",
  output = "interaction_age_access_purchase.html"
)

# Age x Access - Online Banking

interaction_access_banking <- lm(
  dfs_banking ~

    # ACCESS INTERACTIONS
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * age_55_plus +

    # SKILLS (controls)
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)
access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  age_55_plus = "Age 55+",
  "access_smartphone_binary:age_55_plus" = "Smartphone × Age 55+",
  "access_computer_binary:age_55_plus" = "Computer × Age 55+",
  "access_highspeed_binary:age_55_plus" = "High-speed × Age 55+"
)
modelsummary(
  interaction_access_banking,
  coef_map = access_terms,
  stars = TRUE,
  title = "Age Interaction Effects of Digital Access on Online Banking",
  notes = "Controls for digital skills, income, education, geography, labour status, internet quality, gender and demographic characteristics.",
  output = "interaction_age_access_banking.html"
)

# Age x Access - Investing

interaction_access_investing <- lm(
  dfs_investing ~

    # ACCESS INTERACTIONS
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * age_55_plus +

    # SKILLS (controls)
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)

access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  age_55_plus = "Age 55+",
  "access_smartphone_binary:age_55_plus" = "Smartphone × Age 55+",
  "access_computer_binary:age_55_plus" = "Computer × Age 55+",
  "access_highspeed_binary:age_55_plus" = "High-speed × Age 55+"
)

modelsummary(
  interaction_access_investing,
  coef_map = access_terms,
  stars = TRUE,
  title = "Age Interaction Effects of Digital Access on Online Investing",
  notes = "Controls for digital skills, income, education, geography, labour status, internet quality, gender and demographic characteristics.",
  output = "interaction_age_access_investing.html"
)

# Age x Access - Online Tax Filing
interaction_access_tax <- lm(
  dfs_tax ~

    # ACCESS INTERACTIONS
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * age_55_plus +

    # SKILLS (controls)
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # INTERNET QUALITY
    quality_basic_ord +

    # INCOME
    income_quintile +

    # EDUCATION
    highschool_dummy + some_postsec_dummy +

    # GEOGRAPHY
    rural_dummy + factor(province_group) +

    # LABOUR
    not_employed_dummy +

    # GENDER
    factor(gender) +

    # IDENTITY
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,

  data = cius_work
)

access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  age_55_plus = "Age 55+",
  "access_smartphone_binary:age_55_plus" = "Smartphone × Age 55+",
  "access_computer_binary:age_55_plus" = "Computer × Age 55+",
  "access_highspeed_binary:age_55_plus" = "High-speed × Age 55+"
)

modelsummary(
  interaction_access_tax,
  coef_map = access_terms,
  stars = TRUE,
  title = "Age Interaction Effects of Digital Access on Online Tax Filing",
  notes = "Controls for digital skills, income, education, geography, labour status, internet quality, gender and demographic characteristics.",
  output = "interaction_age_access_tax.html"
)

# Combine all Age x Access DFS

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

modelsummary(
  models_access,
  coef_map = access_terms,
  stars = TRUE,
  title = "Age Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = "All regressions control for digital skills, income, education, geography, labour status, internet quality, gender and demographic characteristics.",
  output = "MASTER_ACCESS_INTERACTION_TABLE.html"
)

# New Interaction : Age x Education - DFS

interaction_education_purchase <- lm(
  dfs_purchase ~
    (highschool_dummy + some_postsec_dummy) * age_55_plus +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    factor(gender) +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_banking <- lm(
  dfs_banking ~
    (highschool_dummy + some_postsec_dummy) * age_55_plus +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    factor(gender) +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_investing <- lm(
  dfs_investing ~
    (highschool_dummy + some_postsec_dummy) * age_55_plus +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    factor(gender) +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_tax <- lm(
  dfs_tax ~
    (highschool_dummy + some_postsec_dummy) * age_55_plus +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    factor(gender) +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

education_terms <- c(
  highschool_dummy = "High school or less",
  some_postsec_dummy = "Some post-secondary",
  age_55_plus = "Age 55+",
  "highschool_dummy:age_55_plus" = "High school or less × Age 55+",
  "some_postsec_dummy:age_55_plus" = "Some post-secondary × Age 55+"
)

models_education <- list(
  "Online Purchase" = interaction_education_purchase,
  "Online Banking" = interaction_education_banking,
  "Online Investing" = interaction_education_investing,
  "Online Tax Filing" = interaction_education_tax
)

modelsummary(
  models_education,
  coef_map = education_terms,
  stars = TRUE,
  title = "Age Heterogeneity in the Effect of Education on Digital Financial Services Use",
  notes = "All regressions control for digital skills, digital access, income, geography, labour status, internet quality, gender and demographic characteristics. Reference education group is university.",
  output = "MASTER_EDUCATION_INTERACTION_TABLE.html"
)

