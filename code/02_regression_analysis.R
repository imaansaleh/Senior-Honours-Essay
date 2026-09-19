# 3. BASELINE REGRESSION ANALYSIS

# Model 1
# Online Purchase = f(Digital Skills)
model1 <- lm(
  dfs_purchase ~ 
    skill_files + skill_word + skill_presentation + skill_spreadsheet,
  data = cius_work
)
summary(model1)


# Online Purchase = f(Skills, Access, Quality)

model2 <- lm(
  dfs_purchase ~ 
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord + quality_video_ord,
  data = cius_work
)
summary(model2)

# Check correlation between the two internet-quality measures
cor(cius_work$quality_basic_ord,
    cius_work$quality_video_ord,
    use = "complete.obs")

# Re-estimate model excluding the video-quality measure
model2_clean <- lm(
  dfs_purchase ~ 
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord,
  data = cius_work
)
summary(model2_clean)

# Model 1

# Online Purchase = f(skills, access, quality,income, controls)

model_full <- lm(
  dfs_purchase ~ 
    # Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income
    factor(income_quintile) +

    # Education (university is baseline)
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour market
    not_employed_dummy +

    # Identity controls
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age (25–34 baseline)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

summary(model_full)

# Model 2
# Online Banking = f(skills, access, quality,income, controls)
model_banking <- lm(
  dfs_banking ~ 
    # Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income
    factor(income_quintile) +

    # Education
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour market
    not_employed_dummy +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)
summary(model_banking)

cius_work$income_quintile <- factor(cius_work$income_quintile)

cius_work$income_quintile <- relevel(
  cius_work$income_quintile,
  ref = "5"
)
cius_work$income_quintile <- factor(cius_work$income_quintile)
cius_work$income_quintile <- relevel(
  cius_work$income_quintile,
  ref = "5"
)
levels(cius_work$income_quintile)

# Model 3
# Online Investing = f(skills, access, quality,income, controls)

model_investing <- lm(
  dfs_investing ~ 
    # Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income (richest baseline already set)
    factor(income_quintile) +

    # Education
    highschool_dummy + some_postsec_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour market
    not_employed_dummy +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

summary(model_investing)

# Add online tax-filing outcome
cius_work <- cius_work %>%
  left_join(
    df %>% select(PUMFID, GV_010A),
    by = "PUMFID"
  )
cius_work <- cius_work %>%
  rename(dfs_tax = GV_010A)
names(cius_work)
table(cius_work$dfs_tax, useNA = "ifany")

# Convert online tax-filing outcome to a binary indicator
cius_work <- cius_work %>%
  mutate(
    dfs_tax = case_when(
      dfs_tax == 1 ~ 1,
      dfs_tax == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$dfs_tax, useNA="ifany")

# Model 4 dfs_tax = f(skills, access, quality,income, controls)

model_tax <- lm(
  dfs_tax ~ skill_files + skill_word + skill_presentation +
    skill_spreadsheet + access_smartphone_binary +
    access_computer_binary + access_highspeed_binary +
    quality_basic_ord + factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    rural_dummy + factor(province_group) +
    not_employed_dummy + immigrant_dummy +
    visible_minority_dummy + disability_dummy +
    indigenous_dummy + age_15_24 + age_35_44 +
    age_45_54 + age_55_64 + age_65_plus,
  data = cius_work
)
summary(model_tax)

model_purchase <- model_full

# Generate initial regression table
install.packages("modelsummary")
library(modelsummary)

models <- list(
  "Online Purchase" = model_purchase,
  "Online Banking" = model_banking,
  "Online Investing" = model_investing,
  "Online Tax Filing" = model_tax
)
modelsummary(models)
install.packages("rstudioapi")
library(rstudioapi)
modelsummary(models)

# Reset income reference category to the lowest quintile

cius_work$income_quintile <- factor(cius_work$income_quintile)

cius_work$income_quintile <- relevel(
  cius_work$income_quintile,
  ref = "1"
)

levels(cius_work$income_quintile)

cius_work$income_quintile <- factor(cius_work$income_quintile)
cius_work$income_quintile <- relevel(cius_work$income_quintile, ref = "1")
levels(cius_work$income_quintile)

# Re-estimate baseline models using final reference categories

# 1. Online Purchase
model_purchase_clean <- lm(
  dfs_purchase ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income (lowest quintile baseline)
    income_quintile +

    # Education (high school baseline)
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (Male baseline)
    gender +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age (25–34 baseline)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)
summary(model_purchase_clean)

# Format regression output
install.packages(c("modelsummary", "sandwich", "flextable", "officer"))
library(modelsummary)
library(sandwich)

modelsummary(
  list("Online Purchase" = model_purchase_clean),

  # Robust standard errors
  vcov = "HC1",

  # Rename coefficients
  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",
    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet access",
    "quality_basic_ord" = "Internet quality",
    "income_quintile2" = "Income quintile 2",
    "income_quintile3" = "Income quintile 3",
    "income_quintile4" = "Income quintile 4",
    "income_quintile5" = "Income quintile 5",
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

  # Good formatting
  stars = c("*" = .05, "**" = .01, "***" = .001),
  fmt = 3,

  # Keep useful goodness-of-fit stats
  gof_map = c("nobs", "r.squared", "adj.r.squared"),

  # Notes under table
  notes = c(
    "Dependent variable: Online purchase.",
    "Linear probability model with HC1 robust standard errors.",
    "Reference categories: income quintile 1, high school or less, male, Ontario, urban, employed, age 25–34.",
    "* p < 0.05, ** p < 0.01, *** p < 0.001"
  ),

  output = "table_purchase.docx"
)


library(modelsummary)
library(sandwich)

modelsummary(
  list("Online Purchase" = model_purchase_clean),

  vcov = "HC1",

  coef_omit = "Intercept",

  stars = c("*" = .05, "**" = .01, "***" = .001),
  fmt = 3,
  gof_map = c("nobs","r.squared","adj.r.squared"),

  output = "table_purchase.html"
)
getwd()

# 2. Online Banking
model_banking_clean <- lm(
  dfs_banking ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income (lowest quintile baseline)
    income_quintile +

    # Education (high school baseline)
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (Male baseline)
    gender +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age (25–34 baseline)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)
summary(model_banking_clean)
modelsummary(
  list("Online Banking" = model_banking_clean),

  vcov = "HC1",

  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",
    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet",
    "quality_basic_ord" = "Internet quality",
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
  gof_map = c("nobs","r.squared","adj.r.squared"),

  notes = c(
    "Dependent variable: Online banking.",
    "Linear probability model with HC1 robust standard errors.",
    "Reference groups: Income Q1, High school or less, Male, Ontario, Urban, Employed, Age 25–34."
  ),

  output = "table_banking.html"
)


# 3. Online Investing

model_investing_clean <- lm(
  dfs_investing ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income (lowest quintile baseline)
    income_quintile +

    # Education (high school baseline)
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (Male baseline)
    gender +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age (25–34 baseline)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

modelsummary(
  list("Online Investing" = model_investing_clean),

  vcov = "HC1",

  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",
    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet",
    "quality_basic_ord" = "Internet quality",
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
  gof_map = c("nobs","r.squared","adj.r.squared"),

  notes = c(
    "Dependent variable: Online investing.",
    "Linear probability model with HC1 robust standard errors.",
    "Reference groups: Income Q1, High school or less, Male, Ontario, Urban, Employed, Age 25–34."
  ),

  output = "table_investing.html"
)

# 4. Online Tax Filing

model_tax_clean <- lm(
  dfs_tax ~ 

    # Digital Skills
    skill_files + skill_word + skill_presentation + skill_spreadsheet +

    # Access
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +

    # Income (lowest quintile baseline)
    income_quintile +

    # Education (high school baseline)
    some_postsec_dummy + university_dummy +

    # Geography
    rural_dummy +
    factor(province_group) +

    # Labour
    not_employed_dummy +

    # Gender (Male baseline)
    gender +

    # Identity
    immigrant_dummy +
    visible_minority_dummy +
    disability_dummy +
    indigenous_dummy +

    # Age (25–34 baseline)
    age_15_24 + age_35_44 + age_45_54 + age_55_64 + age_65_plus,

  data = cius_work
)

modelsummary(
  list("Online Tax Filing" = model_tax_clean),

  vcov = "HC1",

  coef_map = c(
    "skill_files" = "File management skills",
    "skill_word" = "Word processing skills",
    "skill_presentation" = "Presentation skills",
    "skill_spreadsheet" = "Spreadsheet skills",
    "access_smartphone_binary" = "Smartphone access",
    "access_computer_binary" = "Computer access",
    "access_highspeed_binary" = "High-speed internet",
    "quality_basic_ord" = "Internet quality",
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
  gof_map = c("nobs","r.squared","adj.r.squared"),

  notes = c(
    "Dependent variable: Online tax filing.",
    "Linear probability model with HC1 robust standard errors.",
    "Reference groups: Income Q1, High school or less, Male, Ontario, Urban, Employed, Age 25–34."
  ),

  output = "table_tax.html"
)

# Combine all four baseline models

models <- list(
  "Online Purchase" = model_purchase_clean,
  "Online Banking" = model_banking_clean,
  "Online Investing" = model_investing_clean,
  "Online Tax Filing" = model_tax_clean
)

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
    "quality_basic_ord" = "Internet quality",

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

  gof_map = c(
    "nobs",
    "r.squared",
    "adj.r.squared"
  ),

  notes = c(
    "Linear probability models with HC1 robust standard errors.",
    "Reference groups: Income Q1, High school or less, Male, Ontario, Urban, Employed, Age 25–34."
  ),

  output = "ALL_MODELS_TABLE.html"
)
