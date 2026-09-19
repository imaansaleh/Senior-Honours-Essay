# Gender x Skills

interaction_gender_purchase <- lm(
  dfs_purchase ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(gender) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_banking <- lm(
  dfs_banking ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(gender) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_investing <- lm(
  dfs_investing ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(gender) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_tax <- lm(
  dfs_tax ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(gender) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master table

gender_skill_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  "factor(gender)Female" = "Female",
  "skill_files:factor(gender)Female" = "File skill × Female",
  "skill_word:factor(gender)Female" = "Word skill × Female",
  "skill_presentation:factor(gender)Female" = "Presentation skill × Female",
  "skill_spreadsheet:factor(gender)Female" = "Spreadsheet skill × Female"
)

models_gender <- list(
  "Online Purchase" = interaction_gender_purchase,
  "Online Banking" = interaction_gender_banking,
  "Online Investing" = interaction_gender_investing,
  "Online Tax Filing" = interaction_gender_tax
)

modelsummary(
  models_gender,
  coef_map = gender_skill_terms,
  stars = TRUE,
  title = "Gender Heterogeneity in the Effect of Digital Skills on Digital Financial Services Use",
  notes = "All regressions control for digital access, income, education, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_GENDER_SKILL_TABLE.html"
)

# Gender x Access

interaction_gender_access_purchase <- lm(
  dfs_purchase ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_access_banking <- lm(
  dfs_banking ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_access_investing <- lm(
  dfs_investing ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_gender_access_tax <- lm(
  dfs_tax ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(gender) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    income_quintile +
    highschool_dummy + some_postsec_dummy +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master table
gender_access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  "factor(gender)Female" = "Female",
  "access_smartphone_binary:factor(gender)Female" = "Smartphone × Female",
  "access_computer_binary:factor(gender)Female" = "Computer × Female",
  "access_highspeed_binary:factor(gender)Female" = "High-speed × Female"
)

models_gender_access <- list(
  "Online Purchase" = interaction_gender_access_purchase,
  "Online Banking" = interaction_gender_access_banking,
  "Online Investing" = interaction_gender_access_investing,
  "Online Tax Filing" = interaction_gender_access_tax
)

modelsummary(
  models_gender_access,
  coef_map = gender_access_terms,
  stars = TRUE,
  title = "Gender Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = "All regressions control for digital skills, income, education, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_GENDER_ACCESS_TABLE.html"
)

# Gender x Education

interaction_gender_education_purchase <- lm(
  dfs_purchase ~
    (highschool_dummy + some_postsec_dummy) * factor(gender) +
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
    (highschool_dummy + some_postsec_dummy) * factor(gender) +
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
    (highschool_dummy + some_postsec_dummy) * factor(gender) +
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
    (highschool_dummy + some_postsec_dummy) * factor(gender) +
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

# master table

gender_education_terms <- c(
  highschool_dummy = "High school or less",
  some_postsec_dummy = "Some post-secondary",
  "factor(gender)Female" = "Female",
  "highschool_dummy:factor(gender)Female" = "High school × Female",
  "some_postsec_dummy:factor(gender)Female" = "Some post-secondary × Female"
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
  notes = "All regressions control for digital skills, digital access, income, age, geography, labour status, internet quality and demographic characteristics. University education is the reference group.",
  output = "MASTER_GENDER_EDUCATION_TABLE.html"
)

# re-do OLS
model_purchase_final <- lm(
  dfs_purchase ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

model_banking_final <- lm(
  dfs_banking ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

model_investing_final <- lm(
  dfs_investing ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

model_tax_final <- lm(
  dfs_tax ~
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)
# master table
models_final <- list(
  "Online Purchase" = model_purchase_final,
  "Online Banking" = model_banking_final,
  "Online Investing" = model_investing_final,
  "Online Tax Filing" = model_tax_final
)

modelsummary(
  models_final,
  stars = TRUE,
  title = "Determinants of Digital Financial Services Use",
  notes = "All regressions control for digital skills, digital access, internet quality, income, education, gender, age, geography, labour status and demographic characteristics.",
  output = "FINAL_MAIN_RESULTS.html"
)

# Education x Skills

interaction_education_skill_purchase <- lm(
  dfs_purchase ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) *
    (highschool_dummy + some_postsec_dummy) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_skill_banking <- lm(
  dfs_banking ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) *
    (highschool_dummy + some_postsec_dummy) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_skill_investing <- lm(
  dfs_investing ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) *
    (highschool_dummy + some_postsec_dummy) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_skill_tax <- lm(
  dfs_tax ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) *
    (highschool_dummy + some_postsec_dummy) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master table

education_skill_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  highschool_dummy = "High school or less",
  some_postsec_dummy = "Some post-secondary",
  "skill_files:highschool_dummy" = "File skill × High school",
  "skill_word:highschool_dummy" = "Word skill × High school",
  "skill_presentation:highschool_dummy" = "Presentation skill × High school",
  "skill_spreadsheet:highschool_dummy" = "Spreadsheet skill × High school",
  "skill_files:some_postsec_dummy" = "File skill × Some postsec",
  "skill_word:some_postsec_dummy" = "Word skill × Some postsec",
  "skill_presentation:some_postsec_dummy" = "Presentation skill × Some postsec",
  "skill_spreadsheet:some_postsec_dummy" = "Spreadsheet skill × Some postsec"
)

models_education_skill <- list(
  "Online Purchase" = interaction_education_skill_purchase,
  "Online Banking" = interaction_education_skill_banking,
  "Online Investing" = interaction_education_skill_investing,
  "Online Tax Filing" = interaction_education_skill_tax
)

modelsummary(
  models_education_skill,
  coef_map = education_skill_terms,
  stars = TRUE,
  title = "Education Heterogeneity in the Effect of Digital Skills on Digital Financial Services Use",
  notes = "University education is the reference group. All regressions control for digital access, income, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_EDUCATION_SKILL_TABLE.html"
)

# Education x Access

interaction_education_access_purchase <- lm(
  dfs_purchase ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) *
    (highschool_dummy + some_postsec_dummy) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_access_banking <- lm(
  dfs_banking ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) *
    (highschool_dummy + some_postsec_dummy) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_access_investing <- lm(
  dfs_investing ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) *
    (highschool_dummy + some_postsec_dummy) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_education_access_tax <- lm(
  dfs_tax ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) *
    (highschool_dummy + some_postsec_dummy) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)


# master table
education_access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  highschool_dummy = "High school or less",
  some_postsec_dummy = "Some post-secondary",
  "access_smartphone_binary:highschool_dummy" = "Smartphone × High school",
  "access_computer_binary:highschool_dummy" = "Computer × High school",
  "access_highspeed_binary:highschool_dummy" = "High-speed × High school",
  "access_smartphone_binary:some_postsec_dummy" = "Smartphone × Some postsec",
  "access_computer_binary:some_postsec_dummy" = "Computer × Some postsec",
  "access_highspeed_binary:some_postsec_dummy" = "High-speed × Some postsec"
)

models_education_access <- list(
  "Online Purchase" = interaction_education_access_purchase,
  "Online Banking" = interaction_education_access_banking,
  "Online Investing" = interaction_education_access_investing,
  "Online Tax Filing" = interaction_education_access_tax
)

modelsummary(
  models_education_access,
  coef_map = education_access_terms,
  stars = TRUE,
  title = "Education Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = "University education is the reference group. All regressions control for digital skills, income, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_EDUCATION_ACCESS_TABLE.html"
)
