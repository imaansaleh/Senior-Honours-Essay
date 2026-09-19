# Income x Skills
interaction_income_skill_purchase <- lm(
  dfs_purchase ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(income_quintile) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_skill_banking <- lm(
  dfs_banking ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(income_quintile) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_skill_investing <- lm(
  dfs_investing ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(income_quintile) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_skill_tax <- lm(
  dfs_tax ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * factor(income_quintile) +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master tables

income_skill_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  "factor(income_quintile)2" = "Income Q2",
  "factor(income_quintile)3" = "Income Q3",
  "factor(income_quintile)4" = "Income Q4",
  "factor(income_quintile)5" = "Income Q5",
  "skill_files:factor(income_quintile)2" = "File skill × Q2",
  "skill_files:factor(income_quintile)3" = "File skill × Q3",
  "skill_files:factor(income_quintile)4" = "File skill × Q4",
  "skill_files:factor(income_quintile)5" = "File skill × Q5",
  "skill_word:factor(income_quintile)2" = "Word skill × Q2",
  "skill_word:factor(income_quintile)3" = "Word skill × Q3",
  "skill_word:factor(income_quintile)4" = "Word skill × Q4",
  "skill_word:factor(income_quintile)5" = "Word skill × Q5",
  "skill_presentation:factor(income_quintile)2" = "Presentation × Q2",
  "skill_presentation:factor(income_quintile)3" = "Presentation × Q3",
  "skill_presentation:factor(income_quintile)4" = "Presentation × Q4",
  "skill_presentation:factor(income_quintile)5" = "Presentation × Q5",
  "skill_spreadsheet:factor(income_quintile)2" = "Spreadsheet × Q2",
  "skill_spreadsheet:factor(income_quintile)3" = "Spreadsheet × Q3",
  "skill_spreadsheet:factor(income_quintile)4" = "Spreadsheet × Q4",
  "skill_spreadsheet:factor(income_quintile)5" = "Spreadsheet × Q5"
)

models_income_skill <- list(
  "Online Purchase" = interaction_income_skill_purchase,
  "Online Banking" = interaction_income_skill_banking,
  "Online Investing" = interaction_income_skill_investing,
  "Online Tax Filing" = interaction_income_skill_tax
)

modelsummary(
  models_income_skill,
  coef_map = income_skill_terms,
  stars = TRUE,
  title = "Income Heterogeneity in the Effect of Digital Skills on Digital Financial Services Use",
  notes = "Income quintile 1 is the reference group. All regressions control for digital access, education, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_INCOME_SKILL_TABLE.html"
)

# Income x Access

interaction_income_access_purchase <- lm(
  dfs_purchase ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(income_quintile) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_access_banking <- lm(
  dfs_banking ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(income_quintile) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_access_investing <- lm(
  dfs_investing ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(income_quintile) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy + visible_minority_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_income_access_tax <- lm(
  dfs_tax ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * factor(income_quintile) +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
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
income_access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  "factor(income_quintile)2" = "Income Q2",
  "factor(income_quintile)3" = "Income Q3",
  "factor(income_quintile)4" = "Income Q4",
  "factor(income_quintile)5" = "Income Q5",
  "access_smartphone_binary:factor(income_quintile)2" = "Smartphone × Q2",
  "access_smartphone_binary:factor(income_quintile)3" = "Smartphone × Q3",
  "access_smartphone_binary:factor(income_quintile)4" = "Smartphone × Q4",
  "access_smartphone_binary:factor(income_quintile)5" = "Smartphone × Q5",
  "access_computer_binary:factor(income_quintile)2" = "Computer × Q2",
  "access_computer_binary:factor(income_quintile)3" = "Computer × Q3",
  "access_computer_binary:factor(income_quintile)4" = "Computer × Q4",
  "access_computer_binary:factor(income_quintile)5" = "Computer × Q5",
  "access_highspeed_binary:factor(income_quintile)2" = "High-speed × Q2",
  "access_highspeed_binary:factor(income_quintile)3" = "High-speed × Q3",
  "access_highspeed_binary:factor(income_quintile)4" = "High-speed × Q4",
  "access_highspeed_binary:factor(income_quintile)5" = "High-speed × Q5"
)

models_income_access <- list(
  "Online Purchase" = interaction_income_access_purchase,
  "Online Banking" = interaction_income_access_banking,
  "Online Investing" = interaction_income_access_investing,
  "Online Tax Filing" = interaction_income_access_tax
)

modelsummary(
  models_income_access,
  coef_map = income_access_terms,
  stars = TRUE,
  title = "Income Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = "Income quintile 1 is the reference group. All regressions control for digital skills, education, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_INCOME_ACCESS_TABLE.html"
)

# Visible minority x skills

interaction_minority_skill_purchase <- lm(
  dfs_purchase ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * visible_minority_dummy +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_skill_banking <- lm(
  dfs_banking ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * visible_minority_dummy +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_skill_investing <- lm(
  dfs_investing ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * visible_minority_dummy +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_skill_tax <- lm(
  dfs_tax ~
    (skill_files + skill_word + skill_presentation + skill_spreadsheet) * visible_minority_dummy +
    access_smartphone_binary + access_computer_binary + access_highspeed_binary +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master table
minority_skill_terms <- c(
  skill_files = "File skill",
  skill_word = "Word skill",
  skill_presentation = "Presentation skill",
  skill_spreadsheet = "Spreadsheet skill",
  visible_minority_dummy = "Visible minority",
  "skill_files:visible_minority_dummy" = "File skill × Minority",
  "skill_word:visible_minority_dummy" = "Word skill × Minority",
  "skill_presentation:visible_minority_dummy" = "Presentation × Minority",
  "skill_spreadsheet:visible_minority_dummy" = "Spreadsheet × Minority"
)

models_minority_skill <- list(
  "Online Purchase" = interaction_minority_skill_purchase,
  "Online Banking" = interaction_minority_skill_banking,
  "Online Investing" = interaction_minority_skill_investing,
  "Online Tax Filing" = interaction_minority_skill_tax
)

modelsummary(
  models_minority_skill,
  coef_map = minority_skill_terms,
  stars = TRUE,
  title = "Visible Minority Heterogeneity in the Effect of Digital Skills on Digital Financial Services Use",
  notes = "Non visible minorities are the reference group. All regressions control for digital access, income, education, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_MINORITY_SKILL_TABLE.html"
)

# Minority x Access
interaction_minority_access_purchase <- lm(
  dfs_purchase ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * visible_minority_dummy +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_access_banking <- lm(
  dfs_banking ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * visible_minority_dummy +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_access_investing <- lm(
  dfs_investing ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * visible_minority_dummy +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

interaction_minority_access_tax <- lm(
  dfs_tax ~
    (access_smartphone_binary + access_computer_binary + access_highspeed_binary) * visible_minority_dummy +
    skill_files + skill_word + skill_presentation + skill_spreadsheet +
    quality_basic_ord +
    factor(income_quintile) +
    highschool_dummy + some_postsec_dummy +
    factor(gender) +
    age_55_plus +
    rural_dummy + factor(province_group) +
    not_employed_dummy +
    immigrant_dummy +
    disability_dummy + indigenous_dummy,
  data = cius_work
)

# master table
minority_access_terms <- c(
  access_smartphone_binary = "Smartphone access",
  access_computer_binary = "Computer access",
  access_highspeed_binary = "High-speed internet",
  visible_minority_dummy = "Visible minority",
  "access_smartphone_binary:visible_minority_dummy" = "Smartphone × Minority",
  "access_computer_binary:visible_minority_dummy" = "Computer × Minority",
  "access_highspeed_binary:visible_minority_dummy" = "High-speed × Minority"
)

models_minority_access <- list(
  "Online Purchase" = interaction_minority_access_purchase,
  "Online Banking" = interaction_minority_access_banking,
  "Online Investing" = interaction_minority_access_investing,
  "Online Tax Filing" = interaction_minority_access_tax
)

modelsummary(
  models_minority_access,
  coef_map = minority_access_terms,
  stars = TRUE,
  title = "Visible Minority Heterogeneity in the Effect of Digital Access on Digital Financial Services Use",
  notes = "Non visible minorities are the reference group. All regressions control for digital skills, income, education, gender, age, geography, labour status, internet quality and demographic characteristics.",
  output = "MASTER_MINORITY_ACCESS_TABLE.html"
)
