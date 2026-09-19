# Senior Honours Essay — Empirical Analysis
# The Role of Digital Access and Skills in Financial Service Use in Canada
# Imaan Saleh | University of Waterloo | ECON 472
#
# ============================================================
# 1. SETUP AND DATA IMPORT
# ============================================================
setwd("~/Desktop/ciusdata")
list.files()
list.files(recursive = TRUE)

install.packages("haven")
library(haven)

df <- read_sas("Data/cius_pumf.sas7bdat")
View(df)

# ============================================================
# 2. VARIABLE SELECTION AND CLEANING
# ============================================================

names(df)[1:20]
library(haven)
install.packages("labelled")
library(labelled)
names(df)[1:20]
var_label(df)[1:20]
# Initial inspection of variable labels

# Rename PROVINCE
table(df$PROVINCE)
library(dplyr)
df <- df %>%
  mutate(
    PROVINCE = recode(PROVINCE,
                      `10` = "Newfoundland and Labrador",
                      `11` = "Prince Edward Island",
                      `12` = "Nova Scotia",
                      `13` = "New Brunswick",
                      `24` = "Quebec",
                      `35` = "Ontario",
                      `46` = "Manitoba",
                      `47` = "Saskatchewan",
                      `48` = "Alberta",
                      `59` = "British Columbia"
    )
  )
table(df$PROVINCE)

# Rename GENDER
table(df$GENDER)
library(dplyr)
df <- df %>%
  mutate(
    GENDER = recode(GENDER,
                    `1` = "Male",
                    `2` = "Female"
    )
  )

library(dplyr)

cius_work <- df %>%
  select(

    # Digital financial services (dependent variables)
    UI_051A,
    EC_030A,
    EC_030E,
    UI_050D,

    # Digital skills
    DS_020A,
    DS_020B,
    DS_020C,
    DS_020D,

    # Accessibility
    DV_010B,
    DV_010D,
    AC_060A,
    AC_080A,
    AC_080B,

    # Income
    HINCQUIN,

    # Controls
    AGE_GRP,
    GENDER,
    PROVINCE,
    LUC_RST,
    RRS_G12,
    EMP,
    EDU,
    IMM_STA,
    VISMIN,
    DIS_10,
    ABM,
  )
ls()
dim(df)
dim(cius_work)

# Rename Digital Skills
library(dplyr)
cius_work <- cius_work %>%
  rename(
    skill_files = DS_020A,
    skill_word = DS_020B,
    skill_presentation = DS_020C,
    skill_spreadsheet = DS_020D
  )
View(cius_work)
cius_work$PUMFID <- df$PUMFID
names(cius_work)
library(dplyr)

cius_work <- cius_work %>%
  relocate(PUMFID)

# Rename Digital Financial Services
cius_work <- cius_work %>%
  rename(
    dfs_investing = UI_051A,
    dfs_payment = EC_030A,
    dfs_transfer = EC_030E,
    dfs_banking = UI_050D
  )
names(cius_work)

# Rename Accessibility
cius_work <- cius_work %>%
  rename(
    access_smartphone = DV_010B,
    access_computer = DV_010D,
    access_highspeed = AC_060A,
    internet_problem_basic = AC_080A,
    internet_problem_video = AC_080B
  )
names(cius_work)

# Rename Income
cius_work <- cius_work %>%
  rename(
    income_quintile = HINCQUIN
  )

# Rename controls
cius_work <- cius_work %>%
  rename(
    age_group = AGE_GRP,
    gender = GENDER,
    province = PROVINCE,
    urban_rural = LUC_RST,
    household_size = RRS_G12,
    employment_status = EMP,
    education = EDU,
    immigrant_status = IMM_STA,
    visible_minority = VISMIN,
    disability = DIS_10,
    indigenous_identity = ABM
  )
names(cius_work)

# Group Provinces

cius_work <- cius_work %>%
  mutate(
    province_group = case_when(
      province %in% c("Newfoundland and Labrador",
                      "Prince Edward Island",
                      "Nova Scotia",
                      "New Brunswick") ~ "Atlantic",

      province %in% c("Manitoba",
                      "Saskatchewan",
                      "Alberta") ~ "Prairies",

      province == "Ontario" ~ "Ontario",
      province == "Quebec" ~ "Quebec",
      province == "British Columbia" ~ "British Columbia"
    )
  )
table(cius_work$province_group)
table(cius_work$province, cius_work$province_group)

# Make Ontario Baseline

cius_work$province_group <- factor(
  cius_work$province_group,
  levels = c("Ontario", "Atlantic", "Prairies", "Quebec", "British Columbia")
)
levels(cius_work$province_group)

# Location Centre Baseline
table(cius_work$urban_rural)
cius_work <- cius_work %>%
  mutate(
    rural_dummy = ifelse(urban_rural %in% c(2, 3), 1, 0)
  )
table(cius_work$urban_rural, cius_work$rural_dummy)

# Gender Dummy
cius_work$gender <- factor(cius_work$gender,
                           levels = c("Male", "Female"))
levels(cius_work$gender)

# Employment Status Dummy
cius_work <- cius_work %>%
  mutate(
    not_employed_dummy = ifelse(employment_status == 2, 1, 0)
  )
table(cius_work$employment_status, cius_work$not_employed_dummy)
cius_work <- cius_work %>%
  mutate(
    not_employed_dummy = case_when(
      employment_status == 1 ~ 0,   # employed
      employment_status == 2 ~ 1,   # not employed
      TRUE ~ NA                    # missing categories
    )
  )
table(cius_work$employment_status, cius_work$not_employed_dummy, useNA = "ifany")

# Create education indicators
cius_work <- cius_work %>%
  mutate(

    some_postsec_dummy = case_when(
      education == 2 ~ 1,
      education %in% c(1,3) ~ 0,
      TRUE ~ NA_real_
    ),

    university_dummy = case_when(
      education == 3 ~ 1,
      education %in% c(1,2) ~ 0,
      TRUE ~ NA_real_
    )

  )

table(cius_work$education, cius_work$highschool_dummy, useNA="ifany")
table(cius_work$education, cius_work$some_postsec_dummy, useNA="ifany")

# Create immigrant-status indicators
cius_work <- cius_work %>%
  mutate(
    non_immigrant_dummy = case_when(
      immigrant_status == 1 ~ 0,   # immigrant (baseline)
      immigrant_status == 2 ~ 1,   # non-immigrant
      TRUE ~ NA_real_
    )
  )
table(cius_work$immigrant_status, cius_work$non_immigrant_dummy, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    immigrant_dummy = case_when(
      immigrant_status == 1 ~ 1,   # immigrant
      immigrant_status == 2 ~ 0,   # non-immigrant (baseline)
      TRUE ~ NA_real_
    )
  )
table(cius_work$immigrant_status, cius_work$immigrant_dummy, useNA="ifany")
# Visible Minority Dummy

cius_work <- cius_work %>%
  mutate(
    non_visible_minority_dummy = case_when(
      visible_minority == 1 ~ 0,   # visible minority (baseline)
      visible_minority == 2 ~ 1,   # not visible minority
      TRUE ~ NA_real_
    )
  )
table(cius_work$visible_minority,
      cius_work$non_visible_minority_dummy,
      useNA = "ifany")
cius_work <- cius_work %>%
  mutate(
    visible_minority_dummy = case_when(
      visible_minority == 1 ~ 1,   # visible minority
      visible_minority == 2 ~ 0,   # non-visible minority (baseline)
      TRUE ~ NA_real_
    )
  )
table(cius_work$visible_minority, cius_work$visible_minority_dummy, useNA="ifany")

names(cius_work)

# Disability Status Dummy
cius_work <- cius_work %>%
  mutate(
    disability_dummy = case_when(
      disability == 1 ~ 1,   # disabled
      disability == 2 ~ 0,   # not disabled (baseline)
      TRUE ~ NA_real_
    )
  )
table(cius_work$disability, cius_work$disability_dummy, useNA="ifany")

# Create Indigenous identity indicator
cius_work <- cius_work %>%
  mutate(
    indigenous_dummy = case_when(
      indigenous_identity == 1 ~ 1,   # Indigenous
      indigenous_identity == 2 ~ 0,   # Non-Indigenous (baseline)
      TRUE ~ NA_real_
    )
  )
table(cius_work$indigenous_identity,
      cius_work$indigenous_dummy,
      useNA = "ifany")

# Create age-group indicators (25–34 omitted reference group)
cius_work <- cius_work %>%
  mutate(
    age_15_24 = case_when(
      age_group == 1 ~ 1,
      age_group %in% c(2,3,4,5,6) ~ 0,
      TRUE ~ NA_real_
    ),

    age_35_44 = case_when(
      age_group == 3 ~ 1,
      age_group %in% c(1,2,4,5,6) ~ 0,
      TRUE ~ NA_real_
    ),

    age_45_54 = case_when(
      age_group == 4 ~ 1,
      age_group %in% c(1,2,3,5,6) ~ 0,
      TRUE ~ NA_real_
    ),

    age_55_64 = case_when(
      age_group == 5 ~ 1,
      age_group %in% c(1,2,3,4,6) ~ 0,
      TRUE ~ NA_real_
    ),

    age_65_plus = case_when(
      age_group == 6 ~ 1,
      age_group %in% c(1,2,3,4,5) ~ 0,
      TRUE ~ NA_real_
    )
  )

table(cius_work$age_group, cius_work$age_15_24, useNA="ifany")
table(cius_work$age_group, cius_work$age_35_44, useNA="ifany")
table(cius_work$age_group, cius_work$age_45_54, useNA="ifany")
table(cius_work$age_group, cius_work$age_55_64, useNA="ifany")
table(cius_work$age_group, cius_work$age_65_plus, useNA="ifany")

# Inspect dependent-variable coding
table(cius_work$dfs_payment, useNA = "ifany")

# Add universe conditions

# Add AC_090A -> use of internet
cius_work$AC_090A <- df$AC_090A
table(cius_work$AC_090A, useNA="ifany")

# Add UI_050E -> conducted activities related to investing
cius_work$UI_050E <- df$UI_050E

# Add online-purchase universe variables
cius_work$EC_010Z <- df$EC_010Z

cius_work$EC_010B <- df$EC_010B
cius_work$EC_010C <- df$EC_010C
cius_work$EC_010D <- df$EC_010D
table(cius_work$EC_010B, useNA = "ifany")
table(cius_work$EC_010C, useNA = "ifany")
table(cius_work$EC_010D, useNA = "ifany")

# Add additional universe-condition variables
cius_work$EC_010A <- df$EC_010A
table(cius_work$EC_010A, useNA = "ifany")
grep("EC_010", names(cius_work), value = TRUE)
cius_work$AC_030A <- df$AC_030A
table(cius_work$AC_030A, useNA = "ifany")
"AC_030A" %in% names(cius_work)

# Rename Universe Variables
cius_work <- cius_work %>%
  rename(univ_use_internet = AC_090A)

cius_work <- cius_work %>%
  rename(online_investing = dfs_investing)
cius_work <- cius_work %>%
  rename(dfs_investing = UI_050E)

names(cius_work)
cius_work <- cius_work %>%
  rename(univ_online_purchase = no_online_purchase)

cius_work <- cius_work %>%
  rename(univ_purchase_physical = EC_010B)

cius_work <- cius_work %>%
  rename(univ_purchase_other = EC_010D)
cius_work <- cius_work %>%
  rename(univ_purchase_accom = EC_010C)
cius_work <- cius_work %>%
  rename(univ_purchase_digital = EC_010A)
cius_work <- cius_work %>%
  rename(access_internet = AC_030A)
names(cius_work)

cius_work <- cius_work %>%
  rename(online_payment_services = dfs_payment)

cius_work <- cius_work %>%
  rename(dfs_purchase = univ_online_purchase)

# Organize table
cius_work <- cius_work %>% select(PUMFID, starts_with("dfs_"), starts_with("skill_"), starts_with("access_"), everything())

names(cius_work)

# Set Universe Restrictions
cius_work <- cius_work %>%
  filter(univ_use_internet == 1, access_internet == 1)

table(cius_work$univ_use_internet)

table(cius_work$access_internet)

# Convert digital financial service outcomes to binary indicators

cius_work <- cius_work %>%
  mutate(
    dfs_investing = case_when(
      dfs_investing == 1 ~ 1,
      dfs_investing == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$dfs_investing, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    dfs_purchase = case_when(
      dfs_purchase == 2 ~ 1,   # purchased online
      dfs_purchase == 1 ~ 0,   # did not purchase
      TRUE ~ NA_real_
    )
  )
table(cius_work$dfs_purchase, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    dfs_banking = case_when(
      dfs_banking == 1 ~ 1,
      dfs_banking == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(cius_work$dfs_banking, useNA = "ifany")

# Convert digital-skill measures to binary indicators

cius_work <- cius_work %>%
  mutate(
    skill_files = case_when(
      skill_files == 1 ~ 1,
      skill_files == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )

table(cius_work$skill_files, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    skill_word = case_when(
      skill_word == 1 ~ 1,
      skill_word == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$skill_word, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    skill_presentation = case_when(
      skill_presentation == 1 ~ 1,
      skill_presentation == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$skill_presentation, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    skill_spreadsheet = case_when(
      skill_spreadsheet == 1 ~ 1,
      skill_spreadsheet == 2 ~ 0,
      TRUE ~ NA_real_
    )
  )
table(cius_work$skill_spreadsheet, useNA = "ifany")

nrow(cius_work)

cius_work <- cius_work %>%
  rename(quality_basic = internet_problem_basic)
cius_work <- cius_work %>%
  rename(quality_video = internet_problem_video)

names(cius_work)

# Additional access-variable cleaning
cius_work$access_computer_binary <- ifelse(cius_work$access_computer == 1, 1,
                                           ifelse(cius_work$access_computer == 2, 0, NA))
table(cius_work$access_computer_binary, useNA = "ifany")

table(cius_work$access_computer, useNA = "ifany")

table(cius_work$access_internet)

cius_work$DV_010A <- df$DV_010A
cius_work <- cius_work %>%
  left_join(df %>% select(PUMFID, DV_010A), by = "PUMFID")
table(cius_work$DV_010A, useNA = "ifany")
cius_work <- cius_work %>%
  rename(
    DV_010B = access_smartphone,
    access_smartphone = DV_010A
  )
cius_work <- cius_work %>%
  select(
    PUMFID,
    starts_with("dfs"),
    starts_with("skill"),
    starts_with("access"),
    everything()
  )
cius_work <- cius_work %>%
  select(PUMFID, everything())
cius_work <- cius_work[, c("PUMFID", setdiff(names(cius_work), "PUMFID"))]

names(cius_work)[1]

names(cius_work)


# Convert access measures to binary indicators

cius_work$access_smartphone_binary <- ifelse(cius_work$access_smartphone == 1, 1,
                                             ifelse(cius_work$access_smartphone == 2, 0, NA))
table(cius_work$access_smartphone_binary, useNA = "ifany")

cius_work$access_computer_binary <- ifelse(cius_work$access_computer == 1, 1,
                                           ifelse(cius_work$access_computer == 2, 0, NA))
table(cius_work$access_computer_binary, useNA = "ifany")

cius_work$access_highspeed_binary <- ifelse(cius_work$access_highspeed == 1, 1,
                                            ifelse(cius_work$access_highspeed == 2, 0, NA))
table(cius_work$access_highspeed_binary, useNA = "ifany")

# Recode internet-quality measures
cius_work <- cius_work %>%
  mutate(
    quality_basic_ord = case_when(
      quality_basic == 1 ~ 0,   # frequent problems
      quality_basic == 2 ~ 1,   # occasional problems
      quality_basic == 3 ~ 2,   # no problems
      TRUE ~ NA_real_
    )
  )
table(cius_work$quality_basic_ord, useNA = "ifany")

cius_work <- cius_work %>%
  mutate(
    quality_video_ord = case_when(
      quality_video == 1 ~ 0,   # frequent problems
      quality_video == 2 ~ 1,   # occasional problems
      quality_video == 3 ~ 2,   # no problems
      TRUE ~ NA_real_
    )
  )
table(cius_work$quality_video_ord, useNA = "ifany")
