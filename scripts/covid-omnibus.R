# covid omnibus
# joseph bulbulia  24 Aug 2022

# options(future.globals.maxSize = 8000 * 1024 ^ 2)  # needed
# set science digits
options(scipen = 999)

library(fs)
# import libraries (jb)
pull_path <-
  fs::path_expand(
    "~/The\ Virtues\ Project\ Dropbox/Joseph\ Bulbulia/00Bulbulia\ Pubs/2021/DATA/ldf.5"
  )
# import functions
pull_path_funs  <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/funs.R")
pull_path_libs  <-
  fs::path_expand("~/The\ Virtues\ Project\ Dropbox/scripts/libs.R")

# #libraries
source(pull_path_libs)

# #  functions
source(pull_path_funs)

# # # read data
dff<- readRDS(pull_path)


# inspect covid vars
cvd_vars = c(
  "COVID19.Timeline",
  "COVID.RiskCatching",
  "COVID.Rumination",
  "COVID.ConfidentRecovery",
  "COVID.RisksExaggerated",
  "COVID.CreatedLab",
  "COVID.InfoSourceGovt",
  "COVID.InfoSourceNewsMedia",
  "COVID.InfoSourceSocialMedia",
  "COVID.SatGovtResponse",
  "COVID.TrustGovtResponse",
  "COVID.ComplianceTesting",
  "COVID.ComplianceVaccinate",
  "COVID.ComplianceMask",
  "COVID.ComplianceIsolateHome",
  "COVID.ComplianceContactTrace",
  "COVID.ComplianceAllMoHGuidelines",
  "COVID.BeenTested",
  "COVID.RequestTest",
  "COVID.DeclineReq"

)
cvd_vars

dat <- dff |>
  dplyr::filter((Wave == "2018" & YearMeasured==1)|
           (Wave == "2019" & YearMeasured==1)|
           Wave == "2020" & YearMeasured != -1) |>
  dplyr::select(c(all_of(cvd_vars), Wave, YearMeasured, TSCORE))

dat2 <- dat |>
  dplyr::filter(Wave == 2020 & YearMeasured ==1)


out2 <- paste0(cvd_vars,
      collapse = "+" )
out2<- noquote(out2)

table1::table1( ~ COVID19.Timeline+COVID.RiskCatching+COVID.Rumination+COVID.ConfidentRecovery+COVID.RisksExaggerated+COVID.CreatedLab+COVID.InfoSourceGovt+COVID.InfoSourceNewsMedia+COVID.InfoSourceSocialMedia+COVID.SatGovtResponse+COVID.TrustGovtResponse+COVID.ComplianceTesting+COVID.ComplianceVaccinate+COVID.ComplianceMask+COVID.ComplianceIsolateHome+COVID.ComplianceContactTrace+COVID.ComplianceAllMoHGuidelines+COVID.BeenTested+COVID.RequestTest+COVID.DeclineReq, data = dat2)

skimr::skim(dat2)

dat3<- dff%>%
#  filter(YearMeasured==1) |>
  select(Id, Wave, Age) |>
  mutate(Wave = as.numeric(Wave)) |>
  droplevels() |>
  tibble()
dat3

dat3$Employed

dat3 |> spread( key = "Wave", value = "Age")
conflicts()

out3 <- dat3 %>%
  tidyr::pivot_wider(names_from = "Wave", value_from = "Age", names_glue = "{variable}_{.value}")
conflicted::conflicted()
conflicts()
matches
rlang::last_error()
# df %>%
#   filter(Wave == 2020 &  YearMeasured == 1) %>%
#   n_distinct("Id")

# # read libraries in
# source(here::here("scripts", "libs.R"))
# source(here::here("scripts", "funs.R"))

#dat$COVID19.Timeline
# bels:
#   value                                                                                              label
# 0.0                                                                                   The Before Times
# 1.0                                31.12.2019 -- 27.02.2020 [First cluster of cases in Wuhan reported]
# 1.1                                      28.02.2020 -- 25.02.2020 [First case recorded in New Zealand]
# 1.2                                                           26.03.2020 -- 27.04-2020 [Alert Level 4]
# 1.3                                                           28.04.2020 -- 13.05.2020 [Alert Level 3]
# 1.4                                                          14.05.2020 -- 08.06.2020 [Alert Level 2].
# 1.5                                                           09.06.2020 -- 11.08.2020 [Alert Level 1]
# 2.1 12.08.2020 -- 30.08.2020 [Second Outbreak - Auckland Alert Level 3, Rest of Country Alert Level 2]
# 2.2                 30.08.2020 -- 21.09.2020 [Auckland Alert Level 2.5, Rest of Country Alert Level 2]
# 2.3                    22.09.2020 -- 07.10.2020 [Auckland Alert Level 2, Rest of Country Alert Level 1
# 2.4                                                              08.10.2020 -- onwards [Alert Level 1]

# dat$REGC_2018

# labels:
#   value                     label
# 1          Northland Region
# 2           Auckland Region
# 3            Waikato Region
# 4      Bay of Plenty Region
# 5           Gisborne Region
# 6         Hawkes Bay Region
# 7           Taranaki Region
# 8 Manawatu-Whanganui Region
# 9         Wellington Region
# 12         West Coast Region
# 13         Canterbury Region
# 14              Otago Region
# 15          Southland Region
# 16             Tasman Region
# 17             Nelson Region
# 18        Marlborough Region
# 99       Area Outside Region

#  This isn't sensible
# dat1 <- dat %>%
#   dplyr::mutate(NZSEI06_lead1 = lead(NZSEI06, n = 1),
#                 KESSLER6_lead1 = lead(KESSLER6, n = 1),
#                 KESSLER6_lag1 = dplyr::lag(KESSLER6),
#                 NZSEI06_lag1 =  dplyr::lag(NZSEI06),
#                 Employed_lead1 = lead(Employed, n = 1),
#                 Employed_lag1 = dplyr::lag(Employed, n = 1))|>
#   dplyr::filter(Wave == 2019 & YearMeasured==1) |>
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
#                                  if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#                                          ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 & REGC_2018 == 1, 4, 3))))
#
# summary(test<- lm(KESSLER6~ cum_lockdowns_baseline + KESSLER6_lag1, data = dat1))
# summary(test<- lm(NZSEI06_lead1 ~ cum_lockdowns_baseline + NZSEI06_lag1, data = dat1))
# summary(test<- glm(Employed_lead1 ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))
# summary(test<- glm(Employed ~ cum_lockdowns_baseline + Employed_lag1, family = "binomial" ,  data = dat1))

# Code for timeline if needed
#   dplyr::mutate(cum_lockdowns_baseline = if_else(COVID19.Timeline < 1.2, 0,
# if_else(COVID19.Timeline >  1:2 & COVID19.Timeline  < 2, 2,
#         ifelse(COVID19.Timeline > 2 & REGC_2018 == 2  | COVID19.Timeline > 2 &
#                  REGC_2018 == 1, 4, 3)))) |>


# Template
# set digits = 3
options(scipen=999)

#libraries and functions
# source(here::here("scripts", "libs.R"))
# source(here::here("scripts", "funs.R"))

# table for participant N
tab_in <- dff %>%
  dplyr::mutate(Euro = if_else(EthCat == 1, 1, 0)) %>%
  dplyr::mutate(Male = ifelse(GendAll == 1, 1, 0)) %>%
  dplyr::filter((Wave == 2018  & YearMeasured  == 1) |
                  (Wave == 2019  &
                     YearMeasured  == 1) |
                  (Wave == 2020))  %>% # Eligibility criteria
  dplyr::filter(YearMeasured  != -1) %>% # remove people who passed away
  # dplyr::filter(Id != 9630) %>% # problematic for income
  group_by(Id) %>%
  dplyr::mutate(org2018 =  ifelse(Wave == 2018 &
                                    YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold18 = mean(org2018, na.rm = TRUE)) %>%  # Hack
  dplyr::filter(hold18 > 0) %>% # hack to enable repeat of baseline
  dplyr::mutate(org2019 = ifelse(Wave == 2019 &
                                   YearMeasured == 1, 1, 0)) %>%  # creating an indicator for the first wave
  dplyr::mutate(hold19 = mean(org2019, na.rm = TRUE)) %>%  # Hack0
  dplyr::filter(hold19 > 0) %>% # hack to enable repeat of baseline in 201
  ungroup() %>%
  droplevels() %>%
  arrange(Id, Wave)
# check n # 34782

length(unique(tab_in$Id)) # 34783

# increasing rate
dat%>%
  group_by(Wave) %>%
  summarise(mean(HLTH.Disability, na.rm = TRUE))

# Do you have a health condition or disability that limits you, and that has lasted for 6+ months?

## select vars
df_cr <- tab_in %>%
  # dplyr::filter(Id != 9630) %>% # problematic
  select(
    Id,
    YearMeasured,
    Wave,
    Age,
    AGREEABLENESS,
    CONSCIENTIOUSNESS,
    EXTRAVERSION,
    HONESTY_HUMILITY,
    NEUROTICISM,
    OPENNESS,
    Alcohol.Frequency,
    Alcohol.Intensity,
    began_relationship,
    BELONG,
    Believe.Spirit,
    Believe.God,
    Bodysat,
    BornNZ,
    CharityDonate,
    ChildrenNum,
    Edu,
    Emp.JobSecure,
    # EmotionRegulation1,
    # EmotionRegulation2,
    # EmotionRegulation3,
    Employed,
    Emp.WorkLifeBalance,
    Euro,
    EthCat,
    #  GenCohort,
    GRATITUDE,
    HLTH.BMI,
    HLTH.Fatigue,
    HLTH.Disability,
    HLTH.SleepHours,
    HomeOwner,
    Household.INC,
    HoursCharity,
    Hours.Exercise,
    Hours.Work,
    ImpermeabilityGroup,
    KESSLER6sum,
    LIFEMEANING,
    LIFESAT,
    lost_job,
    Male,
    NWI,
    NZdep,
    NZSEI13
    Parent,
    Partner,
    partnerlost_job,
    PERFECTIONISM,
    PermeabilityIndividual,
    Pol.Orient,
    POWERDEPENDENCE1,
    POWERDEPENDENCE2,
    Relid,
    Religion.Church2,
    Religion.Prayer2,
    Religion.Scripture2,
    Religious,
    Respect.Self,
    retired,
    RWA,
    Rumination,
    SDO,
    semiretired,
    SELF.CONTROL,
    SELF.ESTEEM,
    SexualSatisfaction,
    SFHEALTH,
    Smoker,
    Spiritual.Identification,
    Standard.Living,
    SUPPORT,
    SWB.SoC01,
    Urban,
    VENGEFUL.RUMIN,
    Your.Health,
    Your.Future.Security,
    Your.Personal.Relationships,
    Alcohol.Frequency,
    Alcohol.Intensity,
  ) %>%
  dplyr::rename(community = SWB.SoC01) %>%
  dplyr::mutate(Edu = as.numeric(Edu)) %>%
  dplyr::mutate(across(!c(Id, Wave), ~ as.numeric(.x))) %>% # make factors numeric for easy of processing
  arrange(Id, Wave) %>%
  dplyr::mutate(
    Edu = as.numeric(Edu),
    Volunteers = if_else(HoursCharity == 1, 1, 0),
    # Depressed = (as.numeric(
    #   cut(
    #     KESSLER6sum,
    #     breaks = c(-Inf, 13, Inf),
    #     labels = c("0", "1"),
    #     right = FALSE
    #   )
    # ) - 1),
    # EthCat = factor(EthCat, labels = c("Euro", "Maori", "Pacific", "Asian")),
    Church = ifelse(Religion.Church2 > 8, 8, Religion.Church2),
    income_log = log(Household.INC + 1),
  ) %>%
  arrange(Id, Wave)  %>% #
  dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%
  #dplyr::mutate(Church_lead1 = lead(Church, n = 1)) %>%  Your.Future.Security
  # inc_prop = (income_log / (income_log_lead1) - 1),
  dplyr::mutate(across(
    c(
      AGREEABLENESS,
      CONSCIENTIOUSNESS,
      EXTRAVERSION,
      HONESTY_HUMILITY,
      NEUROTICISM,
      OPENNESS,
      Alcohol.Frequency,
      Alcohol.Intensity,
      began_relationship,
      BELONG,
      Believe.Spirit,
      Believe.God,
      Bodysat,
      CharityDonate,
      ChildrenNum,
      Edu,
      Emp.JobSecure,
      # EmotionRegulation1,
      # EmotionRegulation2,
      # EmotionRegulation3,
      Employed,
      Emp.WorkLifeBalance,
      Euro,
      EthCat,
      #  GenCohort,
      GRATITUDE,
      HLTH.BMI,
      HLTH.Fatigue,
      HLTH.Disability,
      HLTH.SleepHours,
      HomeOwner,
      Household.INC,
      HoursCharity,
      Hours.Exercise,
      Hours.Work,
      ImpermeabilityGroup,
      KESSLER6sum,
      LIFEMEANING,
      LIFESAT,
      lost_job,
      Male,
      NWI,
      NZdep,
      NZSEI13
      Parent,
      Partner,
      partnerlost_job,
      PERFECTIONISM,
      PermeabilityIndividual,
      Pol.Orient,
      POWERDEPENDENCE1,
      POWERDEPENDENCE2,
      Relid,
      Religion.Church2,
      Religion.Prayer2,
      Religion.Scripture2,
      Religious,
      Respect.Self,
      retired,
      RWA,
      Rumination,
      SDO,
      semiretired,
      SELF.CONTROL,
      SELF.ESTEEM,
      SexualSatisfaction,
      SFHEALTH,
      Smoker,
      Spiritual.Identification,
      Standard.Living,
      SUPPORT,
      SWB.SoC01,
      Urban,
      VENGEFUL.RUMIN,
      Your.Health,
      Your.Future.Security,
      Your.Personal.Relationships,
      Alcohol.Frequency,
      Alcohol.Intensity,
    ),
    ~ lead(.x, n = 2),
    .names = "{col}_lead2"
  )) %>% # make leads
  dplyr::filter(Wave == 2018) %>%
  # dplyr::filter(retired != 1) %>%
  # dplyr::filter(retired_lead1 != 1) %>%  #needed for the intervention
  # dplyr::filter(semiretired != 1) %>%
  #dplyr::filter(semiretired_lead1 != 1) %>%
  #dplyr::filter(!is.na(income_log_lead1) )%>%  #   ABOUT
  #dplyr::filter(!is.na(income_log) )%>% #  THINK ABOUT
  # dplyr::filter(Household.INC >= 30975) %>% # min income
  # dplyr::filter(income_log_lead1 > income_log) %>%
  dplyr::filter(!is.na(Church)) %>%
  dplyr::filter(!is.na(Church_lead1)) %>%
  dplyr::mutate(Religious = as.numeric(Religious)-1) |>
  dplyr::filter(Religious == 1) %>%
  #dplyr::filter(!is.na(Standard.Living) )%>%
  # dplyr::filter(!is.na(Standard.Living_lead1) )%>%
  #  dplyr::filter(semiretired_lead1 != 1) %>%  #needed for the intervention
  dplyr::select(-c(
    Religion.Church2,
    # EthCat,
    Religious,
    HoursCharity,
    Respect.Self_lead2,
    Household.INC,
    #  org2018,
    #  not_euro,
    #  not_euro_lead2,
    # hold18,
    #   Euro,
    Emp.WorkLifeBalance,
    YearMeasured,
    #HLTH.Disability_lead1,
    # org2019,
    # hold19,
    # retired,
    # semiretired,
  )
  ) %>%
  #  dplyr::mutate(across(!c(Id,Wave), ~ scale(.x)))%>%  # standarise vars for easy computing-- do this after imputation
  arrange(Id, Wave) %>%
  droplevels() %>%
  data.frame() %>%
  mutate(across(where(is.double), as.numeric)) %>%
  arrange(Id)



# MICE --------------------------------------------------------------------


# mice model  -------------------------------------------------------------
library(mice)

mice_cc <- dcc %>%
  dplyr::select(-c(Wave, Id))  # won't otherwise run

library(naniar)
naniar::gg_miss_var(mice_cc)
# vis_miss(mice_cc,
#          warn_large_data = FALSE)

# any colinear vars?
mice:::find.collinear(mice_cc)

# impute
mice_cc <- mice::mice(mice_cc,  seed = 0, m = 10)

# save
saveh(mice_cc, "mice_cc")
# checks
outlist2 <-
  row.names(mice_cc)[mice_cc$outflux < 0.5]
length(outlist2)

# checks
head(mice_cc$loggedEvents, 10)

# read
mice_cc <- readh("mice_cc")

# we create two completed data sets -- the one without the missing data will be useful for
# determing causal contrasts -- which we'll describe below.

cc_l <- mice::complete(mice_cc, "long", inc = TRUE)



# inspect data -- what do we care about?  Note that the moderate distress category doesn't look useful
skimr::skim(cc_l)
# create variables in z score
cc_l2 <- cc_l %>%
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Volunteers_lead2 = if_else(HoursCharity_lead2 >0, 1, 0)) |>
  dplyr::mutate(income_log = log(Household.INC + 1)) |>
  dplyr::mutate(income_log_lead2 = log(Household.INC_lead2 + 1)) |>
  # dplyr::mutate(newkids = ChildrenNum_lead2 - ChildrenNum) %>%
  dplyr::mutate(KESSLER6sum_lead2 = round(as.integer(KESSLER6sum_lead2, 0))) %>%
  dplyr::mutate(CharityDonate_lead2 = round(CharityDonate_lead2, 0)) %>%
  plyr::mutate(Alcohol.Intensity = round(Alcohol.Intensity, 0)) %>%
  dplyr::mutate(CharityDonate = round(CharityDonate, 0)) %>%
  dplyr::mutate(Hours.Exercise = round(Hours.Exercise, 0)) %>%
  dplyr::mutate(CharityDonate_log_lead2 = log(CharityDonate_lead2 + 1)) %>%
  dplyr::mutate(CharityDonate_log = log(CharityDonate + 1)) %>%
  dplyr::mutate(SUPPORT_lead2ord = as.integer(round(SUPPORT_lead2, digits = 0))) %>%
  dplyr::mutate(Hours.Exercise_log = log(Hours.Exercise + 1)) %>%
  dplyr::mutate(Hours.Exercise_log_lead2 = log(Hours.Exercise_lead2 + 1)) %>%
  dplyr::mutate(LIFESAT_lead2ord = as.integer(round(LIFESAT_lead2, digits = 0))) %>%
  dplyr::mutate(alcohol_bin2 = if_else(Alcohol.Frequency > 3, 1, 0)) %>%
  dplyr::mutate(alcohol_bin = if_else(Alcohol.Frequency > 2, 1, 0)) %>%
  dplyr::mutate(Hours.Work_10 =  Hours.Work / 10) %>%
  dplyr::mutate(Religion.Prayer_log =  log(Religion.Prayer + 1)) %>%
  dplyr::mutate(Religion.Prayer_lead2_log =  log(Religion.Prayer_lead2 +
                                                   1)) %>%
  dplyr::mutate(Religion.Church_lead2_log =  log(Religion.Church_lead2 +
                                                   1)) %>%
  dplyr::mutate(Religion.Church_log =  log(Religion.Church +
                                             1)) %>%
  dplyr::mutate(Religion.Scripture_log =  log(Religion.Scripture + 1)) %>%
  dplyr::mutate(Religion.Scripture_lead2_log =  log(Religion.Scripture_lead2 +
                                                      1)) %>%
  dplyr::mutate(Religion.CongregationSize_log =  log(Religion.CongregationSize + 1)) %>%
  dplyr::mutate(Religion.CongregationSize_lead1_log =  log(Religion.CongregationSize_lead1 + 1)) %>%
  dplyr::mutate(Religion.CongregationSize_lead1_100 =  Religion.CongregationSize_lead1 /
                  100) %>%
  dplyr::mutate(Religion.CongregationSize_dunbar1 =  if_else(
    Religion.CongregationSize == 0,
    0,
    if_else(
      Religion.CongregationSize > 0 &
        Religion.CongregationSize < 151,
      1,
      2
    )
  )) %>%
  dplyr::mutate(
    Religion.CongregationSize_lead1_dunbar1 =  if_else(
      Religion.CongregationSize_lead1 == 0,
      0,
      if_else(
        Religion.CongregationSize_lead1 > 0 &
          Religion.CongregationSize_lead1 < 151,
        1,
        2
      )
    )
  ) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13 / 10) %>%
  dplyr::mutate(NZSEI13_10 =  NZSEI13/10)%>%
  dplyr::mutate(NZSEI13_lead2_10 =  as.integer(NZSEI13_lead2/10))%>%
  dplyr::mutate(id = as.factor(rep(1:N, 11))) |> # needed for g-comp
  dplyr::group_by(id) |> mutate(PWI = mean(
    c(
      Your.Future.Security,
      Your.Personal.Relationships,
      Your.Health,
      Standard.Living
    ),
    na.rm = TRUE
  )) |>
  dplyr::mutate(PWI_lead2 = mean(
    c(
      Your.Future.Security_lead2,
      Your.Personal.Relationships_lead2,
      Your.Health_lead2,
      Standard.Living_lead2
    ),
    na.rm = TRUE
  )) |>
  ungroup() |>
  dplyr::mutate(across(where(is.numeric), ~ scale(.x), .names = "{col}_z")) %>%
  select(-c(.imp_z, .id_z)) %>%
  dplyr::mutate(EthCat = as.factor(EthCat)) |>
  dplyr::mutate(Religion.CongregationSize_dunbar1 = as.factor(Religion.CongregationSize_dunbar1)) |>
  dplyr::mutate(Religion.CongregationSize_lead1_dunbar1 = as.factor(Religion.CongregationSize_lead1_dunbar1))

# # Get data into shape
cc_l2 <- cc_l2 %>% mutate_if(is.matrix, as.vector)
# cc3l <- cc_l %>% mutate_if(is.matrix, as.vector)

ccu <- mice::as.mids(cc_l2)
ccf <- mice::complete(ccu, "long", inc = F)


saveh(ccf, "ccf")
saveh(ccu, "ccu")


skimr::skim(ccf)

###### READ THIS DATA IN   #########
ccf <- readh("ccf")
ccu <- readh("ccu")



# cvars -------------------------------------------------------------------

# model equations ---------------------------------------------------------
baselinevars = c(
  "Religion.CongregationSize_log",
  "Age_z",
  "Male_z",
  "Edu_z",
  "Urban_z",
  "EthCat",
  "AGREEABLENESS_z",
  "CONSCIENTIOUSNESS_z",
  "EXTRAVERSION_z",
  "HONESTY_HUMILITY_z",
  "NEUROTICISM_z",
  "OPENNESS_z",
  "began_relationship_z",
  "BELONG_z",
  "Believe.Spirit_z",
  "Believe.God_z",
  "Bodysat_z",
  "BornNZ_z",
  "CharityDonate_z",
  "ChildrenNum_z",
  "Edu_z",
  "Employed_z",
  "HLTH.BMI_z",
  "HLTH.Fatigue_z",
  "HLTH.Disability_z",
  "HLTH.SleepHours_z",
  "Household.INC_z",
  "Volunteers_z",
  "Hours.Exercise_log_z",
  "Hours.Work_10_z",
  "KESSLER6sum_z",
  "LIFESAT_z",
  "lost_job_z",
  "NWI_z",
  "NZdep_z",
  "NZSEI13_z",
  "Parent_z",
  "Partner_z",
  "partnerlost_job_z",
  "Pol.Orient_z",
  "Relid_z",
  "Religion.Church_log_z",
  "Religion.Prayer_z",
  "Religion.Scripture_z",
  "Respect.Self_z",
  "retired_z",
  "RWA_z",
  "SDO_z",
  "semiretired_z",
  "SELF.CONTROL_z",
  "SELF.ESTEEM_z",
  "SFHEALTH_z",
  "Smoker_z",
  "Standard.Living_z",
  "SUPPORT_z",
  "community_z",
  "Your.Health_z",
  "Your.Future.Security_z",
  "Your.Personal.Relationships_z",
  "Alcohol.Frequency_z",
  "Alcohol.Intensity_z"
)



# setup -------------------------------------------------------------------



# SET UP ----------------------------------------------------------

# ylimits
# ylim <- c(-.3,.3)
# ylim7<- c(-.7,.7)
# ylim <- c(-.5,.5)
ylim8 = c(-.2,.75)
# data
df <-  ccu
# n imputations
m = 10
ylim_contrast <- c(.6,2.5)
ylim_contrast_diff <- c(-6,2.5)


exp(5)
hist(ccf$Religion.CongregationSize_lead1_log)
# Scripture set up ---------------------------------------------------------------
#How many times did you pray in the last week?
X = "Religion.CongregationSize_lead1_log"
xlab = "Log Religious Congregation Size"
min= 0
max = 7
# baseline
r = 0
# focal contrast
f = 5

# range of estimates
x =  min:max

# for model functions
c = x #c(0,5)
# contrast for graphs -- absolute distance from baseline
p = 5
s = 1 # slot for contrast graph

delta = delta
# functions ---------------------------------------------------------------

## Also use
round( EValue::evalues.OLS( , se = , sd = 1, delta = 2, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)

# sd(df_a$Household.INC_lead1)
# mean(mf$income_log_lead1)
# exp(mean(mf$income_log_lead1))
#
# min((mf$income_log_lead1))
# sd((mf$income_log_lead1))
#
# exp(11.62 - (2 * .58))


# HEALTH  ------------------------------------------------------------------
#  functions
source(pull_path_funs)


# simple g-computation ----------------------------------------------------

# fit
out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Believe.Spirit_lead2 ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(cvars,
            collapse = "+")
    )
  ), family = "poisson"))
}
main = "Believe Spirit Rate"
ylab = "Believe Spirit Rate"
# clean oven
rm(out_m)

rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast_ratio(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
spirit_t<- out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
spirit_t
spirit_p<- ggplot_stglm(out_ct, ylim =c(.9,1.1), main, xlab, ylab, min = min, p=p, r= 1)
spirit_p

round( EValue::evalues.OLS( , se = , sd = 1, delta = delta, true = 0), 3)


round( EValue::evalues.RR( , lo =  , hi = , true = 1), 4) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  kable_minimal(full_width = F)



# continuous g-formula ----------------------------------------------------


# church attendance -------------------------------------------------------

out_f = function(formula) {
  with(df, glm(as.formula(
    paste(
      "Religion.Church_lead2_log_z ~ bs(Religion.CongregationSize_lead1_log) +",
      paste(cvars,
            collapse = "+")
    )
  )))
}
main = "Log Monthly Church  (SD)"
ylab = "Log Monthly Church (SD)"
# clean oven
rm(out_m)
rm(out_ct)
# fit regression model
out_m <- out_f()
# g-computation - contrasts
out_ct <- pool_stglm_contrast(out_m, df = df, m = 10,  X = X, x = c, r= r)
#table
church_t<-out_ct %>%
  slice(1:8) |>
  tibble() |>
  rename(Contrast = row,
         Estimate = est,
         std_error = se,
         CI_hi = ui,
         CI_lo = li) |>
  kbl(caption = main,
      digits = 3,
      "html") |>
  kable_styling() %>%
  row_spec(c(6), bold = T, color = "white", background = "dodgerblue") |>
  kable_minimal(full_width = F)
church_t
church_p<- ggplot_stglm(out_ct, ylim = ylim8, main, xlab, ylab, min = min, p=p, r= 1)
church_p
round( EValue::evalues.OLS( 0.537, se = 0.041, sd = 1, delta = delta, true = 0), 3)
round( EValue::evalues.RR( , lo =  , hi =, true = 1), 4)





# PLOTS OF RELIGIOUS WELL-BEING -------------------------------------------

religion_plots <- prayer_p +  religious_p  + scripture_p +
  god_p + spirit_p + church_p  + plot_annotation(title = "Causal effects of congregation size on religious behaviour", tag_levels = "A") +
  plot_layout(guides = 'collect')  # plot_layout(nrow = 1, byrow = FALSE)


religion_plots
ggsave(
  religion_plots,
  path = here::here(here::here("figs", "congregation")),
  width = 16,
  height = 9,
  units = "in",
  filename = "religion_plots.jpg",
  device = 'jpeg',
  limitsize = FALSE,
  dpi = 1200
)

religion_plots
