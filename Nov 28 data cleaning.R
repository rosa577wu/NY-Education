## 11/28 data cleaning
# pupil
pupil_21 <- read.csv("~/Desktop/fall 23/sta 397/data/Expenditures per Pupil_21.csv")

names(pupil_21) <- tolower(names(pupil_21))

sd_pupil_21 <- pupil_21 %>%
  mutate(entity_cd = format(entity_cd, scientific = FALSE)) %>%
  filter(str_detect(entity_cd, "[^0]0000$"),
         year == 2021) %>%
  select(-c(institution_id, federal_exp, state_local_exp, fed_state_local_exp, data_reported_enr, data_reported_exp))

sd_pupil_21 <- sd_pupil_21[complete.cases(sd_pupil_21),]


# staff
staff_21 <- read.csv("~/Desktop/fall 23/sta 397/data/Staff_21.csv")

names(staff_21) <- tolower(names(staff_21))

sd_staff_21 <- staff_21 %>%
  mutate(entity_cd = format(entity_cd, scientific = FALSE)) %>%
  rename("entity_name" = "district_name") %>%
  filter(str_detect(entity_cd, "[^0]0000$"),
         year == 2021) %>%
  select(-c(school_name, district_cd, grade_range, cso_name, street, city, phone, per_attend))

sd_staff_21 <- sd_staff_21[complete.cases(sd_staff_21),]


# lunch
lunch_21 <- read.csv("~/Desktop/fall 23/sta 397/data/Free Reduced Price Lunch_21.csv")

names(lunch_21) <- tolower(names(lunch_21))

sd_lunch_21 <- lunch_21 %>%
  mutate(entity_cd = format(entity_cd, scientific = FALSE)) %>%
  filter(str_detect(entity_cd, "[^0]0000$"),
         year == 2021) %>%
  select(-c(num_free_lunch, num_reduced_lunch))

sd_lunch_21 <- sd_lunch_21[complete.cases(sd_lunch_21),]


# performance 
exam_18 <- read.csv("~/Desktop/fall 23/sta 397/data/Exam_19.csv")

names(exam_18) <- tolower(names(exam_18))

# ELA 2018 - 2019
sd_ELA_18 <- exam_18 %>%
  mutate(entity_cd = format(entity_cd, scientific = FALSE)) %>%
  filter(str_detect(entity_cd, "[^0]0000$"),
         subgroup_name == "All Students",
         cohort == 2015,
         subject == "ELA") %>%
  mutate(cohort_count.18 = cohort_count,
         ELA_pct_not_tested.18 = ntest_.cohort,
         ELA_per_prof.18 = prof_.cohort) %>%
  select(entity_cd, entity_name, cohort_count.18, ELA_pct_not_tested.18, ELA_per_prof.18)

sd_ELA_18 <- sd_ELA_18[complete.cases(sd_ELA_18),]

sd_ELA_18 <- sd_ELA_18 %>%
  filter(!grepl("s", ELA_per_prof.18)) %>%
  mutate_at(c("cohort_count.18", "ELA_pct_not_tested.18", "ELA_per_prof.18"), as.numeric)

# math 2018 - 2019
sd_math_18 <- exam_18 %>%
  mutate(entity_cd = format(entity_cd, scientific = FALSE)) %>%
  filter(str_detect(entity_cd, "[^0]0000$"),
         subgroup_name == "All Students",
         cohort == 2015,
         subject == "Math") %>%
  mutate(math_pct_not_tested.18 = ntest_.cohort,
         math_per_prof.18 = prof_.cohort) %>%
  select(entity_cd, entity_name, math_pct_not_tested.18, math_per_prof.18)

sd_math_18 <- sd_math_18[complete.cases(sd_math_18),]

sd_math_18 <- sd_math_18 %>%
  filter(!grepl("s", math_per_prof.18)) %>%
  mutate_at(c("math_pct_not_tested.18", "math_per_prof.18"), as.numeric)



## merging
pupil_staff <- left_join(sd_pupil_21, sd_staff_21, by = c("entity_cd", "year"))

pupil_staff <- pupil_staff %>%
  # mutate(name = ifelse(entity_name.x == entity_name.y, 0, 1)) %>%
  mutate(entity_name = entity_name.x) %>%
  select(-c(entity_name.x, entity_name.y))

ps_lunch <- left_join(pupil_staff, sd_lunch_21, by = c("entity_cd", "year"))

ps_lunch <- ps_lunch %>%
  # mutate(name = ifelse(entity_name.x == entity_name.y, 0, 1)) %>%
  mutate(entity_name = entity_name.x) %>%
  select(-entity_name.x, -entity_name.y)

psl_ELA <- left_join(ps_lunch, sd_ELA_18, by = "entity_cd")

psl_ELA <- psl_ELA %>%
  # mutate(name = ifelse(entity_name.x == entity_name.y, 0, 1)) %>%
  mutate(entity_name = entity_name.x) %>%
  select(-entity_name.x, -entity_name.y)

psl_perf <- left_join(psl_ELA, sd_math_18, by = "entity_cd")

psl_perf <- psl_perf %>%
  # mutate(name = ifelse(entity_name.x == entity_name.y, 0, 1)) %>%
  mutate(entity_name = entity_name.x) %>%
  select(-entity_name.x, -entity_name.y)

## removes districts with low participation rate in standardized tests
psl_perf_1 <- psl_perf %>%
  filter(math_pct_not_tested.18 < 50 | ELA_pct_not_tested.18 < 50) %>%
  mutate(student_teacher_ratio = pupil_count_tot/num_teach) %>%
  select(-c(cohort_count.18, ELA_pct_not_tested.18, math_pct_not_tested.18))

removed_districts <- psl_perf %>%
  filter(math_pct_not_tested.18 > 50 | ELA_pct_not_tested.18 > 50)

write.csv(psl_perf_1, "~/Desktop/fall 23/sta 397/data/NOV28_cleaned_edu.csv", row.names = FALSE)

