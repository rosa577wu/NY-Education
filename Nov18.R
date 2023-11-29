library(ggplot2)    # graphics package
library(dplyr)      # joining data frames and data manipulation
library(readr)      # quickly reads files into R
library(sf)         # simple features package for spatial data
library(leaps)
library(tidyr)
library(corrplot)
library(mosaic)
library(patchwork)


####################################################
# Matching function
find_match_general <- function(name, data, search_col, return_col) {
  if(name == "") {
    return(NA)
  }
  if(grepl("^NYC GEOG DIST #", name, ignore.case = TRUE)) {
    exact_match <- which(data[[search_col]] == name)
    if(length(exact_match) == 1) {
      return(data[[return_col]][exact_match])
    }
  }
  words <- unlist(strsplit(name, " "))
  for (i in 1:length(words)) {
    search_str <- paste(words[1:i], collapse = " ")
    matches <- grep(paste("^", search_str, sep = ""), data[[search_col]], ignore.case = TRUE)
    if (length(matches) == 1) {
      return(data[[return_col]][matches])
    }
  }
  return(NA)  # Return NA if no unique match is found
}

####################################################
##             1. Read in Data
####################################################
# annual lunch and expenditure
lunch <- read.csv("clean_data/NOV28_cleaned_edu.csv")
lunch <- lunch %>%
  filter(year == 2021) %>%
  unique()

# id_name is used to merge data
id_name <- read.csv("clean_data/id_name.csv")
id_name <- id_name[-121,]

# annual salary
salary <- read.csv("clean_data/f_avg_salary_21(k-12) - Copy.csv")
salary <- pivot_wider(salary, id_cols = c(report_school_year, district_name), 
                      names_from = staff_ind_desc,
                      values_from = avg_sal)
salary <- merge(salary, id_name, by = "district_name")
salary <- salary %>%
  rename("Teacher_Sal" = "Classroom Teacher") %>%
  rename("Princ_Sal" = "Principals & Asst. Principals") %>%
  rename("id_name" = "state_district_id")

# monthly covid cases and learning mode
monthly.cases.mode <- read.csv("clean_data/monthly.cases.mode.csv")
monthly.cases.mode$id_name <- sapply(monthly.cases.mode$District.Name, function(name) {
  find_match_general(name, id_name, "district_name", "state_district_id")
})
monthly.cases.mode <- filter(monthly.cases.mode, !is.na(id_name))
yearly.cases.mode <- monthly.cases.mode %>%
  group_by(id_name) %>%
  summarise(
    NewCasesStudents = sum(NewCasesStudents, na.rm = TRUE),
    NewCasesTeachers = sum(NewCasesTeachers, na.rm = TRUE),
    NewCasesStaff = sum(NewCasesStaff, na.rm = TRUE),
    Norm.New.Stud = sum(Norm.New.Stud, na.rm = TRUE),
    share_inperson = ifelse(all(is.na(share_inperson)), NA, mean(share_inperson, na.rm = TRUE)),
    share_hybrid = ifelse(all(is.na(share_hybrid)), NA, mean(share_hybrid, na.rm = TRUE)),
    share_virtual = ifelse(all(is.na(share_virtual)), NA, mean(share_virtual, na.rm = TRUE))
  )

####################################################
##             2. Merge Files
####################################################

## i. Monthly File
Merged1 <- left_join(monthly.cases.mode, lunch_test, by = c("id_name" = "entity_cd"))
Merged2 <- left_join(Merged1, salary, by = "id_name")

# Create a lagged column for NewCasesStudents
Merged3 <- Merged2 %>%
  group_by(District.Name) %>%
  arrange(Month, .by_group = TRUE) %>%
  mutate(LaggedStudCases = lag(NewCasesStudents, 1)) %>%
  ungroup() %>%
  filter(Month != as.Date("2020-10-01"))

# Delete extra columns (without Month)
Merged4 <- select(Merged3, -c(NCESDistrictID, year, entity_name, district_name,
                              report_school_year, pop.5.17, num.report.school,
                              NewCasesTeachers, NewCasesStaff, AccuCasesStudents, AccuCasesTeachers,
                              AccuCasesStaff, Norm.New.Stud, share_virtual, num_counselors,
                              num_social, num_princ, District.Name, Month))


# Delete extra columns (keep Month)
Merged5 <- select(Merged3, -c(NCESDistrictID, year, entity_name, district_name,
                              report_school_year, pop.5.17, num.report.school,
                              NewCasesTeachers, NewCasesStaff, AccuCasesStudents, AccuCasesTeachers,
                              AccuCasesStaff, Norm.New.Stud, share_virtual, num_counselors,
                              num_social, num_princ, District.Name))
Merged6 <- na.omit(Merged5)

# ii. Yearly File
Y.Merged1 <- left_join(yearly.cases.mode, lunch_test, by = c("id_name" = "entity_cd"))
Y.Merged2 <- left_join(Y.Merged1, salary, by = "id_name")
Y.Merged3 <- Y.Merged2 %>%
  select(-c(year, entity_name, district_name,
            report_school_year, NewCasesTeachers, NewCasesStaff, Norm.New.Stud, 
            share_virtual, num_counselors, num_social, num_princ)) %>%
  rename("TotalCasesStudent" = "NewCasesStudents")
Y.Merged4 <- na.omit(Y.Merged3)

####################################################
##             3. Monthly Model
####################################################
# Best subset
bestlm <- regsubsets(LaggedStudCases ~ ., data = Merged4, method="exhaustive")
plot(bestlm, scale = "adjr2")
summary(bestlm)$adjr2

## model0 : Main effect Model
model0 <- lm(log(LaggedStudCases+1) ~ Month + share_inperson + NewCasesStudents + 
               per_free_lunch + pupil_count_tot + Teacher_Sal,
             data = Merged6)
summary(model0)

# Check Correlations Plot
correlations <- cor(Merged4, use = "pairwise.complete.obs")
corrplot(correlations)

## model1 : Check all interaction
model1 <- lm(log(LaggedStudCases+1) ~ (Month + share_inperson + NewCasesStudents + 
                                         per_free_lunch + pupil_count_tot + Teacher_Sal)^2,
             data = Merged6)
summary(model1)

## model2 : Interaction Model
model2 <- lm(log(LaggedStudCases+1) ~ Month + share_inperson + NewCasesStudents + 
               per_free_lunch + pupil_count_tot + Teacher_Sal +
               pupil_count_tot*Teacher_Sal + per_free_lunch*pupil_count_tot + 
               NewCasesStudents*per_free_lunch + NewCasesStudents*pupil_count_tot +
               Month*per_free_lunch + Month*pupil_count_tot,
             data = Merged6)
summary(model2)

# Check Residual Plot
mod.res <- model2$residuals
qqnorm(mod.res)


# (ignore this) Randomly select districts to check
#sample.districts <- sample(unique(Merged3$District.Name), 5)
#sample.set <- Merged3 %>% filter(District.Name %in% sample.districts)
#mplot(sample.set)

####################################################
##             write file
####################################################

#write.csv(Merged6, "/Users/rosawu/Documents/2023FALL/STA MAP/Model/Monthly.Merged.csv", row.names = FALSE)
#write.csv(Y.Merged4, "/Users/rosawu/Documents/2023FALL/STA MAP/Model/Yearly.Merged.csv", row.names = FALSE)

####################################################
##             4. Yearly Model
####################################################
## use dataset : Y.Merged4
## response variable : TotalCasesStudent

Y.Merged4$NormalizedCases = 1000 * (Y.Merged4$TotalCasesStudent / Y.Merged4$pupil_count_tot)
Y.Merged4$CasesCategory <- cut(Y.Merged4$NormalizedCases, 
                               breaks = c(0, 30, 60, 90, Inf),
                               labels = c("0-30", "30-60", "60-90", ">90"),
                               include.lowest = TRUE)



#######
Y.Merged4$SalaryCategory <- cut(Y.Merged4$Teacher_Sal, 
                                breaks = c(50003, 62340, 69744, 95366, Inf),
                                labels = c("Low Salary (<25)", "Below Median Salary", "Above Median Salary", "High Salary (>75)"),
                                include.lowest = TRUE)

# Create the plot
ggplot(Y.Merged4, aes(x = per_free_lunch, y = NormalizedCases)) +
  geom_point() +
  facet_wrap(~SalaryCategory) +
  labs(title = "% of Free Lunch vs. Normalized Cases by Teacher Salary",
       x = "Percentage of Free Lunch",
       y = "Total Cases") +
  theme_minimal()

# Histogram for TotalCasesStudent with KDE
p1 <- ggplot(Y.Merged4, aes(x = TotalCasesStudent)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#1f77b4", alpha = 0.6) +
  geom_density(aes(y = 33*..count..), color = "blue", size = 1) +
  labs(title = "Histogram of TotalCasesStudent") +
  theme_minimal()

# Histogram for NormalizedCases with KDE
p2 <- ggplot(Y.Merged4, aes(x = NormalizedCases)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#1f77b4", alpha = 0.6) +
  geom_density(aes(y = 4.5*..count..), color = "blue", size = 1) +
  labs(title = "Histogram of NormalizedCases (per 1000 pupils)") +
  theme_minimal()

# Histogram for share_inperson with KDE
p3 <- ggplot(Y.Merged4, aes(x = per_free_lunch)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#9467bd", alpha = 0.6) +
  geom_density(aes(y = 3.2*..count..), color = "purple", size = 1) +
  labs(title = "Histogram of per_free_lunch") +
  theme_minimal()


# Histogram for pupil_count_tot with KDE
p4 <- ggplot(Y.Merged4, aes(x = pupil_count_tot)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#9467bd", alpha = 0.6) +
  geom_density(aes(y = 1000*..count..), color = "purple", size = 1) +
  labs(title = "Histogram of pupil_count_tot") +
  theme_minimal()

# Histogram for Teacher_Sal with KDE
p5 <- ggplot(Y.Merged4, aes(x = Teacher_Sal)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#9467bd", alpha = 0.6) +
  geom_density(aes(y = 2850*..count..), color = "purple", size = 1) +
  labs(title = "Histogram of Teacher_Sal") +
  theme_minimal()

# Histogram for student_teacher_ratio with KDE
p6 <- ggplot(Y.Merged4, aes(x = student_teacher_ratio)) +
  geom_histogram(aes(y = ..count..), bins = 30, fill = "#9467bd", alpha = 0.6) +
  geom_density(aes(y = 0.555*..count..), color = "purple", size = 1) +
  labs(title = "Histogram of student_teacher_ratio") +
  theme_minimal()

# Arranging plots
layout1 <- (p1 | p2)
layout2 <-  (p3 | p4) /
  (p5 | p6)

# Print the layout
layout1
layout2



# % of Free Lunch vs. (log) Student Cases by Teacher Salary
ggplot(Y.Merged4, aes(x = per_free_lunch, y = log(TotalCasesStudent))) +
  geom_point(color = "#1f77b4") +  # Add color to the points
  facet_wrap(~SalaryCategory) +
  labs(title = "% of Free Lunch vs. (log) Student Cases by Teacher Salary",
       x = "Percentage of Free Lunch",
       y = "(log) Total Cases") +
  theme_minimal()


ggplot(Y.Merged4, aes(x = per_free_lunch, y = NormalizedCases)) +
  geom_point(color = "#1f77b4") +  # Add color to the points
  facet_wrap(~SalaryCategory) +
  labs(title = "% of Free Lunch vs. Normalized Cases by Teacher Salary",
       x = "Percentage of Free Lunch",
       y = "Total Cases (Normalized)") +
  theme_minimal()

