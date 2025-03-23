# Lab 7 Assignment: Difference in Means and ANOVA
# The University of Texas at San Antonio
# URP-5393: Urban Planning Methods II


#---- Instructions ----

# 1. [70 points] Open the R file "Lab7_Assignment.R" and answer the questions bellow
# 2. [30 points] Run a T-test and an ANOVA test in your data.


#---- Part 1. Open the R file "Lab7_Assignment.R" and answer the questions bellow

# 1.1 load the same household data used in the Lab7_Script.R file, create the object `HTS`

library(data.table)
library(foreign)

HTS <- data.table(read.spss("datasets/HTS.household.10regions.sav",to.data.frame = T))


# 2. Recreate the same steps used in the lab to run a T-test, but instead, consider the following:

# 2.1 Run a T-Test to show if the household income means is statistically different between households living in single family residences or not (use the whole sample). Produce two pdfs, one with an histogram pdf plot, and another with the simulated hypothesis testing plot showing where the T-statistic falls. Provide a short interpretation of your results



# Conduct t-test


unique(HTS$sf)

HTS$sf_new <- factor(ifelse(HTS$sf == "single family detached", "single family detached", "other_or_na"))

levels(HTS$sf_new)

two_tailed_t_test <- t.test(hhincome ~ sf_new, data = HTS)
print(two_tailed_t_test)

one_tailed_t_test<-HTS[,t.test(formula = hhincome ~ sf_new, alernative = 'greater')] # one-tailed
one_tailed_t_test



# Histogram plot

library(ggplot2)


HTS_clean <- HTS %>%
  filter(!is.na(sf_new))

# Plot the histogram with the updated sf_new column
ggplot(data = HTS_clean, aes(x = hhincome, fill = sf_new)) +
  geom_histogram(binwidth = 10, color = "black", alpha = 0.7) +
  facet_wrap(~ sf_new) +
  labs(title = "Histogram of Household Income by Housing Type", 
       x = "Household Income", 
       y = "Count") +
  scale_fill_manual(values = c("single family detached" = "blue", "other_or_na" = "red")) +
  theme_minimal() +
  theme(strip.text = element_text(size = 12, face = "bold"))


ggsave("anday14_plot_1.pdf", plot = last_plot(), device = "pdf", width = 8, height = 6)




# Simulated hypothesis testing plot

curve(dt(x, df = one_tailed_t_test$parameter), from = -40, to = 40, 
      main = "Simulated Hypothesis Testing Plot",
      xlab = "T-Statistic", ylab = "Density",
      col = "black", lwd = 2)
abline(h = 0, col = 'blue')
points(x = one_tailed_t_test$statistic, y = 0, col = 'red')


upper975 <- qt(p = .975, df = one_tailed_t_test$parameter)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = one_tailed_t_test$parameter)
abline(v = lower025,y=0,col='red')





pdf("anday14_plot_2.pdf", width = 8, height = 6)

curve(dt(x, df = one_tailed_t_test$parameter), from = -40, to = 40, 
      main = "Simulated Hypothesis Testing Plot",
      xlab = "T-Statistic", ylab = "Density",
      col = "black", lwd = 2)
abline(h = 0, col = 'blue')
points(x = one_tailed_t_test$statistic, y = 0, col = 'red')


upper975 <- qt(p = .975, df = one_tailed_t_test$parameter)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = one_tailed_t_test$parameter)
abline(v = lower025,y=0,col='red')


dev.off()


#INTERPRETATION OF RESULTS: We can see that the difference in household income between each group (those that live in single-family homes vs those that due not), is very significant and is not due to chance. The red point (observed t-stastic) is in the negatives past the red led of the lower quartile. The area under the curve after it is very small. 





# 2.2 Filter the sample to select only the region of San Antonio. Prepare an T-Test to show if the household vehicle miles traveled (in natural logs - lnvmt) is statistically different between households living in neighborhoods with a job-population index (variable `jobpop`) over and under the city median (of the `jobpop` variable of course)


HTS_SA<-HTS[region%in%c("San Antonio, TX")] 


median_jobpop_SA <- median(HTS_SA$jobpop, na.rm = TRUE)


HTS_SA$jobpop_MEDIAN <- ifelse(HTS_SA$jobpop > median_jobpop_SA, "over", "under")




two_tailed_t_test_joppop<-HTS_SA[,t.test(formula = lnvmt ~ jobpop_MEDIAN, data = HTS_SA)] 
two_tailed_t_test_joppop

one_tailed_t_test_jobpop<-HTS_SA[,t.test(formula = lnvmt ~ jobpop_MEDIAN,alernative = 'greater')] 
one_tailed_t_test_jobpop


curve(dt(x, df = one_tailed_t_test_jobpop$parameter), from = -40, to = 40, 
      main = "Simulated Hypothesis Testing Plot SA",
      xlab = "T-Statistic", ylab = "Density",
      col = "black", lwd = 2)
abline(h = 0, col = 'blue')
points(x = one_tailed_t_test_jobpop$statistic, y = 0, col = 'red')


upper975 <- qt(p = .975, df = one_tailed_t_test_jobpop$parameter)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df = one_tailed_t_test_jobpop$parameter)
abline(v = lower025,y=0,col='red')

#Interpretation: the is (just barely) a significant difference between the household vehicle miles traveled for the households living in neighborhoods with a job-population index over and under the city median. 


# 2.2 using the same data set (San Antonio sample), run an ANOVA test to see if there are significant differences between income categories and vehicle miles traveled by household. Follow the same steps used in the ANOVA exercise done in class. Produce three pdfs: one checking the normality assumption of the dependent variable, a second one checking the presence of outliers, and a third one showing the Tukey (post hoc) T-tests plot.


#income categories (income_cat) and vehicle miles traveled by household (lnvmt)



# Normality of dependent variable

hist(HTS_SA$lnvmt,  main = "Histogram of Vehicles Miles Traveled By Household, SA")


pdf("anday14_Plot_3_Normality Assumption.pdf", width = 8, height = 6)

hist(HTS_SA$lnvmt,  main = "Histogram of Vehicles Miles Traveled By Household, SA")

dev.off()


#Presence of outliers

ggplot(data = HTS_SA, aes(x = income_cat, y = lnvmt)) +
  geom_boxplot() +
  labs(title = "Boxplot of Vehicle Miles Traveled By Income Category with Outliers, SA",
       x = "Income Category", y = "Vehicle Miles Traveled")


HTS_SA_bp<-boxplot(HTS_SA$lnvmt~HTS_SA$income_cat)
outliers <- HTS_SA_bp$out

HTS_SA[lnvmt%in%outliers,]
HTS_SA_2<-HTS_SA[!lnvmt%in%outliers,] #this one properly removed them

boxplot(HTS_SA$lnvmt~HTS_SA$income_cat)
boxplot(HTS_SA_2$lnvmt~HTS_SA_2$income_cat, main = "Boxplot with Outliers Removed, SA")


pdf("anday14_Plot_4_Outliers_Checked_and_Removed.pdf", width = 8, height = 6)

boxplot(HTS_SA_2$lnvmt~HTS_SA_2$income_cat, main = "Boxplot with Outliers Removed, SA")

dev.off()



# variance homogeneity?
bartlett.test(lnvmt ~ income_cat, data= HTS_SA_2)


# one-way anova

fit<-aov(HTS_SA_2$lnvmt~HTS_SA_2$income_cat)
summary(fit)



#post-hoc test
TukeyHSD(fit)

plot(TukeyHSD(fit))



pdf("anday14_Plot_5_Tukey_(post hoc)_T-tests.pdf", width = 8, height = 6)

plot(TukeyHSD(fit))

dev.off()




# 2. [30 points] Run a T-test and an ANOVA test in your data.


#Pulling my histori districs shapefile date

library(data.table)

library(sf)

my_data_HIST_DST_SA <- st_read("C:/Users/adrian.gallegos/OneDrive - BASIS.ed/Desktop/SPRING UTSA 25/Project/Historic_Districts_2856596570076965563")


#pulling ACS data for relevant variables for Bexar County

library(tidycensus)
census_api_key("0d539976d5203a96fa55bbf4421110d4b3db3648")



var <- c(poptotal='B03002_001E', 
         hispanic='B03002_012E',
         white='B03002_003E',
         black='B03002_004E', 
         asian='B03002_006E',
         poptotal2='B17017_001E',
         poverty='B17017_002E',
         median_income="B19013_001") 


st <-"TX"
ct <-"Bexar"

Project_Variables <- get_acs(geography = "tract", variables = var, count=ct,
                             state = st,output="wide", year = 2022, geometry = TRUE)


st_crs(my_data_HIST_DST_SA)
st_crs(Project_Variables)

if (st_crs(my_data_HIST_DST_SA) != st_crs(Project_Variables)) {
  Project_Variables <- st_transform(Project_Variables, st_crs(my_data_HIST_DST_SA))
}


#creating a new data frame that combines the two using the geometry/polygon data

joined_data_HISTORIC_DISTRICTS_SA <- st_join(my_data_HIST_DST_SA, Project_Variables, join = st_intersects)



#median for SA based on the original ACS dataframe
median_income <- median(Project_Variables$median_incomeE, na.rm = TRUE)

# Print the result
print(median_income)

#so median income is 62683


#adding the SA median income over/under column to the new historic districs data frame


joined_data_HISTORIC_DISTRICTS_SA$income_classification <- ifelse(joined_data_HISTORIC_DISTRICTS_SA$median_incomeE > median_income, "over", "under")




#Adding a new column in the original "Project_Variables" column showing a yes/no if it is in a historic district

# CRS for both datasets match (if they don't, transform one to the other's CRS)
if (st_crs(Project_Variables) != st_crs(my_data_HIST_DST_SA)) {
  Project_Variables <- st_transform(Project_Variables, st_crs(my_data_HIST_DST_SA))
}

#Perform the spatial join using st_intersects
# This will check if any census tract intersects with any historic district
intersecting_tracts <- st_intersects(Project_Variables, my_data_HIST_DST_SA)

#new column "In_Hist_District"
#check if there is at least one intersection for each census tract
Project_Variables$In_Hist_District <- ifelse(sapply(intersecting_tracts, length) > 0, "yes", "no")






#T-Test to show if the household income means is statistically different between households living in a historic district and those that are not:


two_tailed_t_test_PROJECT <- t.test(median_incomeE ~ In_Hist_District, data = Project_Variables)
print(two_tailed_t_test_PROJECT)

curve(dt(x, df = two_tailed_t_test_PROJECT$parameter), from = -80, to = 80, 
      main = "Simulated Hypothesis Testing Plot for Project",
      xlab = "T-Statistic", ylab = "Density",
      col = "black", lwd = 2)
abline(h = 0, col = 'blue')
points(x = two_tailed_t_test_PROJECT$statistic, y = 0, col = 'red')


upper975 <- qt(p = .975, df = two_tailed_t_test_PROJECT$parameter)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df =two_tailed_t_test_PROJECT$parameter)
abline(v = lower025,y=0,col='red')

#INTERPRETATION OF RESULTS: We can see that the difference in median income between each census tracts that are designated as a historic district and those that are not is very significant and is not due to chance. The red point (observed t-stastic) is in the positive side past the red line of the upper quartile.




pdf("anday14_Plot_6_My_Data_T-Test.pdf", width = 8, height = 6)

two_tailed_t_test_PROJECT <- t.test(median_incomeE ~ In_Hist_District, data = Project_Variables)
print(two_tailed_t_test_PROJECT)

curve(dt(x, df = two_tailed_t_test_PROJECT$parameter), from = -80, to = 80, 
      main = "Simulated Hypothesis Testing Plot for Project",
      xlab = "T-Statistic", ylab = "Density",
      col = "black", lwd = 2)
abline(h = 0, col = 'blue')
points(x = two_tailed_t_test_PROJECT$statistic, y = 0, col = 'red')


upper975 <- qt(p = .975, df = two_tailed_t_test_PROJECT$parameter)
abline(v = upper975,y=0,col='red')

lower025 <- qt(p = .025, df =two_tailed_t_test_PROJECT$parameter)
abline(v = lower025,y=0,col='red')

dev.off()






library(dplyr)

# Create the new 'income_cat' column with categories based on 'median_incomeE'
joined_data_HISTORIC_DISTRICTS_SA <- joined_data_HISTORIC_DISTRICTS_SA %>%
  mutate(income_cat = case_when(
    median_incomeE < 35000 ~ "low (<35K)",  # For income less than 35K
    median_incomeE >= 35000 & median_incomeE <= 75000 ~ "middle (35K-75K)",  # For income between 35K and 75K
    median_incomeE > 75000 ~ "high (>75K)",  # For income greater than 75K
    TRUE ~ "unknown"  # For any missing or unexpected values
  ))



# variance homogeneity?
bartlett.test(white ~ income_cat, data= joined_data_HISTORIC_DISTRICTS_SA)


# one-way anova

fit_2<-aov(joined_data_HISTORIC_DISTRICTS_SA$white ~ joined_data_HISTORIC_DISTRICTS_SA$income_cat)
summary(fit_2)


#post-hoc test
TukeyHSD(fit_2)

plot(TukeyHSD(fit_2))


pdf("anday14_Plot_7_My_Data_Turkey_(post hoc)_Test.pdf", width = 8, height = 6)

fit_2<-aov(joined_data_HISTORIC_DISTRICTS_SA$white ~ joined_data_HISTORIC_DISTRICTS_SA$income_cat)
summary(fit_2)

TukeyHSD(fit_2)

plot(TukeyHSD(fit_2))

dev.off()





# Bonus: [30 points] Provide an HTML file with your answers using R-Markdown.



