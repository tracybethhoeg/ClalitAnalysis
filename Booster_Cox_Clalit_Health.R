if("pacman" %in% installed.packages()){
  require(pacman)
} else {
  install.packages("pacman")
  require(pacman)
}


pacman::p_load("devtools", "htmltools", "survival", "BiocManager", "survminer", "readr", "gtsummary", "ggsurvfit", "lubridate", "tidycmprsk", "condSURV", "finalfit", "tidyverse", "ggplot2")

devtools::install_github("cardiomoon/ggiraphExtra")

setwd("~/Desktop/Academics and Research/COVID-19 Projects/Tracy Boosted Effect Clalit Health")

#install.packages(c("survival", "survminer"))
#library("survival")
#library("survminer")

clalit_data = read_delim('Clalit_Health_Boosting_Data_for_R_Analysis.csv', delim = ',', col_names = T, guess_max = 9000 )

View(clalit_data)
describe(clalit_data)

head(clalit_data)


# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_py_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years, data=clalit_data)
boosted_py_no_adjustment %>%
  summary()

ggPredict(boosted_py_no_adjustment,se=TRUE,interactive=TRUE)

# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_py_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years, data=clalit_data)
unboosted_py_no_adjustment %>%
  summary()


###########################################################################################################
# Incorporate case rates, deaths, and hospitalizations in Israel over the time period of the study
###########################################################################################################

# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_6_day_deaths_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Deaths, data=clalit_data)
boosted_6_day_deaths_no_adjustment %>%
  summary()

# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_6_day_deaths_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Deaths, data=clalit_data)
unboosted_6_day_deaths_no_adjustment %>%
  summary()

# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_6_day_cases_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Cases, data=clalit_data)
boosted_6_day_cases_no_adjustment %>%
  summary()

# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_6_day_cases_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Cases, data=clalit_data)
unboosted_6_day_cases_no_adjustment %>%
  summary()

# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_6_day_hosp_no_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Six_Day_Hospitalizations, data=clalit_data)
boosted_6_day_hosp_no_adjustment %>%
  summary()

# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_6_day_hosp_no_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Six_Day_Hospitalizations, data=clalit_data)
unboosted_6_day_hosp_no_adjustment %>%
  summary()


###########################################################################################################
# Now run multivariate using 6 day hospitalizations and person years of risk
###########################################################################################################

# Univariate Boosted: Incremental COVID deaths as a function of person years
boosted_py_hosp_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
boosted_py_hosp_adjustment %>%
  summary()

equation1=function(x){coef(boosted_py_hosp_adjustment)[2]*x+coef(boosted_py_hosp_adjustment)[1]}
equation2=function(x){coef(boosted_py_no_adjustment)[2]*x+coef(boosted_py_no_adjustment)[1]+coef(boosted_py_no_adjustment)[3]}

ggplot(boosted_py_hosp_adjustment,aes(y=Boosted_Incremental_COVID_Deaths,x=Boosted_Person_Years,color=Six_Day_Hospitalizations))+geom_point()+
  stat_function(fun=equation1,geom="line",color=scales::hue_pal()(2)[1])+
  stat_function(fun=equation2,geom="line",color=scales::hue_pal()(2)[2])

# Univariate Unboosted: Incremental COVID deaths as a function of person years
unboosted_py_hosp_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Six_Day_Hospitalizations, data=clalit_data)
unboosted_py_hosp_adjustment %>%
  summary()

###########################################################################################################
###########################################################################################################
###########################################################################################################

# THE FOLLOWING ARE LIKELY HEAVILY CONFOUNDED WITHOUT ACTUAL NON-COVID DEATH NUMBERS AS THESE HAD TO BE
# IMPUTED. IN OUR CASE, THEY WERE IMPUTED AS A CONSTANT RATE AS A FRACTION OF THE PERSON-YEARS OF EXPOSURE.
# CONSEQUENTLY, THIS CAN CREATE ARTIFACTUAL CORRELATIONS

# Multivariate Boosted: Incremental COVID deaths as a function of person years with additive effect of incremental Non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
boosted_with_non_COVID_adjustment = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Person_Years + Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_with_non_COVID_adjustment %>%
  summary()

# Multivariate Unboosted: Incremental COVID deaths as a function of person years with additive effect of incremental Non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
unboosted_with_non_COVID_adjustment = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Person_Years + Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_with_non_COVID_adjustment %>%
  summary()

# Boosted: COVID deaths as a function of non-COVID deaths
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
boosted_COVID_vs_Non_COVID = lm(formula=Boosted_Incremental_COVID_Deaths ~ Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_COVID_vs_Non_COVID %>%
  summary()

# Unboosted: COVID deaths as a function of non-COVID deaths
# # NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed and not provided
unboosted_COVID_vs_Non_COVID = lm(formula=Unboosted_Incremental_COVID_Deaths ~ Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_COVID_vs_Non_COVID %>%
  summary()

# Boosted Univariate: COVID-to-Non-COVID death rate ratio for boosted
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for boosted
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio %>%
  summary()

# Boosted Multivariate: COVID-to-Non-COVID death rate ratio for boosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio_with_Non_COVID = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years + Boosted_Incremental_Non_COVID_Deaths, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio_with_Non_COVID %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for unboosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio_with_Non_COVID = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years + Unboosted_Incremental_Non_COVID_Deaths, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio_with_Non_COVID %>%
  summary()

# Boosted Multivariate: COVID-to-Non-COVID death rate ratio for boosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
boosted_COVID_to_Non_COVID_Ratio_with_COVID = lm(formula=Boosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Boosted_Person_Years + Boosted_Incremental_COVID_Deaths, data=clalit_data)
boosted_COVID_to_Non_COVID_Ratio_with_COVID %>%
  summary()

# Unboosted Univariate: COVID-to-Non-COVID death rate ratio for unboosted with Non-COVID adjustment
# NOT NECESSARILY MEANINGFUL as the incremental non-COVID deaths were imputed, and so the "rate" was imputed, and not provided
unboosted_COVID_to_Non_COVID_Ratio_with_COVID = lm(formula=Unboosted_Death_Rate_Ratios_COVID_to_Non_COVID ~ Unboosted_Person_Years + Unboosted_Incremental_COVID_Deaths, data=clalit_data)
unboosted_COVID_to_Non_COVID_Ratio_with_COVID %>%
  summary()


###########################################################################################################
###########################################################################################################
###########################################################################################################

# VE Hospitalization bar graphs with error bars
bar_graph_data = read_delim('Bar_Graph_Hospitalization.csv', delim = ',', col_names = T, guess_max = 9000 )

describe(bar_graph_data)

head(bar_graph_data)
plot <-ggplot(bar_graph_data) +
  geom_bar( aes(x=Study, y=Effectiveness), stat="identity", fill="darkgray", alpha=0.7) +
  geom_text(aes(x=Study, y=0.05, label = stringr::str_wrap(Doses, 10)), vjust = "inward", hjust = "leftward") +
  geom_errorbar( aes(x=Study, ymin=Effectiveness-ve_hosp_minus_error, ymax=Effectiveness+ve_hosp_plus_error), width=0.4, colour="orange", alpha=0.9, size=1.3)
plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) + ggtitle("Clalit Health Vaccine Effectiveness Against COVID-19 Hospitalization")

# VE Death bar graphs with error bars
bar_graph_data = read_delim('Bar_Graph_Death.csv', delim = ',', col_names = T, guess_max = 9000 )

describe(bar_graph_data)

head(bar_graph_data)
plot <-ggplot(bar_graph_data) +
  geom_bar( aes(x=Study, y=Effectiveness), stat="identity", fill="darkgray", alpha=0.7) +
  geom_text(aes(x=Study, y=0.05, label = stringr::str_wrap(Doses, 10)), vjust = "inward", hjust = "leftward") +
  geom_errorbar( aes(x=Study, ymin=Effectiveness-ve_death_minus_error, ymax=Effectiveness+ve_death_plus_error), width=0.4, colour="orange", alpha=0.9, size=1.3) +
  geom_hline(yintercept=.766, linetype="dashed", color = "darkgreen", size=1) +
  annotate("text", x=2, y=.60, label=stringr::str_wrap("Possible Healthy Vaccinee Benefit Arbel Dec-21",33), size=4, color="darkgreen") +
  geom_segment(aes(x=2, y=.62, xend=2, yend=0.76), arrow = arrow(length=unit(.3, 'cm')), color="darkgreen")

plot + theme(plot.background = element_rect(fill = "lightgray"),
             panel.background = element_rect(fill = "lightgray", colour="lightgray"),
             panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             plot.title = element_text(hjust = 0.5)) + ggtitle("Clalit Health Vaccine Effectiveness Against COVID-19 Death")
