library(tidyverse)
library(janitor)
library(rvest)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(gghighlight)
library(ggrepel)

url1 <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

covid19_cases <- fread(url1) %>%
  clean_names()

covid19_cases1 <-  covid19_cases %>%
  select(-c("province_state","lat", "long")) %>%
  pivot_longer(-country_region, names_to="data", values_to = "cases" ) %>%
  mutate(data = gsub("x", "0" , data),
         data1 = as.Date(data, "%m_%d_%y")) %>%
  rename(country = country_region) %>%
  arrange(country, data1) %>%
  distinct(country, data1, .keep_all = T)
glimpse(covid19_cases1)

covid19_cases2 <- covid19_cases1 %>%
  filter(country != "China") %>%
  group_by(country) %>%
  mutate(max_cases = max(cases)) %>%
  ungroup()

dat_labels <- covid19_cases2 %>%
  group_by(country) %>% 
  summarise(
  label_position=max(data1),
  label_value=max(cases)
)
 
set.seed(73) 
p0 <- covid19_cases2 %>%
  group_by(country) %>%
  filter(max_cases > 1000 | country == "Brazil") %>%
  ggplot(aes(x=data1, y= cases, group = country, colour=country)) + 
  geom_line() +
  geom_text(data=subset(dat_labels, label_value>1000 | country == "Brazil"),
            aes(x=label_position, y=label_value,label=country),
            vjust=0,position=position_jitter(width=3,height=200)) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines")) 
p0
ggsave(p0, file="num_cases_14_mar.png", scale=.8)

## taxa de crescimento de casos
covid19_cases2 <- covid19_cases2 %>%
  group_by(country) %>%
  mutate(lag_cases = lag(cases),
         lag_cases_2 = lag(cases, 2)) %>%
  filter(!is.na(lag_cases_2), lag_cases_2 > 0,
         lag_cases -lag_cases_2 > 0 ) %>%
  mutate(growth = round((cases - lag_cases)/(lag_cases - lag_cases_2),4))

covid19_cases2 %>%
  group_by(country) %>%
  filter(max_cases > 5000 | country == "Brazil", growth < 20) %>%
  ggplot(aes(x=data1, y= growth, group = country, colour=country)) + 
  geom_line() +
  geom_text(data=subset(dat_labels, label_value>1000 | country == "Brazil"),
            aes(x=label_position, y=label_value,label=country),
            vjust=0,position=position_jitter(width=3,height=200)) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines"))

## casos após dias primeiro caso e após 50 casos
covid19_cases_first <- covid19_cases2 %>%
  group_by(country) %>%
  mutate(first_Case = cases > 0 & !duplicated(cases > 0),
         after_first_case = cases > 0) %>%
  filter(after_first_case ) %>%
  arrange(country, data1) %>%
  mutate(days_since_first = 1:n() - 1) %>%
  filter(days_since_first > 0) %>%
  ungroup()

dat_labels_first <- covid19_cases_first %>%
  group_by(country) %>% 
  summarise(
    label_position=max(days_since_first),
    label_value=max(cases)
  )

p_first_Case <- covid19_cases_first %>%
  group_by(country) %>%
  filter(max_cases > 1000 | country == "Brazil") %>%
  ggplot(aes(x=days_since_first, y= cases,
             group = country, colour=country)) + 
  geom_line() +
  geom_text(data=subset(dat_labels_first, label_value>1000 | country == "Brazil"),
            aes(x=label_position, y=label_value,label=country),
            vjust=0,position=position_jitter(width=3,height=200)) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines")) +
  ylab("casos") + xlab("dias desde o primeiro caso")
p_first_Case
ggsave(p_first_Case, file="num_cases_days_since_first.png", scale=.6,
       width = 18, height = 10)

covid19_cases_first 



# case_50 = cases > 49 & !duplicated(cases > 49)) %>%
#          days_country = abs(data1 - Sys.Date()),
#          first_case_date_aux = case_when(!first_Case ~ Sys.Date(),
#                                          TRUE ~ data1),
#          case_50_aux = case_when(!case_50 ~ Sys.Date(),
#                                  TRUE ~ data1),
#          first_case_date = min(first_case_date_aux),
#          case_50_date = min(case_50_aux),
#          days_since_first_case1 = as.numeric(abs(first_case_date - Sys.Date())),
#          days_since_case_50 = as.numeric(abs(case_50_date - Sys.Date()))) 


# setwd("C:/Users/mczfe/Pessoal/covid19")

## doubling days
df1 <- df %>%
  mutate(days = data1 - Sys.Date() )

reg <- lm(log(cases) ~ days, data= subset(df1, cases > 0 & country =="Brazil"))
summary(reg)
log(2)/reg$coef[2]

df1 %>%
  filter(country == "Brazil", cases > 0) %>%
  ggplot(aes(x=days, y=log(cases))) + geom_point()

### deaths
url_death <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"
  
  
covid19_fatality <- fread(url_death) %>%
  clean_names()
  
covid19_deaths <- covid19_fatality %>%
  select(-c("province_state","lat", "long")) %>%
  pivot_longer(-country_region, names_to="data", values_to = "deaths" ) %>%
  mutate(data = gsub("x", "0" , data),
         data1 = as.Date(data, "%m_%d_%y")) %>%
  rename(country = country_region) %>%
  arrange(country, data1) %>%
  distinct(country, data1, .keep_all = T)

covid19_deaths2 <- covid19_deaths %>%
  filter(country != "China") %>%
  group_by(country) %>%
  mutate(max_cases = max(deaths)) %>%
  ungroup()

dat_labels_deaths <- covid19_deaths2 %>%
  group_by(country) %>% 
  summarise(
    label_position=max(data1),
    label_value=max(deaths)
  ) %>%
  filter(label_value > 10)

set.seed(2310) 
p0a <- covid19_deaths2 %>%
  group_by(country) %>%
  filter(max_cases > 10) %>%
  ggplot(aes(x=data1, y= deaths, group = country, colour=country)) + 
  geom_line() +
  geom_text(data=dat_labels_deaths,
            aes(x=label_position, y=label_value,label=country),
            vjust=0,position=position_jitter(width=1,height=20)) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines")) 
p0a
ggsave(p0a, file="num_deaths.png", scale=.8)

# case fatality rate
covid19_cfr <- covid19_deaths %>%
  inner_join(covid19_cases2, by = c("country", "data1")) %>%
  filter(deaths > 0) %>%
  mutate(cfr = deaths/cases)

covid19_cfr2 <- covid19_cfr %>%
  filter(country != "China") %>%
  group_by(country) %>%
  mutate(max_cfr = max(cfr)) %>%
  ungroup()

dat_labels_cfr <- covid19_cfr2 %>%
  group_by(country) %>% 
  summarise(
    label_position=max(data1),
    label_value=dplyr::last(cfr)
  ) %>% 
  ungroup() %>%
  inner_join(distinct(covid19_cfr2, country, .keep_all=T), by= "country") %>%
  filter(max_cases > 100 & max_cfr < .1)

set.seed(2310) 

p0b <- covid19_cfr2 %>%
  filter(max_cases > 100 & max_cfr < .1) %>%
  group_by(country) %>%
  ggplot(aes(x=data1, y= cfr, group = country, colour=country)) + 
  geom_line() +
  scale_y_continuous(labels = scales::percent, 
                     breaks = c(.005, .01, .02, .03, .05, .08, .1)) + 
  geom_text(data=dat_labels_cfr, 
            aes(x=label_position, y=label_value,label=country),
            vjust=0, position=position_jitter(width=1,height=.002),
            size = 2) +
  scale_colour_discrete(guide = 'none')  +    
  theme(plot.margin = unit(c(1,3,1,1), "lines"))
p0b
ggsave(p0b, file="num_cfr.png", scale=.8)



## modeling with stan
library(rstanarm)

df2 <- covid19_cfr2 %>%
  select(-c("data.x", "data.y")) %>%
  mutate(country = as.factor(country)) %>%
  group_by(country) %>%
  mutate(cases_lag = lag(cases),
         first_Case = cases > 0 & !duplicated(cases > 0),
         days_country = as.numeric(abs(data1 - min(data1))),
         first_case_date_aux = case_when(!first_Case ~ Sys.Date(),
                                         TRUE ~ data1),
         first_case_date = min(first_case_date_aux),
         days_since_first_case1 = abs(Sys.Date() - first_case_date)) %>%
  filter(!is.na(cases_lag))

summary(df2)

df2 %>%
  ggplot(aes(y=cases, x=log_cases_lag, group=country)) +
  geom_line()

df2 %>%
  ggplot(aes(y=cases, x=days_country, group=country)) +
  geom_line()

## modelo
# cases_ij = a + days + + cases_lag_ij + eij, i países, j dias. e_ij ~ N(0, sigma2_j)
# a_j= mu_alpha + u_j, where u_j∼ N(0,sigma2_alpha)



df2 <- df2 %>%
  mutate(log_cases = log(cases),
         days_since_first_case1 = as.numeric(days_since_first_case1),
         days_since_first_case1 = as.factor(days_since_first_case1))

M1_stan <- stan_lmer(formula = log_cases ~ 1 + (1 | days_country), 
                     data = df2,
                     seed = 349,
                     cores = 4, iter = 400)
print(M1_stan, digits = 2)

M2_stan <- stan_lmer(formula = log_cases ~  + days_country
                       log_cases_lag + (1 | country), 
                     data = df2,
                     prior = normal(location = 0, 
                                    scale = 50,
                                    autoscale = FALSE),
                     prior_intercept = normal(location = 0, 
                                              scale = 50, 
                                              autoscale = FALSE),
                     seed = 349,
                     cores = 4, iter = 400)

print(M2_stan, digits = 2)
summaryM2_stan <- summary(M2_stan)
print(summaryM2_stan)

pp_check(SingleLevelModel, nreps = 100) #+ xlab("valence")




## World Values Survey
library(lodown)
# examine all available WVS microdata files
wvs_cat <-
  get_catalog( "wvs" ,
               output_dir = file.path( path.expand( "~" ) , "WVS" ) )

# wave six only
wvs_cat <- subset( wvs_cat , grepl( "Brazil" , full_url ) & wave == 6 )
# download the microdata to your local computer
wvs_cat <- lodown( "wvs" , wvs_cat )

library(survey)

wvs_df <-
  readRDS( 
    file.path( path.expand( "~" ) , "WVS" , 
               "wave 6/F00003106-WV6_Data_United_States_2011_spss_v_2016-01-01.rds" ) 
  )

# construct a fake survey design
# warning( "this survey design produces correct point estimates
#          but incorrect standard errors." )
wvs_design <- 
  svydesign( 
    ~ 1 , 
    data = wvs_df , 
    weights = ~ v258
  )

wvs_design <- 
  update( 
    wvs_design , 
    
    one = 1 ,
    
    language_spoken_at_home =
      factor( v247 , 
              levels = c( 101 , 128 , 144 , 208 , 426 , 800 ) , 
              labels = c( 'chinese' , 'english' , 'french' , 
                          'japanese' , 'spanish; castilian' , 'other' )
      ) ,
    
    citizen = as.numeric( v246 == 1 ) ,
    
    task_creativity_1_10 = as.numeric( v232 ) ,
    
    work_independence_1_10 = as.numeric( v233 ) ,
    
    family_importance =
      factor( v4 , 
              labels = c( 'very' , 'rather' , 'not very' , 'not at all' ) 
      )
  )

## v56 levar vantagem