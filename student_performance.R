
#import the dataset from your working directory 
library(readr)
students_performance <- read_csv("C:/Users/user/Downloads/StudentsPerformance.csv")

#understanding unit of observations 
colnames(students_performance)
#i am using the package janitor to clean names 
library(janitor)
students_performance <- clean_names(students_performance)
colnames(students_performance)


str(students_performance)
library(DataExplorer)
plot_str(students_performance)
plot_missing(students_performance)
plot_intro(students_performance)

#changing categorical variables to factor (better as factor than character)
students_performance$gender <- as.factor(students_performance$gender)
students_performance$race_ethnicity <- as.factor(students_performance$race_ethnicity)
students_performance$parental_level_of_education <- as.factor(students_performance$parental_level_of_education)
students_performance$lunch <-as.factor(students_performance$lunch)
students_performance$test_preparation_course <- as.factor(students_performance$test_preparation_course)
#use attach to attach the column names to the dataset 
attach(students_performance)


#check summary 

summary(students_performance)

#created a function for quick histogram and boxplot 
quick_analysis= function(df){
  data = students_performance
  library(ggplot2)
  histogramm<- ggplot(data,aes(df))+geom_histogram(fill="white",colour="black")
  boxplott<- ggplot(data,aes(df))+geom_boxplot(fill="dark green", colour="black")
  library(ggpubr)
  plot_fun<-ggarrange(histogramm,boxplott,ncol=2)
  print(plot_fun)
  
}



quick_analysis(math_score)

quick_analysis(writing_score)


quick_analysis(reading_score)

library(tidyverse)
#lunch
students_performance %>% 
  count(lunch) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = lunch, y = prop)) +
  geom_col(aes(fill = lunch), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group =lunch),
            position = position_dodge(width = 0.9),
            vjust = 1.5)
#Gender 
students_performance %>% 
  count(gender) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = gender, y = prop)) +
  geom_col(aes(fill = gender), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = gender),
            position = position_dodge(width = 0.9),
            vjust = 1.5)
#test_preparation
students_performance %>% 
  count(test_preparation_course) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = test_preparation_course, y = prop)) +
  geom_col(aes(fill = test_preparation_course), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = test_preparation_course),
            position = position_dodge(width = 0.9),
            vjust = 1.5)
#parental level of education
students_performance %>% 
  count(parental_level_of_education) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = parental_level_of_education, y = prop)) +
  geom_col(aes(fill = parental_level_of_education), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = parental_level_of_education),
            position = position_dodge(width = 0.3),
            vjust = 0.5)  
#race_ethnicity
students_performance %>% 
  count(race_ethnicity) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = race_ethnicity, y = prop)) +
  geom_col(aes(fill = race_ethnicity), position = "dodge") +
  geom_text(aes(label = scales::percent(prop), 
                y = prop, 
                group = race_ethnicity),
            position = position_dodge(width = 0.9),
            vjust = 1.0) 



plot_correlation(students_performance,type = c("continuous"))
plot_correlation(students_performance,
                 maxcat = 4)
plot_correlation(students_performance[, c(3,4,5, 6, 7,8)], maxcat = 5)



#bivirate 

a<- ggplot(students_performance,aes(x=math_score, y = reading_score,colour = test_preparation_course ))+geom_point()+ ggtitle("students performance based on preparation")
b<- ggplot(students_performance,aes(x=math_score, y = writing_score,colour = test_preparation_course ))+geom_point()+ ggtitle("students performance based on preparation")
c<- ggplot(students_performance,aes(x=math_score, y =test_preparation_course, colour= test_preparation_course ))+geom_count()+ ggtitle("accessing performance based on math score")
ggplot(students_performance,aes(x=reading_score, y =parental_level_of_education, colour= test_preparation_course ))+geom_col()+ ggtitle(" ")
ggplot(students_performance,aes(x=writing_score, y =parental_level_of_education, colour= test_preparation_course ))+geom_col()+ ggtitle(" ")
ggplot(students_performance,aes(x=reading_score, y =parental_level_of_education, colour= parental_level_of_education))+geom_col()+ ggtitle(" ")
d<- ggplot(students_performance,aes(x=math_score, y =parental_level_of_education, colour= parental_level_of_education))+geom_col()+ ggtitle("Total no of premium paid")
e<- ggplot(students_performance,aes(x=test_preparation_course, y =parental_level_of_education))+geom_count()+ ggtitle("Parents influence on test completion")
g<- ggplot(students_performance,aes(x=reading_score, y =test_preparation_course, colour= lunch))+geom_col()+ ggtitle("How food contributed to good grades")
h<- ggplot(students_performance,aes(x=math_score, y =test_preparation_course, colour= lunch))+geom_col()+ ggtitle("How food contributed to good grade")
k<- ggplot(students_performance,aes(x=test_preparation_course, y =gender, colour= gender))+geom_col()+ ggtitle("Gender vs test preparation")
m<- ggplot(students_performance,aes(x=reading_score, y =test_preparation_course, colour= gender))+geom_col()+ ggtitle("Influence of gender on performance")
n<- ggplot(students_performance,aes(x=writing_score, y =test_preparation_course, colour= gender))+geom_col()+ ggtitle("Influence of gender on performance")
p<- ggplot(students_performance,aes(x=math_score, y =test_preparation_course, colour= gender))+geom_col()+ ggtitle("Influence of gender on performance")



rat <- ggarrange(a,b,c,e,ncol=2,nrow=2)
print(rat)



rat2 <- ggarrange(g,h,k,m,ncol=2,nrow=2)
print(rat2)


rat3<- ggarrange(n,p,ncol=2)
print(rat3)
