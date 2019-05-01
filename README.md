## Prediciting Trends Between Race and Highly Ranked Universities within the United States

In order to complete this project we used data from two sources. The first source included [College Admissions](https://www.kaggle.com/samsonqian/college-admissions) data. The second dataset consisted of data from [World University Rankings](https://www.kaggle.com/mylesoneill/world-university-rankings) also from kaggle's website. Within this project, we will be discussing the impact race has on a university being highly rated. Below includes the steps/process our group went through in order to draw our conclusions.


---
title: "Final Project"
author: "Swarali Korgaonkar & Akanksha Bhusari"
date: "Due April 30, 2019"
output:
  html_document:
    df_print: paged
---

# Front matter

```{r echo=TRUE, message=FALSE}
# always clean up R environment
rm(list = ls())
# load all packages here
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(mosaic)

require(rpart)
require(partykit)

# user-defined functions here (if any)
clean_data <-
  function(data){
    country <-
      data %>%
        select(1, 3)
    country <-
      unique(country)
    
    AVG_data <-
      data %>%
        group_by(Name) %>%
        summarize(world_rank=mean(world_rank), yr2012=sum(yr2012, na.rm = T), yr2013=sum(yr2013, na.rm = T), yr2014=sum(yr2014, na.rm = T), yr2015=sum(yr2015, na.rm = T), quality_of_education=mean(quality_of_education), alumni_employment=mean(alumni_employment), quality_of_faculty=mean(quality_of_faculty), publications=mean(publications), influence=mean(influence), citations=mean(citations), broad_impact=mean(broad_impact), patents=mean(patents), score=mean(score), Applicants.total=mean(Applicants.total), Admissions.total=mean(Admissions.total), TotalEnrollment=mean(TotalEnrollment), AcceptanceRate=mean(AcceptanceRate), TribalCollege=mean(TribalCollege), AreaType.UCL=mean(AreaType.UCL), Enrollment=mean(Enrollment), AmericanIndianAlaska.Pct=mean(AmericanIndianAlaska.Pct), Asian.Pct=mean(Asian.Pct), Black.Pct=mean(Black.Pct), HispanicLatino.Pct=mean(HispanicLatino.Pct), HawaiianPacificIslander.Pct=mean(HawaiianPacificIslander.Pct), White.Pct=mean(White.Pct), TwoMoreRace.Pct=mean(TwoMoreRace.Pct), UnknownEthnicity.Pct=mean(UnknownEthnicity.Pct), NonResidentAlien.Pct=mean(NonResidentAlien.Pct), AsianHawaiianPacificIslander.Pct=mean(AsianHawaiianPacificIslander.Pct), Women.Pct=mean(Women.Pct), FirstUndergradForeign.Num=mean(FirstUndergradForeign.Num), FirstUndergradForeign.Pct=mean(FirstUndergradForeign.Pct))
    
    TOTALdata <-
      merge(x = AVG_data, y = country, by = "Name", all = TRUE)
    
    return(TOTALdata)
    
  }


# load data
```

# Download datasets and Data Preprocessing 

```{r}
# CollegeRankings <-
#   read.csv(file="/Users/Swarali/Desktop/STAT 380/final-project/FinalData/world-university-rankings/cwurData.csv", header=TRUE, sep=",")
CollegeRankings <-
  read.csv(file="C:/Users/Akanksha Bhusari/Documents/STAT380/final-project/cwurData.csv", header=TRUE, sep=",")

names(CollegeRankings)[names(CollegeRankings) == 'institution'] <- 'Name'

CollegeRankings

# CollegeAdmissions <- 
#   read.csv(file="/Users/Swarali/Desktop/STAT 380/final-project/FinalData/Data-Table1.csv", header=TRUE, sep=",")
CollegeAdmissions <- 
  read.csv(file="C:/Users/Akanksha Bhusari/Documents/STAT380/final-project/Data-Table 1.csv", header=TRUE, sep=",")

#Subset data to only include columns needed
CollegeAdmissions_tidy <- 
  CollegeAdmissions %>%
  select(1:4, 28, 42:44, 46, 53:63, 90:91)

#Change column names
setnames(CollegeAdmissions_tidy, 
         old=c("Enrolled.total", "Percent.admitted...total", "Historically.Black.College.or.University", "Tribal.college", "Degree.of.urbanization..Urban.centric.locale.", "Total..enrollment", "Percent.of.total.enrollment.that.are.American.Indian.or.Alaska.Native", "Percent.of.total.enrollment.that.are.Asian", "Percent.of.total.enrollment.that.are.Black.or.African.American", "Percent.of.total.enrollment.that.are.Hispanic.Latino", "Percent.of.total.enrollment.that.are.Native.Hawaiian.or.Other.Pacific.Islander", "Percent.of.total.enrollment.that.are.White", "Percent.of.total.enrollment.that.are.two.or.more.races", "Percent.of.total.enrollment.that.are.Race.ethnicity.unknown", "Percent.of.total.enrollment.that.are.Nonresident.Alien", "Percent.of.total.enrollment.that.are.Asian.Native.Hawaiian.Pacific.Islander", "Percent.of.total.enrollment.that.are.women", "Number.of.first.time.undergraduates...foreign.countries", "Percent.of.first.time.undergraduates...foreign.countries"), 
         new=c("TotalEnrollment", "AcceptanceRate", "HBCU", "TribalCollege", "AreaType.UCL", "Enrollment", "AmericanIndianAlaska.Pct", "Asian.Pct", "Black.Pct", "HispanicLatino.Pct", "HawaiianPacificIslander.Pct", "White.Pct", "TwoMoreRace.Pct", "UnknownEthnicity.Pct", "NonResidentAlien.Pct", "AsianHawaiianPacificIslander.Pct", "Women.Pct", "FirstUndergradForeign.Num", "FirstUndergradForeign.Pct")) 

CollegeAdmissions_tidy
```
To start the project, we had to download and preprocess the data in order to make the future analysis process more managable. We chose to subset the data because there were over 100 columns and not all were necessary for our project's topic. We also chose to rename some of the columns in order to make scrolling through the data easier. Before, the column names were very long and scrolling through the data wasted a lot of time. Overall, this R chunk was made in part to help with the efficiency throughout the course of working on this project.


# Join datasets (Left Join)

```{r}
total_data<-
  merge(x=CollegeRankings,y=CollegeAdmissions_tidy,by="Name",all.x=TRUE)
total_data
```
In this R chunk we merged the two datasets that we downloaded earlier in order to create one cumulative dataset. The reason to why there are multiple listings of the universities is because the data includes statistics from multiple years and the data per university changes yearly. 




## Spread 

```{r}
total_data <-
  spread(total_data, year, national_rank, fill = NA)

total_data
```
Based on the national rank per year, the total_data is spread.

```{r}
# rename columns 
names(total_data)[37]<-
  "yr2015"
names(total_data)[36]<-
  "yr2014"
names(total_data)[35]<-
  "yr2013"
names(total_data)[34]<-
  "yr2012"

total_data
```
We changed the column names to help make the code more understandable and help with readability. 



## User-Defined Functions

```{r}
FINALdata <-
  clean_data(total_data)
FINALdata
```
This function makes sure there are no repeats of universities within the 'Name' column. Also, all of the values per university were averaged to show each universities overall stats.

## Filtering the top 100 universities in the world

```{r}
top_schools <-
  filter(FINALdata, world_rank < 100)

top_schools
```
Within this section of the code, we decided to filter the universities that were ranked within the top 100 nationally in order to later determine trends within top schools.


## Loops and Control Flow

```{r}
countries <-
  top_schools %>%
    select(36)
countries <-
  unique(countries)

countries

```

```{r}
countriesList <-
  c(countries[1])
countriesList
```

```{r}
countries
```



## Using the `apply` family function within the loop


```{r}
variables <-
  top_schools %>%
    select(2, 7, 9, 10, 11, 14)

for (x in variables){
  V <-
    tapply(x, top_schools$country, FUN=mean)

  V <-
    as.data.frame(V)

  V <-
    cbind(country = rownames(V), V)

  countries <-
    merge(x = countries, y = V, by = "country", all.x = TRUE)
  
}

names(countries)[2]<-
  "AvgWorldRank"
names(countries)[3]<-
  "AvgQEdu"
names(countries)[4]<-
  "AvgQFaculty"
names(countries)[5]<-
  "AvgPublications"
names(countries)[6]<-
  "AvgInfluence"
names(countries)[7]<-
  "AvgPatents"

countries
```
For selected variables, the loop finds the averages of those variables for each country and adds them to the data frame.

# Data Analysis

## Statistical Modeling/Supervised Learning

```{r}
pairs(countries)
countries
```
This graph shows a visual representation of the 6 variables that were just averaged in the previous step.

```{r}
mod_tree <- 
  rpart(country ~ AvgQEdu + AvgQFaculty + AvgPublications + AvgInfluence + AvgPatents, data = countries)

tree_plot <-
  plot(as.party(mod_tree))

tree_plot

```
We chose to not make training and testing data because we are using the data for exploration only not confirmation. We found that the Quality of Education is the biggest/most impactful variable.

```{r}
printcp(mod_tree)
plotcp(mod_tree)

```
This makes sure our tree is optimal. Since there was no lower xerror, we could receive we can conclude that our tree is the optimal tree.

## Unsupervised Learning

```{r}
result <- 
  countries[-1]
row.names(result) <- 
  countries$country
result
```

```{r}
countries_pca <- result %>%
  prcomp(scale = TRUE)  


(-1) * countries_pca$rotation[, 1:2] %>% 
  round(2)

```

```{r}
countries_pca$x %>%
  as.data.frame() %>%  # `ggplot2` expects a data frame object
  ggplot(aes(x = PC1, y = PC2)) + 
  geom_point(aes(color = countries$country), size = 3) + 
  xlab("Best Vector from PCA") + 
  ylab("Second Best Vector from PCA") + 
  ggtitle("Two-dimensional representation of Average Country Education values (colored by country)") 
```
PCA stands for Principal Components Analysis. PCA is used to summarize a large set of correlated variables with a smaller number of representative variables that collectively explain most of the variability in the original data set.

```{r}
countries_std <-
  scale(result) %>%
  as.data.frame()

countries_dist <- 
  dist(countries_std)

countries_dendo <-
  countries_dist %>%
    hclust(method = "complete")

countries_dendo %>%
  plot(cex = 0.9, labels = countries$country, lwd = 2,
       main = "Countries Dendogram with Complete Linkage")

```
This dendogram shows which countries are similar to one another as similar countries are closer together. For example, Canada and the United Kingdom are very similar in regards to education. In general we found that the Asian countries were relatively similar to one another and the European countries were similar to one another. Education systems are very similar based on location.

## User-defined Simulations

```{r}
UniSims <- 
  mosaic::do(100) * 
  top_schools %>%
  group_by(country) %>%
  summarise(AvgWorldRank = mean(world_rank, na.rm = TRUE)) 

UniSims

favstats(world_rank ~ country, data = top_schools) 
```
This user definied simulation allowed us to group schools based on country and find the average stats. 

# Data Visualization

Clear demonstration of proficiency should include at least FOUR distinct and useful data visualizations that are relevant to purpose of the analysis. Include at least one effective visualization with layered data from distinct data structures--e.g., a `geom` that plots data from a secondary data frame AND another must effectively display many--3 or more--variables. All plots must demonstrate good plotting practices.

```{r}
top_schools %>%
  ggplot(aes(x = quality_of_faculty, y = quality_of_education)) + 
  geom_point(aes(color = country), size = (top_schools$world_rank / 10)) + 
  xlab("Quality of Faculty") + 
  ylab("Quality of Education") + 
  ggtitle("How Quality of Faculty Affects Quality of Edu by Country for the world's Top 100 Universities")
```


```{r}
reg <- 
  lm(quality_of_education~quality_of_faculty,data=top_schools) 
with(top_schools,plot(quality_of_faculty, quality_of_education))
	abline(reg)
```
This visualization as well as the one above it showcases the positive correlation between quality of faculty and quality of education.

```{r}
top_USA_schools <-
  filter(top_schools, country == "USA" & world_rank < 10)
top_USA_schools

```
This R chunk filters the top schools within the USA that are within the top 10 rank worldwide.

```{r}
qEdu_distribution <-
rnorm(countries, mean=countries$AvgQEdu, sd=2)
par(mfrow=c(3,1))
hist(qEdu_distribution, main="Average Quality of Education of Universities Around the World")
```
The histogram shows us that the average quality of education of universities around the world is on the lower side of the spectrum.

```{r}
yr2012 <- 
top_USA_schools$yr2012
yr2013 <- 
top_USA_schools$yr2013 
yr2014 <- 
top_USA_schools$yr2014
yr2015 <- 
top_USA_schools$yr2015

plot(top_USA_schools$Name, yr2012, ylim = range(c(yr2012, yr2013, yr2014, yr2015)), type = "l") 
lines(top_USA_schools$Name, yr2013, col = "red") 
lines(top_USA_schools$Name, yr2014, col = "blue") 
lines(top_USA_schools$Name, yr2015, col = "green")
```
We used the top_USA_schools data frame created above to determine the change in national ranking between 2012 and 2015.

```{r}
# Graph from two datasets

countries[order(-countries$AvgWorldRank),]
countries[order(countries$AvgWorldRank),]

Norway <-
  mosaic::do(100) * 
  countries %>%
  group_by(country == "Norway") %>%
  summarise(AvgQFaculty_norway = mean(AvgQFaculty, na.rm = TRUE)) 

Norway <-
  Norway %>%
  select(2)

UK <-
  mosaic::do(100) * 
  countries %>%
  group_by(country == "United Kingdom") %>%
  summarise(AvgQFaculty_uk = mean(AvgQFaculty, na.rm = TRUE)) 

UK <-
  UK %>%
  select(2)

UK
Norway

plot(range(UK, na.rm=T), range(Norway, na.rm=T), type='n')
lines(UK, col="red")
lines(Norway, col="green")
```
First, we determined which country had the lowest average world rank and which school had the highest average world rank. Then, we created two datasets with the average quality of faculty of both countries to determine how much influence faculty quality has on a country's world rank. We were surprised to see that there was not a significant difference between the quality of faculty between the two countries on opposite spectrums of the world rank.


# Conclusions

Throughout the process of completing this project we were focused on looking at the inequalities in education but the greatest challenge we faced was determining which factor in education we actually wanted to look at. Did we want to look at it globally? Did we want to look it for diversity? Did we want to look at it for the cost? However, given the data that we found, we firstly narrowed down our scope by looking at variables we were interested in for universities and their respective countries around the world. Once we started looking at the top universities in the world, we were able to see how different universities compared to one another and then we narrowed it down a bit more to look at the top countries and their numbers since they are the ones that have the best universities and compare them. After doing some analysis, we found that quality of education and quality of faculty are not only related to one another, but align with a country’s top education performance globally as well. We also found that overall, even for the top global universities, quality of education was not as high as it has the potential to go. Since western education has the higher world ranks, it is important for it to have the highest standards. Looking at USA alone, our top universities move up in their world rank as they do their national rank. It is important for model nations to be the top (ex. Norway and UK were so similar even though their rank was far apart). We think that it is important for universities, especially in western nations to treat their faculty better because that is ultimately what contributes to quality of education a lot. In a day and age where education is global, international students look to us for high quality education so we need to do better with our faculty to ensure that for the world and act as a role model.
