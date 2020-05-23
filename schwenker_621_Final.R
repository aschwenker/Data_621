library(tidyverse)
library(sf)
library(units)


#census tract geojson
geo_url <- "https://data.cityofnewyork.us/api/geospatial/fxpq-c8ku?method=export&format=GeoJSON"

# read in geojson of tract geometry 
tract_sf <- read_sf(geo_url) %>%
  st_transform(2263) %>%   # convert to same projection as above
  select(boro_name, boro_ct2010) %>%
  mutate(area = set_units(st_area(.), mi^2)) %>%
  print()  
#saved locally because file was too large to host on github
crime_all <- read.csv("/Users/Sarah/Downloads/NYPD_Complaint_Data_Historic.csv",stringsAsFactors=FALSE)
crime_all$year<-as.character(crime_all$RPT_DT)
crime_all$year<-substr(crime_all$year, 7, 10)
crime_all$date<-as.character(crime_all$RPT_DT)

crime_all$date<-as.Date(crime_all$date,"%m/%d/%Y")

crime_2010 <- subset(crime_all, year = "2010")
print(typeof(crime_all$year))

crime_all %>% group_by(year) %>% tally()
crime<-crime_all[!is.na(crime_all$x_coord_cd),]
crime_all$date<-as.character(crime$rpt_dt)
crime$date<-substr(crime$date, 1, 10)
print(typeof(crime$date))
crime$date<-as.Date(crime$date,"%Y-%m-%d")
crime <- subset(crime_all, date > "2009-12-31" & date < "2011-01-01")

# convert data frame into sf object
crime_sf <- crime %>%
  mutate_at(vars(X_COORD_CD, Y_COORD_CD), as.numeric) %>%   # coordinates must be numeric
  st_as_sf(
    coords = c("X_COORD_CD", "Y_COORD_CD"),
    agr = "constant",
    crs = 2263,        # nad83 / new york long island projection
    stringsAsFactors = FALSE,
    remove = TRUE
  )


# find points within polygons
crime_in_tract <- st_join(crime_sf, tract_sf, join = st_within)
sum(crime_tract_count$n)
# count trees per census tract
crime_tract_count <- count(as_tibble(crime_in_tract), boro_ct2010) %>%
  print()
print(typeof(crime_tract_count$boro_ct2010))
# join  count with tract df
tract_crime_sf <- left_join(tract_sf, crime_tract_count) %>%
  print()

crime_tract_count$boro_ct2010<- as.numeric(as.character(crime_tract_count$boro_ct2010))
crime_tract_count$boro<-substr(crime_tract_count$boro_ct2010, 1, 1)
crime_tract_count$ct<-substr(crime_tract_count$boro_ct2010, 2, 7)
crime_tract_count$ct<-as.numeric(crime_tract_count$ct)
crime_tract_count$BCT<-paste(crime_tract_count$boro,crime_tract_count$ct,sep="")

census <- read.csv("https://raw.githubusercontent.com/aschwenker/Data_621/master/census_population_2010_NYC.csv",stringsAsFactors=FALSE)
census$BCT<-paste(census$X2010.DCP.Borough.Code,census$X2010.Census.Tract,sep="")
census$BCT<-as.numeric(census$BCT)
total <- merge(crime_tract_count,census,by.x=c("BCT"),by.y=c("BCT"))

#reomve comma and convert to number
total$t_pop_2010 <- gsub(",","",total$t_pop_2010)
total$t_pop_2010<-as.numeric(total$t_pop_2010)
#plot and review for linearity
ggplot(total, aes(x = t_pop_2010, y = n)) +
  geom_point() +
  stat_smooth()
#assess outliers

boxplot(total$n, main="Count of Crime", sub=paste("Outlier rows: ", boxplot.stats(total$n)$out))  
boxplot(total$t_pop_2010, main="Total Population by Census Tract")

#subset 
total_no_outliers<-subset(total,n<500)
total_no_outliers<-subset(total_no_outliers,t_pop_2010<10000)

ggplot(total_no_outliers, aes(x = t_pop_2010, y = n)) +
  geom_point() +
  stat_smooth()

cor(total_no_outliers$t_pop_2010, total_no_outliers$n,use = "complete.obs")
#assess outliers after subset
boxplot(total_no_outliers$n, main="Count of Crime subset where crime count< 500 and pop < 10000")  
boxplot(total_no_outliers$t_pop_2010,main="Count of subset of total population by census tract where crime count< 500 and pop < 10000")

#check for distribution normality
plot(density(total_no_outliers$t_pop_2010), main="Density Plot: subsetted population variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(total_no_outliers$t_pop_2010), 2)))  # density plot for 'speed'
polygon(density(total_no_outliers$t_pop_2010), col="red")
plot(density(total_no_outliers$n), main="Density Plot: subsetted crime variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(total_no_outliers$n), 2)))  # density plot for 'speed'
polygon(density(total_no_outliers$n), col="red")
#apply sqrt and see if it improves normality
plot(density(sqrt(total_no_outliers$t_pop_2010)), main="Density Plot: subsetted sqrt of population variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(sqrt(total_no_outliers$t_pop_2010)), 2)))  # density plot for 'speed'
polygon(density(sqrt(total_no_outliers$t_pop_2010)), col="red")
plot(density(sqrt(total_no_outliers$n)), main="Density Plot: subsetted sqrt of crime variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(sqrt(total_no_outliers$n)), 2)))  # density plot for 'speed'
polygon(density(sqrt(total_no_outliers$n)), col="red")
#try log
plot(density(log(total_no_outliers$t_pop_2010)), main="Density Plot: subsetted population variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(log(total_no_outliers$t_pop_2010)), 2)))  # density plot for 'speed'
polygon(density(log(total_no_outliers$t_pop_2010)), col="red")
plot(density(log(total_no_outliers$n)), main="Density Plot: subsetted crime variable", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(log(total_no_outliers$n)), 2)))  # density plot for 'speed'
polygon(density(log(total_no_outliers$n)), col="red")
#build model and summarize
model <- lm(sqrt(n) ~ sqrt(t_pop_2010) , data = total_no_outliers)
summary(model)
par(mfrow=c(2,2))
plot(model)
