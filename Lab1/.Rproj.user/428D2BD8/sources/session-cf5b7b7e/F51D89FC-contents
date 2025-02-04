library(tidyverse)
#make vector
rainfall <- c(1.0, 6.0, 2.0, 2.0, 0.0, 0.0, 3.5,4.0)
#find first element
rainfall[1]

#find days over 3
rainfall[1] >= 3

#make if-else block but only checks first element
if(rainfall[1] >= 3){print('Big Storm')} else{print("little storm")}

#make function
f.storm.test <- function(rainfallAmount){
  if(rainfallAmount >= 3){
    print('Big Storm')} else{
      print("little storm")}}

#make loop, loops are only good for small data
for(i in rainfall){
  f.storm.test(i)
}

#tidy way
rainfall %>% purrr::map(., f.storm.test)

rainfall >= 3

#find the day with the greatest rainfall
max(rainfall) #just tells the greatest amount
which(rainfall == max(rainfall)) #tells which element is the greatest

mydf <- read_csv("./data/ne_counties.csv")
glimpse(mydf)

max(mydf$MedValHous)
which(mydf$MedValHous == max(mydf$MedValHous)) #tells you the row
which(mydf$MedValHous == max(mydf$MedValHous)) %>% mydf[.,] #finds the row
mydf %>% dplyr::slice_max(MedValHous) #finds the row again

#calculate how much less each household median is compared to the max
newdf <- mydf %>% mutate(deviation = MedValHous - max(MedValHous))
newdf %>% dplyr::slice_min(deviation)

#plot it
newdf %>% ggplot(., aes(x =deviation)) +
  geom_histogram()+
  theme_minimal()
#make it nicer
newdf %>% ggplot(., aes(x =deviation)) +
  geom_histogram(fill = "green")+
  theme_classic()+
  labs(title = "Gay Histogram",
       subtitle = "I Love Cats",
       x = "Deviation",
       y = "Count")

newdf %>% ggplot(., aes(x =deviation, y = after_stat(density))) +
  geom_histogram(fill = "darkgreen")+
  geom_vline(xintercept = mean(newdf$deviation), color = "purple", linewidth = 2)+
  geom_density(color = "darkblue",linewidth = 1)+
  theme_classic()+
  labs(title = "Gay Histogram",
       subtitle = "I Love Cats",
       x = "Deviation",
       y = "Count")
