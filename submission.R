
#Install required packages 
install.packages("countrycode")
library(countrycode)
install.packages("tidyverse")
library(tidyverse)
install.packages("viridis")
library(viridis)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("treemapify")
library(treemapify)

#Load the csv data into R
nobel_main <- read.csv("nobel_final.csv")
View(nobel_main)

#Converting country code to country name for birth of the laureate
born_country <- nobel_main %>%
  select(born_country_code)
View(born_country)

country_born <-countrycode(born_country$born_country_code,"iso2c","country.name") 

View(country_born)

# Converting country code to country for death of the laureates 
dead_country <- nobel_main %>%
  select(died_country_code)
View(dead_country)

country_died <-countrycode(dead_country$died_country_code,
                           "iso2c","country.name") 

View(country_died)
View(nobel_main)

#Binding the 2 df to the final df 
final_nobel <- cbind(nobel_main,country_born,country_died)
View(final_nobel)


# Creating a new column
final_nobel$full_name <-paste(final_nobel$firstname,final_nobel$surname)
View(final_nobel)

colnames(final_nobel)[17] <- "Fullname"
View(final_nobel)

#Barchart
# Which gender won in which category
ggplot(final_nobel,aes(x=category))+
  geom_bar(aes(fill=gender))+
  scale_fill_viridis_d(option = "turbo")+
  labs(x="Nobel prize field won by the Laureates",
       title = "The fields in which Males and Females won Nobel prize ",
       fill="Gender")+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"),
        legend.title = element_text(face = "bold",size = 10)) 

#Boxplot
#Age at which they won nobel prize
ggplot(final_nobel,aes(x=category,y=age_get_prize,fill=category))+
  geom_boxplot(alpha=1)+
  scale_fill_viridis_d(option = "plasma")+
  labs(x="The Field in which the Nobel Prize was won",
       y="Age of the Laureates  ",
       title="Age at which the Laureates won their respective Nobel Prize ",
       fill="Field")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"),
        legend.title = element_text(face = "bold"))

#Histogram
#Timeline of when lauretes won their prize
ggplot(final_nobel,aes(year))+
  geom_histogram(bins=6, binwidth=3,fill="cyan3",color="coral",alpha=0.9)+
  labs(title="Number of Laureates that have won in that year",
       x="Year ranges from 1900 ")+
  theme(plot.title = element_text(hjust=0.5,face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"))

#Lolipop chart
#Creating youngest df
youngest<- top_n(final_nobel,-20,age_get_prize)
youngest <- arrange(youngest,(age_get_prize))

#Plotting the Youngest to win the award
ggplot(youngest,aes(x=reorder(full_name,(age_get_prize)),
                    y=age_get_prize, colour=gender))+
  geom_point(stat="identity")+
  geom_segment(aes(xend=full_name,yend=0),size=1.2)+
  scale_colour_viridis_d()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x="Nobel Laureates",y="Age when they received prize",
       title = "Youngest Nobel prize winners")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"),
        legend.title = element_text(face = "bold"))


#Creating oldest df
oldest <- top_n(final_nobel,20,age_get_prize)
oldest <- arrange(oldest,desc(age_get_prize))

#Plotting the oldest to win the award
ggplot(oldest,aes(x=reorder(full_name,desc(age_get_prize)),
                  y=age_get_prize, colour=gender))+
  geom_point(stat="identity")+
  geom_segment(aes(xend=full_name,yend=0),size=1.2)+
  scale_colour_viridis_d()+
  theme(axis.text.x = element_text(angle = 45,hjust = 1))+
  labs(x="Nobel Laureates",y="Age when they received prize",
       title = "Oldest Nobel prize winners")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"),
        legend.title = element_text(face = "bold"))


#Creating a df which has the count of the universites 
nobel_uni <- data.frame(final_nobel)
nobel_uni <- count(final_nobel$name_of_university)

nobel_uni<- as.data.frame(table(final_nobel$name_of_university))
nobel_uni <- nobel_uni[-c(1),]
View(nobel_uni)

#Top universities that nobel laurets studied in 
col_u <- brewer.pal(10,name = "Set3")
ggplot(top_n(nobel_uni,10,Freq),aes(x=Freq,y=reorder(Var1,Freq)))+
  geom_bar(stat = "identity",fill=col_u)+
  theme(axis.text.x = element_text(angle = 0,hjust = 1))+
  labs(x="Number of Nobel prize won",y="University name",
       title = "Top 10 Universities in which \n Nobel Laureates studied")+
  theme(plot.title = element_text(face="bold"),
        axis.title.x = element_text(face = "bold"),
        axis.title.y = element_text(face="bold"))

#Treemap
Birth_country <- as.data.frame(table(final_nobel$country_born))
View(Birth_country)

#UK and USA born Dominance in the Noble awards

ggplot(top_n(Birth_country,10,Freq),aes(area=Freq,label=Var1,fill=Freq))+
  geom_treemap(stat = "identity")+
  scale_fill_viridis_c(option = "plasma",direction = -1)+
  geom_treemap_text(place = "center",fontface="bold")+
  labs(title = "Dominance of Nobel Laureates \n from USA and UK ")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))


col_b <- brewer.pal(11,name = "Spectral")

#country code to country for born
uni_country <- final_nobel %>%
  select(country_of_university)
View(uni_country)

country_of_uni <-countrycode(uni_country$country_of_university,"country.name","iso3c") 
iso_code_df <- data.frame(country_of_uni)
iso_code_df <- setNames(iso_code_df, c("iso_code"))
iso_code_df <- mutate(
  iso_code_df,
  iso_code_fixed=ifelse(
    iso_code=="GBR",
    "UK",
    as.character(iso_code)
  )
)
data.frame(country_of_uni)
names(country_of_uni)
View(country_of_uni)

final_nobel <- cbind(nobel_main,country_born,country_died,iso_code_df,country_of_uni)
View(final_nobel)


country_count <- as.data.frame(table(final_nobel$iso_code_fixed))
View(country_count)

rest_count <- as.data.frame(table(final_nobel$country_of_university))
rest_count <- rest_count[-c(1),]
View(rest_count)




#Creating the df for plotting map
mydata <- data.frame(final_nobel)
data1 <- mutate(mydata,
                country_full=ifelse(country_of_university=="United Kingdom",
                                    "UK",
                                    as.character(country_of_university)))
View(data1)

#Creating a count df for mapping 
final_count <- as.data.frame(table(data1$country_full))
final_count <- final_count[-c(1),]
View(final_count)

#Univesities dominance in map plot
world_map<- map_data("world")
View(world_map)

ggplot(final_count)+
  geom_map(world_map,map=world_map,
           mapping=aes(long,lat,map_id=region),
           color="black",fill="lightgray",size=0.25)+
  labs(title = "Countries of the universities that have \n won the Nobel prize")+
  theme(plot.title = element_text(hjust = 0.5,face="bold"))+
  geom_map(map = world_map, aes(map_id =Var1 , fill = Freq), size = 0.25) +
  scale_fill_viridis_c(name = "Count",direction = -1) +
  expand_limits(x = world_map$long, y = world_map$lat)






colnames(rest_count)[1] <- "Country_name"
View(rest_count)

colnames(country_count)[1] <- "Country_name"
View(country_count)

#

citation("viridis")
citation("RColorBrewer")
