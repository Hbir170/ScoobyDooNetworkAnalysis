#JULY TIDY TUESDAY CHALLENGE
# Scooby Doo Dataset Data Analysis/Walkthrough

#Accessing TidyTuesday via Package
library(tidyverse)
theme_set(theme_minimal()) #changes plot theme

install.packages("tidytuesdayR")
library(tidytuesdayR)
tuesdata <- tidytuesdayR::tt_load('2021-07-13')

scooby <- tuesdata$scoobydoo #pulling scooby dataset from tuesdata
glimpse(scooby) 
#Viewing data for any interesting variables to look into and data cleaning purposes

#Grabbing data we'd like to view in new variable, also converting imdb to numeric
scooby_refined <- scooby %>% 
  select(series_name:format) %>% 
  mutate(imdb = as.double(imdb)) %>% 
  mutate(engagement = as.double(engagement))
#Above: converting imdb ratings from text (characters) to 
#numeric (doubles) so we can analyze it

#Note: Some of the shows did not have imdb ratings which may have been due to 
#it being too soon to collect them when data was stored (this will result in 
#NAs for the column)
scooby$imdb #see above

#Preliminary visualization of IMDB ratings over time to see any interesting trends/path
# Breaking it down by format to see any specific trends
scooby_refined %>% 
  ggplot(aes(x=date_aired,
             y=imdb,
             color = format)) +
  geom_point() + 
  scale_color_brewer(palette = 'Dark2')


scooby_refined

#It seems there was a gap in production somewhere in the late 90s and also TV series 
#were more dominant
# Data Cleaning: Noticed that the Format column actually has a misplaced TV Series 
#which should be segmented
# Changing "Pizza O'Possumm" and 	"The Curse of Half-Beard's Booty" to segmented
row_index <- which(scooby_refined$title == "Pizza O'Possumm's")
scooby_refined$format[row_index] <- "TV Series (segmented)"

row_index <- which(scooby_refined$title == "The Curse of Half-Beard's Booty")
scooby_refined$format[row_index] <- "TV Series (segmented)"

View(scooby_refined)

#EXPLORING DATA
# Exploring Format further
table(scooby_refined$format)
# Digging deeper we notice that TV series (regular and segmented) make up most of the 
#data points, so we can focus on them
# TV series (segmented + non-segmented) make up 550 of the 604 Total Series

# Exploring Crossover Format for further details on what it entails
Crossover <- filter(scooby_refined, format == 'Crossover')
View(Crossover)
# We now see crossover mainly was used to have Scooby-Doo show up in other shows 
#to possibly boost ratings


# Combining together the segmented episodes for purposes of analyzing their collective
# IMDB rating without double counting each full episode as two (due to them being segmented)

segmented <- scooby_refined %>% 
  filter(format=='TV Series (segmented)') %>% 
  group_by(date_aired) %>% 
  summarize(series_name = unique(series_name),
            network = unique(network),
            total_runtime = sum(run_time),
            imdb = mean(imdb),
            engagement = mean(engagement))
View(segmented)



# Filtering all non-segmented TV Shows, grabbing all the same columns as above so we
# can join the two for further analysis
non_segmented <- scooby_refined %>% 
  filter(format == 'TV Series') %>% 
  select(date_aired,
         series_name,
         network,
         total_runtime = run_time,
         imdb,
         engagement)

scooby_tidied <- rbind(non_segmented,
                       segmented)
View(scooby_tidied)

#Creating a GG Plot with Tidied Scooby Doo dataset, focusing on network instead
scooby_tidied %>% 
  ggplot(aes(x=date_aired,
             y=imdb,
             col = network))+
  geom_point()+
  scale_color_brewer(palette="Dark2")
#This now shows us which network is performing least on TV (The CW) which 
# we can now dig deeper into

CW <- scooby_tidied %>% 
  filter(network == "The CW")
View(CW)
#Some exploratory analysis shows they ran their own show called Scooby Doo Get a Clue
#Which focused less on mystery episodes and more on the day to day life of Shaggy/Scooby
#This could explain why the ratings were not as high as other shows/networks

#Overall we are seeing that the network gives us a good indication of how
#a show was being rated with The CW having the worst ratings

#Determining correlation between Network and IMDB ratings
#Is CW truly the least liked network among the rest or is it just a case of
#rare coincidence that it gets rated the least? (ANOVA)

model <- aov(imdb ~ network,
             data = scooby_tidied)
summary(model) #prints anova table

#we notice the P-Value is statistically significant at <2e-16 which indicates
#there is evidence to deny that this is a rare occurrence of data thus we
#have some evidence to reject the null (there is no correlation bw Network and IMDB)

#Digging further into this rejection we can look into a Post-Hoc test (Tukey)
#we need to do this to verify whether there is a significant difference 
#between CW and the rest of the networks ratings as part of our initial inquiry
TukeyHSD(model)
#Looking at the results from the pairwise P-Values we notice that there is
#evidence to state that Cws show performs the worst among other networks

#Bar Chart Visualization of Average Show Rating per Network

scooby_tidied %>% 
  group_by(network) %>% 
  summarize(mean_imdb = mean(imdb,na.rm=TRUE)) %>% 
  ggplot(aes(x=fct_reorder(network,-mean_imdb),
             y=mean_imdb,
             fill = network)) + 
  geom_col() +
  geom_text(aes(label=round(mean_imdb,1)),
                nudge_y = -0.3)+
  scale_fill_brewer(palette="Dark2")+
  labs(x="Network",
       y="Mean IMDB Ratings",
       title="Scooby Doo Ratings by Network")+
  theme(legend.position = "None",
        axis.text.x = element_text(angle=15))
#Geom Col used as we specify x and y variables but this will show a bar chart
#Fct_reorder is factor reorder and reorders the X Axis based on specified value (mean_imdb)
#geom_text used to create labels for each network and nudge them to the right spot
