library(readr)
library(data.table)
library(ggplot2)
library(dplyr)
library(mosaic)
library(magrittr)
library(wordcloud) #  wordcloud
library(DT)       # table format display of data
library(leaflet) # maps
library(highcharter)
library(igraph) #  graphs
library(plotly)
library(stringr)



rm(list=ls())


loans <- read.csv("kiva_loans.csv")
location <- read.csv("kiva_mpi_region_locations.csv")
loan_theme <- read.csv("loan_theme_ids.csv")
loan_themes_region <- read.csv("loan_themes_by_region.csv")

countries_and_continents <- read.csv("countries and continents.csv")
Countries_Longitude_and_Latitude <- read.csv("Countries Longitude and Latitude.csv")

#Country specific Grants 


loans %>%
  group_by(country) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  ungroup() %>% 
  mutate(country = reorder(country,Count)) %>%
  head(10) %>%
  
  ggplot(aes(x = country,y = Count)) +
  geom_bar(stat='identity',colour="white",fill = " dark green") +
  geom_text(aes(x = country, y = 1, label = paste0("(",Count,")",sep="")),
            hjust=0, vjust=.5, size = 4, colour = 'black',
            fontface = 'bold') +
  labs(x = 'country', 
       y = 'Count' 
  ) +
  
  theme_light()

#Sector wise distribution

table_loan_sector <- table(loans$sector )
names_loan_sector <- row.names(table_loan_sector)

m = list(
  l = 200,
  r = 40,
  b = 100,
  t = 50,
  pad = 10
) 

data <- data.frame(names_loan_sector, table_loan_sector)

p <- plot_ly(data, x = ~names_loan_sector, y = ~table_loan_sector, type = 'bar',
             marker = list(color = 'rgb(158,202,225)',
                           line = list(color = 'rgb(108,48,107)',
                                       width = 1.5))) %>%
  layout(title = "Sector  Analysis",
         xaxis = list(title = ""),
         yaxis = list(title = ""))
p %>% layout(autosize = F, margin = m)


#basic plots

par(mfrow=c(1,3))
hist(loans$funded_amount, col="green")
hist(loans$loan_amount, col="red")
hist(loans$term_in_months, col="blue")

#gender based analysis

loans$borr <- if_else((loans$borrower_genders == "female"), "female","male")
k<- data.frame(table(loans$country[loans$borr == "female"]))

k<-as.data.frame(table(loans$borr))

p <- plot_ly(k, labels = ~Var1, values = ~Freq, type = 'pie') %>%
  layout(
    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
p

# deeper gender based analysis

library(dplyr)
x<- data.frame(table(loans$country[loans$borr == "female"]) )
k<- data.frame(table(loans$country))
innerjoin_RM <- merge(x,k,by = "Var1")


names_RM <- innerjoin_RM$Var1

text <- round( (innerjoin_RM$Freq.x/ innerjoin_RM$Freq.y)*100,digits = 2)
text <- paste(text, "%Female" )

data <- data.frame(names_RM, innerjoin_RM$Freq.x, innerjoin_RM$Freq.y - innerjoin_RM$Freq.x)

p <- plot_ly(data, x = ~names_RM, y = ~innerjoin_RM$Freq.x, type = 'bar', text = text ,name = 'Female') %>%
  add_trace(y = ~innerjoin_RM$Freq.y - innerjoin_RM$Freq.x, name = 'Male') %>%
  layout(margin = list(b = 160),yaxis = list(title = 'Count'), barmode = 'stack')
p

# repayment wrt to Countries

p <- (loans %>% count(country, repayment_interval) %>%
        plot_ly(x = ~country, y = ~n, color = ~repayment_interval))
p

#Boxplot of loan amounts in sectors

loans %>% filter(currency == "USD") %>% filter(loan_amount < quantile(loan_amount, 0.99)) %>% select(sector, currency, loan_amount) -> loan_filtered

ggplot(loan_filtered, aes(x=sector, y=loan_amount, col=sector)) + 
  geom_boxplot() +
  theme_bw() + coord_flip() + labs(x="Sector", y="Amount of  loans")


