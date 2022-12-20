library(tidyverse)
library(dplyr)
library(janitor)
library(skimr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(colorspace)
library(here)

library(readr)
Companies_Stock_History <- read_csv("C:/Users/pc/Desktop/my project raw file/Stock-History/Companies_Stock_History.csv")
View(Companies_Stock_History)

Companies_Stock_History$date <- mdy(Companies_Stock_History$Date)
Companies_Stock_History$Month <- format(as.Date(Companies_Stock_History$date), "%m")
Companies_Stock_History$Day <- format(as.Date(Companies_Stock_History$date), "%d")
Companies_Stock_History$Year <- format(as.Date(Companies_Stock_History$date), "%Y")

Companies_yearly_stock <- Companies_Stock_History %>% 
  group_by(Year, Company) %>% 
  drop_na() %>% 
  summarise(max_Open=max(Open),max_High=max(High),max_Low=max(Low),max_Close=max(Close),max_Volume=max(Volume))
View(Companies_yearly_stock)

ggplot(data = Companies_yearly_stock)+
  geom_col(mapping = aes(x=Company, y=max_Volume, fill=Company))+
  theme(axis.text.x = element_text(angle = 45))+
  theme(axis.text.y = element_text(angle = 40))+
  facet_wrap(~Year)


