#library
library(dplyr)
library(ggplot2)
library(sothi)
library(readr)

# Notwohnungen

data <- read_csv("daten/Notwohnungen_Belegung.csv")

p <- data %>% filter(Kategorie=="Total") %>% ggplot(aes(x=Jahr,y=Count, linetype=Kategorie, colour=Kategorie)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200), limits=c(0,200)) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_fill_discrete() +
  #scale_fill
  theme(legend.position="none") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Notwohnungen in Basel-Stadt",
       subtitle = "Anzahl belegte Notwohnungen",
       x = NULL,
       y = "Anzahl belegte Notwohnungen",
       caption="Daten: Statistisches Amt Basel-Stadt, Bestand und Belegung der Notwohnungen")
p

ggsave(file = "plot/BelegungNotwohnungen.png", plot = p, width = 15, height = 10, units = "cm")


# Notschlafstelle

data <- read_csv("daten/Notschlafstelle_Uebernachtungen.csv")

data <- data %>% 
  group_by(Geschlecht) %>% 
  summarise(entwicklung = Count/Count[Jahr==2009], Jahr = Jahr)

data$Geschlecht <- ordered(data$Geschlecht,levels = c("Total","Frauen","Männer"))

p <- data %>% ggplot(aes(x=Jahr,y=entwicklung, linetype=Geschlecht, colour=Geschlecht)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(labels = scales::percent, breaks=c(0,0.5,1,1.5,2,2.5)) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_fill_discrete() +
  scale_fill_manual(values=cbPalette) +
  #scale_fill
  #theme(legend.position="none") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Notwohnungen in Basel-Stadt",
       subtitle = "Anzahl belegte Notwohnungen",
       x = NULL,
       y = "Anzahl belegte Notwohnungen",
       caption="Daten: Statistisches Amt Basel-Stadt, Bestand und Belegung der Notwohnungen")
p

ggsave(file = "plot/BelegungNotwohnungen.png", plot = p, width = 15, height = 10, units = "cm")



