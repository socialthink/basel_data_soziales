#library
library(dplyr)
library(ggplot2)
library(sothi)
library(readr)

# Notwohnungen

data <- read_csv("daten/Notwohnungen_Belegung.csv")
cbp1 <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1 <- c("#f4634c","#009E73","#0072B2")


p <- data %>% filter(Kategorie=="Total") %>% ggplot(aes(x=Jahr,y=Count, linetype=Kategorie, colour=Kategorie)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200), limits=c(0,200)) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
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

ggsave(file = "plot/Belegung_Notwohnungen.png", plot = p, width = 15, height = 10, units = "cm")


# Notschlafstelle

data <- read_csv("daten/Notschlafstelle_Uebernachtungen.csv")
cbp1 <- c("#f4634c","#009E73","#0072B2")


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
  scale_colour_manual(values=cbp1) +
  scale_fill_manual(values=cbp1) +
  #scale_fill
  #theme(legend.position="none") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Notschlafstelle in Basel-Stadt",
       subtitle = "Entwicklung der Übernachtungen seit 2009",
       x = NULL,
       y = "Übernachtungen [in %]",
       caption="Daten: Statistisches Amt Basel-Stadt, Angebot und Nutzung der Notschlafstelle")
p

ggsave(file = "plot/Entwicklung_Notschlafstelle.png", plot = p, width = 15, height = 10, units = "cm")


# Beistandschaften Alter

data <- read_csv("daten/Beistandschaften_Alter.csv")
cbp1 <- c("#f4634c","#009E73","#56B4E9","#0072B2","#CC79A7")


data <- data %>% 
  group_by(Alter) %>% 
  summarise(entwicklung = Count/Count[Jahr==2012], Jahr = Jahr)

data <- data %>% filter(Alter!="Unbekannt")

data$Alter <- ordered(data$Alter,levels = c("Total","0 bis 17 Jahre","18 bis 30 Jahre","31 bis 65 Jahre","> 65 Jahre"))

p <- data %>% ggplot(aes(x=Jahr,y=entwicklung, linetype=Alter, colour=Alter)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_colour_manual(values=cbp1) +
  scale_fill_manual(values=cbp1) +
  #scale_fill
  #theme(legend.position="none") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Beistandschaften in Basel-Stadt",
       subtitle = "Alter der Bebeiständeten seit 2012",
       x = NULL,
       y = "Beistandschaften [in %]",
       caption="Daten: Statistisches Amt Basel-Stadt, Sozialberichterstattung 2022")
p

ggsave(file = "plot/Beistandschaften_Alter.png", plot = p, width = 15, height = 10, units = "cm")


# Beistandschaften Erwachsene Art

data <- read_csv("daten/Beistandschaften_Erwachsene_Art.csv")
cbp1 <- c("#009E73","#0072B2","#CC79A7")


zwischen <- data %>% 
  filter(Art!="Massgeschneiderte Beistandschaften") %>%
  filter(Art!="Umfassende Beistandschaften") %>%
  group_by(Jahr) %>% 
  summarise(Jahr = Jahr,Art="Sonstige",Count = sum(Count))
zwischen <- unique(zwischen)

data <- data %>% filter(Art=="Massgeschneiderte Beistandschaften"|Art=="Umfassende Beistandschaften")

data <- rbind(data,zwischen)

data$Art[data$Art=="Massgeschneiderte Beistandschaften"] <- "Massgeschneiderte\nBeistandschaften"
data$Art[data$Art=="Umfassende Beistandschaften"] <- "Umfassende\nBeistandschaften"

data$Art <- ordered(data$Art,levels = c("Massgeschneiderte\nBeistandschaften","Umfassende\nBeistandschaften","Sonstige"))

p <- data %>% ggplot(aes(x=Jahr,y=Count, linetype=Art, colour=Art)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  #scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_colour_manual(values=cbp1,name = "Art der Beistandschaft") +
  scale_fill_manual(values=cbp1,name = "Art der Beistandschaft") +
  scale_linetype_manual(values = c("longdash","dashed","dotted"),name = "Art der Beistandschaft") +
  #scale_fill
  #theme(legend.position="none") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7),
    legend.text = element_text(
      margin = margin(b = 5, unit = "pt"))
  ) +
  labs(title = "Beistandschaften in Basel-Stadt",
       subtitle = "Art der Beistandschaft seit 2012",
       x = NULL,
       y = "Beistandschaften [Anzahl]",
       caption="Daten: Statistisches Amt Basel-Stadt, Sozialberichterstattung 2022")
p

ggsave(file = "plot/Beistandschaften_Erwachsene_Art.png", plot = p, width = 15, height = 10, units = "cm")






