library(tidyr)
library(readr)

## Vorlage Daten to long

data <- read_csv("Data_BS_Notschlafstelle - Übernachtende Personen nach Anzahl Nächten.csv", 
            col_types = cols(Jahr = col_double()))

data <- read_delim("beistandshcaften.csv", 
           delim = ";", escape_double = FALSE, trim_ws = TRUE)

data <- data %>% pivot_longer(!Jahr,names_to = "Art", values_to = "Count")


write_csv(data, file = "daten/Beistandschaften_Erwachsene_Art.csv")



## Vorlage Plot

p <- data %>% filter(Kategorie=="Total") %>% ggplot(aes(x=Jahr,y=Count, linetype=Kategorie, colour=Kategorie)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  #scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021)) +
  scale_fill_discrete() +
  #scale_fill
  #theme(legend.position="top",
  #      legend.justification="left",
  #      legend.direction="horizontal") +
  #ylim(0,3) +
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Notwohnungen",
       subtitle = "Anz. belegte Notwohnungen",
       x = NULL,
       y = "Anzahl Patient*innen",
       caption="Data: Statistisches Amt des Kantons Basel-Stadt, Statistik zu Bestand und Belegung der Notwohnungen.")
p

ggsave(file = "plot/anzPat_Nordwestschweiz.png", plot = p, width = 15, height = 10, units = "cm")


