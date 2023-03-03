#library
library(dplyr)
library(ggplot2)
library(sothi)
library(readr)
library(pxweb)


# Daten laden
data_download <- pxweb_get_data("https://www.pxweb.bfs.admin.ch/api/v1/de/px-x-1404010100_301/px-x-1404010100_301.px","daten/px_abfrage_sozmed_institutionen.json")
data_download$Jahr <- as.numeric(data_download$Jahr)
colnames(data_download) <- c("Beobachtungseinheit","Region","Beherbergungstyp","Organisationsform","Jahr","Wert")


# Entwicklung rechtlich-wirtschaftlicher Status Anzahl Plätze

data <- data_download

#data <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12."&Beherbergungstyp=="Beherbergungstyp - Total"&Organisationsform!="Rechtlich-wirtschaftlicher Status - Total") %>%
#  filter(Region=="<< Nordwestschweiz")

data <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12."&Beherbergungstyp=="Beherbergungstyp - Total"&Organisationsform!="Rechtlich-wirtschaftlicher Status - Total") %>%
  filter(Region=="- Basel-Stadt")


cbp1 <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1 <- c("#009E73","#0072B2", "#CC79A7")

data$Organisationsform <- ordered(data$Organisationsform, levels = c("Öffentlich","Privat subventioniert","Privat"))

p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Organisationsform, colour=Organisationsform)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,1000,2000,3000),limits=c(0,3000)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1,name = "Status") +
  scale_linetype_manual(values = c("longdash","dashed","dotted"),name = "Status") +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Anzahl Plätze nach rechtlich-wirtschaftlichem Status",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_Status_Plätze_BS.png", plot = p, width = 15, height = 10, units = "cm")


# Entwicklung Beherbergungstyp Anzahl Plätze

data <- data_download

#data <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12."&Organisationsform=="Rechtlich-wirtschaftlicher Status - Total"&Beherbergungstyp!="Beherbergungstyp - Total") %>%
#  filter(Region=="<< Nordwestschweiz") %>% filter(Beherbergungstyp!="Akut- und Übergangspflege")

data <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12."&Organisationsform=="Rechtlich-wirtschaftlicher Status - Total"&Beherbergungstyp!="Beherbergungstyp - Total") %>%
  filter(Region=="- Basel-Stadt") %>% filter(Beherbergungstyp!="Akut- und Übergangspflege")


cbp1 <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1 <- c("#009E73","#0072B2", "#CC79A7","#E69F00")


p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Beherbergungstyp, colour=Beherbergungstyp)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,1000,2000,3000)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotdash","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Anzahl Plätze nach Beherbergungstyp",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_Plätze_Typ_BS.png", plot = p, width = 15, height = 10, units = "cm")

data <- data %>% filter(Beherbergungstyp!="Langzeit")

p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Beherbergungstyp, colour=Beherbergungstyp)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,100,200,300)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotdash","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Anzahl Plätze nach Beherbergungstyp ohne Langzeit",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_Plätze_Typ_ohneLangzeit_BS.png", plot = p, width = 15, height = 10, units = "cm")








# Entwicklung rechtlich-wirtschaftlicher Status Fakturierte Tage

data <- data_download

#data <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage"&Beherbergungstyp=="Beherbergungstyp - Total"&Organisationsform!="Rechtlich-wirtschaftlicher Status - Total") %>%
#  filter(Region=="<< Nordwestschweiz")

data <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage"&Beherbergungstyp=="Beherbergungstyp - Total"&Organisationsform!="Rechtlich-wirtschaftlicher Status - Total") %>%
  filter(Region=="- Basel-Stadt")


cbp1 <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1 <- c("#009E73","#0072B2", "#CC79A7")

data$Organisationsform <- ordered(data$Organisationsform, levels = c("Öffentlich","Privat subventioniert","Privat"))

p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Organisationsform, colour=Organisationsform)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,250000,500000,750000,1000000)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1,name = "Status") +
  scale_linetype_manual(values = c("longdash","dashed","dotted"),name = "Status") +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Fakturierte Tage nach rechtlich-wirtschaftlichem Status",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_FakturierteTage_Plätze_BS.png", plot = p, width = 15, height = 10, units = "cm")



# Entwicklung Beherbergungstyp Fakturierte Tage

data <- data_download

#data <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage"&Organisationsform=="Rechtlich-wirtschaftlicher Status - Total"&Beherbergungstyp!="Beherbergungstyp - Total") %>%
#  filter(Region=="<< Nordwestschweiz") %>% filter(Beherbergungstyp!="Akut- und Übergangspflege")

data <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage"&Organisationsform=="Rechtlich-wirtschaftlicher Status - Total"&Beherbergungstyp!="Beherbergungstyp - Total") %>%
  filter(Region=="- Basel-Stadt") %>% filter(Beherbergungstyp!="Akut- und Übergangspflege")


cbp1 <- c("#000000","#999999", "#E69F00", "#56B4E9", "#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7")
cbp1 <- c("#009E73","#0072B2", "#CC79A7","#E69F00")


p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Beherbergungstyp, colour=Beherbergungstyp)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,250000,500000,750000,1000000)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotdash","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Fakturierte Tage nach Beherbergungstyp",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_FakturierteTage_Typ_BS.png", plot = p, width = 15, height = 10, units = "cm")

data <- data %>% filter(Beherbergungstyp!="Langzeit")

p <- data %>% ggplot(aes(x=Jahr,y=Wert, linetype=Beherbergungstyp, colour=Beherbergungstyp)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0,5000,10000,15000,20000,25000)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotdash","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Fakturierte Tage nach Beherbergungstyp ohne Langzeit",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_FakturierteTage_Typ_ohneLangzeit_BS.png", plot = p, width = 15, height = 10, units = "cm")




# Verhältnis Fakturierte Tage vs. Anz. Beherbergungsplätze * 365

data <- data_download

cbp1 <- c("#009E73","#0072B2", "#CC79A7")

data <- data %>%
  filter(Beherbergungstyp=="Beherbergungstyp - Total") %>%
  filter(Organisationsform=="Rechtlich-wirtschaftlicher Status - Total")
data_faktu <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage") %>% select(Region,Jahr,Wert)
data_platze <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12.") %>% select(Region,Jahr,Wert)

data <- merge(x = data_faktu, y = data_platze, by = c("Region","Jahr"), all = TRUE)

data$Wert <- data$Wert.x/(data$Wert.y*365)

data <- data %>% select(Region,Jahr,Wert)

data$Region[data$Region=="<< Nordwestschweiz"] <- "Nordwestschweiz"
data$Region[data$Region=="- Basel-Stadt"] <- "Basel-Stadt"

p <- data %>%
  ggplot(aes(x=Jahr,y=Wert, linetype=Region, colour=Region)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(labels = scales::percent,breaks = c(0.85,0.9,0.95,1), limits = c(0.8,1.05)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in der Nordwestschweiz",
       subtitle = "Auslastung (Verhältnis fakturierter Tage zur Anzahl an Plätzen)",
       x = NULL,
       y = "Auslastung",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_Verhältnis_gesamt_NWS.png", plot = p, width = 15, height = 10, units = "cm")






# Verhältnis Fakturierte Tage vs. Anz. Beherbergungsplätze * 365

data <- data_download

cbp1 <- c("#009E73","#0072B2", "#CC79A7")

data <- data %>% filter(Region=="- Basel-Stadt") %>% filter(Beherbergungstyp!="Akut- und Übergangspflege")
data_faktu <- data %>% filter(Beobachtungseinheit=="Fakturierte Tage") %>% select(Beherbergungstyp,Organisationsform,Jahr,Wert)
data_platze <- data %>% filter(Beobachtungseinheit=="Anzahl Plätze per 31.12.") %>% select(Beherbergungstyp,Organisationsform,Jahr,Wert)

data <- merge(x = data_faktu, y = data_platze, by = c("Beherbergungstyp","Organisationsform","Jahr"), all = TRUE)

data$Wert <- data$Wert.x/(data$Wert.y*365)

data <- data %>% select(Beherbergungstyp,Organisationsform,Jahr,Wert.x,Wert.y,Wert)



p <- data %>%
  filter(Beherbergungstyp!="Beherbergungstyp - Total") %>%
  filter(Organisationsform!="Rechtlich-wirtschaftlicher Status - Total") %>%
  filter(Beherbergungstyp!="Langzeit") %>%
  ggplot(aes(x=Jahr,y=Wert, linetype=Beherbergungstyp, colour=Organisationsform)) +
  geom_line() +
  #geom_bar(stat="identity",position="dodge") +
  #theme_sothi() +
  theme_upk() +
  scale_y_continuous(breaks = c(0.25,0.5,0.75,1,1.25,1.5,2,2.5)) +
  scale_x_continuous(breaks=c(2006,2009,2012,2015,2018,2021)) +
  scale_fill_discrete() +
  scale_colour_manual(values=cbp1) +
  scale_linetype_manual(values = c("longdash","dashed","dotted")) +
  #scale_fill
  #ylim(0,3) +
  
  theme(
    axis.text.x = element_text(size = 7)
  )+
  labs(title = "Sozialmedizinische Institutionen in Basel-Stadt",
       subtitle = "Fakturierte Tage nach Beherbergungstyp ohne Langzeit",
       x = NULL,
       y = "Anzahl Plätze",
       caption="Daten: Bundesamt für Statistik, Angebot der sozialmedizinischen Institutionen\nAbfrage: 03. März 2023 [pxweb R package 0.16.2]")
p

ggsave(file = "plot/SozMedInsitutionen_Verhältnis_BS.png", plot = p, width = 15, height = 10, units = "cm")

