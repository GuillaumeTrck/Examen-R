setwd("./")

# Load the packages
library(DBI)
library(tidyverse)
library(magrittr)
library(dplyr)
library(shiny)
library(ggplot2)
library(readxl)
library(RColorBrewer)
notes<-read_excel("note.xlsx")%>%as.data.frame()
notes%>%mutate(success=ifelse((`Crédits acquis Année`==`Crédits PAE Année`),"Reussit",
                              ifelse((`Crédits acquis Année`>=45 &`Crédits acquis Année`<`Crédits PAE Année`),"Semi-reussit",'Raté')
                              ))%>%
mutate(success=factor(success,levels=c("Reussit","Semi-reussit","Raté")))%>%
ggplot(aes(x=Inscription,y=`Crédits acquis Année`,fill=success,)) + geom_bar(stat = "identity",position="fill")+
  scale_fill_manual(values=c("green","blue","red"))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
