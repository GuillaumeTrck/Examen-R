library(DBI)
library(tidyverse)
library(magrittr)
library(dplyr)
library(shiny)
library(ggplot2)
library(tidylog)
library(readxl)
library(reshape2)
library(patchwork)
library(corrplot)
library(DT)

notes<-read_excel("note.xlsx")%>%as.data.frame()

#Code de la page web
ui <- fluidPage(
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Sélectionner le fichier Excel", accept = c(".xlsx"))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Taux de réussite", 
                 plotOutput("tendance")),
        
        tabPanel("Tendance progress", 
                 plotOutput("tend")),
        
        tabPanel("Différence de performance", 
                 h3("Contenu de l'onglet Différence de performance"))
      )
    )
  )
)

#Code serveur 
server <- function(input, output) {
  output$tendance<-renderPlot({
    notes%>%mutate(success=ifelse((`Crédits acquis Année`==`Crédits PAE Année`),"Reussit",
                                  ifelse((`Crédits acquis Année`>=45 &`Crédits acquis Année`<`Crédits PAE Année`),"Semi-reussit",'Raté')
    ))%>%
      mutate(success=factor(success,levels=c("Reussit","Semi-reussit","Raté")))%>%
      ggplot(aes(x=Inscription,y=`Crédits acquis Année`,fill=success,)) + geom_bar(stat = "identity",position="fill")+
      scale_fill_manual(values=c("green","blue","red"))+ theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  })
  output$tend<-renderPlot({
    moyenne_annee<-notes$`Moyenne pondérée (/20) Année`*notes$`Crédits PAE Année`
    moyenne_janvier<-notes$`Moyenne pondérée (/20)`*notes$`Crédits PAE Janvier`
    credits_juin<-notes$`Crédits PAE Année`-notes$`Crédits PAE Janvier`
    
    notes_progression <- notes %>%
      mutate(moyenne_janvier = `Moyenne pondérée (/20)` * `Crédits PAE Janvier`) %>% 
      mutate(moyenne_juin = (moyenne_annee - moyenne_janvier)/credits_juin) %>% 
      filter(moyenne_juin >= 0)
    
    donnees_janvier <- data.frame(Note =notes_progression$`Moyenne pondérée (/20)` , Mois = 'Janvier')
    donnees_juin <- data.frame(Note = notes_progression$moyenne_juin, Mois = 'Juin')
    donnees <- rbind(donnees_janvier, donnees_juin)
    
    
    
    ggplot(donnees, aes(x = Note, fill = Mois)) + 
      geom_density(alpha = 0.7) + 
      scale_fill_manual(values = c("Janvier" = "red", "Juin" = "blue")) + 
      theme_minimal() + 
      labs(title = "Comparaison des Distributions des Notes en Janvier et en Juin",
           x = "Note", 
           y = "Densité")
  })
}

shinyApp(ui = ui, server = server)
