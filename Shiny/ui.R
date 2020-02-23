    # Création de différents modèles à inclure dans le rapport

library(shiny)
library(DT)

shinyUI(navbarPage(
    # Titre de l'application 
    "Service de reporting automatisé",
    
    # Onglet : Importation / Exportation
    # Importation : Jeu de données avec séparateur
    # Exportation : Rapport sur le modèle
    
    tabPanel("Importation / Exportation",
             sidebarLayout(
                 sidebarPanel(
                     tags$h2("Importation"),
                     fileInput("file1", "Choisir un fichier",
                               multiple = FALSE,
                               accept = c("text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                     textInput('separator','Séparateur'),
                     tags$h2("Exportation"),
                     textInput('titleOutput','Titre du rapport'),
                     radioButtons('format', 'Format du document', c('PDF', 'HTML', 'Word')),
                     downloadButton('downloadReport')
                 ),
                 # Show a plot of the generated distribution
                 mainPanel(
                    dataTableOutput("mydataset")
                 )
             )
    ), 
    # Création de différents modèles à inclure dans le rapport
    tabPanel("Modélisation",
             sidebarLayout(
                 sidebarPanel(
                     # Selection du type de problème
                     radioButtons('TypeMod', 'Choisir le type de problème :', choices = c('Classifcation','Régression'),selected='Régression'),
                     
                     # Panneau pour la Regression
                     conditionalPanel(
                         condition = "input.TypeMod == 'Régression'",
                         selectInput("smoothMethod", "Méthode",
                                     list("lm", "glm", "gam", "loess", "rlm"))
                         ),
                     # Panneau pour la Classification
                     conditionalPanel(
                         condition = "input.TypeMod == 'Régression'",
                         selectInput("smoothMethod", "Méthode",
                                     list("lm", "glm", "gam", "loess", "rlm"))
                     )
                     ),
                 mainPanel(
                 )
             )
    ),
    tabPanel("Prédiction", 
             sidebarLayout(
                 sidebarPanel(),mainPanel()
             )
    )
))
