    # Création de différents modèles à inclure dans le rapport

library(shiny)
library(DT)
library(markdown)

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
                     textInput('separator','Séparateur',value=",",placeholder = "Par défaut : virgule"),
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
    # 
    # Onglet : Modélisation
        # Création de différents modèles à inclure dans le rapport
        # Exportation : Rapport sur le modèle
    
    tabPanel("Modélisation",
             splitLayout(
                wellPanel(
                     # Selection du type de problème
                     radioButtons('TypeMod', 'Choisir le type de problème :', choices = c('Classifcation','Régression'),selected='Régression'),
            
                     # Panneau pour la Regression
                     conditionalPanel(condition = "input.TypeMod == 'Régression'",
                                      selectInput("smoothMethod", "Méthode",list("lm", "glm", "gam", "loess", "rlm"))
                                      ),
                     # Panneau pour la Classification
                     conditionalPanel(condition = "input.TypeMod == 'Classifcation'",
                                      textInput("Formula", "Formule",placeholder = 'Y.~X1+...+Xn'),
                                      selectInput("Algo", "Méfrthode",list("lm", "glm", "gam", "loess", "rlm"))
                                      )
                     ),
                wellPanel(
                    splitLayout(
                         radioButtons("column1","select columns",choices = "",inline = T),
                         checkboxGroupInput("checkGroup", label = h3("Checkbox group"), 
                                            choices = list("All" = '.', "Choice 2" = 2, "Choice 3" = 3),
                                            selected = 1)
                         )
                     ),
             
                    absolutePanel(bottom = 20, right = 20, width = 500,draggable = TRUE,style = "opacity: 0.92",
                                    wellPanel(HTML(markdownToHTML(fragment.only=TRUE, text=c("Panneau optimisation des hypers paramètres"))),
                                              sliderInput("n", "", min=3, max=20, value=5)
                                    )
                )
             )
    ),
    tabPanel("Prédiction",)
))
