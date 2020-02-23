# Création de différents modèles à inclure dans le rapport
library(shiny)
library(DT)
library(markdown)


# tweaks, un objet de liste pour configurer les multicols pour la checkboxGroupInput
tweaks <- 
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: auto;
                                   -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 3;    /* Firefox */ 
                                   column-count: 3; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    ))

# les valeurs à afficher ou à ne pas afficher, il s'agira des valeurs "choices' et 'selected' our la case à checkboxGroupInput
controls <-
     



shinyUI(navbarPage(
    # Titre de l'application 
    "Service de reporting automatisé",
    
    # Onglet : Importation / Exportation
        # Importation : Jeu de données avec séparateur
        # Exportation : Rapport sur le modèle
    
    tabPanel("Importation / Exportation",
             sidebarLayout(
                 sidebarPanel(
                     # Importation
                     tags$h2("Importation"),
                     fileInput("file1", "Choisir un fichier",multiple = FALSE,accept = c("text/csv","text/comma-separated-values,text/plain",".csv"),buttonLabel = "Chercher un dataset", placeholder = "Aucun fichier sélectionné"),
                     textInput('separator','Séparateur',value=",",placeholder = "Par défaut : virgule"),
                     
                     # Exportation
                     tags$h2("Exportation"),
                     textInput('titleOutput','Titre du rapport'),
                     radioButtons('format', 'Format du document', c('PDF', 'HTML', 'Word')),
                     downloadButton('downloadReport')
                 ),
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
                     radioButtons('TypeMod', label='Choisir le type de problème :', choices = c('Classifcation','Régression'),selected='Régression'),
            
                     # Panneau pour la Regression
                     conditionalPanel(condition = "input.TypeMod == 'Régression'",
                                      selectInput("smoothMethod", "Méthode",list("lm", "glm", "gam", "loess", "rlm"))
                                      ),
                     # Panneau pour la Classification
                     conditionalPanel(condition = "input.TypeMod == 'Classifcation'",
                                      textInput("Formula", "Formule", placeholder = 'Y.~X1+...+Xn'),
                                      selectInput("Algo", "Méfrthode",list("lm", "glm", "gam", "loess", "rlm"))
                                      )
                     ),
                    wellPanel(tweaks,
                              list(h3("Sélection des variables"),
                                   selectInput("TargetVar","Variable Cible",list("Télécharger un jeu de données")),
                                   tags$div(align = 'left',class = 'multicol',
                                            checkboxGroupInput('ExpVar',"Variables Explicatives", list("Télécharger un jeu de données"=NULL),inline   = FALSE))),
                               )
                )
             ),
    tabPanel("Prédiction",
             absolutePanel(bottom = 20, right = 20, width = 500,draggable = TRUE,style = "opacity: 0.92",
                           wellPanel(HTML(markdownToHTML(fragment.only=TRUE, text=c("Panneau optimistion des hypers paramètres"))),
                                     sliderInput("n", "", min=3, max=20, value=5)
                                    )
    ),
    sidebarLayout(sidebarPanel('Panneau pour créer un nouvel individu'),mainPanel('Prédictions selon le modèle créer','Carte de Paris avec la création')))
))

