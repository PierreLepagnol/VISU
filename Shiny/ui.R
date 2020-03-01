# Création de différents modèles à inclure dans le rapport
library(shiny)
library(DT)
library(markdown)
library(leaflet)

# tweaks, un objet de liste pour configurer les multicols pour la checkboxGroupInput
tweaks <- list(tags$head(tags$style(HTML(".multicol { height: auto;
                                                    -webkit-column-count: 3; /* Chrome, Safari, Opera */ 
                                                    -moz-column-count: 3;    /* Firefox */ 
                                                    column-count: 3; 
                                                    -moz-column-fill: auto;
                                                    -column-fill: auto;}"))))

shinyUI(
    navbarPage(
    
    # Titre de l'application 
    title ="Service de reporting automatisé",
    
    # Onglet : Importation / Exportation
        # Importation : Jeu de données avec séparateur
        # Exportation : Rapport sur le modèle
    tabPanel(title="Importation / Exportation",
             sidebarLayout(
                 sidebarPanel(
                     # Importation
                     actionButton("button", "Show"),
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
    
    # Onglet : Modélisation
        # Création de différents modèles à inclure dans le rapport
    
    tabPanel("Modélisation",
             splitLayout(
                wellPanel(
                    splitLayout(radioButtons('TypeValid', label='Choisir le type de Validation :', choiceNames = c('Train/Test Validation','CrossValidation','Bootstrap'), choiceValues = c('LCV','cv','boot')),
                                sliderInput('SliderFold', label='Nombre de blocs :',min=1,max=80,value = 20,)
                        ),
                        splitLayout(
                            # Selection du type de problème
                        radioButtons('TypeMod', label='Choisir le type de problème :', choices = c('Classifcation','Régression')),
                            # Panneau Choix Algo
                        checkboxGroupInput("AlgoInput", "Méthode",list())                        
                     )),
                    wellPanel(tweaks,
                              list(h3("Sélection des variables"),
                                   selectizeInput("TargetVar","Variable Cible", choices = NULL, options = list(placeholder = "Télécharger un jeu de données")),
                                   tags$div(align = 'left',class = 'multicol',
                                            checkboxGroupInput('ExpVar',"Variables Explicatives", list("Télécharger un jeu de données"=NULL),inline   = FALSE)))
                              )
                )
             ),
    
    tabPanel(
        title="Prédiction",
        absolutePanel(bottom = 20, right = 20, width = 500,draggable = TRUE,style = "opacity: 0.92",
                      wellPanel(HTML(markdownToHTML(fragment.only=TRUE, text=c("Panneau optimisation des hypers paramètres"))),
                                sliderInput("nTabs", "", min=3, max=20, value=5)
                                )
                      ),
        sidebarLayout(sidebarPanel('Panneau pour créer un nouvel individu'),mainPanel('Prédictions selon le modèle créer','Carte de Paris avec la création',uiOutput('TabsPred')))
        ),
    
    tabPanel(
        title="Jeu",
        absolutePanel(bottom=-5, left=15, width = '32%',fixed = TRUE,style = "opacity: 0.92;z-index: 10;",
                      wellPanel(
                          HTML('<button data-toggle="collapse" data-target="#demo"><i class="fa fa-bar-chart" ></i></button> Voir Les consignes'),
                          tags$div(id = 'demo',  class="collapse",
                          HTML(markdownToHTML(fragment.only=TRUE, file='JeuConsigne.md'))
                                ))
        ),
        sidebarLayout(sidebarPanel('Panneau pour créer un nouvel individu', actionButton("btn_game", "Nouvel Individus")),
                      mainPanel(
                          absolutePanel(top=80, right=20, width = '32%',fixed = TRUE,style =" color:#fff;z-index: 10;",
                                        wellPanel(style = "background: #27ae60;opacity:0.80;",'hello')
                          ),
                          leafletOutput("mymap",height = '100vh')))
    )
 )
)

