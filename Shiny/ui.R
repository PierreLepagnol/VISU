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
    title =paste("Service de reporting automatisé",emo::ji("bar_chart")),
    
    # Onglet : Importation / Exportation
        # Importation : Jeu de données avec séparateur
        # Exportation : Rapport sur le modèle
    tabPanel(title=paste("Importation / Exportation",emo::ji("open_file_folder")),
             sidebarLayout(
                 sidebarPanel(
                     # Importation
                     
                     downloadButton("downloadHow","Télécharger la notice d\'utilisation"),
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
    
    tabPanel(paste("Modélisation",emo::ji("hammer_and_wrench"),' / ',"Prédiction",emo::ji("1st_place_medal")),
             tabsetPanel(
                 tabPanel(title=paste("Modélisation",emo::ji("hammer_and_wrench")), 
                          splitLayout(
                              wellPanel(
                                  splitLayout(radioButtons('TypeValid', label='Choisir le type de Validation :', choiceNames = c('Train/Test Validation','CrossValidation','Bootstrap'), choiceValues = c('LCV','cv','boot')),
                                              uiOutput("ValidParams")
                                  ),
                                  splitLayout(
                                      # Selection du type de problème
                                      radioButtons('TypeMod', label='Choisir le type de problème :', choices = c('Classifcation','Régression'),selected ='Régression'),
                                      # Panneau Choix Algo
                                      radioButtons("AlgoInput", "Méthode",choices = c(''),selected=NULL)
                                  ),
                                  uiOutput("TunningParams"),
                                  actionButton("runModels", "Ajuster les modèles")
                              ),
                              wellPanel(tweaks,
                                        list(h3("Sélection des variables"),
                                             span(style="color:red", 'Uniquement Variables Numériques'),
                                             selectizeInput("TargetVar","Variable Cible", choices = NULL, options = list(placeholder = "Télécharger un jeu de données")),
                                             tags$div(align = 'left',class = 'multicol',
                                                      checkboxGroupInput('ExpVar',"Variables Explicatives", list("Télécharger un jeu de données"=NULL),inline   = FALSE)))
                              )
                          )
                          ),
                 tabPanel(paste("Prédiction",emo::ji("1st_place_medal")),
                          absolutePanel(bottom = 20, right = 20, width = 500,draggable = TRUE,style = "opacity: 0.92",
                                        wellPanel(HTML(markdownToHTML(fragment.only=TRUE, text=c("Panneau optimisation des hypers paramètres"))),
                                                  sliderInput("nTabs", "", min=3, max=20, value=5)
                                        )
                          ),
                          sidebarLayout(sidebarPanel(verbatimTextOutput('ModelText', placeholder = FALSE)),mainPanel(uiOutput('TabsPred')))
                          ),
                 id = 'TabmOd',type='tabs' )
             ),

    
    tabPanel(
        title=paste("Jeu",emo::ji("video_game")),
        absolutePanel(bottom=-5, left=15, width = '32%',fixed = TRUE,style = "opacity: 0.92;z-index: 10;",
                      wellPanel(
                          HTML('<button data-toggle="collapse" data-target="#demo"><i class="fa fa-bar-chart" ></i></button> Voir Les consignes'),
                          tags$div(id = 'demo',  class="collapse",
                          HTML(markdownToHTML(fragment.only=TRUE, file='JeuConsigne.md'))
                                ))
        ),
        sidebarLayout(sidebarPanel(h4('Nouvel individu'),actionButton("btn_game", "Nouvel Individus"),dataTableOutput('NewIndiv'),
                                   numericInput('user_gess', 'Ma prédiction', value=0),actionButton("btn_predict", "Soumettre ma prédiction")),
                      mainPanel(
                          absolutePanel(top=80, right=20, width = '32%',fixed = TRUE,style =" color:#fff;z-index: 10;",
                                        wellPanel(style = "background: #27ae60;opacity:0.80;",textOutput("predicted"))
                          ),
                          leafletOutput("mymap",height = '100vh')))
    )
 )
)

