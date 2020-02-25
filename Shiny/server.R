library(shiny)
library(readr)
library(DT)
library(leaflet)

geocodeGratuit <- function(adresses){
  # adresses est un vecteur contenant toutes les adresses sous forme de chaine de caracteres
  nominatim_osm <- function(address = NULL){
    ## details: http://wiki.openstreetmap.org/wiki/Nominatim
    ## fonction nominatim_osm proposée par D.Kisler
    if(suppressWarnings(is.null(address)))  return(data.frame())
    tryCatch(
      d <- jsonlite::fromJSON(
        gsub('\\@addr\\@', gsub('\\s+', '\\%20', address),
             'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
      ), error = function(c) return(data.frame())
    )
    if(length(d) == 0) return(data.frame())
    return(c(as.numeric(d$lon), as.numeric(d$lat)))
  }
  tableau <- t(sapply(adresses,nominatim_osm))
  colnames(tableau) <- c("lon","lat")
  return(tableau)
}


# Fonction Serveur 
shinyServer(function(session,input, output) {
    # Upload du Dataset 
    datasetInput <- reactive({
        req(input$file1,input$separator) # Nécessite les 2 Variables : file1 & separator 
        tryCatch({data=read_delim(input$file1$datapath,delim=input$separator)},
                  error = function(e) {# return a safeError if a parsing error occurs
                        stop(safeError(e))})
        })
    
    Algo_List= reactive({
      req(input$TypeMod) # Nécessite les 1 Variable : TypeMod 
      if(input$TypeMod=='Régression'){
        # Listes des Méthodes / Algo : Regression
        return(list('LM','GLM','Algo_3'))
      }
      if(input$TypeMod=='Classifcation'){
        # Listes des Méthodes / Algo : Classifcation
        return(list('GLM','Algo_3','dfghjk'))
      }
    })
    
    
    # Impression du Dataset
    output$mydataset = renderDataTable(
        DT::datatable(datasetInput(),options = list(width = 30,scrollX = TRUE,scrollY = TRUE,searching=FALSE) )
    )
    
    
   # Observation de la Variable datasetInput()
    observe({
      vchoices <- names(datasetInput())
      updateSelectInput(session, "TargetVar", choices = vchoices)
      updateCheckboxGroupInput(session, "ExpVar", choices = vchoices)
    })
    # Observation de la Variable Algo_List()
    observe({
      AlgoChoices <- Algo_List()
      updateCheckboxGroupInput(session, "AlgoInput", choices = AlgoChoices)
    })
    
    
    
    TestMethod=Algo_List= reactive({
      req(input$TypeMod)
      if(input$TypeMod=='Régression'){
        # Listes des Méthodes / Algo : Regression
        return(list('LM','GLM'))
      }
      if(input$TypeMod=='Classifcation'){
        # Listes des Méthodes / Algo : Classifcation
        return(list('lda','qda','knn','rpart','RandomForest'))
      }
    })
    
    # Mett a jour le nombre de tab panel
    output$TabsPred = renderUI({
      
      nTabs = length(input$AlgoInput)
      myTabs = lapply(1:nTabs, function(x){
          tabPanel(title=input$AlgoInput[x], input$AlgoInput[x] 
                   )
        });
      do.call(tabsetPanel, myTabs)
      
    })
    stringTargetVar=reactiveValues()
    output$mymap <- renderLeaflet({
      Paris <- geocodeGratuit("paris")
      data=datasetInput()
      m=leaflet(data) %>% setView(lng = Paris[1], lat = Paris[2], zoom = 12) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))
      
      m %>% addMarkers(lng = ~lon, lat = ~lat,popup ='dfgh', label =~input$TargetVar )
    })
    
  #  TargetVarGAME=observe(input$TargetVar,input$TargetVar)
    
      # Fonction de téléchargement du rapport
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste(input$titleOutput, sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },

      content =function(file) {
        
        withProgress(message = 'Calculation in progress', detail = 'This may take a while...', value = 0, {
        params <- list(title = input$titleOutput, 
                       mesdata = datasetInput(),
                       mesmodeles=c('KNN','LM'),
                       PCA=FALSE
                       )
        src <- normalizePath('report.Rmd')
        incProgress(1/10)
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        library(rmarkdown)
        
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ),
        incProgress(2/10,message = 'Création du Rapport', detail = 'knitr')
        )
        
        file.rename(out, file)
      })
      }
    )
    
  
})
