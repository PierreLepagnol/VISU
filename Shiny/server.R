library(shiny)

library(DT)
library(leaflet)
library(rlang)
library(tidyverse)
library(caret)

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
      req(input$TypeMod)
      if(input$TypeMod=='Régression'){
        # Listes des Méthodes / Algo : Regression
        return(list('LM','GLM'))
      }
      if(input$TypeMod=='Classifcation'){
        # Listes des Méthodes / Algo : Classifcation
        return(list('lda (Linear Discriminant Analysis)','qda (Quadratic Discriminant Analysis)','knn (K-Nearest Neighbors)','rpart (Decision Tree)','rf (RandomForest)'))
      }
    })
    
    Valid_Method=reactive({
      req(input$TypeValid)
      print(input$TypeValid)
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
    
    
    Train_n_Test_model=function(model_str,MethodValidation){
      model_name=str_split(model_str, ' ', n = Inf, simplify = FALSE)[[1]][1]
      
      ctrl1 <- trainControl(method="cv",number=10,index=list(indapp))
      KK <- data.frame(k=k_cand)
      ee1 <- train(Y~.,data=donnees,method="knn",trControl=ctrl1,tuneGrid=KK)
      ee1
       return(model_name)
    }
    
    
    # Met à jour le nombre de tab panel
    output$TabsPred = renderUI({
      Valid_Method()
      
      
      Algo_Result=map(input$AlgoInput,~Train_n_Test_model(.x,input$TypeValid))
      nTabs = length(input$AlgoInput)
      
      myTabs = lapply(1:nTabs, function(x){
          tabPanel(title=input$AlgoInput[x],Algo_name[x]
                   )
        });
      do.call(tabsetPanel, myTabs)
    })
    
    output$mymap <- renderLeaflet({
      Paris <- geocodeGratuit("paris")
      data=datasetInput()
      m=leaflet(data) %>% setView(lng = Paris[1], lat = Paris[2], zoom = 12) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))
      m %>% addMarkers(lng = ~lon, lat = ~lat,popup ='Veuillez construire un modèle.', label ='Veuillez construire un modèle.' )
    })
    
    # Observe le changement de variable : TargetVar
    
    observeEvent(input$TargetVar, {
      leafletProxy(mapId = "mymap",session = session,data = datarefactored_poopup()) %>%clearMarkers() %>%addMarkers(lng = ~lon, lat = ~lat, popup=~poopup,label =~get(input$TargetVar))
      }
    )
    
    # Observe le changement de variable : ExpVar
    observeEvent(input$ExpVar, {
      leafletProxy(mapId = "mymap",session = session,data = datarefactored_poopup()) %>%clearMarkers() %>%addMarkers(lng = ~lon, lat = ~lat, popup=~poopup,label =~get(input$TargetVar))
    }
    )
    
    datarefactored_poopup=reactive({
      req(input$ExpVar,input$TargetVar)
      vect_name=unlist(input$ExpVar)
      dataset=Poopup_refactor(datasetInput(),vect_name,input$TargetVar)
    })
    
    Poopup_refactor=function(dataset_raw,vect_name,targetvar){
      # Selection des variables pertinantes
      dataset2=dataset_raw %>% select(vect_name)
      # Ajout du nom de la variable pour chaque ligne
      dataset2[] <- Map(paste, names(dataset2), dataset2, sep = ' : ')
      # création du popup
      dataset_raw$poopup <- apply( dataset2[ , vect_name] , 1 , FUN= paste,collapse = "<br/>" )
      # Selection des variables utiles à leaflet
      newDs=dataset_raw %>% select(lon,lat,poopup,targetvar)
      return(newDs)
    }
    
    
    
    
    
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
