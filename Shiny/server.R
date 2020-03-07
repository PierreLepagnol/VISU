library(shiny)
library(DT)
library(leaflet)
library(rlang)
library(tidyverse)
library(caret)

# Fonction Serveur
shinyServer(function(session, input, output) {

  ## Upload du Dataset####
  datasetInput <- reactive({
    req(input$file1, input$separator) # Nécessite les 2 Variables : file1 & separator
    tryCatch({
      data = read_delim(input$file1$datapath, delim = input$separator)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    })
  })
  
  
  
  ## Listes des algorithmes disponibles####
  Algo_List = reactive({
    req(input$TypeMod)
    if (input$TypeMod == 'Régression') {
      # Listes des Méthodes / Algo : Regression
      return(list('lm', 'glm'))
    }
    if (input$TypeMod == 'Classifcation') {
      # Listes des Méthodes / Algo : Classifcation
      return(
        list(
          'lda (Linear Discriminant Analysis)',
          'qda (Quadratic Discriminant Analysis)',
          'knn (K-Nearest Neighbors)',
          'rpart (Decision Tree)',
          'rf (RandomForest)'
        )
      )
    }
  })
  
  
  ## Listes des méthodes de validation disponibles####
  Valid_Method = reactive({
    req(input$TypeValid)
    print(input$TypeValid)
  })
  

  ##Impression du Dataset####
  output$mydataset = renderDataTable(DT::datatable(
    datasetInput(),
    options = list(
      width = 30,
      scrollX = TRUE,
      scrollY = TRUE,
      scroller = TRUE,
      searching = FALSE
    )
  ))
  
  # Gestion de la modélisation
 
  ## Observation de la Variable datasetInput ####
  observe({
    vchoices <- names(datasetInput() %>% select_if(is.numeric))
    updateSelectInput(session, "TargetVar", choices = vchoices)
    updateCheckboxGroupInput(session, "ExpVar", choices = vchoices)
  })
  ## Observation de la Variable Algo_List ####  
  observe({
    AlgoChoices <- Algo_List()
    updateRadioButtons(session, "AlgoInput", choices = AlgoChoices)
  })
  
  
  Algo_Result=reactiveValues()
  
  Validation=reactiveValues()
  
  observeEvent({input$AlgoInput},{Algo_Result$model_name= str_split(input$AlgoInput, ' ',n = Inf, simplify = FALSE)[[1]][1]}) 
  
  observeEvent({input$runModels},{
    TuneParams=getTuneParams(model_name)
    # Algo_Result$results=Train_n_Test_model()
  })
  
  ## Train_n_Test_model ####
  Train_n_Test_model = function(model_str, MethodValidation,TuneParams) {
    TuneParams
    
    ctrl1 <-trainControl(method = MethodValidation,number = 10,index = list(indapp))
    parameter_to_tune=caret::getModelInfo(model=model_name)$parameters
  
    TuneGrid_List <- data.frame(k = k_cand)
    
    ee1 <-train(Y ~ .,data = donnees,method = "knn",trControl = ctrl1,tuneGrid = TuneGrid_List)

    ee1
    return(model_name)
  }
  
  
  ## Function Renvoi les hyper-parametres du modèle ####
  getTuneParams=function(model_name){
    model_params=getModelInfo(model=model_name,regex = FALSE)[[1]]
    model_params=model_params$parameters
    print(model_params)
    return('heloo')
  }

  
  output$TunningParams <- renderUI({
    if (is.null(Algo_Result$model_name))
      return()
    
    # Depending on input$input_type, we'll generate a different
    # UI component and send it to the client.
    switch(Algo_Result$model_name,
           "lda" = sliderInput("dynamic", "Dynamic",
                                  min = 1, max = 20, value = c(10,12)),
           "knn" = list(textInput("dynamic", "Dynamic",
                              value = "starting value"),textInput("dynamic", "Dynamic",
                                                                  value = "starting value")),
           "numeric" =  numericInput("dynamic", "Dynamic",
                                     value = 12),
           "checkbox" = checkboxInput("dynamic", "Dynamic",
                                      value = TRUE),
           "checkboxGroup" = checkboxGroupInput("dynamic", "Dynamic",
                                                choices = c("Option 1" = "option1",
                                                            "Option 2" = "option2"),
                                                selected = "option2"
           ),
           "radioButtons" = radioButtons("dynamic", "Dynamic",
                                         choices = c("Option 1" = "option1",
                                                     "Option 2" = "option2"),
                                         selected = "option2"
           ),
           "selectInput" = selectInput("dynamic", "Dynamic",
                                       choices = c("Option 1" = "option1",
                                                   "Option 2" = "option2"),
                                       selected = "option2"
           ),
           "selectInput (multi)" = selectInput("dynamic", "Dynamic",
                                               choices = c("Option 1" = "option1",
                                                           "Option 2" = "option2"),
                                               selected = c("option1", "option2"),
                                               multiple = TRUE
           ),
           "date" = dateInput("dynamic", "Dynamic"),
           "daterange" = dateRangeInput("dynamic", "Dynamic")
    )
  })
  
  
  
  output$TabsPred = renderUI({
    nTabs = length(input$AlgoInput)
    
    myTabs = lapply(1:nTabs, function(x) {
      tabPanel(title = input$AlgoInput[x], Algo_Result$results[x])
    })
    
    do.call(tabsetPanel, myTabs)
  })
  
  ## Met à jour le nombre de tab panel ####
  output$TabsPred = renderUI({
    nTabs = length(input$AlgoInput)
  
    myTabs = lapply(1:nTabs, function(x) {
      tabPanel(title = input$AlgoInput[x], Algo_Result$results[x])
    })
    
    do.call(tabsetPanel, myTabs)
  })
  
  
  ## Creation de la carte ####
  output$mymap <- renderLeaflet({
    data = datasetInput()
    centerPos = c(mean(data$lon), mean(data$lat))
    m = leaflet(data) %>% setView(lng = centerPos[1],lat = centerPos[2],
                                  zoom = 12) %>% addTiles() %>% addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(noWrap = TRUE))
    m %>% addMarkers(
      lng = ~ lon,
      lat = ~ lat,
      popup = 'Veuillez construire un modèle.',
      label = 'Veuillez construire un modèle.'
    )
  })
  
  
  ## Modification de la carte (Observation de la variable TargetVar,ExpVar) ####
  observeEvent({
    input$TargetVar
    input$ExpVar
    }, {
    leafletProxy(mapId = "mymap",
                 session = session,
                 data = datarefactored_poopup()) %>% clearMarkers() %>% addMarkers(
                   lng = ~ lon,
                   lat = ~ lat,
                   popup =  ~ poopup,
                   label =  ~ get(input$TargetVar)
                 )
  })
  
  
  
  ## Expression reactive Refactoring Dataset pour PopUp ####
  datarefactored_poopup = reactive({
    req(input$ExpVar, input$TargetVar)
    vect_name = unlist(input$ExpVar)
    dataset = Poopup_refactor(datasetInput(), vect_name, input$TargetVar)
  })
  
  
  ## Function Création du PopUp ####
  Poopup_refactor = function(dataset_raw, vect_name, targetvar) {
    # Selection des variables pertinantes
    dataset2 = dataset_raw %>% select(vect_name)
    # Ajout du nom de la variable pour chaque ligne
    dataset2[] <-
      Map(paste, names(dataset2), dataset2, sep = ' : ')
    # création du popup
    dataset_raw$poopup <-
      apply(dataset2[, vect_name] , 1 , FUN = paste, collapse = "<br/>")
    # Selection des variables utiles à leaflet
    newDs = dataset_raw %>% select(lon, lat, poopup, targetvar)
    return(newDs)
  }
  
  
  

  ## Downloader Notice d'utilisation ####
  output$downloadHow = downloadHandler(
    filename = "Mode d'emploi.pdf",
    content = function(file) {
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     src <- normalizePath('howto.Rmd')
                     
                     incProgress(1 / 10)
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'howto.Rmd', overwrite = TRUE)
                     library(rmarkdown)
                     
                     out <-
                       render(
                         'howto.Rmd',
                         pdf_document(),
                         incProgress(2 / 10, message = 'Création du de la notice', detail = 'Ceci peut prendre longtemps')
                       )
                     
                     file.rename(out, file)
                   })
    }
    
  )
  
  #  TargetVarGAME=observe(input$TargetVar,input$TargetVar)
  
  ## Downloader Rapport ####
  output$downloadReport = downloadHandler(
    filename = function() {
      paste(input$titleOutput, sep = '.', switch(
        input$format,
        PDF = 'pdf',
        HTML = 'html',
        Word = 'docx'
      ))
    },
    
    content = function(file) {
      withProgress(message = 'Calculation in progress',
                   detail = 'This may take a while...',
                   value = 0,
                   {
                     params <- list(
                       title = input$titleOutput,
                       mesdata = datasetInput(),
                       mesmodeles = c('KNN', 'LM'),
                       PCA = FALSE
                     )
                     src <- normalizePath('report.Rmd')
                     incProgress(1 / 10)
                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))
                     file.copy(src, 'report.Rmd', overwrite = TRUE)
                     library(rmarkdown)
                     
                     out <- render(
                       'report.Rmd',
                       switch(
                         input$format,
                         PDF = pdf_document(),
                         HTML = html_document(),
                         Word = word_document()
                       ),
                       incProgress(2 / 10, message = 'Création du Rapport', detail = 'knitr')
                     )
                     
                     file.rename(out, file)
                   })
    }
  )
  
  # Gestion du jeu :
  
  
  ## Gestion du Jeu ####
  game <- reactiveValues()
  
  # Reactive Nouvel Individu ####
  NewIndiv_Reactive <- eventReactive(input$btn_game, {
    vect_name = unlist(input$ExpVar)
    newIndiv = createNewIndiv(datasetInput(), vect_name, input$TargetVar)
  })
  
  ## Fonction pour générer un individu ####
  createNewIndiv=function(dataset_raw, vect_name, TargetVar){
    newIndiv  = dataset_raw %>% select(vect_name) %>% map(generate) 
    newIndiv= as.data.frame(newIndiv)
    names(newIndiv)=vect_name
    return(newIndiv)
  }
  ## Fonction pour générer un individu selon `col` ####
  generate=function(col){
    m=mean(col)
    s=sd(col)
    NewVal=rnorm(1,m,s)
    return(NewVal)
  }
  
  
  # Affichage du Nouvel Individu ####
  output$NewIndiv = renderDataTable(DT::datatable(
    data=NewIndiv_Reactive(),
    options = list(width = 300,scrollX = TRUE,scrollY = TRUE,scroller = TRUE,searching = FALSE))
    )

  
  
  
  ## Cacul de la TargetVar pour Nouvel Individu ####
  observeEvent({input$btn_predict}, {
    game$UserPrediction=input$user_gess
    # model=Selected_Model()
    game$Score=CalclPredic('',NewIndiv_Reactive())
    #
  })
  
  output$predicted <- renderText({ 
    paste('Mon Score :',as.character(game$UserPrediction),as.character(game$Score))
    
  })
  
  CalclPredic=function(type_pb,model,Indiv){
    if(type_pb=="Classification"){
      predicted=predict(model,newdata=Indiv)$class
    }
    else{
      predicted=predict(model,newdata=Indiv)
    }
   return(predicted)
    
  }
  
  })