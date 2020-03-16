library(shiny)
library(DT)
library(leaflet)
library(rlang)
library(tidyverse)
library(caret)
library(e1071)
library(randomForest)
library(rpart.plot)
library(lmtest)
library(DataExplorer)
library(knitr)
library(lmtest)
# Fonction Serveur
shinyServer(function(session, input, output) {

  ## Upload du Dataset####
  datasetInput <- reactive({
    req(input$file1, input$separator) # Nécessite les 2 Variables : file1 & separator
   read_delim(input$file1$datapath, delim = input$separator) %>% mutate_if(is.character, as.factor)
  })
  ## Listes des algorithmes disponibles####
  Algo_List = reactive({
    req(input$TypeMod)
    if (input$TypeMod == 'Régression') {
      # Listes des Méthodes / Algo : Regression
      return(list('lm (Linear Model)'))#, 'glm (Logistic Regression)'))
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
  
  
  ## Impression du Dataset####
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
    
    name=datasetInput() %>% dplyr::summarise_all(class) %>% tidyr::gather(variable, class)
    
    updateSelectInput(session, "TargetVar", choices = name$variable)
    updateCheckboxGroupInput(session, "ExpVar", choiceNames = paste(name$variable,'(',name$class,')'), choiceValues = name$variable)
  })
 
  ## Observation de la Variable Algo_List ####  
  observe({
    AlgoChoices <- Algo_List()
    updateRadioButtons(session, "AlgoInput", choices = AlgoChoices)
  })
  
  
  
  ## Observation du boutton AlgoInput : Recuperation du nom du modèle ####
  Algo=reactiveValues()
  observeEvent({input$AlgoInput},{
    Algo$model_name= str_split(input$AlgoInput, ' ',n = Inf, simplify = FALSE)[[1]][1]
    }) 
  

  ## Observation du boutton runModels : Ajustement du modèle ####
  observeEvent({input$runModels},{
    # Récuperer les valeurs de l'Algo créé.
    TuneParams=getTuneParams(Algo$model_name)
    sequence=seq.int(from=1, to=length(TuneParams$parameter))
    
    Algo$TuneParams=map2(sequence,TuneParams$parameter,function(x,y){return(list('name'=as.character(y),'value'=input[[paste0("TuneParams",x)]]) )}) 
    formule=createFormula(input$TargetVar,input$ExpVar)
    
    
    Algo$Liste_sets=createTTSets(datasetInput(),input$TargetVar,input$ExpVar)
    Algo$model=Train_n_Test_model(Algo$model_name,input$TypeValid,input$ValidValue,Algo$TuneParams,formule,Algo$Liste_sets)
  })
  
  ## Function création des ensembles de Train et et de Test ####
  createTTSets=function(donnees,TargetVar,ExpVar){

    donnees=donnees %>% select(all_of(TargetVar),all_of(ExpVar))
    index = createDataPartition(y=getElement(donnees,TargetVar), p=0.8, list=FALSE)
    train = donnees[index,] 
    test = donnees[-index,]
    return(list('train'=train,'test'=test))
  }
  
  ## Function création de la formule de fitting ####
  createFormula=function(TargetVar,ExpVar){
    if(is.null(ExpVar)){ ExpVar='.' }
    form=paste0(TargetVar,"~.")
    return(as.formula(form))
  }
  
  ## Function création du Dataframe des paramètres à Tuner ####
  buildTuneMatrix=function(TuneParams) {
    if(class(TuneParams$value)!="integer"){
      df=as.data.frame(TuneParams$value)
    }else{
      df=as.data.frame(seq(TuneParams$value[1],TuneParams$value[2]))
    }
    colnames(df)=TuneParams$name
    return(df)
  }
  
  ## Train_n_Test_model ####
  Train_n_Test_model = function(model_str,MethodValidation,ValidValue,TuneParams,FORMU,Liste_sets) {
    donnees=Liste_sets$train
    
    if(MethodValidation=='none'){fitControl<-NULL}
    else{fitControl<-trainControl(method=MethodValidation,number =ValidValue)}
    
    if(is.null(TuneParams[[1]]$value)){
      mod <-train(FORMU, data =donnees, method=model_str, trControl =fitControl)
    }else{
      ## creation de la matrice tunegrid selon les paramètres
      TuneDF=map_df(TuneParams,buildTuneMatrix)
      print(TuneDF)

      mod  <- train(FORMU, data = donnees,method=model_str,trControl =fitControl,tuneGrid = TuneDF)
    }
    return(mod)
  }
  
  ## Function Renvoi les hyper-parametres du modèle ####
  getTuneParams=function(model_name){
    print(model_name)
    model_params=getModelInfo(model=model_name,regex = FALSE)[[1]]
    model_params=model_params$parameters
    return(model_params)
  }

 
   ## Function ==> liste de slider par paramètres ####
  selectUiComp=function(param,class,label,i){
    if (param=='parameter'){return()}
    return(switch(as.character(class),
           "character"=textInput(paste0("TuneParams",i), label,value =NULL),
           "numeric"=sliderInput(paste0("TuneParams",i),paste(label,param,sep=' : '),min=1,max=100,value=c(1,10))))
  }
  
  ## Render UI : Interface des Tunning Parameters ####
  output$TunningParams <- renderUI({
    # Si Aucun Modèle selectionné ==> Affichage d'aucun components
    if (is.null(Algo$model_name)){return()}
    else{
      TuneParams=getTuneParams(Algo$model_name)
      sequence=seq.int(from=1, to=length(TuneParams$parameter))
      liste_UI=pmap(list(as.vector(TuneParams$parameter),as.vector(TuneParams$class),as.vector(TuneParams$label),sequence),selectUiComp)
    }
    liste_UI
  })
  
  output$ValidParams= renderUI({
    switch(input$TypeValid,
           'boot'=sliderInput('ValidValue', label='Nombre de Echantillons Bootstrap : ',min=1,max=80,value=20),
           'cv'=sliderInput('ValidValue', label='Nombre de blocs : ',min=1,max=80,value=20),
           'LCV'=sliderInput('ValidValue', label='Nombre de blocs : ',min=1,max=nrow(datasetInput()),value=80),
           ''
           )
    
  })
  
  output$TunningModel <- renderPrint({
    Algo$model
  })
  
  output$FinalModel <- renderPrint({
    Algo$model$finalModel
  })
  
  output$Graphs =renderUI({
    switch(Algo$model_name,
             ## Plot lm
           'lm'=list(plotOutput('LM'),verbatimTextOutput('Txt')),
           ## Plot LDA
           'lda'=list(plotOutput('LDA'),verbatimTextOutput('Txt')),
           ## Plot QDA
           'qda'=list(plotOutput('QDA'),verbatimTextOutput('Txt')),
           ## Plot CART
           'rpart'=list(plotOutput('RPART'),verbatimTextOutput('Txt')),
           ## Plot Random Forest
           'rf'={},
           return()
    )
    
  })
  
  output$RPART=renderPlot({
    rpart.plot(Algo$model$finalModel,box.palette="red")
    
  })
  # output$Facets=renderPlot()
  output$LDA=renderPlot({
    Ytest=Algo$Liste_sets$test %>% select(input$TargetVar,input$ExpVar)
    pred=predict(Algo$model)
    # saveRDS(pred,file='./pred')
    plot(pred)
    }
  )
  
  output$QDA=renderPlot({
    Ytest=Algo$Liste_sets$test %>% select(input$TargetVar,input$ExpVar)
    pred=predict(Algo$model)
    # saveRDS(pred,file='./pred')
    
    plot(pred)
  }
  )
  
  
  output$Txt=renderPrint(Algo$model_name)
  
  
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
  observeEvent({input$TargetVar 
    input$ExpVar 
    input$runModels
    input$jouer
    }, {
    leafletProxy(mapId = "mymap",
                 session = session,
                 data = datarefactored_poopup()) %>% clearMarkers() %>% addMarkers(lng = ~ lon,lat = ~ lat,popup =  ~ poopup,label =  ~ get(input$TargetVar))
  })
  
  
  
  ## Expression reactive Refactoring Dataset pour PopUp ####
  datarefactored_poopup = reactive({
    req(input$ExpVar, input$TargetVar,input$runModels,input$jouer)
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
                       model_name=Algo$model_name,
                       modeles = Algo$model
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
    newIndiv  = dataset_raw %>% select(all_of(vect_name)) %>% map(generate) 
    newIndiv= as.data.frame(newIndiv)
    names(newIndiv)=vect_name
    return(newIndiv)
  }
  ## Fonction pour générer un individu selon `col` ####
  generate=function(col){
    m=mean(col)
    s=sd(col)/2
    NewVal=rnorm(1,m,s)
    return(NewVal)
  }

  output$UI_user_gess=renderUI({
    dfval=datasetInput() %>% select(input$TargetVar)
    classe=lapply(dfval,class)
    switch(classe[[1]],
      'numeric'=numericInput('user_gess', 'Ma prédiction', value=0),
      'factor'=selectizeInput('user_gess', 'Ma prédiction',choices=dfval)
        # textInput('user_gess', 'Ma prédiction',placeholder = as.character(input$TargetVar))
    )
  })
  
  # Affichage du Nouvel Individu ####
  output$NewIndiv = renderDataTable(DT::datatable(
    data=NewIndiv_Reactive(),
    options = list(width = 300,scrollX = TRUE,scrollY = TRUE,scroller = TRUE,searching = FALSE))
    )

  
  
  
  ## Cacul de la TargetVar pour Nouvel Individu ####
  observeEvent({input$btn_predict}, {
    # model=Selected_Model()
    game$Pred=predict(Algo$model$finalModel,newdata=NewIndiv_Reactive())
    print(game$Pred)
    game$Score=CalcScore(input$user_gess,game$Pred)
    #
  })
  
  output$predicted <- renderText({ 
    paste0('Mon Score :',as.character(game$Pred),'-',as.character(game$Score))
    
  })
  
  CalcScore=function(UserPred,AlgoPred){
    
    switch (class(AlgoPred),
            "factor" ={
              score=1*(UserPred==AlgoPred) 
              return(score)
            },
            "numeric"={return(abs(UserPred-AlgoPred))},
    )
    
  }
  
  })


