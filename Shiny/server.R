library(shiny)
library(readr)
library(DT)

# Fonction Serveur 
shinyServer(function(input, output) {
    # Upload du Dataset 
    datasetInput <- reactive({
      #
        req(input$file1,input$separator)
        tryCatch({data=read_delim(input$file1$datapath,delim=input$separator)},
                  error = function(e) {# return a safeError if a parsing error occurs
                        stop(safeError(e))})
        })
    
    # Impression du Dataset
    output$mydataset = renderDataTable(
        DT::datatable(datasetInput(),options = list(width = 30,scrollX = TRUE,scrollY = TRUE,searching=FALSE) )
    )
    
    
   
    
    
    # Upload du Dataset
    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$dynamic + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')

    })
    
    
      # Fonction de téléchargement du rapport
    
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste(input$titleOutput, sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
    
      content = withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                 
      function(file) {
        params <- list(title = input$titleOutput, mesdata = datasetInput())
        src <- normalizePath('report.Rmd')
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
                 })
    )
    
  
})
