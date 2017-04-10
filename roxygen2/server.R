library(shiny)

shinyServer(function(input, output, session) {

    FILEURL <- reactive({
        fileUploaded <- input$THEFILE
        if (is.null(fileUploaded)){
            return(NULL)
        } else {
            return(fileUploaded$datapath)
        }
    })
    
    DATASET <- reactive({
        input$RUN
        fileurl <- isolate(FILEURL())
        if (is.null(fileurl)){
            return(NULL)
        } else {
            dados <- read.table(fileurl,
                                header = input$HEADER,
                                sep = input$SEPARATOR,
                                quote = input$QUOTATION,
                                dec = input$DECIMAL)
            return(dados)
        }
    })
    
    output$TABELA <- renderTable({
        DATASET()
    })
    
    output$RDOC <- renderUI({
        mydataset <- DATASET()
        if (!is.null(mydataset)) {
            rdoc <- capture.output(
                roxy_data(mydataset,
                          source = "",
                          keywords = "",
                          file = NA,
                          print = TRUE))
            rdoc <- paste(rdoc, collapse = "\n")
            textAreaInput(inputId = "DOC",
                          label = NA,
                          value = rdoc,
                          width = "800px",
                          height = "800px")
        }
    })

})
