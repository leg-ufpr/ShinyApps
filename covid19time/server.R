server <- function(input, output) {

    output$plot <- renderPlot({
        observe({
            if(input$exit > 0) {
                stopApp(NULL)
            }
        })

        if (FALSE) {
            
            input <- list(
                variables='cases',
                local=c('Curitiba(SM), PR - BR'), 
                     ##'PR - Brasil', 
                     ##'Brasil',
                        ##                     'Pinhais, PR - BR'), ##,'NY - US', 'US'),
                dateRange=c('01/01/20', '10/10/21'),
                plots=allpls[c(1,10:12)],
                ##c("Daily counts",
                  ##      "Reproduction number",
                    ##    "Fatality rate (%)",
                      ##  "retail_and_recreation",
                        ##"grocery_and_pharmacy", "parks",
                        ##'workplaces', 'residential')[c(1:2, 4:5)],
                showPoints=TRUE,
                legend='top',
                transf='log10')
            
            source("global.R")
            
        }

##        stop(safeError('testing'))

        sdata <- dataPrepare(input$local)
        
        if (length(input$plots)<1)
          if (pt) {
            stop(safeError(
              'Favor selecionar pelo menos um gráfico a ser mostrado!'))
          } else {
            stop(safeError(
              'Please select at least one plot to be shown!'))
          }

        
        data2plot(d=sdata, 
                  variables=input$variables,
                  dateRange=input$dateRange, 
                  plots=input$plots,
                  showPoints=input$showPoints,
                  transf=input$transf, 
                  legpos=input$legend)

    })
    
}
