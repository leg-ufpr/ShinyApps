library(shiny)

shinyServer(
    function(input, output){
        output$CONTROLS <- renderUI({
            get(sprintf("%s_controls", input$DIST))
        })
        output$DISTPLOT <- renderPlot({
            distplot <- sprintf("%s_distplot", input$DIST)
            arglist <- build_arg_list(distplot)
            do.call(distplot, args = c(eval(parse(text = arglist))))
        },
        family = "Palatino", width = 600, height = 480)
    }) # shinyServer()
