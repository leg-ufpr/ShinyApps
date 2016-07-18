library(shiny)

shinyServer(
    function(input, output, session) {
        observe({
            L <- datasets[[input$DATASET]]
            if (input$LOG) {
                L$x <- log(L$x)
            }
            ht <- hist(L$x, plot = FALSE)
            rg <- range(ht$breaks)
            updateSliderInput(session,
                              inputId = "WIDTH",
                              min = 0.05 * diff(rg),
                              max = diff(rg),
                              value = median(rg),
                              step = diff(rg)/100)
            updateSliderInput(session,
                              inputId = "CENTER",
                              min = rg[1],
                              max = rg[2],
                              value = median(L$x),
                              step = diff(rg)/100)
        })
        output$PLOT_DENSITY <- renderPlot({
            L <- datasets[[input$DATASET]]
            if (input$LOG) {
                L$x <- log(L$x)
            }
            # Estimação da densidade.
            aux <- density(L$x,
                           width = input$WIDTH,
                           kernel = input$KERNEL)
            plot(aux,
                 main = NA,
                 col = input$CURVE_COLOR,
                 xlab = L$xlab,
                 sub = sprintf("N: %d   Largura de banda: %0.3f",
                               length(L$x), aux$bw))
            # Indica a banda.
            arrows(x0 = input$CENTER - 0.5 * input$WIDTH, y0 = 0,
                   x1 = input$CENTER + 0.5 * input$WIDTH, y1 = 0,
                   length = 0.1, code = 3, angle = 90, col = 2)
            # Exibe o ponto sobre a função densidade.
            y0 <- approx(aux$x, aux$y, xout = input$CENTER)
            arrows(x0 = input$CENTER, y0 = 0,
                   x1 = input$CENTER, y1 = y0$y,
                   length = 0.1, col = 2)
            # Representa a função kernel para 1 observação.
            d <- density(input$CENTER,
                         width = input$WIDTH,
                         kernel = input$KERNEL)
            lines(x = d$x, y = d$y/length(precip), col = 2)
            # Inclui as marcas sobre o eixo.
            if (input$DRAW_RUG){
                rug(L$x)
            }
        },
        width = 600, height = 480, family = "Palatino") # renderPLot
    }) # shinyServer
