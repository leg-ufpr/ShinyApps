library(shiny)

kernels <- eval(formals(density.default)$kernel)

shinyUI(
    fluidPage(
        includeHTML("../GLOBAL/header.html"),
        includeHTML("../GLOBAL/MathJax.html"),
        includeCSS("../GLOBAL/style.css"),
        titlePanel("Gráfico da Densidade Empírica Kernel"),
        sidebarLayout(
            sidebarPanel(
                selectInput(inputId = "DATASET",
                            label = "Conjunto de dados:",
                            choices = names(datasets),
                            selected = names(datasets)[1]),
                checkboxInput(inputId = "LOG",
                              label = "Usar o log?"),
                radioButtons(inputId = "KERNEL",
                             label = "Função kernel:",
                             choices = kernels,
                             selected = sample(x = kernels, size = 1)),
                checkboxInput(inputId = "DRAW_RUG",
                              label = "Colocar o 'rug':",
                              value = TRUE),
                textInput(inputId = "CURVE_COLOR",
                          label = "Cor da linha:",
                          value = "black"),
                sliderInput(inputId = "WIDTH",
                            label = "Largura:",
                            min = 5, max = 70, value = 10, step = 1),
                sliderInput(inputId = "CENTER",
                            label = "Valor de referência:",
                            min = 7, max = 67, value = 30, step = 1)
            ), # sidebarPanel
            mainPanel(
                plotOutput("PLOT_DENSITY")
            ) # mainPanel
        ), # sidebarLayout
        includeHTML("../GLOBAL/footer.html")
    ) #fluidPage
) # shinyUI
