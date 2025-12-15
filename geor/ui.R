library(shiny)

cvm <- c("Exponencial" = "exponential",
         "Gaussiano" = "gaussian",
         "Esférico" = "spherical")

aju <- c("Visual" = "eyefitted",
         "Máxima verossimilhança restrita (REML)" = "likfit",
         "Modelo não linear" = "variofit")

shinyUI(fluidPage(
    # includeHTML("../GLOBAL/header.html"),
    # includeHTML("../GLOBAL/MathJax.html"),
    includeCSS("../GLOBAL/style.css"),
    titlePanel("Geoestatística"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "VARIABLE",
                        label = "Variável:",
                        choices = variav),
            radioButtons(inputId = "AJUSTE",
                         label = "Tipo de ajuste:",
                         choices = aju),
            radioButtons(inputId = "COVMODEL",
                         label = "Modelo de variograma:",
                         choices = cvm),
            sliderInput(inputId = "SILL",
                        label = "Alcance:",
                        min = 0, max = 10, value = 5, step = 1),
            sliderInput(inputId = "PHI",
                        label = "Phi:",
                        min = 0, max = 500, value = 200, step = 5),
            checkboxInput(inputId = "ESTIMATENUGGET",
                          label = "Estimar nugget?",
                          value = FALSE),
            conditionalPanel(condition = "input.ESTIMATENUGGET",
                             sliderInput(inputId = "NUGGET",
                                         label = "Nugget:",
                                         min = 0, max = 2,
                                         value = 1, step = 0.1)),
            selectInput(inputId = "PALETTE",
                        label = "Palheta de cores:",
                        choices = colorPal,
                        selected = sample(colorPal, size = 1)),
            checkboxInput(inputId = "BUBBLES",
                          label = "Sobrepor valores observados?",
                          value = FALSE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Visualização", plotOutput("BUBBLEPLOT")),
                tabPanel("Variograma", plotOutput("VARIOPLOT")),
                tabPanel("Krigagem", plotOutput("KRIGPLOT"))
            )
        ) # mainPanel
    ) # sidebarLayout
) # fluidPage
) # shinyUI
