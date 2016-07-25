library(shiny)

shinyUI(
    fluidPage(
        includeHTML("../GLOBAL/header.html"),
        includeHTML("../GLOBAL/MathJax.html"),
        includeCSS("../GLOBAL/style.css"),
        withMathJax(),
        titlePanel("Distribuições de probabilidade"),
        sidebarPanel(
            selectInput(inputId = "DIST",
                        label = "Distribuição",
                        choices = dist_choices,
                        selected = sample(dist_choices, size = 1)),
            uiOutput("CONTROLS")
        ),
        mainPanel(
            plotOutput("DISTPLOT")
        ) # mainPanel
    ) # fluidPage
) # shinyUI
