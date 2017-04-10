library(shiny)

# write.table(mtcars,
#             file = "mtcars.csv",
#             sep = ";",
#             quote = FALSE,
#             row.names = FALSE)

shinyUI(fluidPage(
    includeHTML("../GLOBAL/header.html"),
    includeHTML("../GLOBAL/MathJax.html"),
    includeCSS("../GLOBAL/style.css"),
    
    titlePanel("Upload para a documentação roxygen dos dados"),

    sidebarLayout(

        sidebarPanel(
            fileInput(inputId = "THEFILE",
                      label = "Selecione o arquivo CSV/TSV:",
                      accept = c(
                          "text/csv",
                          "text/comma-separated-values",
                          ".csv", 
                          ".txt")), # fileInput
            checkboxInput(inputId = "AVANCADO",
                          label = "Opções avançadas",
                          value = FALSE),
            conditionalPanel(
                "input.AVANCADO",
                checkboxInput(inputId = "HEADER",
                              label ="Possui cabeçalho:",
                              value = TRUE),
                radioButtons(inputId = "SEPARATOR",
                             label = "Separador de campo:",
                             choices = c(
                                 "Ponto e vírgula" = ";",
                                 "Vírgula" = ",",
                                 "Tabulação" = "\t"),
                             selected = ";"),
                radioButtons(inputId = "DECIMAL",
                             label = "Caracter para o decimal:",
                             choices = c(Ponto = ".",
                                         Vírgula = ","),
                             selected = "."),
                radioButtons(inputId = "QUOTATION",
                             label = "Strings delimitadas com aspas:",
                             choices = c(
                                 Não = "",
                                 Dupla = '"',
                                 Simples = "'"),
                             selected = "")
            ), # conditionalPanel
            actionButton(inputId = "RUN",
                         label = "Processar")
        ), # sidebarPanel
        mainPanel(
            tabsetPanel(
                type = "tabs",
                tabPanel("Preencher documentação", 
                         tags$style(type = "text/css", 
                                    "p {max-width: 800px;}"),
                         includeMarkdown("LEIAME.md"),
                         tags$style(type = "text/css", 
                                    "#DOC {font-family: monospace;}"),
                         uiOutput("RDOC")),
                tabPanel("Tabela", 
                         tableOutput("TABELA"))
             ) # tabsetPanel
        )
    ) # sidebarLoyout
))


