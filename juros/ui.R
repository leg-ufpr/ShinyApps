library(shiny)

shinyUI(
    fluidPage(
        includeHTML("../GLOBAL/header.html"),
        includeHTML("../GLOBAL/MathJax.html"),
        includeCSS("../GLOBAL/style.css"),
        titlePanel("Simulação de Financiamento Imobiliário"),
        sidebarLayout(
            sidebarPanel(
                numericInput(inputId = "VALTOT",
                             label = "Valor do imóvel (R$):",
                             value = 300000),
                numericInput(inputId = "FRACFINANC",
                             label = "Fração financiada (%):",
                             value = 50, min = 0,
                             max = 100, step = 0.1),
                sliderInput(inputId = "TXAA",
                            label = "Taxa de juros ao ano (%):",
                            value = 10, min = 1, max = 25, step = 0.1),
                numericInput(inputId = "NANO",
                             label = "Número de anos para pagar:",
                             value = 20, min = 2, max = 50, step = 1)
            ), # sidebarPanel
            mainPanel(
                tabsetPanel(
                    tabPanel(
                        "Resumo",
                        h3("Resumo do financiamento"),
                        tableOutput("RESUMO")),
                    tabPanel(
                        "Parcelas",
                        h3("Valor das prestações"),
                        p(paste(
                            "Essa tabela exibe o valor fixo de cada",
                            "parcela ou amortização, que é o total",
                            "emprestado/número de meses, a quantia",
                            "referente aos juros (baseado na dívida",
                            "restante) e o valor da parcela mensal",
                            "(amortização + juros).")),
                        tableOutput("PARCELAS")),
                    tabPanel(
                        "Gráfico",
                        plotOutput("GRAFICO"))
                ) # tabsetPanel
            ) # mainPanel
        ), # sidebarLayout
        includeHTML("../GLOBAL/footer.html")
    ) # fluidPage
) # shiniUI
