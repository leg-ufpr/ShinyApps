library(shiny)

ui <- fluidPage(
  
  titlePanel("Distribuições de probabilidade"),
  
  fluidRow(
    
    column(3, wellPanel(
      
      radioButtons(
      
          inputId = "dist",
          label = "Escolha a distribuição",
          choices = c("Binomial", "Poisson", "Geométrica",
                      "Normal", "Exponencial", "t-Student"),
          inline = FALSE,
          selected = "Binomial")),
    
    
    wellPanel(uiOutput("ui")))
      
      
    ),
    
    
  
    plotOutput("plot")
  
    
)

server <- function(input, output){
  
  output$ui <- renderUI({

    if(!is.null(input$dist)){
    
      if(input$dist == "Binomial"){
        list(numericInput(inputId = "n",
                     label = "Número de ensaios",
                     value = 5),
             sliderInput(inputId = "p",
                    label = "Probabilidade de sucesso",
                    value = .5, min = 0, max = 1, step = .01),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
      else if(input$dist == "Poisson"){
        list(numericInput(inputId = "lambda",
                     label = "Taxa de ocorrência",
                     value = 1),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
      else if(input$dist == "Geométrica"){
        list(sliderInput(inputId = "p",
                    label = "Probabilidade de sucesso",
                    value = .5, min = 0, max = 1, step = .01),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
      else if(input$dist == "Normal"){
        list(numericInput(inputId = "mu",
                          label = "Média",
                          value = 0),
             numericInput(inputId = "sd",
                          label = "Desvio Padrão",
                          value = 1),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
      else if(input$dist == "Exponencial"){
        list(numericInput(inputId = "lambda",
                     label = "Taxa de ocorrência",
                     value = 1),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
      else if(input$dist == "t-Student"){
        list(numericInput(inputId = "df",
                     label = "Graus de Liberdade",
                     value = 9),
             numericInput(inputId = "N",
                          label = "Tamanho da amostra",
                          value = 1000))
      }
    }

  })
  
  output$plot <- renderPlot({

    if(!is.null(input$dist)){
      if(input$dist == "Binomial"){
        distr <- table(rbinom(input$N, input$n, input$p))
        plot(distr, t = "h", xlab = "x", ylab = "P(X=x)")
      }
      else if(input$dist == "Poisson"){
        distr <- table(rpois(input$N, input$lambda))
        plot(distr, t = "h", xlab = "x", ylab = "P(X=x)")
      }
      else if(input$dist == "Geométrica"){
        distr <- table(rgeom(input$N, input$p))
        plot(distr, t = "h", xlab = "x", ylab = "P(X=x)")
      }
      else if(input$dist == "Normal"){
        distr <- density(rnorm(input$N, input$mu, input$sd))
        plot(distr, xlab = "x", ylab = "Fx(x)", main = "")
      }
      else if(input$dist == "Exponencial"){
        distr <- density(rexp(input$N, input$lambda))
        plot(distr, xlab = "x", ylab = "Fx(x)", main ="")
      }
      else if(input$dist == "t-Student"){
        distr <- density(rt(input$N, input$df))
        plot(distr, xlab = "x", ylab = "Fx(x)", main = "")
      }
    }

  })
  
}

shinyApp(ui = ui, server = server)