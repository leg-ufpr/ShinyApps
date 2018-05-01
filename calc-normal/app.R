library(shiny)
library(scales)

options(scipen=1)

ui <- navbarPage(
  
  "Distribuição Normal",
  
  tabPanel(
    "Calculadora da Distribuição Normal",
    
    column(3, wellPanel(
    
    numericInput(
      inputId = "xlower",
      label = "Limite inferior de x",
      value = -Inf
      ),
    
    numericInput(
      inputId = "xupper",
      label = "Limite superior de x",
      value = Inf
    ),
    
    numericInput(
      inputId = "mu",
      label = "Média",
      value = 0
    ),
    
    numericInput(
      inputId = "sd",
      label = "Desvio padrão",
      value = 1,
      min = .00000000001
    ),
    
    verbatimTextOutput("norm"),
    
    submitButton(
      text = "Calcular!"
    )
    
    
  )),
  
  column(9, wellPanel(
    
    plotOutput("plot")
    
  ))
),

  tabPanel(
    "Calculadora da Distribuição Normal Quantilica",
    
    column(3, wellPanel(
      
      numericInput(
        inputId = "q",
        label = "Valor do Quantil",
        value = .5,
        min = 0,
        max = 1
      ),
      
      numericInput(
        inputId = "mu_q",
        label = "Média",
        value = 0
      ),
      
      numericInput(
        inputId = "sd_q",
        label = "Desvio padrão",
        value = 1,
        min = .00000000001
      ),
      
      verbatimTextOutput("norm_q"),
      
      submitButton(
        text = "Calcular!"
      )
      
      
    )),
    
    column(9, wellPanel(
      
      plotOutput("plot_q")
      
    ))
  )

)

server <- function(input, output){
  
  output$plot <- renderPlot({
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        upper <- input$mu + 4*input$sd
        x <- seq(from = lower, to = upper, length.out = 10000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$xupper)){
        lower <- input$mu - 4*input$sd
        upper <- input$xupper
        x <- seq(from = lower, to = upper, length.out = 10000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
        x <- seq(from = lower, to = upper, length.out = 10000)
        col = NA
        border = NA
      }
    }
    
    y <- dnorm(x, input$mu, input$sd)
    
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    curve(dnorm(x, input$mu, input$sd), xlim = c(input$mu - 4*input$sd, input$mu + 4*input$sd), ylab = "Probabilidade")
    
    polygon(x = xcoord,
            y = ycoord,
            col = col, border = border)
    
    text(x = input$mu,
         y = median(ycoord),
         labels = round((pnorm(upper, input$mu, input$sd) -
           pnorm(lower, input$mu, input$sd)), 4),
         cex = 4)
    
  })
  
  output$plot_q <- renderPlot({
    
    curve(dnorm(x, input$mu_q, input$sd_q), xlim = c(input$mu_q - 4*input$sd_q, input$mu_q + 4*input$sd_q), ylab = "Probabilidade")
    
    y_q <- qnorm(input$q, input$mu_q, input$sd_q)
    x_q <- seq(from = input$mu_q - 4*input$sd_q, to = y_q, length.out = 1000)
    y_q.coord <- dnorm(x_q, input$mu_q, input$sd_q)
    
    
    polygon(x = c(input$mu_q - 4*input$sd_q, x_q, y_q),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    text(x = input$mu_q,
         y = dnorm(input$mu_q)/2,
         labels = round(y_q),
         cex = 4)
    
  })
  
  output$norm <- renderPrint({
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        upper <- input$mu + 4*input$sd
      }
      else if(is.finite(input$xupper)){
        lower <- input$mu - 4*input$sd
        upper <- input$xupper
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
      }
    }
    
    pnorm(upper, input$mu, input$sd) - pnorm(lower, input$mu, input$sd)
    
  })
  
  output$norm_q <- renderPrint({
    
    qnorm(input$q, input$mu_q, input$sd_q)
    
  })
  
}

shinyApp(ui = ui, server = server)


