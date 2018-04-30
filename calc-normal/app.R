library(shiny)
library(scales)

options(scipen=10000)

ui <- fluidPage(
  titlePanel("Calculadora da Distribuição Normal"),
  
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
    
    submitButton(
      text = "Calcular!"
    )
    
    
  )),
  
  column(9, wellPanel(
    
    plotOutput("plot")
    
  ))
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
        upper <- input$mu + 3*input$sd
        x <- seq(from = lower, to = upper, length.out = 10000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$xupper)){
        lower <- input$mu - 3*input$sd
        upper <- input$xupper
        x <- seq(from = lower, to = upper, length.out = 10000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 3*input$sd
        upper <- input$mu + 3*input$sd
        x <- seq(from = lower, to = upper, length.out = 10000)
        col = NA
        border = NA
      }
    }
    
    y <- dnorm(x, input$mu, input$sd)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    curve(dnorm(x, input$mu, input$sd), xlim = c(input$mu - 3*input$sd, input$mu + 3*input$sd), ylab = "Probabilidade")

    polygon(x = xcoord,
            y = ycoord,
            col = col, border = border)
    
    text(x = input$mu,
         y = median(ycoord),
         labels = round((pnorm(input$xupper, input$mu, input$sd) -
           pnorm(input$xlower, input$mu, input$sd)), 4),
         cex = 4)
    
  })
  
}

shinyApp(ui = ui, server = server)
