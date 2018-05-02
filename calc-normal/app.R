library(shiny)
library(scales)

ui <- navbarPage(
  
  "Calculadoras de Distribuições de Probabilidade",
  
  navbarMenu("Distribuições Contínuas",
    
    tabPanel(
      
      "Distribuição Normal",
      
      fluidRow(
      
        column(3,
               
          wellPanel(
            
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
          )
        ),
      
        column(9,
             
          wellPanel(
          
            plotOutput("plot")
        
          )
        )
      )
    ),
    
    tabPanel(
      
      "Quantis da Distribuição Normal",
      
      fluidRow(
      
        column(3,
               
          wellPanel(
            
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
          )
        ),
      
        column(9,
               
          wellPanel(
            
            plotOutput("plot_q")
          )
        )
      )
    ),
      
    tabPanel(
      
      "Distribuição t-Student",
      
      fluidRow(
    
        column(3,
               
          wellPanel(
        
            numericInput(
              inputId = "x_t_lower",
              label = "Limite inferior de x",
              value = -5
            ),
        
            numericInput(
              inputId = "x_t_upper",
              label = "Limite superior de x",
              value = 5
            ),
        
            numericInput(
              inputId = "df_t",
              label = "Graus de liberdade",
              value = 5,
              min = 2
            ),
        
            verbatimTextOutput("t"),
            
            submitButton(
              text = "Calcular!"
            )
          )
        ),
        
        column(9,
               
          wellPanel(
          
            plotOutput("plot_t")
          
          )
        )
      )
    ),
    
    tabPanel(

      "Distribuição Chi quadrado",

      fluidPage(

        column(3,
        
          wellPanel(
            
            numericInput(
              inputId = "x_chisq_lower",
              label = "Limite inferior de x",
              value = 0,
              min = 0
            ),
            
            numericInput(
              inputId = "x_chisq_upper",
              label = "Limite superior de x",
              value = 5,
              min = 0
            ),
            
            numericInput(
              inputId = "df_chisq",
              label = "Graus de liberdade",
              value = 2,
              min = 1
            ),
            
            verbatimTextOutput("chisq"),
            
            submitButton(
              text = "Calcular!"
            )
          )          
        ),
        
        column(9,
          
          wellPanel(
                 
             plotOutput("plot_chisq")
                 
          )
        )
      )
    )
  ),
  
  navbarMenu(
    
    "Distribuições Discretas",
    
    tabPanel(
      
      "Em obras!",
    
      fluidPage(
      
        column(12,
             
          img(src = "http://leg.ufpr.br/~hektor/men-at-work.png", width = 300)           
        
        )
      )
    )
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
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$xupper)){
        lower <- input$mu - 4*input$sd
        upper <- input$xupper
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    y <- dnorm(x, input$mu, input$sd)
    
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    crv_norm <- curve(dnorm(x, input$mu, input$sd), xlim = c(input$mu - 4*input$sd, input$mu + 4*input$sd), ylab = "Probabilidade")
    
    polygon(x = xcoord,
            y = ycoord,
            col = col, border = border)
    
    text(x = input$mu,
         y = max(crv_norm$y)/2,
         labels = round((pnorm(upper, input$mu, input$sd) -
           pnorm(lower, input$mu, input$sd)), 4),
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
  
  output$plot_q <- renderPlot({
    
    crv_q <- curve(dnorm(x, input$mu_q, input$sd_q), xlim = c(input$mu_q - 4*input$sd_q, input$mu_q + 4*input$sd_q), ylab = "Probabilidade")
    
    y_q <- qnorm(input$q, input$mu_q, input$sd_q)
    x_q <- seq(from = input$mu_q - 4*input$sd_q, to = y_q, length.out = 1000)
    y_q.coord <- dnorm(x_q, input$mu_q, input$sd_q)
    
    
    polygon(x = c(input$mu_q - 4*input$sd_q, x_q, y_q),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    text(x = input$mu_q,
         y = max(crv_q$y)/2,
         labels = round(y_q),
         cex = 4)
    
  })
  
  output$norm_q <- renderPrint({
    
    qnorm(input$q, input$mu_q, input$sd_q)
    
  })
  
  output$plot_t <- renderPlot({
    
    crv_t <- curve(dt(x, input$df_t), xlim = c(-4 * input$df_t/(input$df_t - 2), 4 * input$df_t/(input$df_t - 2)), ylab = "Probabilidade")
    
    x <- seq(from = input$x_t_lower, to = input$x_t_upper, length.out = 1000)
    
    y <- dt(x, input$df_t)
    
    xcoord <- c(input$x_t_lower, x, input$x_t_upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    text(x = 0,
         y = max(crv_t$y)/2,
         labels = round((pt(input$x_t_upper, input$df_t) -
                           pt(input$x_t_lower, input$df_t)), 4),
         cex = 4)
    
  })
  
  output$t <- renderPrint({
    
    pt(input$x_t_upper, input$df_t) - pt(input$x_t_lower, input$df_t)
    
  })
  
  output$plot_chisq <- renderPlot({
    
    if(-8 * input$df_chisq < 0){
      lower_chisq <- 0
    }
    else{
      lower_chisq <- -8 * input$df_chisq
    }
    
    crv_chisq <- curve(dchisq(x, input$df_chisq), xlim = c(lower_chisq, 8 * input$df_chisq), ylab = "Probabilidade")
    
    x <- seq(from = input$x_chisq_lower, to = input$x_chisq_upper, length.out = 1000)
    
    y <- dchisq(x, input$df_chisq)
    
    xcoord <- c(input$x_chisq_lower, x, input$x_chisq_upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    text(x = 6 * input$df_chisq,
         y = max(crv_chisq$y)/2,
         labels = round((pchisq(input$x_chisq_upper, input$df_chisq) -
                           pchisq(input$x_chisq_lower, input$df_chisq)), 4),
         cex = 4)
    
  })
  
  output$chisq <- renderPrint({
    
    pchisq(input$x_chisq_upper, input$df_chisq) - pchisq(input$x_chisq_lower, input$df_chisq)
    
  })
  
}

shinyApp(ui = ui, server = server)
