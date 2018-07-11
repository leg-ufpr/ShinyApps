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
            
            checkboxInput(
              inputId = "tail_norm",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
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
              inputId = "p",
              label = "Probabilidade",
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
            
            checkboxInput(
              inputId = "tail_q_norm",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
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
              value = -Inf
            ),
        
            numericInput(
              inputId = "x_t_upper",
              label = "Limite superior de x",
              value = Inf
            ),
        
            numericInput(
              inputId = "df_t",
              label = "Graus de liberdade",
              value = 5,
              min = 2
            ),
            
            checkboxInput(
              inputId = "tail_t",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
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
            
            checkboxInput(
              inputId = "tail_chisq",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
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
    ),
    
    tabPanel(
      
      "Distribuição Exponencial",
      
      fluidPage(
        
        column(3,
          
          wellPanel(
            
            numericInput(
              inputId = "x_exp_lower",
              label = "Limite inferior de x",
              value = -Inf,
              min = 0
            ),
                 
            numericInput(
              inputId = "x_exp_upper",
              label = "Limite superior de x",
              value = Inf,
              min = 0
            ),
                 
            numericInput(
              inputId = "lambda_exp",
              label = "Taxa",
              value = 2,
              min = 1
            ),
                 
            checkboxInput(
              inputId = "tail_exp",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
            ),
                 
            verbatimTextOutput("exp"),
                 
            submitButton(
              text = "Calcular!"
            )
          )          
        ),
        
        column(9,
          
          wellPanel(
            
            plotOutput("plot_exp")
          )
        )
      )
    ),
    
    tabPanel(
      
      "Distribuição Gama",
      
      fluidPage(
        
        column(3,
          
          wellPanel(
            numericInput(
              inputId = "x_gamma_lower",
              label = "Limite inferior de x",
              value = -Inf,
              min = 0
            ),
                 
            numericInput(
              inputId = "x_gamma_upper",
              label = "Limite superior de x",
              value = Inf,
              min = 0
            ),
                 
            numericInput(
              inputId = "gamma_alpha",
              label = "Alfa",
              value = 2,
              min = .0001
            ),
            
            numericInput(
              inputId = "gamma_beta",
              label = "Beta",
              value = 4,
              min = .0001
            ),
                 
            checkboxInput(
              inputId = "tail_gamma",
              label = "Cauda inferior (P[X < x])",
              value = TRUE
            ),
                 
            verbatimTextOutput("gamma"),
                 
            submitButton(
              text = "Calcular!"
            )
          )          
        ),
        
        column(9,
          
          wellPanel(
            
            plotOutput("plot_gamma")
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
        if(input$tail_norm == FALSE){
          lower <- input$xlower
          upper <- input$mu + 4*input$sd
        }
        else{
          lower <- input$mu - 4*input$sd
          upper <- input$xlower
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$xupper)){
        if(input$tail_norm == FALSE){
          lower <- input$xupper
          upper <- input$mu + 4*input$sd
        }
        else{
          lower <- input$mu - 4*input$sd
          upper <- input$xupper
        }
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
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      lab <- pnorm(upper, input$mu, input$sd) - pnorm(lower, input$mu, input$sd)
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        lab <- 1 - pnorm(lower, input$mu, input$sd, input$tail_norm == FALSE)
      }
      else if(is.finite(input$xupper)){
        upper <- input$xupper
        lab <- pnorm(upper, input$mu, input$sd, input$tail_norm)
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lab <- 1
      }
    }
    
    text(x = input$mu,
         y = max(crv_norm$y)/2,
         labels = round(lab, 4),
         cex = 4)
    
  })
  
  output$norm <- renderPrint({
    
    if(is.finite(input$xlower) & is.finite(input$xupper)){
      lower <- input$xlower
      upper <- input$xupper
      pnorm(upper, input$mu, input$sd) - pnorm(lower, input$mu, input$sd)
    }
    else{
      if(is.finite(input$xlower)){
        lower <- input$xlower
        1 - pnorm(lower, input$mu, input$sd, input$tail_norm == FALSE)
      }
      else if(is.finite(input$xupper)){
        upper <- input$xupper
        pnorm(upper, input$mu, input$sd, input$tail_norm)
      }
      else if(!is.finite(input$xlower) & !is.finite(input$xlower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
      }
    }
    

    
  })
  
  output$plot_q <- renderPlot({
    
    crv_q <- curve(dnorm(x, input$mu_q, input$sd_q), xlim = c(input$mu_q - 4*input$sd_q, input$mu_q + 4*input$sd_q), ylab = "Probabilidade")
    
    y_q <- qnorm(input$p, input$mu_q, input$sd_q, lower.tail = input$tail_q_norm)
    
    if(input$tail_q_norm == FALSE){
      x_q <- seq(from = qnorm(input$p, input$mu_q, input$sd_q, lower.tail = FALSE), to = input$mu_q + 4*input$sd_q, length.out = 1000)
    }
    else{
      x_q <- seq(from = input$mu_q - 4*input$sd_q, to = y_q, length.out = 1000)
    }
    
    y_q.coord <- dnorm(x_q, input$mu_q, input$sd_q)
    
    
    polygon(x = c(min(x_q), x_q, max(x_q)),
            y = c(0, y_q.coord, 0),
            col = "red", border = "black")
    
    # text(x = input$mu_q,
    #      y = max(crv_q$y)/2,
    #      labels = round(y_q),
    #      cex = 4)
    
  })
  
  output$norm_q <- renderPrint({
    
    qnorm(input$p, input$mu_q, input$sd_q, lower.tail = input$tail_q_norm)
    
  })
  
  output$plot_t <- renderPlot({
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_t_lower)){
        if(input$tail_t){
          lower <- -6
          upper <- input$x_t_lower
        }
        else{
          lower <- input$x_t_lower
          upper <- 6
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_t_upper)){
        if(input$tail_t){
          lower <- -6
          upper <- input$x_t_upper
        }
        else{
          lower <- input$x_t_upper
          upper <- 6
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lower <- -6
        upper <- 6
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_t <- curve(dt(x, input$df_t), xlim = c(-4 * input$df_t/(input$df_t - 2), 4 * input$df_t/(input$df_t - 2)), ylab = "Probabilidade")
    
    y <- dt(x, input$df_t)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      lab <- pt(upper, input$df_t) - pt(lower, input$df_t)
    }
    else{
      if(is.finite(input$x_t_lower)){
        lower <- input$x_t_lower
        lab <- 1 - pt(lower, input$df_t, lower = input$tail_t == FALSE)
      }
      else if(is.finite(input$x_t_upper)){
        upper <- input$x_t_upper
        lab <- pt(upper, input$df_t, lower = input$tail_t)
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lab <- 1
      }
    }
    
    text(x = 0,
         y = max(crv_t$y)/2,
         labels = round(lab, 4),
         cex = 4)
    
  })
  
  output$t <- renderPrint({
    
    if(is.finite(input$x_t_lower) & is.finite(input$x_t_upper)){
      lower <- input$x_t_lower
      upper <- input$x_t_upper
      pt(upper, input$df_t) - pt(lower, input$df_t)
    }
    else{
      if(is.finite(input$x_t_lower)){
        lower <- input$x_t_lower
        1 - pt(lower, input$df_t, lower = input$tail_t == FALSE)
      }
      else if(is.finite(input$x_t_upper)){
        upper <- input$x_t_upper
        pt(upper, input$df_t, lower = input$tail_t)
      }
      else if(!is.finite(input$x_t_lower) & !is.finite(input$x_t_lower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
      }
    }
    
  })
  
  output$plot_chisq <- renderPlot({
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        if(input$tail_chisq){
          lower <- 0
          upper <- input$x_chisq_lower
        }
        else{
          lower <- input$x_chisq_lower
          upper <- input$df_chisq * 8
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_chisq_upper)){
        if(input$tail_chisq){
          lower <- 0
          upper <- input$x_chisq_upper
        }
        else{
          lower <- input$x_chisq_upper
          upper <- input$df_chisq * 8
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lower <- 0
        upper <- input$df_chisq * 8
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_chisq <- curve(dchisq(x, input$df_chisq), xlim = c(0, 8 * input$df_chisq), ylab = "Probabilidade")
    
    x <- seq(from = lower, to = upper, length.out = 1000)
    
    y <- dchisq(x, input$df_chisq)
    
    if(input$df_chisq == 1 & x[1] == 0){
      y[1] <- 0
    }
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      lab <- pchisq(upper, input$df_chisq) - pchisq(lower, input$df_chisq)
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        lower <- input$x_chisq_lower
        lab <- 1 - pchisq(lower, input$df_chisq, lower = input$tail_chisq == FALSE)
      }
      else if(is.finite(input$x_chisq_upper)){
        upper <- input$x_chisq_upper
        lab <- pchisq(upper, input$df_chisq, lower = input$tail_chisq)
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lab <- 1
      }
    }
    
    text(x = 6 * input$df_chisq,
         y = max(crv_chisq$y)/2,
         labels = round(lab, 4),
         cex = 4)
    
  })
  
  output$chisq <- renderPrint({
    
    if(is.finite(input$x_chisq_lower) & is.finite(input$x_chisq_upper)){
      lower <- input$x_chisq_lower
      upper <- input$x_chisq_upper
      pchisq(upper, input$df_chisq) - pchisq(lower, input$df_chisq)
    }
    else{
      if(is.finite(input$x_chisq_lower)){
        lower <- input$x_chisq_lower
        1 - pchisq(lower, input$df_chisq, lower = input$tail_chisq == FALSE)
      }
      else if(is.finite(input$x_chisq_upper)){
        upper <- input$x_chisq_upper
        pchisq(upper, input$df_chisq, lower = input$tail_chisq)
      }
      else if(!is.finite(input$x_chisq_lower) & !is.finite(input$x_chisq_lower)){
        lower <- input$mu - 4*input$sd
        upper <- input$mu + 4*input$sd
        pchisq(upper, input$df_chisq, lower = input$tail_chisq) - pchisq(lower, input$df_chisq, lower = input$tail_chisq)
      }
    }
    
  })
  
  output$plot_exp <- renderPlot({
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_exp_lower)){
        if(input$tail_exp){
          lower <- 0
          upper <- input$x_exp_lower
        }
        else{
          lower <- input$x_exp_lower
          upper <- input$lambda_exp
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_exp_upper)){
        if(input$tail_exp){
          lower <- 0
          upper <- input$x_exp_upper
        }
        else{
          lower <- input$x_exp_upper
          upper <- input$lambda_exp
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lower <- 0
        upper <- input$lambda_exp
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_exp <- curve(dexp(x, input$lambda_exp), xlim = c(0, input$lambda_exp), ylab = "Probabilidade")
    
    x <- seq(from = lower, to = upper, length.out = 1000)
    
    y <- dexp(x, input$lambda_exp)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      lab <- pexp(upper, input$lambda_exp) - pexp(lower, input$lambda_exp)
    }
    else{
      if(is.finite(input$x_exp_lower)){
        lower <- input$x_exp_lower
        lab <- 1 - pexp(lower, input$lambda_exp, lower = input$tail_exp == FALSE)
      }
      else if(is.finite(input$x_exp_upper)){
        upper <- input$x_exp_upper
        lab <- pexp(upper, input$lambda_exp, lower = input$tail_exp)
      }
      else if(input$lambda_exp == 0){
        lab <- 0
      }
      else if(!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lab <- 1
      }
    }
    
    text(x = 6 * input$lambda_exp,
         y = max(crv_exp$y)/2,
         labels = round(lab, 4),
         cex = 4)
    
  })
  
  output$exp <- renderPrint({
    
    if(is.finite(input$x_exp_lower) & is.finite(input$x_exp_upper)){
      lower <- input$x_exp_lower
      upper <- input$x_exp_upper
      pexp(upper, input$lambda_exp) - pexp(lower, input$lambda_exp)
    }
    else{
      if(is.finite(input$x_exp_lower)){
        lower <- input$x_exp_lower
        1 - pexp(lower, input$lambda_exp, lower = input$tail_exp == FALSE)
      }
      else if(is.finite(input$x_exp_upper)){
        upper <- input$x_exp_upper
        pexp(upper, input$lambda_exp, lower = input$tail_exp)
      }
      else if(!is.finite(input$x_exp_lower) & !is.finite(input$x_exp_lower)){
        lower <- 0
        upper <- input$mu + 4*input$sd
        pexp(upper, input$lambda_exp, lower = input$tail_exp) - pexp(lower, input$lambda_exp, lower = input$tail_exp)
      }
    }
    
  })
  
  output$plot_gamma <- renderPlot({
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      x <- seq(from = lower, to = upper, length.out = 1000)
      col <- "red"
      border <- "black"
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        if(input$tail_gamma){
          lower <- 0
          upper <- input$x_gamma_lower
        }
        else{
          lower <- input$x_gamma_lower
          upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(is.finite(input$x_gamma_upper)){
        if(input$tail_gamma){
          lower <- 0
          upper <- input$x_gamma_upper
        }
        else{
          lower <- input$x_gamma_upper
          upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        }
        x <- seq(from = lower, to = upper, length.out = 1000)
        col <- "red"
        border <- "black"
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lower <- 0
        upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        x <- seq(from = lower, to = upper, length.out = 1000)
        col = NA
        border = NA
      }
    }
    
    crv_gamma <- curve(dgamma(x, shape = input$gamma_alpha, scale = input$gamma_beta),
                       xlim = c(0, qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)),
                       ylab = "Probabilidade")
    
    x <- seq(from = lower, to = upper, length.out = 1000)
    
    y <- dgamma(x, shape = input$gamma_alpha, scale = input$gamma_beta)
    
    xcoord <- c(lower, x, upper)
    ycoord <- c(0, y, 0)
    
    polygon(x = xcoord,
            y = ycoord,
            col = "red", border = "black")
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      lab <- pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta) -
        pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta)
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        lower <- input$x_gamma_lower
        lab <- 1 - pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma == FALSE)
      }
      else if(is.finite(input$x_gamma_upper)){
        upper <- input$x_gamma_upper
        lab <- pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
      else if(input$gamma_alpha == 0 | input$gamma_beta == 0){
        lab <- 0
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lab <- 1
      }
    }
    
    text(x = input$gamma_alpha*input$gamma_beta,
         y = max(crv_gamma$y)/2,
         labels = round(lab, 4),
         cex = 4)
    
  })
  
  output$gamma <- renderPrint({
    
    if(is.finite(input$x_gamma_lower) & is.finite(input$x_gamma_upper)){
      lower <- input$x_gamma_lower
      upper <- input$x_gamma_upper
      pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta) -
        pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta)
    }
    else{
      if(is.finite(input$x_gamma_lower)){
        lower <- input$x_gamma_lower
        1 - pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma == FALSE)
      }
      else if(is.finite(input$x_gamma_upper)){
        upper <- input$x_gamma_upper
        pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
      else if(!is.finite(input$x_gamma_lower) & !is.finite(input$x_gamma_lower)){
        lower <- 0
        upper <- qgamma(.999, shape = input$gamma_alpha, scale = input$gamma_beta)
        pgamma(upper, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma) -
          pgamma(lower, shape = input$gamma_alpha, scale = input$gamma_beta, lower = input$tail_gamma)
      }
    }
    
  })
  
}

shinyApp(ui = ui, server = server)