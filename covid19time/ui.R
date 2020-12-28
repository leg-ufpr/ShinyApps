ui <- fluidPage(
  includeHTML("../GLOBAL/header.html"),
  includeHTML("../GLOBAL/MathJax.html"),
  includeCSS("../GLOBAL/style.css"),
  withMathJax(),
  titlePanel("Visualização temporal de dados de COVID19"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "variables", 
        label = "Variável (Total Mundo)", 
        choices=lb.n, 
        selected = 'cases'),
      selectizeInput(
        inputId = "local",
        label = "Local",
        choices = olocals,
        multiple = TRUE, 
        selected = c('Curitiba(SM), PR - BR', 
                     'PR - BR', 
                     'Brasil', 'NY - US', 'US')),
      dateRangeInput(
        inputId = 'dateRange',
        label = 'Data (intervalo):',
        start = as.Date('2020-01-20'),
        end = Sys.Date(),
        format = "dd/mm/yy", 
        language = 'pt'),
      checkboxGroupInput(
        inputId = "plots", 
        label = 'Para mostrar', 
        choices = allpls, 
        selected = allpls[c(1:2, 5)]),
      checkboxInput(
        inputId = 'showPoints',
        label = 'Mostrar pontos',
        value = TRUE),
      radioButtons(
        inputId = "legend",
        label = "Legenda (posição)",
        choices = c('À direita' = 'right',
                    'Acima' = 'top'),
        selected = 'top'),
      radioButtons(
        inputId = "transf", 
	      label = "Tranformação", 
	      choices=c('Nenhuma'='none', 
	                'sqrt'='sqrt', 
	                'log10'='log10'), 
        selected = 'log10'),
      actionButton(
        inputId="exit", 
        label="Exit")),
    mainPanel(
      plotOutput("plot"))
    ),
  includeHTML("../GLOBAL/footer.html")
) # fluidPage
