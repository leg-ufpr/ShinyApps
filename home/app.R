#-----------------------------------------------------------------------
# Instruções:
# 1. Criar o diretório ~/ShinyApps/home.
# 2. Colocar este arquivo dentro.
# 3. Caso queira, altere a lista de aplicações e serem não exibidas ou
#    exibidas.

library(shiny)

# Url base das aplicações Shiny do LEG.
url <- "http://shiny.leg.ufpr.br"

# Nome de usuário extraido do sistema.
user <- Sys.info()["user"]

# Lista de diretórios em ~/ShinyApps/ que são aplicações Shiny.
appdirs <- list.dirs(path = "../",
                     full.names = FALSE,
                     recursive = FALSE)

# Verifica quais os diretórios é de aplicação Shiny pela presença dos
# "arquivos sinalizantes".
i <- sapply(appdirs,
            FUN = function(dir) {
                path <- paste0("../", dir, "/",
                               c("ui.R",
                                 "server.R",
                                 "app.R",
                                 "app.Rmd"))
                any(file.exists(path))
            })
appdirs <- appdirs[i]

# Lista de diretórios aplicações Shiny que não deseja exibir por serem
# privadas, estarem em construção ou descontinuadas.
exclude <- c("home")

# Portanto, lista de aplicações que será exibida.
appdirs <- appdirs[!(appdirs %in% exclude)]

# Verifica se tem arquivo DESCRIPTION e lê o campo `Title` e `Author`.
# Lista HTML com as aplicações que serão exibidas.
applist <-
lapply(appdirs,
       FUN = function(x) {
           f <- paste0("../", x, "/DESCRIPTION")
           if (file.exists(f)) {
               txt <- read.dcf(f, fields = c("Title", "Author"))
           } else {
               txt <- NULL
           }
           url <- paste(url, user, x, sep = "/")
           tags$li(tags$a(x, href = url, target = "_blank"),
                   if (!is.null(txt)) {
                       paste0(
                           if (!is.na(txt[, "Title"]))
                               paste(":", txt[, "Title"]),
                           if (!is.na(txt[, "Author"]))
                               paste(" -", txt[, "Author"]),
                           ".")
                   })
       })

#-----------------------------------------------------------------------
# Define UI e SERVER.

# Arquivos de customização.
header <- "../GLOBAL/header.html"
mathjax <- "../GLOBAL/MathJax.html"
css <- "../GLOBAL/style.css"

ui <- shinyUI(
    fluidPage(
        if (file.exists(header))
            includeHTML(header),
        if (file.exists(mathjax))
            includeHTML(mathjax),
        if (file.exists(css))
            includeCSS(css),
        titlePanel("Lista de Aplicações em Shiny"),
        mainPanel(
            p("Clique para ir para uma aplicação:"),
            tags$ol(applist)
        )
    )
)

server <- shinyServer(function(input, output) {})

shinyApp(ui = ui, server = server)

#-----------------------------------------------------------------------
