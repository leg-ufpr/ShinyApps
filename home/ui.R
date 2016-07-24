library(shiny)

# Url das aplicações Shiny do LEG.
url <- "http://shiny.leg.ufpr.br"

# Nome do usuário.
user <- Sys.info()["user"]

# Lista de diretórios que são aplicações Shiny.
appdirs <- list.dirs(path = "../",
                     full.names = FALSE,
                     recursive = FALSE)

# Verifica quais os diretórios é de aplicação Shiny.
i <- sapply(appdirs,
            FUN = function(dir) {
                path <- paste0("../", dir, "/",
                               c("ui.R", "server.R", "app.R"))
                any(file.exists(path))
            })
appdirs <- appdirs[i]

# Lista de aplicações Shiny que não deseja exibir.
exclude <- c("home")

# Lista de aplicações que será exibida.
appdirs <- appdirs[!(appdirs %in% exclude)]

# Lista HTML com as aplicações que serão exibidas.
applist <- lapply(appdirs,
                  FUN = function(x) {
                      url <- paste(url, user, x, sep = "/")
                      tags$a(x,
                             href = url,
                             target = "_blank")
                  })

shinyUI(
    fluidPage(
        includeHTML("../GLOBAL/header.html"),
        includeHTML("../GLOBAL/MathJax.html"),
        includeCSS("../GLOBAL/style.css"),
        titlePanel("Lista de Aplicações em Shiny"),
        mainPanel(
            p("Clique para ir para uma aplicação:"),
            tags$ol(lapply(applist, FUN = tags$li))
        )
    )
)
