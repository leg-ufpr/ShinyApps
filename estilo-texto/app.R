#-----------------------------------------------------------------------
# Interface para a realização de experimento fatorial 2^k.
# Disponível em http://shiny.leg.ufpr.br/walmes/estilo-texto/.
#
#                           Vinicius Riffel · https://github.com/vriffel
#                             Walmes Zeviani · https://github.com/walmes
#
#                      Laboratory of Statistics and Geoinformation (LEG)
#                Department of Statistics · Federal University of Paraná
#                                       2019-Set-02 · Curitiba/PR/Brazil
#-----------------------------------------------------------------------

#-----------------------------------------------------------------------
# Limpa espaço de memória.

rm(list = ls())

# DANGER: remove os arquivos de log.
# unlink("counter.txt")
# file.rename(from = "output.txt",
#             to = sprintf("output-%d.txt", as.integer(Sys.time())))

#-----------------------------------------------------------------------
# Carrega pacotes necessários.

library(shiny)

#-----------------------------------------------------------------------
# Importa o texto.

# Sorteia o arquivo texto.
smpread <- sample(list.files(pattern = "texto.*.txt")[1:4],
                  size = 1)

# Importa o texto.
toread <- readLines(con = smpread)

# # Usa um único texto.
# toread <- readLines(con = "texto1.txt")

#-----------------------------------------------------------------------
# Define argumentos do estilo do texto.

# Fatorial 2⁵.
style_list <- list("font-family" = c("arial", "times"),
                   "font-size" = c("9px", "14px"),
                   "line-height" = c("12px", "30px"),
                   "width" = c("300px", "800px"),
                   "color" = c("black", "#a4a4a4"))

# ATTENTION! Mais opções para extender a aplicação:
#   background: {blue, red};
#   font-weight: {normal, bold};
#   letter-spacing: {-0.05cm, 5px};

# #-----------------------------------------------------------------------
# # Lê arquivo com realizações anteriores para ponderar a escolha do ponto
# # experimental a ser apresentado para o usuário.
#
# # Todas as combinações entre os níveis.
# all_cells <- do.call(expand.grid, args = style_list)
# attr(all_cells, "out.attrs") <- NULL
#
# # Prepara tabela conforme existência de informação prévia.
# if (file.exists("output.txt")) {
#     cells <- read.table("output.txt", header = FALSE, sep = ";")
#     cells <- cells[, 3:(2 + ncol(all_cells))]
#     names(cells) <- names(all_cells)
# } else {
#     cells <- all_cells[0, ]
# }
#
# # Concatena os pontos experimentais disponíveis com os realizados.
# done_cells <- rbind(all_cells, cells)
#
# # Determina a frequência de cada ponto experimental.
# cell_freq <- as.data.frame(xtabs(~ ., done_cells))
# names(cell_freq)[-ncol(cell_freq)] <- names(all_cells)
#
# #-----------------------------------------------------------------------
# # Faz o sorteio do ponto experimental com prioridade inversa a
# # frequência observada.
#
# # Favoresce o sorteio de pontos experimentais menos frequentes.
# i <- sample(1:nrow(cell_freq),
#             size = 1,
#             prob = 1/(cell_freq$Freq))
#
# # Cria o vetor de estilos.
# style_vec <- unlist(as.list(cell_freq[i, names(style_list)]))
# style <- sprintf("%s: %s;",
#                  names(style_vec),
#                  style_vec)
# style <- paste(style, collapse = " ")
# style
#
# # Adiciona propriedades fixas.
# style <- paste(style, "margin: 2em;")

#-----------------------------------------------------------------------

# Se arquivo com contagem não existir, então criar.
if (!file.exists("counter.txt")) {
    cat(0, "\n", file = "counter.txt")
}
cnt <- as.integer(readLines("counter.txt"))

# Todas as combinações entre os níveis.
all_cells <- do.call(expand.grid, args = style_list)
attr(all_cells, "out.attrs") <- NULL

# Numeração dos pontos experimentais.
pp <- 1:nrow(all_cells)

# Ordem de execução dos pontos experimentais.
set.seed(123)
u <- sample(pp)

# Determina a ordem da corrida experimental que será feita.
cnt <- cnt + 1
i <-  cnt %% nrow(all_cells)

u[i] # Ponto experimental.

# Cria o vetor de estilos.
style_vec <- unlist(as.list(all_cells[u[i], names(style_list)]))
style_vec <- setNames(as.character(style_vec),
                      names(style_vec))
style <- sprintf("%s: %s;",
                 names(style_vec),
                 style_vec)
style <- paste(style, collapse = " ")
style

# Adiciona propriedades fixas.
style <- paste(style, "margin: 2em;")

#-----------------------------------------------------------------------
# Frontend.

credits <- paste("<br><p>Desenvolvido por <a href='https://github.com/vriffel'>Vinicius Riffel</a>.",
                 "Código fonte disponível em <a href='https://github.com/leg-ufpr/ShinyApps/tree/master/estilo-texto'>github.com/leg-ufpr/ShinyApps</a>.</p>")

ui <- fluidPage(
    includeHTML("../GLOBAL/header.html"),
    includeHTML("../GLOBAL/MathJax.html"),
    includeCSS("../GLOBAL/style.css"),
    titlePanel("Experimento fatorial"),
    mainPanel(
        tags$head(tags$style(paste0("#text {", style, "}"))),
        uiOutput("markdown"),
        hr(),
        textOutput("text"),
        hr(),
        actionButton("start",
                     label = "Começar",
                     width = "10em",
                     icon = icon("play"),
                     class = "btn btn-primary"),
        actionButton("submit",
                     label = "Submeter",
                     width = "10em",
                     icon = icon("check-square"),
                     class = "btn btn-success"),
        actionButton("cancel",
                     label = "Cancelar",
                     width = "10em",
                     icon = icon("stop-circle"),
                     class = "btn btn-danger"),
        textOutput("endtext"),
        HTML(credits)
    ) # End mainPanel
) # End fluidPage

#-----------------------------------------------------------------------
# Backend.

server <- function(input, output) {

    # Para exibir orientações do arquivo markdown.
    output$markdown <- renderUI({
        includeMarkdown("orientacoes.md")
    })

    # Inicia retornando vazio.
    output$text <- renderText({
        NULL
    })

    # Inicia retornando vazio.
    output$endtext <- renderText({
        NULL
    })

    # Evento de clique para começar a leitura do texto.
    observeEvent(
        eventExpr = input$start,
        handlerExpr = {
            assign(x = "starttime",
                   value = Sys.time(),
                   envir = globalenv())
            output$text <- renderText({
                as.character(toread)
            }) # End renderText
        }) # End observeEvent

    # Cancela a execução da aplicação.
    observeEvent(
        eventExpr = input$cancel,
        handlerExpr = {
            stopApp()
        }
    )

    observeEvent(
        eventExpr = input$submit,
        handlerExpr = {
            endtime <- Sys.time()
            timediff <- as.numeric(difftime(endtime,
                                            starttime,
                                            units = "secs"))
            output$endtext  <- renderText({
                p <- paste("Seu tempo de leitura foi de %0.3f segundos!",
                           "Obrigado por participar.")
                sprintf(p, timediff)
            })
            output$text <- renderText({
                NULL
            })
            cat(cnt, "\n", file = "counter.txt")
            returnvalue <- c(
                "session_id" = substr(basename(tempfile()), 6, 20),
                "text_id" = smpread,
                style_vec,
                order = i,
                cell = u[i],
                ts = as.integer(Sys.time()),
                "time_elapsed" = timediff)
            cat(paste(returnvalue, collapse = ";"),
                "\n",
                file = "output.txt",
                append = TRUE)
            Sys.sleep(1.5)
            stopApp()
        }) # End observeEvent

} # End server

shinyApp(ui, server)

#-----------------------------------------------------------------------
