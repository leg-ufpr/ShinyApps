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

#-----------------------------------------------------------------------
# Define argumentos do estilo do texto.

# Fatorial 2⁵.
style_list <- list("font-family" = c("arial", "times"),
                   "font-size" = c("9px", "14px"),
                   "line-height" = c("12px", "30px"),
                   "width" = c("300px", "800px"),
                   "color" = c("black", "#a4a4a4"))

# style_vec <- sapply(style_list, FUN = sample, size = 1)
# style <- sprintf("%s: %s;",
#                  names(style_vec),
#                  style_vec)
# style <- paste(style, collapse = " ")
# style

#-----------------------------------------------------------------------
# Lê arquivo com realizações anteriores para ponderar a escolha do ponto
# experimental a ser apresentado para o usuário.

# Todas as combinações entre os níveis.
all_cells <- do.call(expand.grid, args = style_list)
attr(all_cells, "out.attrs") <- NULL

# Prepara tabela conforme existência de informação prévia.
if (file.exists("output.txt")) {
    cells <- read.table("output.txt", header = FALSE, sep = ";")
    cells <- cells[, 3:(2 + ncol(all_cells))]
    names(cells) <- names(all_cells)
} else {
    cells <- all_cells[0, ]
}

# Concatena os pontos experimentais disponíveis com os realizados.
done_cells <- rbind(all_cells, cells)

# Determina a frequência de cada ponto experimental.
cell_freq <- as.data.frame(xtabs(~ ., done_cells))
names(cell_freq)[-ncol(cell_freq)] <- names(all_cells)

#-----------------------------------------------------------------------
# Faz o sorteio do ponto experimental com prioridade inversa a
# frequência observada.

# Favoresce o sorteio de pontos experimentais menos frequentes.
i <- sample(1:nrow(cell_freq),
            size = 1,
            prob = 1/(cell_freq$Freq))

# Cria o vetor de estilos.
style_vec <- unlist(as.list(cell_freq[i, names(style_list)]))
style <- sprintf("%s: %s;",
                 names(style_vec),
                 style_vec)
style <- paste(style, collapse = " ")
style

# Adiciona propriedades fixas.
style <- paste(style, "margin: 2em;")

#-----------------------------------------------------------------------
# Frontend.

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
        actionButton("start", label = "Começar"),
        actionButton("end", label = "Terminar"),
        textOutput("endtext"),
        HTML("<br><p>Desenvolvido por <a href='https://github.com/vriffel'>Vinicius Riffel</a>.</p>")
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

    observeEvent(
        eventExpr = input$end,
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
            returnvalue <- c(
                "session_id" = substr(basename(tempfile()), 6, 20),
                "text_id" = smpread,
                style_vec,
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
