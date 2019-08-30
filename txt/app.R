rm(list = ls())
#-------------------------------------------------------------------------------
library(shiny)

#-------------------------------------------------------------------------------

# Define argumentos do style
txt <- "style = font-family: %s; font-size: %s; line-height: %s; width: %s;"

# Importa e sorteia o arquivo texto
smpread <- sample(list.files(pattern = "*.txt"), 1)
toread <- read.table(file = smpread, sep = "\t", header = FALSE,
                     stringsAsFactors = F)

# Aleatoriza o style
family <- sample(c("arial", "times"), 1)
size <- sample(c("9px", "14px"), 1)
height <- sample(c("11px", "30px"), 1)
width <- c("400px", "600px", 1)
style <- sprintf(txt, family, size, height, width)

#-------------------------------------------------------------------------------

ui <- fluidPage(
    mainPanel(
        textOutput("text"),
        tags$head(tags$style(paste0(
                           "#text{", style, "}"))),
        hr(),
        actionButton("start", label = "Começar"),
        actionButton("end", label = "Terminar")
    ) # End mainPanel
) # End fluidPage

server <- function(input, output) {
    output$text <- renderText({
        paste("Para começar a leitura do texto clique em começar. Ao terminar a
 leitura clique em terminar.")
    }) # End renderText
    observeEvent(input$start, {
        output$text <- renderText({
            starttime <- Sys.time()
            assign("starttime", starttime, envir = globalenv())
            paste(as.character(toread))
        }) # End renderText
    })# End observeEvent
    observeEvent(input$end, {
        endtime <- Sys.time()
        timediff <- difftime(endtime, starttime)
        output$text  <- renderText({paste("Obrigado por participar!!")})
        returnvalue <- c("timediff" = timediff, "texto" = smpread,
                         "family" = family, "size" = size, "height" = height)
        cat(paste(returnvalue, collapse = ";"), file = "output.txt",
            append = TRUE)
        Sys.sleep(3)
        stopApp()
}) # End observeEvent
} # End server
