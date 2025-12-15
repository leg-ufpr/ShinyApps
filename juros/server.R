library(shiny)

# input <- list(TXAA = 10, NANO = 0.5,
#               VALTOT = 20000, FRACFINANC = 50)

shinyServer(function(input, output) {
    FUN <- reactive({

        # Valor total do imóvel.
        vtot <- input$VALTOT
        # Valor financiado do imóvel.
        vfinanc <- vtot * input$FRACFINANC/100
        # Número total de meses.
        nmes <- input$NANO * 12
        # Valor amortizado ao mês (parcela).
        vam <- vfinanc/nmes
        # Taxa de juros ao ano.
        txaa <- input$TXAA/100
        # Taxa de juros ao mês.
        txam <- (1 + txaa)^(1/12) - 1

        # Tabela da condição a cada mês.
        tab <- data.frame(Mês = 0:nmes,
                          Amortizado = vam,
                          "Amort. acum." = NA,
                          Juros = NA,
                          "Jur. acum." = NA,
                          Parcela = NA,
                          "Parc. acum." = NA,
                          "Restante" = vfinanc - (0:nmes) * vam,
                          # "Total pago" = NA,
                          "Custo se quitar" = NA,
                          check.names = FALSE)

        tab[nrow(tab), 3:7] <- 0
        tab[1, 2:7] <- 0
        tab$Juros[2:(nmes + 1)] <- tab$Restante[1:(nmes)] * txam
        tab$Parcela <- tab$Amortizado + tab$Juros
        tab[, "Amort. acum."] <- cumsum(tab$Amortizado)
        tab[, "Jur. acum."] <- cumsum(tab$Juros)
        tab[, "Parc. acum."] <- cumsum(tab$Parcela)
        tab[, "Custo se quitar"] <- tab$"Parc. acum." + tab$Restante
        names(tab)[-1] <- paste(names(tab)[-1], "(R$)")

        # Total pago apenas em juros.
        tju <- sum(tab$Juros)
        # Valor final do empréstimo.
        vfem <- sum(tab$Parcela)

        # Tabela resumo do financiamento.
        m <- c(2, 3, 5) * 12 + 1
        res <- data.frame(
            Item = c(
                "Valor do imóvel (R$)",
                "Percentual financiado (%)",
                "Valor financiado (R$)",
                "Taxa de juros ao ano (%)",
                "Taxa de juros ao mês (%)",
                "Valor total acumulado em juros (R$)",
                "Total em juros relativo ao valor financiado (%)",
                "Valor médio em juros por mês (R$)",
                "Valor final pago ao banco pelo financiamento (R$)",
                "Valor final pago relativo a quantidade financiada (%)",
                "Valor médio da parcela ao mês (R$)",
                "Custo do financiamento se amortizado em 2 anos (R$)",
                "Custo do financiamento se amortizado em 3 anos (R$)",
                "Custo do financiamento se amortizado em 5 anos (R$)"),
            Valor = c(
                vtot,
                input$FRACFINANC,
                vfinanc,
                100 * txaa,
                100 * txam,
                tju,
                100 * tju/vfinanc,
                tju/nmes,
                vfem,
                100 * vfem/vfinanc,
                vfem/nmes,
                rowSums(tab[m, 7:8])))

        L <- list(res = res, tab = tab)
        return(L)
    })

    output$RESUMO <- renderTable({
        FUN()$res
    },
    include.rownames = FALSE, digits = c(0, 0, 2))

    output$PARCELAS <- renderTable({
        FUN()$tab
    },
    include.rownames = FALSE)

    output$GRAFICO <- renderPlot({
        L <- FUN()
        # Valor total.
        vtot <- L$res[1, 2]
        # Valor finaciado.
        vfinanc <- vtot * L$res[2, 2]/100

        par(mar = c(5.1, 4.1, 4.1, 4.1))
        plot(c(L$tab[, 7]) ~ c(L$tab[, 1]/12),
             type = "l",
             # ylim = c(vtot, 2 * vtot),
             xlab = "Tempo de uso do empréstimo (anos)",
             ylab = "Valor pago do empréstimo (R$)")
        grid()
        title(main = list(sprintf(
                  "Entrada de R$ %0.2f\tEmprétimo de R$ %0.2f",
                  vtot - vfinanc, vfinanc), font = 1, cex = 1))
        segments(x0 = 0,
                 y0 = 0,
                 x1 = max(L$tab[, 1])/12,
                 y1 = vtot - vfinanc, col = 2)
        legend("bottomright",
               legend = c("Parcelas com juros", "Parcelas sem juros"),
               lty = 1, col = 1:2, bty = "n")

        par()$mar
        s <- par()$yaxp
        s <- seq(s[1], s[2], length.out = s[3] + 1)
        axis(side = 4,
             at = s,
             labels = formatC(100 * s/vfinanc, digits = 3))
        mtext(side = 4, line = 2.5,
              text = "Valor pago com juros sobre o valor amortizado (%)")
        x <- c(2, 3, 5, 10, 15)
        x <- x[x < (nrow(L$tab) - 1)/12]
        if (length(x) > 0) {
            y <- L$tab[x * 12 + 1, 7]
            points(x = x, y = y)
            text(x = x, y = y, pos = 4,
                 labels = sprintf("%0.0f (%0.0f%s)",
                                  y, 100 * y/vfinanc, "%"))
        }

    },
    # family = "Palatino",
    width = 500,
    height = 400)
})
