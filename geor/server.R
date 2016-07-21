library(shiny)

shinyServer(function(input, output, session) {
    GETDATA <- reactive({
        i <- input$VARIABLE
        gd$data <- da[, i]
        key <- keyFun(i, leg[[i]])
        vario <- variog(geodata = gd, max.dist = 600)
        L <- list(i = i, gd = gd, key = key, vario = vario)
        return(L)
    })

    output$BUBBLEPLOT <- renderPlot({
        L <- GETDATA()
        key <- L$key
        i <- L$i
        with(da01, {
            xyplot(y ~ x,
                   data = db,
                   aspect = "iso",
                   key = key,
                   type = "l", xlab = "", ylab = "", col = 1) +
                as.layer(xyplot(y ~ x, col = 1, cex = get(i)))
        })
    },
    family = "Palatino")

    observe({
        L <- GETDATA()
        gd <- L$gd
        vario <- L$vario
        # Chute para o patamar.
        sill <- max(vario$v)
        updateSliderInput(session, inputId = "SILL",
                          max = 1.3 * sill, value = sill)
        updateSliderInput(session, inputId = "NUGGET",
                          max = sill, value = 0.1 * sill)
    })

    KRIGE <- reactive({
        L <- GETDATA()
        gd <- L$gd
        vario <- L$vario
        cov.model <- input$COVMODEL
        ini.cov.pars <- c(phi = input$PHI, sill = input$SILL)
        fit <- switch(
            input$AJUSTE,
            eyefitted = {
                list(cov.model = input$COVMODEL,
                     ini.cov.pars = ini.cov.pars,
                     fix.nugget = !input$ESTIMATENUGGET,
                     nugget = input$ESTIMATENUGGET *
                         input$NUGGET)
            },
            likfit = {
                mlfit <- likfit(geodata = gd,
                                ini.cov.pars = ini.cov.pars,
                                cov.model = cov.model,
                                fix.nug = !input$ESTIMATENUGGET,
                                nugget = input$ESTIMATENUGGET *
                                    input$NUGGET,
                                lik.met = "REML")
                return(mlfit)
            },
            variofit = {
                nlfit <- variofit(vario = vario,
                                  ini.cov.pars = ini.cov.pars,
                                  cov.model = cov.model,
                                  fix.nug = !input$ESTIMATENUGGET,
                                  nugget = input$ESTIMATENUGGET *
                                      input$NUGGET)
                return(nlfit)
            })
        return(fit)
    })

    output$VARIOPLOT <- renderPlot({
        fit <- KRIGE()
        L <- GETDATA()
        vario <- L$vario
        i <- L$i

        with(vario,
             plot(v ~ u, type = "o",
                  xlab = "Distância (m)",
                  ylab = "Semivariância",
                  xlim = c(0, max(u)),
                  ylim = c(0, max(v)),
                  main = leg[[i]]))

        switch(
            input$AJUSTE,
            eyefitted = {
                with(fit,
                     eyefitted(ini.cov.pars = ini.cov.pars,
                               cov.model = cov.model,
                               fix.nugget = fix.nugget,
                               nugget = nugget, col = 2))
            },
            likfit = {
                lines(fit, col = 2)
            },
            variofit = {
                lines(fit, col = 2)
            })
    },
    family = "Palatino")

    KRG <- reactive({
        fit <- KRIGE()
        L <- GETDATA()
        gd <- L$gd

        krg <- if (input$AJUSTE == "eyefitted") {
                   with(fit,
                        krige.control(cov.model = cov.model,
                                      cov.pars = ini.cov.pars,
                                      nugget = nugget))
               } else {
                   krige.control(obj.model = fit)
               }

        kc <- krige.conv(geodata = gd,
                         locations = loci[loci$inside, ],
                         krige = krg)

        loci$predict <- NA
        loci[loci$inside, ]$predict <- kc$predict

        return(loci)
    })

    output$KRIGPLOT <- renderPlot({
        loci <- KRG()
        L <- GETDATA()
        i <- L$i

        colp <- brewer.pal(
            n = brewer.pal.info[input$PALETTE, ]$maxcolors,
            name = input$PALETTE)
        colp <- colorRampPalette(colp, space = "rgb")

        # p1 <-
        p <- levelplot(predict ~ x + y,
                       data = loci,
                       aspect = "iso",
                       col.regions = colp(100),
                       main = list(leg[[i]], font = 1),
                       xlab = NULL,
                       ylab = NULL,
                       scales = list(y = list(rot = 90))) +
            as.layer(xyplot(y ~ x, db, type = "l", col = 1))

        if (input$BUBBLES) {
            p <- p +
                as.layer(with(da01,
                              xyplot(y ~ x, col = 1,
                                     cex = eval(parse(text = i)),
                                     par.settings = ps)))
        }
        plot(p)
    },
    family = "Palatino")

})
