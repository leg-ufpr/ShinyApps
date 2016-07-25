#-----------------------------------------------------------------------

get_formals <- function(func) {
    # func: objeto que seja uma função.
    f <- names(formals(func))
    i <- !as.logical(cumsum(f == "..."))
    return(f[i])
}

build_arg_list <- function(func) {
    # func: string nome de uma função.
    argm <- get_formals(get(func))
    arglist <- sprintf("list(%s)",
                       paste(sprintf("%s = input$%s",
                                     argm, argm),
                             collapse = ", "))
    return(arglist)
}

#-----------------------------------------------------------------------
# Poisson.

poisson_controls <- tagList(
    withMathJax(),
    sliderInput(inputId = "LAMBDA",
                label = "Média da Poisson (\\(\\lambda\\))",
                min = 0.1,
                max = 20,
                value = 10),
    checkboxInput(inputId = "MEDIA",
                  label = "Mostrar a média")
)

poisson_distplot <- function(LAMBDA, MEDIA) {
    x <- 0:30
    px <- dpois(x, lambda = LAMBDA)
    plot(x = x, y = px, type = "h", xlab = "x", ylab = "Pr(x)")
    if (MEDIA) {
        abline(v = LAMBDA, col = 2, lwd = 2)
    }
}

#-----------------------------------------------------------------------
# Binomial.

binomial_controls <- tagList(
    withMathJax(),
    sliderInput(inputId = "SIZE",
                label = "Número de ensaios (\\(n\\))",
                min = 0, max = 30, value = 10, step = 1),
    sliderInput(inputId = "PROB",
                label = "Probabilidade de sucesso (\\(p\\))",
                min = 0.02, max = 0.98, value = 0.5, step = 0.02),
    checkboxInput(inputId = "MEDIA",
                  label = "Mostrar a média")
)

binomial_distplot <- function(SIZE, PROB, MEDIA) {
    x <- 0:SIZE
    px <- dbinom(x, size = SIZE, prob = PROB)
    plot(x, px, type = "h",
         xlab = "x", ylab = "Pr(x)")
    if (MEDIA) {
        abline(v = PROB * SIZE, col = 2, lwd = 2)
    }
}

#-----------------------------------------------------------------------
# Beta.

beta_controls <- tagList(
    withMathJax(),
    sliderInput(inputId = "SHAPE1",
                label = "Parâmetro de forma 1",
                min = 0.01, max = 7, value = 1, step = 0.1),
    sliderInput(inputId = "SHAPE2",
                label = "Parâmetro de forma 2",
                min = 0.01, max = 7, value = 1, step = 0.1),
    checkboxInput(inputId = "MEDIA",
                  label = "Mostrar a média")
)

beta_distplot <- function(SHAPE1, SHAPE2, MEDIA) {
    curve(dbeta(x, shape1 = SHAPE1, shape2 = SHAPE2),
          from = 0, to = 1,
          xlab = "x", ylab = "f(x)")
    if (MEDIA) {
        abline(v = SHAPE1/(SHAPE1 + SHAPE2), col = 2, lwd = 2)
    }
}

#-----------------------------------------------------------------------
# Gamma.

gamma_controls <- tagList(
    withMathJax(),
    sliderInput(inputId = "SHAPE",
                label = "Parâmetro de forma",
                min = 0.01, max = 7, value = 1, step = 0.1),
    sliderInput(inputId = "RATE",
                label = "Parâmetro de taxa",
                min = 0.01, max = 7, value = 1, step = 0.1),
    checkboxInput(inputId = "MEDIA",
                  label = "Mostrar a média")
)

gamma_distplot <- function(SHAPE, RATE, MEDIA) {
    curve(dgamma(x, shape = SHAPE, rate = RATE),
          from = 0, to = 20,
          xlab = "x", ylab = "f(x)")
    if (MEDIA) {
        abline(v = SHAPE/RATE, col = 2, lwd = 2)
    }
}

#-----------------------------------------------------------------------
# Normal.

normal_controls <- tagList(
    withMathJax(),
    sliderInput(inputId = "MEAN",
                label = "Média da normal (\\(\\mu\\))",
                min = -3, max = 3, value = 0, step = 0.05),
    sliderInput(inputId = "SD",
                label = "Desvio-padrão da normal (\\(\\sigma\\))",
                min = 0.1, max = 3, value = 1, step = 0.05),
    checkboxInput(inputId = "MEDIA",
                  label = "Mostrar a média")
)

normal_distplot <- function(MEAN, SD, MEDIA) {
    curve(dnorm(x, mean = MEAN, sd = SD),
          from = -3, to = 3,
          xlab = "x", ylab = "f(x)")
    if (MEDIA) {
        abline(v = MEAN, col = 2, lwd = 2)
    }
}

#-----------------------------------------------------------------------
# Vetor com o nome das distribuições.

dist_choices <- sub(x = grep(x = ls(),
                             pattern = "_distplot",
                             value = TRUE),
                    pattern = "_distplot",
                    replacement = "")

#-----------------------------------------------------------------------
