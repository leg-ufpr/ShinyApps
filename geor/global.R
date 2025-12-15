#-----------------------------------------------------------------------
# Carrega pacotes.

rm(list = ls())

library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(sgeostat)
library(geoR)

# Palhetas de cores disponíveis.
colorPal <- brewer.pal.info
colorPal <- rownames(subset(colorPal, category %in% c("seq", "div")))

#-----------------------------------------------------------------------
# Ler os dados.

da <- read.table("dados.txt", header = TRUE, sep = "\t")
db <- read.table("borda.txt", header = TRUE, sep = "\t")

str(da)
str(db)

# # Transloca as coordenadas.
# cbind(range(da$x), range(da$y))
# cbind(range(db$x), range(db$y))

da <- transform(da,
                x = 1e+05 * (x - min(db$x, na.rm = TRUE)),
                y = 1e+05 * (y - min(db$y, na.rm = TRUE)))
db <- transform(db,
                x = 1e+05 * (x - min(x)),
                y = 1e+05 * (y - min(y)))

# Objeto de classe geodata.
gd <- as.geodata(da, coords.col = 2:3, data.col = 6)

#-----------------------------------------------------------------------

# Legenda: nome = label.
leg <- list("mo" = expression(MO~(g~kg^{-1})),
            "phcacl2" = expression(pH~CaCl[2]),
            "p" = expression(P~(mg~dm^{-3})),
            "k" = expression(K~(cmol[c])),
            "ca" = expression(Ca~(cmol[c])),
            "mg" = expression(Mg~(cmol[c])),
            "al" = expression(Al~(cmol[c])),
            "hal" = expression(H+Al~(cmol[c])),
            "sb" = expression(SB~(cmol[c])),
            "t" = expression(CTC~(cmol[c])),
            "v" = expression(V~("%")),
            "cu" = expression(Cu~(mg~dm^{-3})),
            "mn" = expression(Mn~(mg~dm^{-3})),
            "fe" = expression(Fe~(mg~dm^{-3})),
            "zn" = expression(Zn~(mg~dm^{-3})),
            "areia" = expression(Areia~(g~kg^{-1})),
            "silte" = expression(Silte~(g~kg^{-1})),
            "argila" = expression(Argila~(g~kg^{-1})))

v <- c("Matéria orgânica",
       "pH em CaCl_2",
       "Fósforo",
       "Potássio",
       "Cálcio",
       "Magnésio",
       "Alumínio",
       "Hidróxido de alumínio",
       "Soma de bases",
       "CTC",
       "Saturação de bases",
       "Cobre",
       "Manganês",
       "Ferro",
       "Zinco",
       "Areia",
       "Silte",
       "Argila")
variav <- names(leg)
names(variav) <- v

# Função para fazer a legenda.
keyFun <- function(name, label) {
    q <- seq(min(da[, name]),
             max(da[, name]),
             length.out = 5)
    q01 <- seq(min(da01[, name]),
               max(da01[, name]),
               length.out = 5)
    q <- data.frame(lab = q, cex = q01)
    key <- list(columns = 1,
                space = "left",
                title = label,
                cex.title = 1.1,
                text = list(sprintf("%0.2f", q$lab)),
                points = list(pch = 1, cex = q$cex))
    return(key)
}

#-----------------------------------------------------------------------
# Malha para fazer a predição.

loci <- with(db,
             expand.grid(x = seq(min(x) - 30, max(x) + 30, by = 15),
                         y = seq(min(y) - 30, max(y) + 30, by = 15)))
nrow(loci)

# xyplot(y ~ x, data = loci, aspect = "iso")

loci$inside <- with(loci,
                    in.polygon(x0 = x, y0 = y, x = db$x, y = db$y))

str(loci)

# xyplot(y ~ x, data = loci, aspect = "iso", col = loci$inside + 1)

#-----------------------------------------------------------------------

# Function to translate and scale data to the init interval.
to01 <- function(z, limits = c(0, 1)) {
    z[!is.finite(z)] <- NA
    m <- (min(z, na.rm = TRUE))
    M <- (max(z, na.rm = TRUE))
    z <- (z - m)/(M - m)
    z <- min(limits) + (max(limits) - min(limits)) * z
    return(z)
}

i <- names(leg)
da01 <- da
da01[, i] <- sapply(da[, i], FUN = to01, limits = c(0.5, 3))

#-----------------------------------------------------------------------

# *gaussian*: rho(h) = exp(-(h/phi)^2)
# *exponential*: rho(h) = exp(-h/phi)
# *spherical*: rho(h) = 1 - 1.5 * (h/phi) + 0.5(h/phi)^3 if h < phi,
#                       0   otherwise

# Variograma ajustado com o olho.
eyefitted <- function(ini.cov.pars, cov.model,
                      fix.nugget, nugget, ...) {
    if (fix.nugget) {
        nugget <- 0
    }
    with(as.list(ini.cov.pars), {
        switch(cov.model,
               exponential = {
                   curve(nugget + (sill - nugget) *
                         (1 - exp(-h/phi)),
                         xname = "h", add = TRUE, ...)
               },
               gaussian = {
                   curve(nugget + (sill - nugget) *
                         (1 - exp(-(h/phi)^2)),
                         xname = "h", add = TRUE, ...)
               },
               spherical = {
                   curve(nugget + (sill - nugget) *
                         (1 - (1 - 1.5 * (h/phi) + 0.5 *
                               (h/phi)^3) * (h < phi)),
                         xname = "h", add = TRUE, ...)
               })
    })
}

#-----------------------------------------------------------------------
