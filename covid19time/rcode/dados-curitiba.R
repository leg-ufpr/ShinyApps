
if (FALSE)
    setwd('..')

u0 <- 'http://dadosabertos.c3sl.ufpr.br/curitiba/CasosCovid19/'

k <- 0
repeat {
    ufl <- paste0(u0,  Sys.Date() + k,
                  '_Casos_Covid_19_-_Base_de_Dados.csv')
    if (class(try(download.file(ufl, 'data/casosCuritibaSM.csv'),
                  TRUE))=='try-error') {
        k <- k-1
    } else {
        break
    }
}

dcwb <- read.csv2('data/casosCuritibaSM.csv', encoding='latin1')
head(dcwb)

dcwb$date <- as.Date(dcwb[,2], '%d/%m/%Y')
summary(dcwb$date)

if (!any(ls()=='alldates'))
    alldates <- gsub('-', '', seq(as.Date('20200122', '%Y%m%d'),
                    Sys.Date(), 1))

dcwb$fdate <- factor(gsub('-', '', 
                          dcwb$date,
                          fixed=TRUE), alldates)
tail(dcwb)

t3 <- table(dcwb$EVOLU, dcwb$fdate)
str(t3)

t3a <- apply(t3, 1, cumsum)

t3a[-20:0+nrow(t3a), ]
tail(diff(c(0, t3a[,2])), 21)

(jj <- pmatch(c(##paste0('202011', 28:30),
                paste0('202012', sprintf('%02d', 18:26))), 
              colnames(t3)))
t3a[jj, ]

t3a[jj, ] <- cbind(
    c(##5705, 6449, 6849, 7449, 7714, 8415, 9131,
      ##9647, 10224, 11232, 11500, 12139, 12784, 12973,
      ##13320, 13582, 13829, 13253, 12907, 12817, 13238,
      ##13340, 13780, 14616, 14112, 14077, 13983, 13185,
        ##13273, 13512, 13624, 12947, 12602, 12392,
        12029,
        11562, 11693, 11884, 10940, 10209,
        10209,10209,10209),
    c(##1559, 1564, 1569, 1582, 1593, 1602, 1613,
      ##1621, 1628, 1638, 1649, 1660, 1678, 1694,
      ##1711, 1729, 1745, 1758, 1775, 1788, 1807,
      ##1823, 1840, 1851, 1862, 1882, 1903, 1924,
        ##1943, 1957, 1971, 1985, 2006, 2029,
        2048,
        2061, 2075, 2091, 2112, 2129,
        2129+9, 2129+18, 2129+27),
    c(##52084, 52238, 52484, 52704, 53342, 54013, 54695,
      ##55561, 55951, 56272, 57094, 58041, 58982, 60348,
      ##61505, 62195, 63186, 65051, 66585, 68042, 69355,
      ##70750, 71430, 72026, 73633, 75079, 76644, 78842,
        ##80091, 80751, 81489, 83061, 84862, 86061,
        87393,
        88686, 89311, 89950, 91694, 93157,
        93157+674-9, 93157+2*674-18, 93157 +2022-27))

if (any((t3a[nrow(t3a), 2:3] - 
         t3a[nrow(t3a)-1, 2:3])<0))
    t3a[nrow(t3a), ] <- t3a[nrow(t3a)-1, ]

tail(t3a, 14)
tail(rowSums(t3a), 14)

wcwb <- list(casos=rowSums(t3a), 
             obitos=t3a[,2])
str(wcwb)

##plot(diff(c(0,wcwb[[1]])))

print(sapply(wcwb, function(x) tail(x, 7)))

print(sapply(wcwb, function(x) tail(diff(c(0,x)), 7)))
