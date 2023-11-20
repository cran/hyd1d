## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE, 
    comment = "#>", 
    fig.align="center", 
    fig.width = 7, 
    fig.height = 4, 
    root.dir = "vignettes"
)

## ----language, eval = TRUE, include = FALSE-----------------------------------
# set german language
Sys.setlocale(category = "LC_MESSAGES", locale = "de_DE.UTF-8")

## ----captions, echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
library(hyd1d)
library(stringr)
library(yaml)
library(desc)

# Determine the output format of the document
outputFormat <- knitr::opts_knit$get("rmarkdown.pandoc.to")
if (outputFormat == "html") {
    is_html <- TRUE
} else {
    is_html <- FALSE
}

# Figure and Table Caption Numbering, for HTML do it manually
capTabNo <- 1
capFigNo <- 1

# Function to add the Table Number
capTab <- function(x) {
    if(outputFormat == 'html'){
        x <- paste0("**Tab. ", capTabNo, "**: ", x)
        capTabNo <<- capTabNo + 1
    } else if (outputFormat == 'latex'){
        y <- str_replace_all(x, '(^.*)(\\[.*\\])(\\(.*\\))(.*$)', 
                             '\\1\\\\href{\\3}{\\2}\\4')
        y <- gsub("{(", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("{[", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub(")}", "}", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("]}", "}", y, fixed = TRUE, useBytes = TRUE)
        x <- gsub("_", "\\_", y, fixed = TRUE, useBytes = TRUE)
    }
    return(x)
}

# Function to add the Figure Number
capFig <- function(x) {
    if(outputFormat == 'html'){
        x <- paste0("**Abb. ", capFigNo, "**: ", x)
        capFigNo <<- capFigNo + 1
    } else if (outputFormat == 'latex'){
        y <- str_replace_all(x, '(^.*)(\\[.*\\])(\\(.*\\))(.*$)', 
                             '\\1\\\\href{\\3}{\\2}\\4')
        y <- gsub("{(", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("{[", "{", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub(")}", "}", y, fixed = TRUE, useBytes = TRUE)
        y <- gsub("]}", "}", y, fixed = TRUE, useBytes = TRUE)
        x <- gsub("_", "\\_", y, fixed = TRUE, useBytes = TRUE)
    }
    return(x)
}

bf <- function(x) {
    if (outputFormat == 'html') {
        x <- paste0("**", x, "**")
    } else if (outputFormat == 'latex') {
        x <- paste0("\\textbf{", x, "}")
    }
    return(x)
}

href <- function(x, y) {
    if (outputFormat == 'html') {
        x <- paste0("[", x, "](", y, ")")
    } else if (outputFormat == 'latex') {
        x <- paste0("\\href{", y, "}{", x, "}")
    }
    return(x)
}

# Function to simplify linking to references/rd
lrd <- function(x, y) {
    # standard url
    url <- "https://hyd1d.bafg.de"
    
    # url from DESCRIPTION file
    if (file.exists("DESCRIPTION")) {
        url_desc <- description$new("DESCRIPTION")$get_urls()[1]
    }
    
    # url from pkgdown/_pkgdown.yml
    pwd <- Sys.getenv("PWD")
    if (pwd != "") {
        if (file.exists(paste0(pwd, "/pkgdown/_pkgdown.yml"))) {
            url_pkgdown <- yaml.load_file(
                paste0(pwd, "/pkgdown/_pkgdown.yml"))$url
        }
    }
    
    if (exists("url_desc")) {
        url <- url_desc
        if (exists("url_pkgdown")) {
            url <- url_pkgdown
        }
    }
    
    # outputformat latex
    if (knitr::is_latex_output()) {
        if (missing(y)) {
            if (endsWith(x, "()")) {
                x1 <- gsub("()", "", x, fixed = TRUE)
                str <- paste0("[", x, "](", url, "/reference/", x1, ".html)")
            } else {
                str <- paste0("[", x, "](", url, "/reference/", x, ".html)")
            }
        } else {
            str <- paste0("[", x, "](", url, "/reference/", y, ")")
        }
        return(str)
    }
    
    # outputformat html
    if (missing(y)) {
        if (endsWith(x, "()")) {
            # x1 <- gsub("()", "", x, fixed = TRUE)
            str <- paste0("`", x, "`")
        } else {
            str <- paste0("<code>[", x, "](", url, "/reference/", x, ".html)</",
                          "code>")
        }
    } else {
        str <- paste0("<code>[", x, "](", url, "/reference/", y, ")</code>")
    }
    
    return(str)
}

## ----link-pegelonline, eval = is_html, echo = FALSE, results = 'asis'---------
cat('<p style="text-align: center;"><a href="https://pegelonline.wsv.de/gast/start" target="_blank">https://pegelonline.wsv.de/gast/start</a></p>')

## ----figure01, echo = FALSE, error = FALSE, fig.cap = capFig("Tagesgemittelte Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau."), fig.show = 'asis', message = FALSE, warning = FALSE----
library(hyd1d)
xlim_min <- 257
xlim_max <- 263
ylim_min <- 53.8
ylim_max <- 55.7
wldf <- WaterLevelDataFrame(river = "Elbe",
                            time = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))
wldf1 <- waterLevel(wldf, TRUE)
gs <- getGaugingStations(wldf1)
id <- gs$km_qps >= xlim_min & gs$km_qps <= xlim_max

{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "n")
}

## ----link-flys, eval = is_html, echo = FALSE, results = 'asis'----------------
cat('<p style="text-align: center;"><a href="https://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html" target="_blank">FLYS3 – Flusshydrologischer Webdienst</a></p>')

## ----figure02, fig.show = 'asis', fig.cap = capFig("[FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html)-Wasserspiegellagen an der Elbe bei Rosslau und Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
flys3_water_levels <- c("0.5MNQ", "MNQ", "0.5MQ", "a", "0.75MQ", "b", "MQ", "c",
                        "2MQ", "3MQ", "d", "e", "MHQ", "HQ2", "f", "HQ5", "g", 
                        "h", "HQ10", "HQ15", "HQ20", "HQ25", "HQ50", "HQ75", 
                        "HQ100", "i", "HQ150", "HQ200", "HQ300", "HQ500")

{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), ylim = c(53, 62), 
         xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    for (a_wl in flys3_water_levels){
        wldf_temp <- waterLevelFlys3(wldf, a_wl)
        if (a_wl %in% c("0.5MNQ", "MNQ", "MQ", "MHQ", "HQ10", "HQ100", "HQ500")){
            lines(wldf_temp$station, wldf_temp$w, lty = 1, col = "darkblue")
            text(262.0, wldf_temp$w[nrow(wldf_temp)], a_wl, pos = 4, cex = 0.6)
        } else {
            lines(wldf_temp$station, wldf_temp$w, lty = 3, lwd = 0.2, 
                  col = "darkblue")
        }
    }
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 53.5, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 61.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 61.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
}

## ----figure03, fig.show = 'asis', fig.cap = capFig(paste0("Ausgewählte stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), ", ", bf("a"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
mq_0.5 <- waterLevelFlys3(wldf, "0.5MQ")
a <- waterLevelFlys3(wldf, "a")
mq_0.75 <- waterLevelFlys3(wldf, "0.75MQ")
mq <- waterLevelFlys3(wldf, "MQ")

{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    lines(mq_0.5$station, mq_0.5$w, col = "darkblue")
    lines(a$station, a$w, col = "darkblue")
    lines(mq_0.75$station, mq_0.75$w, col = "darkblue")
    text(262, min(mq_0.5$w), "0.5MQ", pos = 4, cex = 0.6)
    text(262, min(a$w), "a", pos = 4, cex = 0.6)
    text(262, min(mq_0.75$w), "0.75MQ", pos = 4, cex = 0.6)
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "n")
}

## ----figure04, fig.show = 'asis', fig.cap = capFig("[FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html)-Wasserspiegellage an der Elbe bei Rosslau und Dessau bei einem Wasserstand von 174 cm am Pegel Wittenberg (Tagesmittelwert des 21.12.2016)."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
wldf2 <- waterLevelFlys3InterpolateY(wldf, "WITTENBERG", shiny = TRUE)
wldf3 <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", shiny = TRUE)
wldf4 <- waterLevelFlys3InterpolateY(wldf, "DESSAU", shiny = TRUE)

{
    plotShiny(wldf2, FALSE, FALSE, FALSE, xlim = c(xlim_min, xlim_max),
              ylim = c(ylim_min, ylim_max))
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "n")
    #text(262.5, 55.5, "Bezugs-\npegel", cex = 0.7)
    text(262, min(wldf2$w), "Bezugspegel\nWITTENBERG", pos = 4, cex = 0.6)
    #text(262, min(wldf3$w), "ROSSLAU", pos = 4, cex = 0.6)
    #text(262, min(wldf4$w), "DESSAU", pos = 4, cex = 0.6)
}

## ----figure05, fig.show = 'asis', fig.cap = capFig("[FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html)-Wasserspiegellagen an der Elbe bei Rosslau und Dessau berechnet mittels der Bezugspegel Wittenberg, Rosslau und Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plotShiny(wldf2, FALSE, FALSE, FALSE, xlim = c(xlim_min, xlim_max),
              ylim = c(ylim_min, ylim_max))
    polygon(x = c(wldf3$station, rev(wldf4$station)),
            y = c(wldf3$w, rev(wldf4$w)), col = "grey95", border = NA)
    lines(wldf2$station, wldf2$w, lty = 1, col = "darkblue")
    lines(wldf3$station, wldf3$w, lty = 2, col = "darkblue")
    lines(wldf4$station, wldf4$w, lty = 3, col = "darkblue")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "n")
    text(262.5, 54.7, "Bezugspegel", cex = 0.7)
    text(262, min(wldf2$w), "WITTENBERG", pos = 4, cex = 0.6)
    text(262, min(wldf3$w), "ROSSLAU", pos = 4, cex = 0.6)
    text(262, min(wldf4$w), "DESSAU", pos = 4, cex = 0.6)
}

## ----link-pegelonline-restapi, eval = is_html, echo = FALSE, results = 'asis'----
cat('<p style="text-align: center;"><a href="https://www.pegelonline.wsv.de/webservice/dokuRestapi" target="_blank">https://www.pegelonline.wsv.de/webservice/dokuRestapi</a></p>')

## ----figure06, fig.show = 'asis', fig.cap = capFig("Relevante Bezugspegel für die Berechnungsstrecke von Kilometer 257 bis 262 an der Elbe bei Rosslau und Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(243, 276.8), 
         ylim = c(51, 58), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # berechnungsstrecke
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
}

## ----figure07, fig.show = 'asis', fig.cap = capFig("Wasserstände an den relevanten Bezugspegeln für die Berechnungsstrecke von Kilometer 257 bis 262 an der Elbe bei Rosslau und Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(243, 276.8), 
         ylim = c(51, 58), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "o", box.col = "white")
    box()
}

## ----figure08, fig.show = 'asis', fig.cap = capFig("Berechnungsabschnitte für die Berechnungsstrecke von Kilometer 257 bis 262 an der Elbe bei Rosslau und Dessau."), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(243, 276.8), 
         ylim = c(51, 58), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # stretch
    polygon(x = c(257, 262, 262, 257),
            y = c(50.8, 50.8, 58.2, 58.2),
            col = "grey95", border = NA)
    
    # sections
    for (i in 1:(nrow(gs) - 1)) {
        rect(gs$km_qps[i], 53, gs$km_qps[i + 1], 54, col = "lightblue", border = NA)
        text((gs$km_qps[i] + gs$km_qps[i + 1])/2, 53.5, i, font = 2)
        lines(rep(gs$km_qps[i], 2), c(53, 54), lwd = 2)
        lines(rep(gs$km_qps[i + 1], 2), c(53, 54), lwd = 2)
    }
    
    # landmarks
    abline(v = gs$km_qps, lty = 3, lwd = 0.5)
    text(gs$km_qps[1:2], c(52, 52), gs$gauging_station[1:2], cex = 0.7)
    text(gs$km_qps[3:4], c(57, 57), gs$gauging_station[3:4], cex = 0.7)
    
    # gauging data
    points(gs$km_qps, gs$wl, pch = 21, col = "darkblue", bg = "darkblue")
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "o", box.col = "white")
    box()
}

## ----figure09, fig.show = 'asis', fig.cap = capFig(paste0("Ausgewählte stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    for (a_wl in flys3_water_levels){
        wldf_temp <- waterLevelFlys3(wldf, a_wl)
        lines(wldf_temp$station, wldf_temp$w, lty = 3, lwd = 0.2, 
              col = "darkblue")
    }
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i], col = "darkblue")
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "darkblue")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(a$w[i]), "a", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6)
    
    # legend
    legend("topright", 
           pch = 21, col = "darkblue", pt.bg = "darkblue", pt.cex = 1,
           legend = "Wasserstand", cex = 0.7, bty = "n")
}

## ----figure10, fig.show = 'asis', fig.cap = capFig(paste0("Ausgewählte stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau und deren relative Lage zwischen den ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen.")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.52)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 4,
         font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 2,
         font = 2, col = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(257.8, max(mq_0.5$w[i]), "0", pos = 2, font = 2)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    text(257.8, max(mq_0.75$w[i]), "1", pos = 2, font = 2, col = "red")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA), 
           lty = c(0, 0, 1, 1), 
           legend = c("Wasserstand", "Gewicht", "obere FLYS-WSL", 
                      "untere FLYS-WSL"), 
           text.font = c(1, 2, 1, 1), 
           text.col = c(1, "darkblue", 1, 1),
           cex = 0.7, bty = "n")
}

## ----figure11, fig.show = 'asis', fig.cap = capFig(paste0("Relatives Gewicht für die Interpolation der Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau zwischen den ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen ", bf("0.5MQ"), " und ", bf("0.75MQ"), ".")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(-0.1, 1.1), xlab = "Flusskilometer (km)", 
         ylab = "relatives Gewicht")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], -0.05, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 1.05, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 1.05, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # weighting
    lines(x = c(gs$km_qps[id][1], gs$km_qps[id][2]),
          y = c(gs$weight_up[id][1], gs$weight_do[id][2]))
    points(gs$km_qps[id][1], gs$weight_up[id][1], pch = 21, col = 1, bg = 1)
    points(gs$km_qps[id][2], gs$weight_do[id][2], pch = 21, col = 1, bg = 1)
}

## ----formula-latex, eval = !is_html, echo = FALSE, results = 'asis'-----------
#  cat('
#  \\begin{center}
#  $WSL = WSL_{0.5MQ} + Gewicht * (WSL_{0.75MQ} - WSL_{0.5MQ})$
#  \\end{center}
#  ')

## ----formula-html, eval = is_html, echo = FALSE, results = 'asis'-------------
cat('<p style="text-align: center;">$WSL = WSL_{0.5MQ} + Gewicht * (WSL_{0.75MQ} - WSL_{0.5MQ})$</p>')

## ----figure12, fig.show = 'asis', fig.cap = capFig(paste0("Interpolierte Wasserspiegellage, ausgewählte stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plot(1, 1, type = "n", xlim = c(xlim_min, xlim_max), 
         ylim = c(ylim_min, ylim_max), xlab = "Flusskilometer (km)", 
         ylab = "H\u00f6he (m \u00fcber NHN (DHHN92))")
    
    # landmarks
    abline(v = gs$km_qps[2:3], lty = 3, lwd = 0.5)
    hyd1d:::.boxed.labels(gs$km_qps[2], 54, gs$gauging_station[2], cex = 0.7,
                          border = FALSE)
    hyd1d:::.boxed.labels(gs$km_qps[3], 55.5, gs$gauging_station[3], cex = 0.7,
                          border = FALSE)
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # gauging data
    points(gs$km_qps[id], gs$wl[id], pch = 21, col = "darkblue", 
           bg = "darkblue")
    
    # FLYS
    i <- which(mq_0.5$station >= gs$km_qps[2] & mq_0.5$station <= gs$km_qps[3])
    lines(mq_0.5$station[i], mq_0.5$w[i])
    lines(mq_0.75$station[i], mq_0.75$w[i], col = "red")
    lines(wldf1$station[i], wldf1$w[i], col = "darkblue", lwd = 2)
    text(261.2, min(mq_0.5$w[i]), "0.5MQ", pos = 4, cex = 0.6)
    text(261.2, min(mq_0.75$w[i]), "0.75MQ", pos = 4, cex = 0.6, col = "red")
    
    # weighting
    text(gs$km_qps[id][1], gs$wl[id][1], round(gs$weight_up[id][1], 2), pos = 2,
         cex = 0.6, font = 2, col = "darkblue")
    text(gs$km_qps[id][2], gs$wl[id][2], round(gs$weight_do[id][2], 2), pos = 4,
         cex = 0.6, font = 2, col = "darkblue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 2, 1, 1),
           legend = c("Wasserstand", "Gewicht", "waterLevel", "obere FLYS-WSL", 
                      "untere FLYS-WSL"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           text.font = c(1, 2, 1, 1, 1),
           cex = 0.7, bty = "n")
}

## ----figure13, fig.show = 'asis', fig.cap = capFig(paste0("Interpolierte Wasserspiegellage, berechnungsrelevante stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), ", ", bf("a"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), echo = FALSE, error = FALSE, warning = FALSE, message = FALSE----
{
    plotShiny(wldf1, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              ylim = c(ylim_min, ylim_max))
    
    # landmark
    abline(v = 259.6, lty = 3, lwd = 0.5, col = "blue")
    hyd1d:::.boxed.labels(259.6, 55.5, "MULDE", cex = 0.7, border = FALSE,
                          col = "blue")
    
    # legend
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 2, 1, 1),
           legend = c("Wasserstand", "Gewicht", "waterLevel", "obere FLYS-WSL", 
                      "untere FLYS-WSL"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           cex = 0.7, bty = "n")
    box()
}

## ----install-cran, eval = FALSE-----------------------------------------------
#  install.packages("hyd1d")

## ----install-git, eval = FALSE------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github("bafg-bund/hyd1d")

## ----library, eval = FALSE----------------------------------------------------
#  library(hyd1d)

## ----wldf, eval = TRUE--------------------------------------------------------
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

## ----str-summary, eval = TRUE-------------------------------------------------
str(wldf)

summary(wldf)

## ----waterlevel-summary, eval = TRUE------------------------------------------
wldf <- waterLevel(wldf)
summary(wldf)

## ----figure14, fig.show = 'asis', fig.cap = capFig(paste0("Interpolierte Wasserspiegellage, berechnungsrelevante stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), ", ", bf("a"), " und ", bf("0.75MQ"), ") und Wasserstände vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), eval = TRUE----
wldf <- waterLevel(wldf, shiny = TRUE)
summary(wldf)

xlim_min <- 257
xlim_max <- 263
plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max))
legend("topright", 
       col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
       pch = c(21, NA, NA, NA, NA), 
       pt.bg = c("darkblue", NA, NA, NA, NA), 
       pt.cex = c(1, NA, NA, NA, NA), 
       lty = c(0, 0, 1, 1, 1), 
       lwd = c(0, 0, 2, 1, 1),
       legend = c("Wasserstand", "Gewicht", "waterLevel", "obere FLYS-WSL", 
                  "untere FLYS-WSL"), 
       text.col = c(1, "darkblue", 1, 1, 1),
       cex = 0.7, bty = "n")

## ----figure15, fig.show = 'asis', fig.cap = capFig(paste0("Interpolierte Wasserspiegellage, berechnungsrelevante stationäre ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen und Wasserstände vom ", strftime(Sys.time() - 3600, format = "%d.%m.%Y %H:%M")," Uhr an der Elbe bei Rosslau und Dessau.")), eval = hyd1d:::.pegelonline_status()----
# one hour ago
time <- as.POSIXct(Sys.time() - 3600)

# initialize a WaterLevelDataFrame
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = time,
                            station = seq(257, 262, 0.1))

# compute w
wldf <- waterLevelPegelonline(wldf, shiny = TRUE)
summary(wldf)

# and plot the results
plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max))
legend("topright", 
       col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
       pch = c(21, NA, NA, NA, NA), 
       pt.bg = c("darkblue", NA, NA, NA, NA), 
       pt.cex = c(1, NA, NA, NA, NA), 
       lty = c(0, 0, 1, 1, 1), 
       lwd = c(0, 0, 2, 1, 1),
       legend = c("Wasserstand", "Gewicht", "waterLevel", "obere FLYS-WSL", 
                  "untere FLYS-WSL"), 
       text.col = c(1, "darkblue", 1, 1, 1),
       cex = 0.7, bty = "n")

## ----figure16-prep------------------------------------------------------------
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

wldf1 <- waterLevelFlood1(wldf, "ROSSLAU", shiny = TRUE)
summary(wldf1)

wldf2 <- waterLevelFlood1(wldf, "DESSAU", shiny = TRUE)
summary(wldf2)

wldf3 <- waterLevelFlood2(wldf)
summary(wldf3)

## ----figure16, fig.show = 'asis', fig.cap = capFig("Wasserspiegellagen nach Flut1 mit den Bezugspegeln Rosslau (wldf1) und Dessau (wldf2) und Flut2 (wldf3) für den 21.12.2016 an der Elbe bei Rosslau und Dessau."), eval = TRUE, echo = FALSE----
df.gs2 <- getGaugingStations(wldf2)

{
    plotShiny(wldf1, FALSE, FALSE, FALSE, xlim = c(xlim_min, xlim_max))
    lines(wldf2$station, wldf2$w, col = "darkblue", lty = 2)
    lines(wldf3$station, wldf3$w, col = "red", lty = 2)
    abline(v = df.gs2$km_qps, lty = 3, lwd = 0.5)
    points(df.gs2$km_qps, df.gs2$wl, pch=21, col="darkblue", bg="darkblue")
    hyd1d:::.boxed.labels(df.gs2$km_qps, 55.4, df.gs2$gauging_station,
                          bg="white", srt = 90, border = FALSE, xpad = 4,
                          ypad = 0.7, cex = 0.7)
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red"), 
           pch = c(21, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA), 
           lty = c(0, 1, 2, 2), 
           lwd = c(0, 1, 1, 1), 
           legend = c("Wasserstand", "wldf1", "wldf2", "wldf3"), 
           cex = 0.7, bty = "n")
}

## ----figure17-prep------------------------------------------------------------
wldf <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", shiny = TRUE)
summary(wldf)

## ----figure17, fig.show = 'asis', fig.cap = capFig(paste0("Wasserspiegellage nach ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), " mit dem Bezugspegel Rosslau für den 21.12.2016 an der Elbe bei Rosslau und Dessau.")), eval = TRUE, echo = FALSE----
{
    plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max))
    abline(v = df.gs2$km_qps, lty = 3, lwd = 0.5)
    points(df.gs2$km_qps, df.gs2$wl, pch=21, col="darkblue", bg="darkblue")
    hyd1d:::.boxed.labels(df.gs2$km_qps, 55.4, df.gs2$gauging_station,
                          bg="white", srt = 90, border = FALSE, xpad = 4,
                          ypad = 0.7, cex = 0.7)
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 2, 1, 1),
           legend = c("Wasserstand", "Gewicht", "waterLevel", "obere FLYS-WSL", 
                      "untere FLYS-WSL"), 
           text.col = c(1, "darkblue", 1, 1, 1),
           cex = 0.7, bty = "n")
}

## ----link-waterlevel, eval = is_html, echo = FALSE, results = 'asis'----------
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/waterlevel/" target="_blank">https://shiny.bafg.de/waterlevel/</a></p>')

## ----figure20, echo = FALSE, fig.cap = capFig(paste0("Screenshot der ", href("waterLevel-ShinyApp", "https://shiny.bafg.de/waterlevel/"), " mit der interpolierten Wasserspiegellage, berechnungsrelevanten stationären ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("0.5MQ"), ", ", bf("a"), " und ", bf("0.75MQ"), ") und Wasserständen vom 21.12.2016 an der Elbe bei Rosslau und Dessau.")), fig.show = 'asis', out.width = "100%", results = 'asis'----
knitr::include_graphics('screenshot_waterLevel.png')

## ----link-waterlevelpegelonline, eval = is_html, echo = FALSE, results = 'asis'----
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/waterlevelpegelonline/" target="_blank">https://shiny.bafg.de/waterlevelpegelonline/</a></p>')

## ----figure21, echo = FALSE, fig.cap = capFig(paste0("Screenshot der ", href("waterLevelPegelonline-ShinyApp", "https://shiny.bafg.de/waterlevelpegelonline/"), " mit der interpolierten Wasserspiegellage und den berechnungsrelevanten stationären ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-Wasserspiegellagen (", bf("a"), ", ", bf("0.75MQ"), " and ", bf("0.5MQ"), ") und Wasserständen vom 13.04.2018 11:00 Uhr an der Elbe bei Rosslau und Dessau.")), fig.show = 'asis', out.width = "100%"----
knitr::include_graphics('screenshot_waterLevelPegelonline.png')

## ----link-hydflood, eval = is_html, echo = FALSE, results = 'asis'------------
cat('<p style="text-align: center;"><a href="https://hydflood.bafg.de" target="_blank">https://hydflood.bafg.de</a></p>')

