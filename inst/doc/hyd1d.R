## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
    collapse = TRUE,
    comment = "#>",
    fig.align="center",
    fig.width = 7,
    fig.height = 4, 
    root.dir = "vignettes"
)

## ----captions, echo = FALSE, error = FALSE, warning = FALSE, message = FALSE, include = FALSE----
library(hyd1d)
library(plotrix)
library(stringr)
library(yaml)
library(desc)

# set english locale to produce english plot labels
Sys.setlocale(category = "LC_MESSAGES", locale = "en_US.utf8")

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
capTab <- function(x){
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
capFig <- function(x){
    if(outputFormat == 'html'){
        x <- paste0("**Fig. ", capFigNo, "**: ", x)
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

href <- function(x, y) {
    if (outputFormat == 'html') {
        x <- paste0("[", x, "](", y, ")")
    } else if (outputFormat == 'latex') {
        x <- paste0("\\href{", y, "}{", x, "}")
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

## ----install_git, eval = FALSE------------------------------------------------
#  install.packages("devtools")
#  library(devtools)
#  devtools::install_github("bafg-bund/hyd1d")

## ----library, eval = TRUE, echo = TRUE, error = FALSE, warning = FALSE, message = FALSE----
library(hyd1d)

## ----wldf, eval = TRUE--------------------------------------------------------
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

## ----structure, eval = TRUE---------------------------------------------------
str(wldf)
summary(wldf)

## ----waterlevel, eval = TRUE--------------------------------------------------
wldf <- waterLevel(wldf)
summary(wldf)

## ----figure1, fig.show = 'asis', fig.cap = capFig("Interpolated water level, computation-relevant stationary [FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html) water levels (**0.5MQ**, **a** and **0.75MQ**) and gauge height as of 2016-12-21 at River Elbe between Rosslau and Dessau."), eval = TRUE----
wldf <- waterLevel(wldf, shiny = TRUE)
summary(wldf)

xlim_min <- 257
xlim_max <- 263
{
    plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              xlab = "river station (km)",
              ylab = "elevation (m a.s.l. (DHHN92))")
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 0.6, 0.6), 
           legend = c("gauge height", "gauge weight", "waterLevel", 
                      "upper FLYS w.l.", "lower FLYS w.l."), 
           text.col = c(1, "darkblue", 1, 1, 1), 
           cex = 0.7, bty = "n")
}

## ----figure2, fig.show = 'asis', fig.cap = capFig(paste0("Interpolated water level, computation-relevant stationary [FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html) water levels (**0.5MQ**, **a** and **0.75MQ**) and gauge height as of ", strftime(Sys.time() - 3600, format = "%Y-%m-%d %H:%M")," at River Elbe between Rosslau and Dessau.")), eval = TRUE----
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
{
    plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              xlab = "river station (km)",
              ylab = "elevation (m a.s.l. (DHHN92))")
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 0.6, 0.6), 
           legend = c("gauge height", "gauge weight", "waterLevel", 
                      "upper FLYS w.l.", "lower FLYS w.l."), 
           text.col = c(1, "darkblue", 1, 1, 1), 
           cex = 0.7, bty = "n")
}


## ----figure3, fig.show = 'asis', fig.cap = capFig("Water levels computed according to the Flood1-method with the reference gauges Rosslau (wldf1) and Dessau (wldf2) and the Flood2-method as of 2016-12-21 at River Elbe between Rosslau and Dessau."), eval = TRUE----
wldf <- WaterLevelDataFrame(river   = "Elbe",
                            time    = as.POSIXct("2016-12-21"),
                            station = seq(257, 262, 0.1))

wldf1 <- waterLevelFlood1(wldf, "ROSSLAU", shiny = TRUE)
summary(wldf1)

wldf2 <- waterLevelFlood1(wldf, "DESSAU", shiny = TRUE)
summary(wldf2)

wldf3 <- waterLevelFlood2(wldf)
summary(wldf3)

df.gs2 <- getGaugingStations(wldf2)

{
    plotShiny(wldf1, FALSE, FALSE, FALSE, xlim = c(xlim_min, xlim_max),
              xlab = "river station (km)",
              ylab = "elevation (m a.s.l. (DHHN92))")
    lines(wldf2$station, wldf2$w, col = "darkblue", lty = 2)
    lines(wldf3$station, wldf3$w, col = "red", lty = 2)
    abline(v = df.gs2$km_qps, lty = 3, lwd = 0.5)
    points(df.gs2$km_qps, df.gs2$wl, pch=21, col="darkblue", bg="darkblue")
    boxed.labels(df.gs2$km_qps, 55.4, df.gs2$gauging_station, bg="white", 
                 srt = 90, border = FALSE, xpad = 4, ypad = 0.7, cex = 0.7)
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red"), 
           pch = c(21, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA), 
           lty = c(0, 1, 2, 2), 
           lwd = c(0, 1, 1, 1), 
           legend = c("gauge height", "wldf1", "wldf2", "wldf3"), 
           cex = 0.7, bty = "n")
}

## ----figure4, fig.show = 'asis', fig.cap = capFig("Water levels according to [FLYS3](http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html) with the reference gauge Rosslau as of 2016-12-21 at River Elbe between Rosslau and Dessau."), eval = TRUE----
wldf <- waterLevelFlys3InterpolateY(wldf, "ROSSLAU", shiny = TRUE)
summary(wldf)

{
    plotShiny(wldf, TRUE, TRUE, TRUE, xlim = c(xlim_min, xlim_max),
              xlab = "river station (km)",
              ylab = "elevation (m a.s.l. (DHHN92))")
    abline(v = df.gs2$km_qps, lty = 3, lwd = 0.5)
    points(df.gs2$km_qps, df.gs2$wl, pch=21, col="darkblue", bg="darkblue")
    boxed.labels(df.gs2$km_qps, 55.4, df.gs2$gauging_station, bg="white", 
                 srt = 90, border = FALSE, xpad = 4, ypad = 0.7, cex = 0.7)
    legend("topright", 
           col = c("darkblue", "darkblue", "darkblue", "red", "black"), 
           pch = c(21, NA, NA, NA, NA), 
           pt.bg = c("darkblue", NA, NA, NA, NA), 
           pt.cex = c(1, NA, NA, NA, NA), 
           lty = c(0, 0, 1, 1, 1), 
           lwd = c(0, 0, 1, 0.6, 0.6), 
           legend = c("gauge height", "gauge weight", "waterLevel", 
                      "upper FLYS w.l.", "lower FLYS w.l."), 
           text.col = c(1, "darkblue", 1, 1, 1), 
           cex = 0.7, bty = "n")
}

## ----link_waterlevel, eval = is_html, echo = FALSE, results = 'asis'----------
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/waterlevel/" target="_blank">https://shiny.bafg.de/waterlevel/</a></p>')

## ----figure20, echo = FALSE, fig.cap = capFig(paste0("Screenshot of the ", href("waterLevel-ShinyApp", "https://shiny.bafg.de/waterlevel/"), " with the interpolated water level, caomputationrevelvant stationary ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-waterlevels (", bf("0.5MQ"), ", ", bf("a"), " and ", bf("0.75MQ"), ") and gauge heights at 2016-12-21 at the River Elbe between Rosslau and Dessau, Germany.")), fig.show = 'asis', out.width = "100%", results = 'asis'----
knitr::include_graphics('screenshot_waterLevel.png')

## ----link_waterlevelpegelonline, eval = is_html, echo = FALSE, results = 'asis'----
cat('<p style="text-align: center;"><a href="https://shiny.bafg.de/waterlevelpegelonline/" target="_blank">https://shiny.bafg.de/waterlevelpegelonline/</a></p>')

## ----figure21, echo = FALSE, fig.cap = capFig(paste0("Screenshot of the ", href("waterLevelPegelonline-ShinyApp", "https://shiny.bafg.de/waterlevelpegelonline/"), " with the interpolated water level, computationrevelvant stationary ", href("FLYS3", "http://www.bafg.de/DE/08_Ref/M2/03_Fliessgewmod/01_FLYS/flys_node.html"), "-waterlevels (", bf("a"), ", ", bf("0.75MQ"), " and ", bf("0.5MQ"), ") and gauge heights at 2018-04-13 11:00 a.m. at the River Elbe between Rosslau and Dessau, Germany.")), fig.show = 'asis', out.width = "100%"----
knitr::include_graphics('screenshot_waterLevelPegelonline.png')

## ----link_hydflood, eval = is_html, echo = FALSE, results = 'asis'------------
cat('<p style="text-align: center;"><a href="https://hydflood.bafg.de" target="_blank">https://hydflood.bafg.de</a></p>')

