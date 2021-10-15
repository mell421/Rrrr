# devtools::document("ajoutRep")
# install.packages("gsheet")
# usethis::use_vignette("base_de_donnees_principale")
# devtools::document("ajoutRep")
# devtools::install("ajoutRep")

loadMesSources <- function(){
  source("~/GitHub/monSiteR/fct/accueil.R")
  source("~/GitHub/monSiteR/fct/copy.R")
  source("~/GitHub/monSiteR/fct/nombre.R")
  source("~/GitHub/monSiteR/fct/max.resume.R")
  source("~/GitHub/monSiteR/fct/resume.R")
  source("~/GitHub/monSiteR/fct/page.R")
}
installMesPackages <- function(){
  install.packages("gsheet")
  install.packages("tm")
  install.packages("SnowballC")
  install.packages("wordcloud")
  install.packages("RColorBrewer")
  install.packages("syuzhet")
  install.packages("ggplot2")
  install.packages(utf8)
  install.packages(dplyr)
}



loadMesPackages <- function(){
  library("gsheet")
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer")
  library("syuzhet")
  library("ggplot2")
  library(utf8)
  suppressWarnings(library(tidyverse))
  library(webshot)
  library("htmlwidgets")
  require(devtools)
  library(wordcloud2)
  library(Hmisc)
  library(knitr)
  library(rmarkdown)
  library(car)
  library(dplyr)
  library(ggraph)
  library(tidygraph)
}

loadMesPackages()
loadMesSources()
