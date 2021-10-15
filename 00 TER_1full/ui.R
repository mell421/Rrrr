ui <- function(){
  library(shiny)
  library(shinydashboard)
  require(devtools)
  library(wordcloud2)
  # library('rsconnect')
  suppressWarnings(source("./fctR/sources.R"))
  suppressWarnings(library(tidyverse))
  header <- dashboardHeader(
    title = "My application"
  )

  sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Wordcloud", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Graph", tabName = "graph", icon = icon("th")),
      menuItem("Listes", tabName = "widgets", icon = icon("th"))
      # menuItem("Esc", tabName = "esc", icon = icon("th"))

    )
  )

  sidebarpanel <- sidebarPanel(
    selectInput("fct","function:",
                list("accueil"="accueil",
                     "en_cours"="en_cours",
                     "recent"="recent",
                     "copy"="copy",
                     "conclu"="conclu",
                     "conclubis"="conclubis",
                     "sem"="sem",
                     "mois"="mois",
                     "max"="max",
                     "eff"="eff",
                     "resume"="resume",
                     "termine"="termine"
                )
    ),

    selectInput("form","form:",
                list("circle"="circle",
                     "cardioid"="cardioid",
                     "diamond"="diamond",
                     "triangle-forward"="triangle-forward",
                     "triangle"="triangle",
                     "pentagon"="pentagon",
                     "star"="star"
                )
    ),
    numericInput(inputId ="size1", 'min freq:', 1),
    numericInput(inputId ="size2", 'max freq:', 1000),
    numericInput(inputId ="size3", 'nb in barplot:', 30),
    textInput(inputId ="color","color(random-light,random-dark,other)", "random-light"),

    textInput(inputId ="bgc","backgroundcolor", "black"),
    textAreaInput("texte","text:","")

  )

  ui <- dashboardPage(
    header,
    sidebar,
    dashboardBody(
      tabItems(
        # First tab content
        tabItem(tabName = "dashboard",
                sidebarLayout(
                  sidebarpanel,
                  mainPanel(
                    tabsetPanel(
                      type = "tabs",
                      tabPanel("wordcloud",
                               wordcloud2Output('wordcloud2')
                      ),
                      tabPanel("table",
                               dataTableOutput('table')
                      ),
                      tabPanel("barplot",
                               plotOutput('barplot')
                      )
                    )
                  )
                )
        ),
        tabItem(tabName = "graph",
                column(3,
                       selectInput("period","periode:",
                                   list("sem"="sem",
                                        "mois"="mois",
                                        "recent"="recent",
                                        "all"="all",
                                        "tous"="tous"
                                   )
                       )
                ),
                column(3,
                       selectInput("hms","hh mm ss:",
                                   list("hh"="hh",
                                        "mm"="mm",
                                        "ss"="ss"
                                   )
                       )
                ),
                tabsetPanel(
                  type = "tabs",
                  tabPanel("ec",
                           plotOutput('ec')
                  ),
                  tabPanel("pie",
                           plotOutput('pie')
                  ),
                  tabPanel("ter",
                           plotOutput('ter')
                  ),
                  tabPanel("temps",
                           plotOutput('temps')
                  )
                )


        ),
        # Second tab content
        tabItem(tabName = "widgets",

                tabsetPanel(
                  type = "tabs",

                  tabPanel("hist",
                           p("historique"),
                           dataTableOutput('hist')
                  ),
                  tabPanel("nb",
                           dataTableOutput('nb')
                  ),
                  tabPanel("premder",
                           dataTableOutput('premder')
                  ),
                  tabPanel("conclu",
                           dataTableOutput('conclu')
                  ),
                  tabPanel("conclubis",
                           dataTableOutput('conclubis')
                  ),
                  tabPanel("sem",
                           dataTableOutput('sem')
                  ),
                  tabPanel("mois",
                           dataTableOutput('mois')
                  ),
                  tabPanel("eff",
                           dataTableOutput('eff')
                  ),
                  tabPanel("max",
                           dataTableOutput('max')
                  ),
                  tabPanel("copy",
                           dataTableOutput('copy')
                  )

                )


        )
        # tabItem(tabName = "esc",
        #        p("eurovision"),
        #        dataTableOutput('eurovision')
        #)
      )
    )
  )

}
