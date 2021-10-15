# install.packages('shiny')
#library(shiny)
#library(shinydashboard)
require(devtools)

suppressWarnings(source("./fctR/sources.R"))


# ---------------------------------
header <- dashboardHeader(
    title = "My tvt1me"
)
# ---------------------------------
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Wordcloud", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Graph", tabName = "graph", icon = icon("th")),
        menuItem("Listes", tabName = "widgets", icon = icon("th"))
        # menuItem("Esc", tabName = "esc", icon = icon("th"))

    )
)
# ---------------------------------
sidebarpanel <- sidebarPanel(
    selectInput("fct","function:",
                list("accueil"="accueil",
                     "en_cours"="en_cours",
                     "recent"="recent",
                     "pageappres"="pageappres",
                     "pageappressup"="pageappressup",
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
# ---------------------------------
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
# ---------------------------------
server <- function(input, output) {

    # wordcloud
    output$wordcloud2 <- renderWordcloud2({

        if(input$fct == "copy"){
            copy <- data.frame(copy())
            text <- copy
        } else if(input$fct == "conclu"){
            conclu <- data.frame(aTestConclu())
            text <- conclu[2]
        } else if(input$fct == "conclubis"){
            conclubis <- data.frame(aTestConcluBis())
            text <- conclubis[2]
        } else if(input$fct == "sem"){
            sem <- data.frame(copy.sem())
            text <- sem[2]
        } else if(input$fct == "mois"){
            mois <- data.frame(copy.mois())
            text <- mois[2]
        } else if(input$fct == "max"){
            max <- aTestMax()
            text <- max[2]
        } else if(input$fct == "eff"){
            eff <- aTestEff()
            text <- eff[2]
        } else if(input$fct == "resume"){
            resume <- data.frame(aTestResume())
            text <- resume[2]
        } else if(input$fct == "accueil"){
            accueil <- data.frame(accueil())
            text <- accueil[2]
        } else if(input$fct == "en_cours"){
            accueil <- data.frame(listDesc.ec())
            text <- accueil[2]
        } else if(input$fct == "termine"){
            termine <- data.frame(listDesc.ter())
            text <- termine[2]
        } else if(input$fct == "recent"){
            recent <- data.frame(aAccueil())
            text <- recent$Titre_1
        }  else if(input$fct == "pageappres"){
            pa <- data.frame(pageappres())
            text <- pa[2:16]
        } else if(input$fct == "pageappressup"){
            pas <- data.frame(pageappressup())
            text <- pas[2:16]
        }

        main <- function(text){
            TextDoc <- Corpus(VectorSource(text))

            #Replacing "/", "@" and "|" with space
            toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
            removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
            TextDoc <- tm_map(TextDoc, toSpace, "/")
            TextDoc <- tm_map(TextDoc, toSpace, "@")
            TextDoc <- tm_map(TextDoc, toSpace, "\\|")
            # Convert the text to lower case
            TextDoc <- tm_map(TextDoc, content_transformer(tolower))
            # Remove numbers
            TextDoc <- tm_map(TextDoc, removeNumbers)
            # Remove english common stopwords
            # TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
            # Remove your own stop word
            # specify your custom stopwords as a character vector
            TextDoc <- tm_map(TextDoc, removeWords, c("conclu", "conclubis", "eff","the","titre",
                                                      "mois","sem","conclucompi","conclucompibis"))
            # Remove punctuations
            TextDoc <- tm_map(TextDoc, removePunctuation)
            # Eliminate extra white spaces
            TextDoc <- tm_map(TextDoc, stripWhitespace)
            # Eliminate spaces
            # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


            # Build a term-document matrix
            TextDoc_dtm <- TermDocumentMatrix(TextDoc)
            dtm_m <- as.matrix(TextDoc_dtm)
            # Sort by descearing value of frequency
            dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
            dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)
            # Display the top 20 most frequent words
            head(dtm_d, 30)
            dtm_d <- dtm_d %>% filter(freq >= input$size1,freq <= input$size2)

            #generate word cloud

            wordcloud2(data = dtm_d, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form)
        }
        main(text)
    })
    output$barplot <- renderPlot({
        if(input$fct == "copy"){
            copy <- data.frame(copy())
            text <- copy
        } else if(input$fct == "conclu"){
            conclu <- data.frame(aTestConclu())
            text <- conclu[2]
        } else if(input$fct == "conclubis"){
            conclubis <- data.frame(aTestConcluBis())
            text <- conclubis[2]
        } else if(input$fct == "sem"){
            sem <- data.frame(copy.sem())
            text <- sem[2]
        } else if(input$fct == "mois"){
            mois <- data.frame(copy.mois())
            text <- mois[2]
        } else if(input$fct == "max"){
            max <- aTestMax()
            text <- max[2]
        } else if(input$fct == "eff"){
            eff <- aTestEff()
            text <- eff[2]
        } else if(input$fct == "resume"){
            resume <- data.frame(aTestResume())
            text <- resume[2]
        } else if(input$fct == "accueil"){
            accueil <- data.frame(accueil())
            text <- accueil[2]
        } else if(input$fct == "en_cours"){
            accueil <- data.frame(listDesc.ec())
            text <- accueil[2]
        } else if(input$fct == "termine"){
            termine <- data.frame(listDesc.ter())
            text <- termine[2]
        } else if(input$fct == "recent"){
            recent <- data.frame(aAccueil())
            text <- recent$Titre_1
        } else if(input$fct == "pageappres"){
            pa <- data.frame(pageappres())
            text <- pa[2:16]
        } else if(input$fct == "pageappressup"){
            pas <- data.frame(pageappressup())
            text <- pas[2:16]
        }
        max <- input$size3
        TextDoc <- Corpus(VectorSource(text))

        #Replacing "/", "@" and "|" with space
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
        TextDoc <- tm_map(TextDoc, toSpace, "/")
        TextDoc <- tm_map(TextDoc, toSpace, "@")
        TextDoc <- tm_map(TextDoc, toSpace, "\\|")
        # Convert the text to lower case
        TextDoc <- tm_map(TextDoc, content_transformer(tolower))
        # Remove numbers
        TextDoc <- tm_map(TextDoc, removeNumbers)
        # Remove english common stopwords
        # TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
        # Remove your own stop word
        # specify your custom stopwords as a character vector
        TextDoc <- tm_map(TextDoc, removeWords, c("conclu", "conclubis", "eff","the","titre",
                                                  "mois","sem","conclucompi","conclucompibis"))
        # Remove punctuations
        TextDoc <- tm_map(TextDoc, removePunctuation)
        # Eliminate extra white spaces
        TextDoc <- tm_map(TextDoc, stripWhitespace)
        # Eliminate spaces
        # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


        # Build a term-document matrix
        TextDoc_dtm <- TermDocumentMatrix(TextDoc)
        dtm_m <- as.matrix(TextDoc_dtm)
        # Sort by descearing value of frequency
        dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
        dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)

        barplot(dtm_d[1:max,]$freq, las = 2, names.arg = dtm_d[1:max,]$word,
                col =brewer.pal(8, "Dark2"), main = paste("Top",max,input$fct,sep = " "),
                ylab = "Word frequencies")
    })
    output$table <- renderDataTable({
        if(input$fct == "copy"){
            copy <- data.frame(copy())
            text <- copy
        } else if(input$fct == "conclu"){
            conclu <- data.frame(aTestConclu())
            text <- conclu[2]
        } else if(input$fct == "conclubis"){
            conclubis <- data.frame(aTestConcluBis())
            text <- conclubis[2]
        } else if(input$fct == "sem"){
            sem <- data.frame(copy.sem())
            text <- sem[2]
        } else if(input$fct == "mois"){
            mois <- data.frame(copy.mois())
            text <- mois[2]
        } else if(input$fct == "max"){
            max <- aTestMax()
            text <- max[2]
        } else if(input$fct == "eff"){
            eff <- aTestEff()
            text <- eff[2]
        } else if(input$fct == "resume"){
            resume <- data.frame(aTestResume())
            text <- resume[2]
        } else if(input$fct == "accueil"){
            accueil <- data.frame(accueil())
            text <- accueil[2]
        } else if(input$fct == "en_cours"){
            accueil <- data.frame(listDesc.ec())
            text <- accueil[2]
        } else if(input$fct == "termine"){
            termine <- data.frame(listDesc.ter())
            text <- termine[2]
        } else if(input$fct == "recent"){
            recent <- data.frame(aAccueil())
            text <- recent$Titre_1
        }  else if(input$fct == "pageappres"){
            pa <- data.frame(pageappres())
            text <- pa[2:16]
        } else if(input$fct == "pageappressup"){
            pas <- data.frame(pageappressup())
            text <- pas[2:16]
        }

        main <- function(text){
            TextDoc <- Corpus(VectorSource(text))

            #Replacing "/", "@" and "|" with space
            toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
            removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
            TextDoc <- tm_map(TextDoc, toSpace, "/")
            TextDoc <- tm_map(TextDoc, toSpace, "@")
            TextDoc <- tm_map(TextDoc, toSpace, "\\|")
            # Convert the text to lower case
            TextDoc <- tm_map(TextDoc, content_transformer(tolower))
            # Remove numbers
            TextDoc <- tm_map(TextDoc, removeNumbers)
            # Remove english common stopwords
            # TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
            # Remove your own stop word
            # specify your custom stopwords as a character vector
            TextDoc <- tm_map(TextDoc, removeWords, c("conclu", "conclubis", "eff","titre",
                                                      "mois","sem","conclucompi","conclucompibis"))
            # Remove punctuations
            TextDoc <- tm_map(TextDoc, removePunctuation)
            # Eliminate extra white spaces
            TextDoc <- tm_map(TextDoc, stripWhitespace)
            # Eliminate spaces
            # TextDoc <- gsub("[[:blank:]]", "", TextDoc)


            # Build a term-document matrix
            TextDoc_dtm <- TermDocumentMatrix(TextDoc)
            dtm_m <- as.matrix(TextDoc_dtm)
            # Sort by descearing value of frequency
            dtm_v <- sort(rowSums(dtm_m),decreasing=TRUE)
            dtm_d <- data.frame(word = names(dtm_v) ,freq=dtm_v)
            # Display the top 20 most frequent words



            dtm_d <- dtm_d %>% filter(freq >= input$size1,freq <= input$size2)
        }
        main(text)
    })

    # graph
    output$ec <- renderPlot({
        CopyS <- data.frame(aECocc())
        CopyS <- data.frame(CopyS[1:6])
        CopyS <- na.omit(CopyS)
        if(input$period == "sem"){
            sem <- CopyS %>% filter(occ.ECS >= 1)
            barplot(sem$occ.ECS,
                    las = 2,
                    names.arg = sem$en.cours,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top semaine",
                    ylab = "Word frequencies",
                    ylim=c(0,max(sem$occ.ECS)*1.1))
        } else if(input$period == "mois"){
            mois <- CopyS %>% filter(occ.ECM >= 1)
            barplot(mois$occ.ECM,
                    las = 2,
                    names.arg = mois$en.cours,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top mois",
                    ylab = "Word frequencies",
                    ylim=c(0,max(mois$occ.ECM)*1.1))
        } else if(input$period == "recent"){
            rec <- CopyS %>% filter(occ.ECR >= 1)
            barplot(rec$occ.ECR,
                    las = 2,
                    names.arg = rec$en.cours,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top recent",
                    ylab = "Word frequencies",
                    ylim=c(0,max(rec$occ.ECR)*1.1))
        } else if(input$period == "all"){

            barplot(CopyS$count,
                    las = 2,
                    names.arg = CopyS$tisa.en.cours,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top all",
                    ylab = "Word frequencies",
                    ylim=c(0,max(CopyS$count)*1.1))
        } else if(input$period == "tous"){
            barplot(t(as.matrix(CopyS[2:4])),
                    las = 2,
                    names.arg = CopyS$en.cours,
                    col =c("red","blue","green"),
                    main ="Top ---",
                    ylab = "Word frequencies",
                    beside = TRUE,
                    ylim=c(0,max(CopyS$occ.ECR)*1.1))
            legend('topright',fill=c("red","blue","green"),legend=c('recent','mois','sem'))
        }

    })
    output$pie <- renderPlot({
        CopyS <- data.frame(aECocc())
        CopyS <- data.frame(CopyS[1:6])
        CopyS <- na.omit(CopyS)
        if(input$period == "sem"){
            d1 <- CopyS %>% filter(occ.ECS >= 1) %>% arrange(desc(occ.ECS))
            pie(d1$occ.ECS,label=paste(d1$en.cours,"",d1$occ.ECS))
        } else if(input$period == "mois"){
            d1 <- CopyS %>% filter(occ.ECM >= 1) %>% arrange(desc(occ.ECM))
            pie(d1$occ.ECM,label=paste(d1$en.cours,"",d1$occ.ECM))
        } else if(input$period == "recent"){
            d1 <- CopyS %>% filter(occ.ECR >= 1) %>% arrange(desc(occ.ECR))
            pie(d1$occ.ECR,label=paste(d1$en.cours,"",d1$occ.ECR))
        } else if(input$period == "all"){
            pie(CopyS$count,label=paste(CopyS$tisa.en.cours,"",CopyS$count))
        } else {

        }

    })
    output$ter <- renderPlot({
        CopyS <- data.frame(aECocc())
        CopyS <- data.frame(CopyS[7:12])
        CopyS <- na.omit(CopyS)
        if(input$period == "sem"){
            sem <- CopyS %>% filter(occ.FINIS >= 1)
            barplot(sem$occ.FINIS,
                    las = 2,
                    names.arg = sem$fini,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top semaine",
                    ylab = "Word frequencies",
                    ylim=c(0,max(sem$occ.FINIS)*1.1))
        } else if(input$period == "mois"){
            mois <- CopyS %>% filter(occ.FINIM >= 1)
            barplot(mois$occ.FINIM,
                    las = 2,
                    names.arg = mois$fini,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top mois",
                    ylab = "Word frequencies",
                    ylim=c(0,max(mois$occ.FINIM)*1.1))
        } else if(input$period == "recent"){
            barplot(CopyS$occ.FINIR,
                    las = 2,
                    names.arg = CopyS$fini,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top recent",
                    ylab = "Word frequencies",
                    ylim=c(0,max(CopyS$occ.FINIR)*1.1))
        } else if(input$period == "all"){
            barplot(CopyS$count_1,
                    las = 2,
                    names.arg = CopyS$tisa.terminée,
                    col =brewer.pal(8, "Dark2"),
                    main ="Top all",
                    ylab = "Word frequencies",
                    ylim=c(0,max(CopyS$count_1)*1.1))
        } else if(input$period == "tous"){
            barplot(t(as.matrix(CopyS[2:4])),
                    las = 2,
                    names.arg = CopyS$fini,
                    col =c("red","blue","green"),
                    main ="Top ---",
                    ylab = "Word frequencies",
                    beside = TRUE,
                    ylim=c(0,max(CopyS$occ.FINIR)*1.1))
            legend('topright',fill=c("red","blue","green"),legend=c('recent','mois','sem'))
        }

    })
    output$temps <- renderPlot({
        df <- data.frame(accueil())
        if(input$hms == "hh"){
            barplot(table(df$hh),
                    las = 2,
                    col =brewer.pal(8, "Dark2"),
                    main ="ep per hours",
                    ylab = "nb ep",
                    ylim=c(0,max(table(df$hh))*1.1)
            )
            box()
        } else if(input$hms == "mm"){
            barplot(table(df$minute),
                    las = 2,
                    col =brewer.pal(8, "Dark2"),
                    main ="ep per minutes",
                    ylab = "nb ep",
                    ylim=c(0,max(table(df$minute))*1.1)
            )
            box()
        } else if(input$hms == "ss"){
            barplot(table(df$sec),
                    las = 2,
                    col =brewer.pal(8, "Dark2"),
                    main ="ep per seconds",
                    ylab = "nb ep",
                    ylim=c(0,max(table(df$sec))*1.1)
            )
            box()
        }


    })

    # listes


    output$hist <- renderDataTable({
        data <- data.frame(listDesc.desc())
        data %>% select(tisaep,Horodateur,status)
    })
    output$premder <- renderDataTable({
        data <- data.frame(premder.premder())
        data %>% filter(tisaep.all_1 != "") %>% select(tisaep.all_1,date.premder.all_1,status.all_1)
    })
    output$conclu <- renderDataTable({
        data <- data.frame(aTestConclu())
        data <- data[1:4]
        data %>% filter(conclucompi.... != "")
    })
    output$conclubis <- renderDataTable({
        data <- data.frame(aTestConcluBis())
        data <- data[1:4]
        data %>% filter(conclucompibis.... != "")
    })
    output$mois <- renderDataTable({
        data <- data.frame(copy.mois())
        data <- data[1:5]
        data %>% filter(resumemois.... != "")
    })
    output$sem <- renderDataTable({
        data <- data.frame(copy.sem())
        data <- data[1:5]
        data %>% filter(resumesem.... != "")
    })
    output$eff <- renderDataTable({
        data <- data.frame(aEff())
        data <- data[1:4]
        data %>% filter(eff.... != "")
    })
    output$max <- renderDataTable({
        data <- data.frame(aTestMax())
        data <- data[1:4]
        data %>% filter(max.... != "")
    })
    output$copy <- renderDataTable({
        data <- data.frame(aCopyAll())
        data <- data[1:2]
        data %>% filter(tout != "")
    })
    output$nb <- renderDataTable({
        data <- data.frame(aListes())


        data %>% filter(titre_1 != "") %>% mutate(sum=nbDeLignes+nbPage) %>% rename("nb"=nbDeLignes,"nbBis"=nbPage) %>% select(titre,nb,titre_1,nbBis,sum)
    })

    # esc
    output$eurovision <- renderDataTable({
        esc <- function(){
            df <- suppressWarnings(data.frame(gsheet2tbl('https://docs.google.com/spreadsheets/d/1BTbjPp-CPe_GQNEe-uKq8U3tLUocMM1YzvqPGZ0ScNE/edit#gid=380071250')))
            df
        }
        esc <- esc()
        esc <- esc %>% filter(DF=="FI"||DF=="DF") %>% select(annee:Points,-Country)

        esc
    })
}
# ---------------------------------
shinyApp(ui, server)

# options(encoding = "UTF-8")
# deployApp()

