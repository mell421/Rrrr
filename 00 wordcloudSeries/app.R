library(shiny)
library('rsconnect')
suppressWarnings(source("./fctR/sources.R"))
suppressWarnings(library(tidyverse))
if(require(shiny)){
    library(wordcloud2)

    # Define the UI
    ui <- fluidPage(
        titlePanel("my wordcloud"),
        sidebarLayout(
            sidebarPanel(
                selectInput("fct","function:",
                            list("accueil"="accueil",
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
                numericInput(inputId ="size1", 'min freq:', 1),
                # numericInput(inputId ="size2", 'max freq:', 1000),

                textInput(inputId ="color","color(random-light,random-dark,other)", "random-light"),
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
                textInput(inputId ="bgc","backgroundcolor", "black"),
            ),
            mainPanel(
                tabsetPanel(
                    type = "tabs",
                    tabPanel("wordcloud",
                             wordcloud2Output('wordcloud2')
                    ),
                    tabPanel("table",
                             tableOutput('table')
                    )
                )

                # wordcloud2Output('wordcloud2'),
                # tableOutput('table')
            )
        )


    )


    # Define the server code
    server <- function(input, output) {
        dataInput <- function(){
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
                sem <- data.frame(aTestSem())
                text <- sem[2]
            } else if(input$fct == "mois"){
                mois <- data.frame(aTestMois())
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
                accueil <- data.frame(listDesc.ec())
                text <- accueil[2]
            } else if(input$fct == "termine"){
                termine <- data.frame(listDesc.ter())
                text <- termine[2]
            } else if(input$fct == "recent"){
                recent <- data.frame(aAccueil())
                text <- recent$Titre_1
            }

        }

        output$wordcloud2 <- renderWordcloud2({
            dataInput()

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
                dtm_d <- dtm_d %>% filter(freq >= input$size1)

                #generate word cloud

                wordcloud2(data = dtm_d, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form)
            }
            main(text)
        })

        output$table <- renderTable({
            dataInput()

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



                dtm_d <- dtm_d %>% filter(freq >= input$size1)
            }
            main(text)
        })
    }
    # Return a Shiny app object
    shinyApp(ui = ui, server = server)
}
