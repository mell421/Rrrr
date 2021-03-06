library(shiny)

if(require(shiny)){
  library(wordcloud2)
  #library('rsconnect')
  suppressWarnings(source("./fctR/sources.R"))
  suppressWarnings(library(tidyverse))

  # Define the UI
  ui <- fluidPage(
    titlePanel("text"),
    sidebarLayout(
      sidebarPanel(
        selectInput("fct","source:",
                    list("hp"="hp",
                         "spring"="spring",
                         "texte"="texte",
                         "file"="file"
                    )
        ),
        fileInput("file", label = "your file"),
        textInput("texte", "your text","a z e r t y"),
        numericInput(inputId ="size", 'freq min in the table', 1),
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


      )
    )


  )


  # Define the server code
  server <- function(input, output) {
    output$wordcloud2 <- renderWordcloud2({
      if(input$fct == "hp"){
        hp <- read_lines("./txt/HP1.txt")
        texte <- hp
      } else if(input$fct == "spring"){
        spring <- read_lines("./txt/spring.txt")
        texte <- spring
      } else if(input$fct == "file"){
        filef <- input$file
        texte <- read_lines(filef)
      }

      main <- function(texte){
        TextDoc <- Corpus(VectorSource(texte))

        #Replacing "/", "@" and "|" with space
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
        TextDoc <- tm_map(TextDoc, toSpace, "/")
        TextDoc <- tm_map(TextDoc, toSpace, "@")
        TextDoc <- tm_map(TextDoc, toSpace, "\\|")
        TextDoc <- tm_map(TextDoc, toSpace, "\'")
        TextDoc <- tm_map(TextDoc, toSpace, "\"")
        TextDoc <- tm_map(TextDoc, toSpace, "\>\>")
        TextDoc <- tm_map(TextDoc, toSpace, "\<\<")
        # Convert the text to lower case
        TextDoc <- tm_map(TextDoc, content_transformer(tolower))
        # Remove numbers
        TextDoc <- tm_map(TextDoc, removeNumbers)
        # Remove english common stopwords
        TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
        TextDoc <- tm_map(TextDoc, removeWords, stopwords("french"))

        # Remove your own stop word
        # specify your custom stopwords as a character vector
        TextDoc <- tm_map(TextDoc, removeWords, c("the","and","-"))
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
        dtm_d <- dtm_d %>% filter(freq >= input$size)
        #generate word cloud

        wordcloud2(data = dtm_d, color = input$color, size=1,backgroundColor=input$bgc,shape=input$form)
      }
      main(texte)
    })

    output$table <- renderTable({
      if(input$fct == "hp"){
        hp <- read_lines("./txt/HP1.txt")
        texte <- hp
      } else if(input$fct == "spring"){
        spring <- read_lines("./txt/spring.txt")
        texte <- spring
      } else if(input$fct == "file"){
        filef <- input$file
        texte <- read_lines(filef)
      }

      main <- function(texte){
        TextDoc <- Corpus(VectorSource(texte))

        #Replacing "/", "@" and "|" with space
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        removeSpace <- content_transformer(function (x , pattern ) gsub(pattern, "", x))
        TextDoc <- tm_map(TextDoc, toSpace, "/")
        TextDoc <- tm_map(TextDoc, toSpace, "@")
        TextDoc <- tm_map(TextDoc, toSpace, "\\|")
        TextDoc <- tm_map(TextDoc, toSpace, "\'")
        TextDoc <- tm_map(TextDoc, toSpace, "\“")
        TextDoc <- tm_map(TextDoc, toSpace, "\”")


        # Convert the text to lower case
        TextDoc <- tm_map(TextDoc, content_transformer(tolower))
        # Remove numbers
        TextDoc <- tm_map(TextDoc, removeNumbers)
        # Remove english common stopwords
        TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
        TextDoc <- tm_map(TextDoc, removeWords, stopwords("french"))

        # Remove your own stop word
        # specify your custom stopwords as a character vector
        TextDoc <- tm_map(TextDoc, removeWords, c("the","and","-"))
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

        dtm_d %>% filter(freq >= input$size)
      }
      main(texte)
    })
    output$name <- renderDataTable({

    })
    output$name <- renderDataTable({

    })

  }
  # Return a Shiny app object
  shinyApp(ui = ui, server = server)
}
