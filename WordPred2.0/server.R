library(shiny)
library(dplyr)
library(stringr)
library(hunspell)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
    uni_words <- readRDS("uni_words.rds")
    bi_words <- readRDS("bi_words.rds")
    tri_words <- readRDS("tri_words.rds")
    quad_words <- readRDS("quad_words.rds")
    penta_words <- readRDS("penta_words.rds")
    
    
    # The prediction app
    pred_words <- function(str){
        if (str == ""){
            list_word <- list(first = "",
                              second = "",
                              third = "")
        }
        else{
            require(quanteda)
            length_word = str_count(str,"\\w+")
            tokens <- tokens(x = char_tolower(str))
            tokens <- char_wordstem(tokens[[1]], language = "english")
            
            if(length_word == 1){
              predicted2bi <- bi_words %>% filter(One_Word == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predicted2bi)->predicted2bi
              list_word <- list(bifirst = predicted2bi[1, 2],
                                  bisecond = predicted2bi[2, 2],
                                  bithird = predicted2bi[3, 2])
              
            }
            else if(length_word == 2){
              predictedtri <- tri_words %>% filter(One_Word == tokens[length_word-1], Two_Words == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedtri)->predictedtri
              listtrigrams <- list(trifirst = predictedtri[1, 3],
                                   trisecond = predictedtri[2, 3],
                                   trithird = predictedtri[3, 3])
              
              
              predictedbi <- bi_words %>% filter(One_Word == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedbi)->predictedbi
              listbigrams <- list(bifirst = predictedbi[1, 2],
                                  bisecond = predictedbi[2, 2],
                                  bithird = predictedbi[3, 2])
              list_word <- c(listtrigrams,listbigrams)
            }
            else if (length_word ==3){
              predictedquad <- quad_words %>% filter(One_Word == tokens[length_word-2], Two_Words == tokens[length_word-1], Three_Words==tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedquad)->predictedquad
              listquadgrams <- list(quadfirst = predictedquad[1, 4],
                                    quadsecond = predictedquad[2, 4],
                                    quadthird = predictedquad[3, 4])
              
              
              
              
              predictedtri <- tri_words %>% filter(One_Word == tokens[length_word-1], Two_Words == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedtri)->predictedtri
              listtrigrams <- list(trifirst = predictedtri[1, 3],
                                   trisecond = predictedtri[2, 3],
                                   trithird = predictedtri[3, 3])
              
              
              predictedbi <- bi_words %>% filter(One_Word == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedbi)->predictedbi
              listbigrams <- list(bifirst = predictedbi[1, 2],
                                  bisecond = predictedbi[2, 2],
                                  bithird = predictedbi[3, 2])
              list_word <- c(listquadgrams,listtrigrams,listbigrams) 
            }
            else if(length_word >= 4){
              predictedpenta <- penta_words %>% filter(One_Word == tokens[length_word-3], Two_Words == tokens[length_word-2], Three_Words==tokens[length_word-1], Four_Words==tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedpenta)->predictedpenta
              listpentagrams <- list(pentafirst = predictedpenta[1, 5],
                                    pentasecond = predictedpenta[2, 5],
                                    pentathird = predictedpenta[3, 5])
              
              
              predictedquad <- quad_words %>% filter(One_Word == tokens[length_word-2], Two_Words == tokens[length_word-1], Three_Words==tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedquad)->predictedquad
              listquadgrams <- list(quadfirst = predictedquad[1, 4],
                                   quadsecond = predictedquad[2, 4],
                                   quadthird = predictedquad[3, 4])
              
              
            
              
              predictedtri <- tri_words %>% filter(One_Word == tokens[length_word-1], Two_Words == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedtri)->predictedtri
              listtrigrams <- list(trifirst = predictedtri[1, 3],
                                   trisecond = predictedtri[2, 3],
                                   trithird = predictedtri[3, 3])
              
              
              predictedbi <- bi_words %>% filter(One_Word == tokens[length_word]) %>% arrange(desc(Frequency))
              
              as.data.frame(predictedbi)->predictedbi
              listbigrams <- list(bifirst = predictedbi[1, 2],
                                  bisecond = predictedbi[2, 2],
                                  bithird = predictedbi[3, 2])
            list_word <- c(listpentagrams,listquadgrams,listtrigrams,listbigrams)
            }
            
          list_word<-list_word[!is.na(list_word)]
          if(sum(!is.na(list_word))==0){
            list_word<-list(
              check1= hunspell_suggest(tokens[length_word])[[1]][1],
              check2= hunspell_suggest(tokens[length_word])[[1]][2],
                check3= hunspell_suggest(tokens[length_word])[[1]][3]
            )
          }
        }
        return(list_word)
    }
    
    
    spell_check  <- function(str){
      length_word = str_count(str,"\\w+")
      tokens <- tokens(x = char_tolower(str))
      tokens <- char_wordstem(tokens[[1]], language = "english")
      
      x<-hunspell_suggest(tokens[length_word])[[1]][1]
       return(x)
      }
    
    button <- reactive({
    pred_words(input$inputString)
    })
    
    spell <- reactive({
      spell_check(input$inputString)
    })
    
    
    
    output$show <- renderUI({
        tags$div(
            actionButton("predict1", label = button()[[1]]), 
            actionButton("predict2", label = spell()),
            actionButton("predict3", label = button()[[3]]))
    })
    
    observeEvent(input$predict1, {
        updateTextInput(session, "inputString", value = paste(input$inputString, button()[[1]]))
    }) 
    observeEvent(input$predict2, {
        updateTextInput(session, "inputString", value = paste(gsub("\\s*\\w*$", "", input$inputString), spell()))
    }) 
    observeEvent(input$predict3, {
        updateTextInput(session, "inputString", value = paste(input$inputString, button()[[3]]))
    }) 
    
})