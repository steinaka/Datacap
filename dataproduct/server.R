

verbose <<- TRUE
debug <- TRUE


library(shiny)
#library(shinyjs)


options(shiny.trace = debug)
source("utils.R", local = TRUE)
 load("data/ngramLookupTables.en_US.RData")
 load("data/ngramLookupTablesProf.en_US.RData")
 load("data/ngramLookupTables1.en_US.RData")
 load("data/ngramLookupTables2.en_US.RData")
 
 
 

shinyServer(function(input, output, session) {
  output$suggest <- renderText({
    updateTypeahead(input$text, input$AHCH)
  })
  
  output$debug = renderText({
    updateTypeahead(input$typedText, input$AHCH)
  })
  
  updateTypeahead <- function(text, AHCH){
    log(paste("input:",text))
    if(!is.null(text) && nchar(text)>0){
      endOfWord <- grepl("[^a-z]$", text)
      preprocessedText <- preProcessString(text)
      if(nchar(preprocessedText)>0){
        #suggestion <- predictSuggest(text, AHCH, completeWord = endOfWord)
        if(AHCH == "ngramLookupTablesProf1"){
          suggestions <- predictTop(preprocessedText, ngramLookupTablesProf1, completeWord = endOfWord)}
        
        else if(AHCH == "ngramLookupTablesProf2"){
          suggestions <- predictTop(preprocessedText, ngramLookupTablesProf2, completeWord = endOfWord)}
        else if(AHCH == "ngramLookupTablesProf"){
        suggestions <- predictTop(preprocessedText, ngramLookupTablesProf, completeWord = endOfWord)}
        else{suggestions <- predictTop(preprocessedText, ngramLookupTables, completeWord = endOfWord)}
        log(suggestions[[1]])
        
        dataset <- suggestionsToTypeAhead(text,suggestions,endOfWord)
        
        tokens <- unname(sapply(dataset$sentence, strsplit, split=" "))
        session$sendCustomMessage(type = "updateSuggestions", list(
          id="text"
          ,placeholder="Type a sentence"
          ,local=dataset
          ,valueKey="sentence"
          ,tokens=tokens
          ,template = HTML("<p class='suggest'>{{suggestion}}</p>")
          )
        )
        paste(dataset[1,"sentence"])
      }else{
        ""
      }
    }else{
      ""  
    }
  }
# 
 suggestionsToTypeAhead <- function(text, suggestions, endOfWord){
   suggestions$sentence <- sapply(suggestions$lookup, concatWithOverlap, str1=text)
   if(endOfWord){
     suggestions$suggestion <- suggestions$suggest
     suggestions$sentence <- paste0(suggestions$sentence, suggestions$suggest)
   }else{
     suggestions$suggestion <- paste(suggestions$lookup, suggestions$suggest)
     suggestions$sentence <- paste(suggestions$sentence, suggestions$suggest)
    }
    
    suggestions[,c("suggestion", "sentence")]
  }
  
  
})
