
# devtools::install_github("r-lib/remotes")
# .rs.restartR()

#
# if(!"shinysky" %in% rownames(installed.packages())){
  # devtools::install_github("ShinySky","AnalytixWare")
# }
# install.packages("remotes")
# 
# devtools::install_github("AnalytixWare/ShinySky")
# remotes::install_github("AnalytixWare/ShinySky")
#   devtools::install_github("AnalytixWare/ShinySky")
#   devtools::install_github('rstudio/shinyapps')
# ##https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/

library(shiny)
library(shinysky)

#library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  #useShinyjs(),
  tags$head(
    tags$script(src="extra.js"),
    tags$link(rel = "stylesheet", type = "text/css", href = "extra.css")
  ),
  
  mainPanel(
    navbarPage("Adult Filter", tabPanel("Adult Filter",sidebarPanel(
      selectInput("AHCH", label = h4("Adult or Child"),
                  choices = list("Child" = "ngramLookupTables","Child+" = "ngramLookupTables1","Child++" = "ngramLookupTables2", "Adult" = "ngramLookupTablesProf")),
    ), p("Adult filter was a fun thought to do, i realised that my adult list scrubbing is not entirely complete. However, if you look at the variants, you can see that not all have the same predictions. I do need to work further for this to better perform, but i think this is good here for the assignment. Please do not take results too seriously. Thought it would be fun to do something like that."),
    ), tabPanel("Normal", h3(""),
                
        div("",
        br(),br()
                                                                                     
                                                                                 )
    )),
    
    titlePanel("Predictive wording"),
    
    
    
    
    
    p("Simplified predictive text model using n-grams. It allows for non-character input, but works better without."),
    p("Use up/down arrow keys & enter to select a suggestion or the right arrow key to tigger the autocomplete."),
    textInput.typeahead(
      id="text"
      ,placeholder="Type a sentence"
      ,local=data.frame(sentence=c(),suggestion=c())
      ,valueKey="sentence"
      ,tokens=c()
      ,template = HTML("<p class='suggest'>{{suggestion}}</p>")
    )
    ,br(),br(),
    verbatimTextOutput("suggest"),
    verbatimTextOutput("debug")
    
    
    
    
  ),
  
))
