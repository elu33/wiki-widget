

library(shiny)
library(httr)
library(tidyverse)
library(jsonlite)
library(rvest)
library(tools)

regex <- "(<span class=\"searchmatch\">)"   #this will be used later to filter text

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wiki-Widget"),

    # Sidebar input 
    sidebarLayout(
        sidebarPanel(
            textInput(inputId = "searchterm",
                        label = "Enter Initial Search Query:"),
            
            actionButton(inputId = "go",
                         label = "search"),
            
            numericInput(inputId = "delve",
                         label = "I want to look further at result #:",
                         value = 1, min = 1, max = 50
                
            ),
            
            actionButton(inputId = "inquire",
                         label = "show result summary"),
            
            actionButton(inputId = "subheadshow",
                         label = "show webpage subheadings/tags"),
            
            actionButton(inputId = "everything",
                         label = "show full content of webpage")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           verbatimTextOutput("info"),
           verbatimTextOutput("hits"),
           verbatimTextOutput("results"),
           verbatimTextOutput("snippet"),
           verbatimTextOutput("subcontent1"),
           verbatimTextOutput("all")
        )
    )
)


server <- function(input, output) {

    data1 <- reactiveValues(hits = NULL, results = "results appear here",
                            snippet = "a short summary appears here",
                            subcontent2 = "topics from the Wikipedia webpage appear here",
                            all = "The full contents of the Wikipedia webpage appear here")
    errors <- reactiveValues(one = 0)
    
    
    #purpose: take in a searchterm, check for spaces, use the media-wiki API to return 
    # the number of hits from the searchterm, as well as the title of the results. This is updated
    #everytime the user hits the search button
    observeEvent(input$go,{        

        data1$urlbase <- if(str_detect(input$searchterm, " ")){
            modified <- str_split(input$searchterm, " ")
            paste0("https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=",
                   modified[[1]][1],"%20",modified[[1]][2], "&format=json") #limitation: only handles on or two word phrases
        }   
        else{paste0("https://en.wikipedia.org/w/api.php?action=query&list=search&srsearch=",
                          input$searchterm, "&format=json")
        }
        
        data1$hits <- fromJSON(content(GET(data1$urlbase),as = "text", encoding = "UTF-8"))$query$searchinfo$totalhits
        
        data1$results <- fromJSON(content(GET(data1$urlbase),as = "text", encoding = "UTF-8"))$query$search$title
        

    })
    #purpose: After the searchterm results have been shown to the user, they can choose the result that
    #they want to look further into. They enter in an integer that corresponds with the result they want
    #to look at. Using the media-wiki API, a summary generated from the API that corresponds to the searchterm
    #since certain searchterms results can be several words long, spaces are accounted for. this is executed
    #when the show summary button is pressed, meaning the user can input one searchterm and look at several
    #different result summaries by changing the input number
    observeEvent(input$inquire,{  
        
        if(length(data1$results)==0){   #ensures a that user has input a valid searchterm
            errors$one <- 1
        }
        if(input$delve <= length(data1$results) && input$delve > 0){    #ensures user inputs a valid number
        errors$one <- 0
        data1$snippet <- gsub("</span>", "", fromJSON(content(GET(data1$urlbase),as = "text", encoding = "UTF-8"))$query$search$snippet[input$delve] %>%
            str_remove_all(regex))
        }else{errors$one <- 1}
    })
    
    #purpose: After the user has had the chance to enter a searchterm, look at relevant results and 
    #corresponding summaries, scrapes the webpage of a selected searchterm result.
    #this allows users to look at subheadings/important tags of the webpage.
    
    observeEvent(input$subheadshow, {  
        if(errors$one ==1){     #if an input is already invalid, prevent code from running and crashing
            errors$one <- 1
        }
        else{
        if(length(data1$results)==0){  #removes possibility that somone enters invalid search term
            errors$one <- 1             # and tries to get a subheading without inquiring
        }
        else{
        errors$one <- 0
        data1$updatedsearchterm <- fromJSON(content(GET(data1$urlbase),as = "text", encoding = "UTF-8"))$query$search$title[input$delve]
        
        data1$the_url <- if(str_detect(data1$updatedsearchterm, " ")){
            paste0("https://en.wikipedia.org/wiki/",gsub(" ", "%20", data1$updatedsearchterm))
        }else{
            paste0("https://en.wikipedia.org/wiki/",data1$updatedsearchterm)
        }
        
        #after having the newly updated search term, go to that physical website and scrape
        #for content headings
         data1$subcontent <- read_html(data1$the_url) %>%
             html_nodes("li") %>%
             html_node("a") %>%
             html_attr("href") %>%
             na.omit() %>%
             str_remove_all("wiki")
         
         data1$subcontent_vector <- data1$subcontent %>% str_detect("#")
         data1$subcontent1 <- data1$subcontent[data1$subcontent_vector] %>% na.omit()

         data1$x <- input$delve - input$delve    #making it equal to zero, and also reset each time new input
         for(i in 1:length(data1$subcontent1)){
             data1$x <- data1$x+1
             if(str_detect(data1$subcontent1[i], "References")){ 
                 break
             }
         }
         data1$subcontent2 <- data1$subcontent1[1:data1$x-1]
        }
    }       
    })
    
    #purpose: for a given searchterm and corresponding result, scrape the text from its corresponding
    #wikipedia page
    observeEvent(input$everything,{     
        #the updated searchterm and url is also put under this action button in case a user
        #doesnt want to display subheadings but wants to see the whole text so that url and searchterm
        #can be calculated indpendent of subheadings and prevent a crash due to using uncalculated variables
        
        if(errors$one ==1){     # if an input is already invalid, prevent code from running and crashing
            errors$one <- 1
        }
        else{
        
        if(length(data1$results)==0){   #eliminates possible error if a search term is invalid
            errors$one <- 1             # and user immediately clicks show everything
        }
        else{
        errors$one <- 0
        data1$updatedsearchterm <- fromJSON(content(GET(data1$urlbase),as = "text", encoding = "UTF-8"))$query$search$title[input$delve]
        
        data1$the_url <- if(str_detect(data1$updatedsearchterm, " ")){
            paste0("https://en.wikipedia.org/wiki/",gsub(" ", "%20", data1$updatedsearchterm))
        }else{
            paste0("https://en.wikipedia.org/wiki/",data1$updatedsearchterm)
        }

        
        data1$all <- read_html(data1$the_url) %>%
            html_nodes("div#mw-content-text") %>%
            html_nodes("div.mw-parser-output") %>%
            html_nodes("p") %>% html_text()
        }
    }
    })
                 
    
    output$hits <- renderPrint(#API content
        paste("Number of results returned:",data1$hits)
    )
    
    output$results <- renderPrint(#API content
        if(is.null(data1$results)){
            paste("no valid results")
        }
        else{data1$results}
    )
    output$snippet <- renderPrint(#API content
        if(errors$one ==1){
            paste("no valid summary")
        }
        else{data1$snippet}
    )
    
    output$subcontent1 <- renderPrint(#webscraped content
        if(errors$one==1){
            paste("no valid subcontent")
        }
        else{data1$subcontent2}
    )
    
    output$all <- renderPrint(#webscraped content
        if(errors$one ==1){
            paste("no valid content")
        }
        else{data1$all}
    )
    
    output$info <- renderPrint(
        paste("PLEASE READ: Due to the highly diverse layout of every wikipedia",
              "page on the internet, this app is optimized for searching famous people, famous",
              "historical events, and general things/places, such as glass or water or uc davis.",
              "Highly specific or less popular queries are given less priority and less detail")
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
