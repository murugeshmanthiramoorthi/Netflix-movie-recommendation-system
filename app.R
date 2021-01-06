# This is a basic skeleton of the Shiny app which we will be creating
# This skeleton is created using basic features of Shiny without any beautification contents

library(shiny)
library(shinythemes)
source("recommender.R")

`%notin%` <- Negate(`%in%`)

# Define UI for application
ui <- fluidPage(theme=shinytheme("readable"),
                
    # Application title
    titlePanel("Netflix Recommender App"),
    
    # Sidebar with a slider input for number of bins
    tabsetPanel(tabPanel("Introduction",
                         p("a new movie recommendation engine for our clients to  help the customer to find products she/he wants to buy faster, promote cross-selling by suggesting additional products and can improve customer loyalty through creating a value-added relationship.Collaborative Filtering works on the fact that users with similar behavior will have similar interests in future, and using this notion collaborative filtering recommends items to user. However, the sparseness in data and high dimensionality has become a challenge. To resolve such issues, model based, matrix factorization techniques have well emerged. These techniques have evolved from using simple user-item rating information to auxiliary information such as time and trust.
"),
                         img(src = "netflix.jpg", height = 500, width = 880,style="text-align: center;display: inline-block;")
                         
                         
                         ),
                
                
                
                tabPanel("FM_Model",sidebarLayout(
        sidebarPanel(
            
            textInput("userid",
                      label="User ID",
                      value="Enter User ID..."),
            
            tags$head(tags$style(type="text/css", "
             #loadmessage {
               position: fixed;
               top: 0px;
               left: 0px;
               width: 100%;
               padding: 5px 0px 5px 0px;
               text-align: center;
               font-weight: bold;
               font-size: 100%;
               color: #000000;
               background-color: #CCFF66;
               z-index: 105;
             }
          ")),
            conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                             tags$div("Loading...",id="loadmessage")),
            
            sliderInput("number_of_movies",
                        "How many movies you want to see?",
                        min = 1,
                        max = 50,
                        value = 5),
            selectInput("input_genre1",
                      label="Genre 1",
                      c("Action", "Adventure", "Animation", "Children", 
                        "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                        "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                        "Sci.Fi", "Thriller", "War", "Western")),
            selectInput("input_genre2",
                        label="Genre 2",
                        c("Action", "Adventure", "Animation", "Children", 
                          "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                          "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                          "Sci.Fi", "Thriller", "War", "Western")),
            selectInput("input_genre3",
                        label="Genre 3",
                        c("Action", "Adventure", "Animation", "Children", 
                          "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                          "Film.Noir", "Horror", "Musical", "Mystery","Romance",
                          "Sci.Fi", "Thriller", "War", "Western")),
            sliderInput("release_year",
                        "Select the range of period",
                        min = 1891,
                        max = 2015,
                        value = c(2010,2015))
        )                
        ,
        
          
        
        # Show a plot of the generated distribution
        mainPanel(dataTableOutput("result")
        
            
        ))
    
),
        tabPanel("Team",
                 tags$b('About'),
                 tags$hr(),
                 div(
                     tags$img(src='tbs-logo.png', height=100, width=100), style="text-align: center; margin-bottom:10px;"),
                 h3('This project has been developed by 2020 MSc-D2M students from Toulouse Business School.',style="text-align: center"),
                 div(  
                     div(tags$img(src='enhe.jpg', width=200, height=200),style="width: 20%;text-align: center;display: inline-block;float: left;"),
                     div(tags$img(src='glow.jpg', width=200, height=200),style="width: 20%;text-align: center;display: inline-block;float: left;"),
                     div(tags$img(src='kalai.jfif', width=200, height=200),style="width: 20%;text-align: center;display: inline-block;float: left;"),
                     div(tags$img(src='murugesh.jpg', width=200, height=200),style="width: 20%;text-align: center;display: inline-block;float: left;"),
                     div(tags$img(src='sibyl.jfif', width=200, height=200),style="width: 20%;text-align: center;display: inline-block;float: left;"),
                     style="text-align: center;width: 100%; height: 30px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
                 div(  
                     div(h5("Enhe DONG, MSc Big Data, Marketing and Management"),style="width: 20%; text-align: center;display: inline-block;float:left;border-radius: 40px "),
                     div(h5("Gloria PADHI, MSc Big Data, Marketing and Management"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("Kalairani VIGNESH, MSc Big Data, Marketing and Management"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("Murugesh MANTHIRAMOORTHI, MSc Big Data, Marketing and Management"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("Xingpeng LI(Sibyl), MSc Big Data, Marketing and Management"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;m"),
                     style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;"),
                 div(  
                     div(h5("You can get to know more about her :"),tags$a(href="https://www.linkedin.com/in/xinpengli","here"),style="width: 20%; text-align: center;display: inline-block;float:left;border-radius: 40px "),
                     div(h5("You can get to know more about her :"),tags$a(href="https://www.linkedin.com/in/gloria-gayatri-padhi-b66b3276","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("You can get to know more about her :"),tags$a(href="https://www.linkedin.com/in/kalairani-vignesh","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("You can get to know more about him :"),tags$a(href="https://www.linkedin.com/in/murugesh-manthiramoorthi","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     div(h5("You can get to know more about her :"),tags$a(href="https://www.linkedin.com/in/xinpengli","here"),style="width: 20%;text-align: center;display: inline-block;float: left;border-radius: 40px;"),
                     style="text-align: center;width: 100%; height: 80px; margin-left: auto; margin-right: auto; margin-top:20px; border-radius: 20px;")
                 
)))

# Define server function to show the result-
server <- function(input, output,session) {

    #data1 <- reactive({
     #   validate(
      #      need(input$userid %in% unique(total$userId), "Please check your UserID")
      #  )
       # movies
    #})
    
    
    
    
    
    output$result <- renderDataTable({
            validate(
                need(input$userid %in% unique(total$userId), "Please check your UserID")
                )            
        
            datatable(
                top(movies,input$userid,input$number_of_movies,input$release_year,input$input_genre1,input$input_genre2,input$input_genre3),
                rownames=FALSE
            )
    })
    output$text<-renderText({
       paste("This is intro tab")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
