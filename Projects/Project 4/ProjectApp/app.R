#Libraries
library(data.table)
library(DT)
library(readr)
library(recommenderlab)
library(shiny)
library(shinydashboard)
library(shinyjs)
library(ShinyRatingInput)
library(shinythemes)
library(tidyverse)
#Helper functions for app
source('functions/helpers.R')
#Import Genre selection data
GenreRec = read_delim("GenreRecommend.txt", 
                             delim = "\t", escape_double = FALSE, trim_ws = TRUE)
#Import average ratings data
AppData = read_delim("AppData.txt",
                     delim = "\t", escape_double = FALSE, trim_ws = TRUE)
#Importing the UBCF results
UBCF = readRDS("UBCFresults.rds")
IDs = read.table("IDs.txt", quote = "\"", comment.char="") #IDs
# Read in movie data (Feng's Code)
movies = readLines("movies.dat")
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)
movies$Title = iconv(movies$Title, "latin1", "UTF-8")

small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))
#List of genres for usage in the app
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
# Start of Feng's code
get_user_ratings = function(value_list) {
  dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                    function(x) ifelse(length(x) > 1, x[[2]], NA)),
                   Rating = unlist(as.character(value_list)))
  dat = dat[!is.null(Rating) & !is.na(MovieID)]
  dat[Rating == " ", Rating := 0]
  dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
  dat = dat[Rating > 0]
}

# Define UI for application
ui <- navbarPage("Movie Recommender App", theme = shinytheme("cosmo"),
      #Main Page
      tabPanel("Main Page",
               #Text for main page
               fluidPage(
                   h1("How To Use This App", align = "center"),
                   p("Welcome to our movie recommender app. If you are looking for a 
             fantastic new movie to watch, you have come to the right place. Our app 
             provides two different ways to discover new movies."),
                   br(),
                   p(strong("Genre: "),"Select your genre of interest, and our app 
                   will return the top 10 highest rated movies from that genre."),
                   br(),
                   
                   p(strong("Rating Input: "),"Rank the movies presented to you that
                    you have seen from 1-5 stars, and our app 
                   will return movies based on your recommendations."),
               )),
      #Genre Selection Page
      tabPanel("Genre Selection",
               fluidPage(
                 h1("Genre Selection"),
                 #Genre user input for selection
                 selectInput(inputId = "Genre",
                             label = "Please select a movie genre.",
                             choices = c("", genre_list)),
                 #Output based on user input
                 mainPanel(uiOutput("images"), #Images for top 10 movies
                           uiOutput("genre_data")) #Top 10 movies with ratings
               )),
      #User Input Page
      tabPanel("Rating Input Selection",
               fluidPage(
                 h1("Rating Input Selection"),
                 fluidRow(
                   box(width = 12, title = "Step 1: Rate Movies", 
                       status = "info", solidHeader = TRUE,
                       uiOutput('ratings')
                   )
                 ),
                 fluidRow(
                   useShinyjs(),
                   box(
                     width = 12, status = "info", solidHeader = TRUE,
                     title = "Step 2: Discover Movies",
                     br(),
                     withBusyIndicatorUI(
                       actionButton("btn", "Get Movie Recommendations")
                     ),
                     br(),
                     tableOutput("results")
                   )
                 )
                 
               )),
      #About Page text
      tabPanel("About",
               p("This app was created by Paul C. Holaway, Albert Li, & 
                 Matt Schroeder for STAT542; Statistical Learning Fall 2022, 
                 at the University of Illinois Urbana-Champaign under the supervision
                 of Professor Feng Liang."))

)

server <- function(input, output) {
  #Data table output for genre selection page.     
  output$genre_data <- renderUI({
    if(input$Genre == ""){
      #If no user input is given, then the table is blank.
      GenreRec = data.frame(matrix(ncol = 3, nrow = 0))
      colnames(GenreRec) <- c("Image","Title","Rating")
      GenreRec %>% select(Title, Rating) %>% 
        datatable(class = "nowrap hover row-border", escape = FALSE, 
        options = list(dom = 't', scrollX = TRUE, autoWidth = FALSE))
    } else {
      #Returning data from top 10 movies by genre data
      GenreRec %>% select(contains(input$Genre)) %>% 
        rename(Image = paste0(input$Genre,".Image")) %>%
        rename(Title = paste0(input$Genre,".Title")) %>%
        rename(Rating = paste0(input$Genre,".Rating")) %>%
        select(Title, Rating) %>%
        datatable(class = "nowrap hover row-border", escape = FALSE, 
                  options = list(dom = 't', scrollX = TRUE, autoWidth = FALSE))
    }
  })
  #Printing movie poster images for genre selection page.
  output$images <- renderUI({
    if(input$Genre == ""){
      #Prints out nothing if no user input given.
      print("")
    } else {
      
      num_rows <- 2
      num_movies <- 5 # movies per row
      
      movies = GenreRec %>% select(contains(input$Genre)) %>% 
        rename(Image = paste0(input$Genre,".Image")) %>%
        rename(Title = paste0(input$Genre,".Title")) %>%
        rename(Rating = paste0(input$Genre,".Rating"))
      #Printing of the movie images for the top 10 by genre.
      lapply(1:num_rows, function(i) {
        list(fluidRow(lapply(1:num_movies, function(j) {
          list(box(width = 2,
                   div(style = "text-align:center", 
                       img(src = movies$Image[(i - 1) * num_movies + j], height = 150))
          ))})))
      })
    }
  })
  
  output$ratings <- renderUI({
    num_rows <- 20
    num_movies <- 5 # movies per row
    #Random sample of movies to review; selecting the top ranked movies for familiarity.
    samp = AppData %>% filter(No.Reviews > 50) %>% arrange(desc(Avg.Rating)) %>% head(250)
    samp = samp[sample(nrow(samp), size = 100, replace = FALSE),]
    samp$image_url = sapply(samp$MovieID, 
                              function(x) paste0(small_image_url, x, '.jpg?raw=true'))
    #Outputting randomly selected movies for review
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        list(box(width = 2,
                 div(style = "text-align:center", 
                     img(src = samp$image_url[(i - 1) * num_movies + j], height = 150)),
                 div(style = "text-align:center", strong(samp$Title[(i - 1) * num_movies + j])),
                 div(style = "text-align:center; font-size: 150%; color: #ceb888;", 
                     ratingInput(paste0("select_", samp$MovieID[(i - 1) * num_movies + j]), 
                                 label = "", dataStop = 5)))) #00c0ef
      })))
    })
  })
  # Calculate recommendations when the submission button is clicked
  df <- eventReactive(input$btn, {
    withBusyIndicatorServer("btn", { # showing the busy indicator
      # hide the rating container
      useShinyjs()
      jsCode <- "document.querySelector('[data-widget=collapse]').click();"
      runjs(jsCode)
      
      # get the user's rating data
      value_list <- reactiveValuesToList(input)
      user_ratings <- get_user_ratings(value_list)
      #Preparing the test data
      user_ratings$MovieID = paste0("m",user_ratings$MovieID)
      
      ids = colnames(getModel(UBCF)$data@data)
      test_data = data.frame(MovieID = ids) %>% left_join(user_ratings, "MovieID")
      test_data$UserID = rep("u9999", nrow(test_data))
      
      test_data = data.frame(UserID = test_data$UserID,
                             MovieID = test_data$MovieID,
                             Rating = as.integer(test_data$Rating),
                             stringsAsFactors = TRUE) %>% drop_na()

      Rmat = sparseMatrix(dims = c(1,length(ids)),as.integer(test_data$UserID), 
                          as.integer(test_data$MovieID), x = test_data$Rating)
      rownames(Rmat) = levels(test_data$UserID)
      colnames(Rmat) = levels(test_data$MovieID)

      Rmat = new('realRatingMatrix', data = Rmat)
      #Doing the prediction based on user input
      p.UBCF <- predict(UBCF, Rmat, type = "ratings")
      gc(verbose = FALSE)
      pred_results <- data.frame(as.character(IDs[,1]), as.numeric(as(p.UBCF, "matrix")))
      colnames(pred_results) <- c("MovieID", "Results")
      pred_results = pred_results %>% drop_na(Results) %>% arrange(desc(Results)) %>% head(10)
      pred_results = data.frame(as.numeric(str_remove(pred_results$MovieID, "m")))
      colnames(pred_results) <- c("MovieID")
      
      user_predicted_ids = pred_results[1:10,1]
      recom_results <- data.table(Rank = 1:10, 
                                  MovieID = movies$MovieID[user_predicted_ids], 
                                  Title = movies$Title[user_predicted_ids])
    }) # still busy
    
  }) # clicked on button
  
  
  # display the recommendations
  output$results <- renderUI({
    num_rows <- 2
    num_movies <- 5
    recom_result <- df()
    
    lapply(1:num_rows, function(i) {
      list(fluidRow(lapply(1:num_movies, function(j) {
        box(width = 2, status = "success", 
            solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
            
            div(style = "text-align:center", 
                a(img(src = movies$image_url[recom_result$MovieID[(i - 1) * num_movies + j]], 
                      height = 150))
            ),
            div(style="text-align:center; font-size: 100%", 
                strong(movies$Title[recom_result$MovieID[(i - 1) * num_movies + j]])
            )
            
        )        
      }))) # columns
    }) # rows
    
  }) # renderUI function

}

# Run the application 
shinyApp(ui = ui, server = server)