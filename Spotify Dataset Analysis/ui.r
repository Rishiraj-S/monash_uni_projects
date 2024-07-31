# Unpacking all the required libraries
# Remove comment and install them from below if required

#install.packages("shinythemes")
#install.packages("shinycssloaders")
library(shinycssloaders)
library(shiny)
library(shinythemes)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Loading the wrangled personal spotify dataset
spotifydf <- read_csv("spotifydf.csv")
totalminsstreamed <- sum(spotifydf$minutesPlayed)

# Defining the shiny UI
shinyUI(navbarPage("Your Spotify Summary",
                   theme = shinytheme("flatly"), # Using the flatly theme
  tabPanel("Intro",
           fluidRow(
             column(9,
                    h1('Hey there!'),
                    h5("This app is my analysis and understanding of the", span(tags$a(href="https://www.spotify.com/us/wrapped/","Spotify Wrapped")), "feature as a part of the Data Visualization Project for FIT5147 at Monash University."),
                    h5("Here, I have analyzed my personal Spotify streaming history from July 2021 to July 2022 as well as compared it with the most streamed songs worldwide the same time period to draw conclusions about my taste in music and tried to answer the following questions:"),
                    h5("1. What are my Top 5 Streamed artists and Top 5 Streamed songs in the given period and how many minutes did I spend listening to each for the entire period?"),
                    h5("2. How unique is my 2021 streaming history compared to the global streaming history in 2021 (July to December)?")
                    ),
             column(3,tags$img(src='spotifylogo.png', align="right", height = "204px", width = "248px"), tags$figcaption(span(tags$a(href="https://www.pngitem.com/middle/mwTiwb_spotify-logo-small-spotify-logo-transparent-hd-png/","Ref"))))),
           fluidRow(
             column(12,
                    h1("How to use the app?"),
                    h5("Use the tabs on the top to navigate through the app. Here's what to expect in each tab:"),
                    h5("1. ",span(strong("Your Streams:"))," It has details of the tracks I have streamed in the time period"),
                    h5("2. ",span(strong("Listening with the World:"))," Details on the common tracks between my streams and the top global streams")
                    )
           ),
           fluidRow(
             column(12,
                    h5("Made by:"),
                    h5("Rishiraj Sinharay"),
                    h5("Student ID: 32435851"),
                    h5("Tutor: Nina Sophia Dsouza"))
           )),
  tabPanel("Your Streams",
           fluidRow(
             column(6,
                    h2("Your Top Tracks"),
                    h5("The plot shows the Top 5 Tracks you listened to the most."),
                    helpText("Hover over each lollipop to know details like Track Name, Artist Name, minutes streamed and number of time the track was streamed"),
                    plotlyOutput("top5tracks") %>% withSpinner()),
             column(6,
                    h2("Your Top Artists"),
                    h5("The plot shows the Top 5 Artists you listened to the most"),
                    helpText("Hover over each lollipop to know details like Artist Name, minutes streamed and number of time the track was streamed"),
                    plotlyOutput("top5artists")%>% withSpinner())),
           fluidRow(h2('Your Minutes Listened')),
           fluidRow(sidebarPanel(width = 3,
                                 h5('In this duration, you spent a total of'),h1(paste(totalminsstreamed)),h5(' minutes on', span(tags$a(href="https://open.spotify.com/","Spotify")) , "streaming music and podcasts."),
                                 helpText('Clicking the Play button in the adjacent plot will show the minutes streamed every month.'),
                                 helpText('Hover over the kinks in the displayed graph for details like Minutes streamed and Number of Streams for the particular month')),
                    mainPanel(h3("Minutes Streamed Monthly"),plotlyOutput("timeseries")%>% withSpinner()))),
  tabPanel("Listening with the World",
           fluidRow(
             sidebarPanel(width=3,
                          h2("What you had common with the world?"),
                          h5("This section shows the Top 5 tracks streamed by you which were also the most streamed songs globally.")
                          ),
             mainPanel(
               plotOutput("top5common")%>% withSpinner())),
           fluidRow(sidebarPanel(width = 3,
                                 h2("What kind of songs do you prefer?"),
                                 h5("On comparison with the most streamed songs globally, certain characteristics of the common tracks popped out.
                                    This is an attempt to figure out the kind of songs you listened to based on their characteristics."),
                                 helpText("Choose a characteristic to see the scatter plot and the trend line."),
                                 radioButtons("characteristic", NULL,
                                              c("Acousticness" = "Acousticness",
                                                "Danceability" = "Danceability",
                                                "Energy" = "Energy",
                                                "Loudness" = "Loudness",
                                                "Speechiness" = "Speechiness",
                                                "Tempo" = "Tempo"))),
                    mainPanel(column(10,plotlyOutput("scatter", height = 500)%>% withSpinner()),
                              column(2,textOutput("chardescription"))))
             )
           ))