# Unpacking all the required libraries
# Remove comment and install them from below if required

#install.packages("lubridate")
#install.packages("plotly")
#install.packages("tidyverse")
#install.packages("shiny")
#install.packages("dplyr")
#install.packages("readr")
library(lubridate)
library(tidyverse)
library(ggplot2)
library(shiny)
library(plotly)
library(dplyr)
library(readr)

# Loading the datasets made during the Data Exploration Process

# Loading the wrangled personal spotify dataset
spotifydf <- read_csv("spotifydf.csv")

# Loading the wrangled combined dataset
combineddf <- read_csv("cleandata2.csv")

# Splitting the "endTime" column into "Date" and "Time"
spotifydf <- spotifydf %>%
  separate(endTime, c("Date", "Time"), " ")

#Converting the "Date" column to date type
spotifydf$Date <- ymd(spotifydf$Date)

# Grouping by month
timedf <- spotifydf %>%
  group_by(month= lubridate::ceiling_date(Date, 'month')) %>%
  # Summarizing minutes played monthly and monthly streams
  summarise("month_minutes" = sum(minutesPlayed), "streams"= length(month))

# Total minutes streamed in the entire time period
totalminsstreamed <- sum(spotifydf$minutesPlayed)

# Grouping and summarizing for each track
toptracks <- spotifydf %>%
  group_by(trackName, artistName) %>%
  summarise("streams"=length(trackName), "minsplayed"=sum(minutesPlayed))

# Ordering toptracks by the streams
toptracks <- toptracks[order(-toptracks$streams),]

# Slicing out the top 5 tracks
top5tracksdf <- toptracks[1:5,]

# Grouping and summarizing for each artist
topartists <- spotifydf %>%
  group_by(artistName) %>%
  summarise("streams"= length(artistName), "mins"=sum(minutesPlayed))

# Ordering top artists by the streams
topartists <- topartists[order(-topartists$streams),]

# Slicing out the top 5 artists
top5artistsdf <- topartists[1:5,]

# Defining the shiny server
shinyServer(function(input, output, session){
  
  # Plot for the top 5 tracks
  output$top5tracks <- renderPlotly({
    
    # Using the plot_ly function to make lollipop chart
    toptracksplot <- plot_ly(top5tracksdf, #Using the dataframe with the Top 5 tracks
                             x=~top5tracksdf$streams, # streams column on x-axis
                             y=~ top5tracksdf$trackName, # trackName on y-axis
                             type = 'scatter', # For lollipop heads
                             # Text for tooltip
                             text = paste("Track Name:", top5tracksdf$trackName, "<br>",
                                                            "Artist Name:", top5tracksdf$artistName,"<br>",
                                                            "Streams:", top5tracksdf$streams, "<br>",
                                                            "Minutes Streamed:", top5tracksdf$minsplayed),
                             hoverinfo="text",
                             mode='markers', # For lollipop heads
                             marker = list(color = "#1DB954",size = 30, opacity = 1), # color w.r.t Spotify colors
                             orientation = "h") # For horizontal plot
    toptracksplot <- toptracksplot %>% add_bars( # For lollipop stems
      y= ~top5tracksdf$trackName, # trackName on y-axis
      x= ~top5tracksdf$streams, # streams on x-axis
      width = 0.05 # bar width
    ) %>% layout(
      xaxis = list(
        title = "Minutes Streamed", # x-axis label
        zeroline = F
      ),
      yaxis = list(
        title = "Track Name", # y-axis label
        zeroline = F,
        categoryorder='total ascending' # arranging in ascending order of streams
      ), showlegend = FALSE
    )
  })
  
  # Plot for top 5 artists
  output$top5artists <- renderPlotly({
    
    # Using plot_ly function to make lollipop chart
    topartistsplot <- plot_ly(top5artistsdf, # using the top 5 artists dataframe
                              x=~top5artistsdf$streams, # streams as x-axis
                              y=~ top5artistsdf$artistName, # artistname as y-axis
                              type = 'scatter', # for lollipop heads
                              # Text for tooltip
                              text = paste("Artist Name:", top5artistsdf$artistName,"<br>",
                                           "Streams:", top5artistsdf$streams, "<br>",
                                           "Minutes Streamed:", top5artistsdf$mins),
                              hoverinfo="text",
                              mode='markers', # For Lollipop headsa
                              marker = list(color = "#1DB954",size = 30, opacity = 1), # Color is w.r.t spotify colors
                              orientation = "h") # Horizontal plot
    topartistsplot  <- topartistsplot %>% add_bars( # For lollipop stems
      y= ~top5artistsdf$artistName, # artistName is y-axis
      x= ~top5artistsdf$streams, # streams is x-axis
      width = 0.05 # width of bar
    ) %>% layout(
      xaxis = list(
        title = "Minutes Streamed", # x-axis label
        zeroline = F
      ),
      yaxis = list(
        title = "Artist Name", # y-axis label
        zeroline = F,
        categoryorder='total ascending' # arranging in ascending order of streams
      ), showlegend = FALSE 
    )
  })
  
  # Animated Timeseries in plotly
  
  # Defining a function to accumulate the frames for the animation
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x){
      cbind(dat[var %in% lvls[seq(1,x)],], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  
  # accumylating w.r.t the month column
  timedf <- timedf %>% accumulate_by(~month)
  
  # Defining the timeseries plot
  myplot <- timedf %>%
    plot_ly(
      x=~month, # month is x-axis
      y=~month_minutes, # month_minutes is y-axis
      frame = ~frame, 
      type = 'scatter', # For points on the plot
      mode='lines', # Points joined by lines to make a line plot
      fill='tozeroy', # For area plot
      fillcolor="#1DB954", # Color w.r.t spotify
      line=list(color="#191414"),
      # Text for tooltip
      text = ~paste(month,"<br>","Minutes streamed:", month_minutes,"<br>",
                    "Number of streams:",streams),
      # Text to be displayed on hover
      hoverinfo='text'
    )
  
  myplot <- myplot %>%layout(
    xaxis = list(
      title = "Month", # x-axis label
      zeroline = F,
      range=c(min(timedf$month), max(timedf$month)) # setting x-axis range from the minimum to maximum dates in the month column
    ),
    yaxis = list(
      title = "Minutes Streamed", # y- axis label
      zeroline = F,
      range=c(0, 8000) # y-axis range from 0-8000
    )
  )
  myplot <- myplot %>% animation_opts(
    frame=100, # animation with 100 frames
    transition=0,
    redraw = FALSE
  )
  myplot <- myplot %>% animation_slider(
    active = 95,
    currentvalue = list(
      prefix = "Month"
    )
  )
  
  
  # rendering the timeseries plot and asssigning it to the output variable
  output$timeseries <- renderPlotly({
    myplot
  })
  
  # Now we will use the merged dataframe from our Data Exploration Project to show the top 5 common tracks
  
  # Grouping the combineddf by trackName and artistName
  # summarising for the number of streams and total minutes each track has been played 
  commontracks <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # Ordering the commontracks by the number of streams
  commontracks <- commontracks[order(-commontracks$streams),]
  
  # Assigning the output plot for the top 5 common streams
  output$top5common <- renderPlot({
    
    # Using ggplot() to plot a circular bar plot
    ggplot(commontracks[1:5,], # Using the top 5 values from the commontracks dataset
           aes(x= reorder(trackName,streams),# trackName as x-axis, Ordering by streams
               y= streams))+ #Streams as y-axis
      geom_bar(stat='identity', fill=alpha("#1DB954", 1))+ # bar chart in spotify colors
      xlab("Track Names")+ # x-axis label
      ylab("Number of streams")+ # y-axis label
      ylim(-180,180)+ # ploar limits
      ggtitle("What I had Common with the World!")+ # Plot title
      coord_polar(start=0)+ # for circular bar plot
      # Plot labels
      geom_text(stat="identity",position="identity",aes(x= reorder(trackName,streams), y= streams, label = paste(trackName,"\n",artistName,"\n Streams:",streams), vjust=0))+
      # Theme settings minimal for neat plot
      theme_minimal()+
      theme(
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1,20), "cm") 
      )
  })
  # For characteristic plots:
  
  # Creating dataframes for all different characteristics
  
  # For Acousticness
  acousticdf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Acousticness"= mean(Acousticness), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # For Danceability
  dancedf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Danceability"= mean(Danceability), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # For Energy
  energydf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Energy"= mean(Energy), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # For Loudness
  louddf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Loudness"= mean(Loudness), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # For Speechiness
  speechdf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Speechiness"= mean(Speechiness), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # For Tempo
  tempodf <- combineddf %>%
    group_by(trackName, artistName) %>%
    summarise("Tempo"= mean(Tempo), "streams"=length(trackName), "minsplayed"=sum(minutesPlayed))
  
  # renderring plot for characteristic to the output variable
  output$scatter <- renderPlotly({
    
    # Plot w.r.t radio input conditions. Plotting scatter plots with trend lines using ggplot()
    # Using ggplotly() to convert to plotly() for tooltip 
    
    # For Acousticness
    if (input$characteristic == "Acousticness") {
      fig1 <- ggplot(acousticdf, aes(x= streams, y=Acousticness))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Acousticness")+
        xlab("Streams")+
        ggtitle("Acoustic-ness of the track from 0.0(least) to 1.0(most)")
      ggplotly(fig1)
      
      # For Danceability
    } else if (input$characteristic == "Danceability") {
      fig2 <- ggplot(dancedf, aes(x= streams, y=Danceability))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Danceability")+
        xlab("Streams")+
        ggtitle("Tells if the track is suitable for dancing from 0.0(least) to 1.0(most)")
      ggplotly(fig2)
      
      # For Energy
    } else if (input$characteristic == "Energy") {
      fig3 <- ggplot(energydf, aes(x= streams, y=Energy))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Energy")+
        xlab("Streams")+
        ggtitle("Feel of intensity of the track from 0.0(least) to 1.0(most)")
      ggplotly(fig3)
      
      # For Loudness
    } else if (input$characteristic == "Loudness"){
      fig4 <- ggplot(louddf, aes(x= streams, y=Loudness))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Loudness")+
        xlab("Streams")+
        ggtitle("Average loudness of a track in decibels (dB) between -60 and 0 dB")
      ggplotly(fig4)
      
      # For Speechiness
    } else if (input$characteristic == "Speechiness"){
      fig5 <- ggplot(speechdf, aes(x= streams, y=Speechiness))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Speechiness")+
        xlab("Streams")+
        ggtitle("Measure of speech (words) in the track from 0.0(least) to 1.0(most)")
      ggplotly(fig5)
      
      # For Tempo
    } else if (input$characteristic == "Tempo"){
      fig6 <- ggplot(tempodf, aes(x= streams, y=Tempo))+
        geom_point(aes(text=paste("Track Name:",trackName)))+
        geom_smooth(method = lm)+
        ylab("Average Tempo")+
        xlab("Streams")+
        ggtitle("Tempo of the track in beats per minute (BPM)")
      ggplotly(fig6)
    }
  })
  output$chardescription <- renderText({
    if (input$characteristic == "Acousticness") {
      "The trend line has a decreasing slope. It shows you like songs with less acoustics"
    }
    else if (input$characteristic == "Danceability") {
      "The trend line has a slightly increasing slope. It shows you like songs which can be Danced on."
    }
    else if (input$characteristic == "Energy") {
      "The trend line has an increasing slope. It shows you like songs which are Energetic."
    }
    else if (input$characteristic == "Loudness"){
      "The trend line has an increasing slope. It shows you like songs which are Loud."
    }
    else if (input$characteristic == "Speechiness"){
      "The trend line has a decreasing slope. It shows you prefer songs with less Speech or Lyrics and more Music."
    }
    else if (input$characteristic == "Tempo"){
      "The slope of the trend line increases rapidly. It shows you prefer music with a high Tempo."
    }
  })
  })

## REFERENCES

#[1] Curtis Joe, How to see Spotify Wrapped results, stats, and playlists, 30/05/2022. URL: https://www.androidauthority.com/how-to-see-spotify-wrapped-3069969/
#[2] 2021 Wrapped, https://open.spotify.com/genre/2021-page
#[3] Wikipedia, Spotify Wrapped, https://en.wikipedia.org/wiki/Spotify_Wrapped 
#[4] Rishiraj Sinharay, Project Proposal, FIT5147, Analysis of my personal Spotify dataset, drawing inspiration from “Spotify Wrapped”
#[5] Spotify Colors, U.S. Brand Colors, https://usbrandcolors.com/spotify-colors/#:~:text=The%20official%20Spotify%20colors%20are,to%20visit%20the%20company%20website.
#[6] Rishiraj Sinharay, Data Exploration Project Report, FIT5147, Analysis of my personal Spotify dataset, drawing inspiration from “Spotify Wrapped”
#[7] Cumulative Animations in R, https://plotly.com/r/cumulative-animations/
#[8] Shiny Themes, https://rstudio.github.io/shinythemes/
#[9] Hover Text and Formatting in R, https://plotly.com/r/hover-text-and-formatting/#format-hover-text
#[10] Different inputs & sidebars for each tab, https://community.rstudio.com/t/different-inputs-sidebars-for-each-tab/1937
#[11] Zach, May 12, 2022, How to Group Data by Month in R (With Example), https://www.statology.org/r-group-by-month/
#[12] Spotify Logo, https://www.pngitem.com/middle/mwTiwb_spotify-logo-small-spotify-logo-transparent-hd-png/
  