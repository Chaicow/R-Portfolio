library(shiny)
library(tidyverse)
library(leaflet)
library(lubridate)
library(gtrendsR)
library(plotly)

#Data Cleaning

#kmeans
pokemon <- data.table::fread(here::here("csv", "All_Pokemon.csv"))


pokemon <- pokemon[- grep("Mega ", pokemon$Name),] #remove mega evolutions because they are only temporary!

legendary <- pokemon %>% 
  mutate(`PokemonEvolution` = ifelse(Legendary == 1, "Legendary", NA)) %>% 
  drop_na()

legendary_names <- as.data.frame(legendary$Name)
colnames(legendary_names)[1] <- "Name"

pokemon <- pokemon %>% 
  anti_join(legendary_names)

pokemon <- pokemon %>% 
  mutate(`PokemonEvolution` = ifelse(`Final Evolution` == 1, "Fully Evolved", "Not Fully Evolved" ))

pokemon <- rbind(pokemon, legendary)

pokemon <- pokemon %>% 
  select(HP, Att, Def, Spa, Spd, Spe, BST, Name, `PokemonEvolution`) %>% 
  mutate(PokemonEvolution = factor(`PokemonEvolution`, c("Legendary", "Fully Evolved", "Not Fully Evolved")))

colnames(pokemon) <- c('HP', 'Attack', 'Defense', 'SpecialAttack', 'SpecialDefense', 'Speed', 'Total', 'Name', 'PokemonEvolution')

#leaflet

ash_pikachu <- gtrends(keyword = c("Ash Ketchum", "Pikachu", 'Pokemon'), geo = 'US', time = "now 1-d", hl = 'en-US')$interest_by_region

ash_pikachu <- ash_pikachu %>% 
  pivot_wider(names_from = keyword, values_from = hits) 
ash_pikachu_pop <- ash_pikachu %>% 
  select(`Ash Ketchum`, Pikachu)

ash_pikachu_pop <- colnames(ash_pikachu_pop)[apply(ash_pikachu_pop, 1, which.max)]

ash_pikachu <- cbind(ash_pikachu, ash_pikachu_pop)

colnames(ash_pikachu)[1] <- "name"

# Spacial dataframe came from here: https://rstudio.github.io/leaflet/choropleths.html
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")

states@data <- head(states@data, n = 51)
states@polygons <- head(states@polygons, n = 51)

states@data <- merge(states@data, ash_pikachu)

latlong <- read.table("https://people.sc.fsu.edu/~jburkardt/datasets/states/state_capitals_ll.txt", header = FALSE) # This is the same lat/long data from lab 3

colnames(latlong) <- c("name", "lat", "lon")

latlong <- latlong %>% 
  filter(name != "US", 
         name != "PR")

latlong$name <- state.name[match(latlong$name, state.abb)]

latlong$name <- latlong$name %>% 
  replace_na("District of Columbia")

states@data <- merge(states@data, latlong)

#Visualization

#I used the gtrendsR guide to help me with this portion: https://cran.r-project.org/web/packages/gtrendsR/gtrendsR.pdf

gtrend_by_country <- function(countries){ #writing a function because gtrends only allows you to scrape 5 countries
  
  gtrends(keyword = "pokemon", 
          geo = countries, 
          time = "2022-01-01 2022-06-01", 
          hl = 'en-US', 
          onlyInterest = TRUE)
}

country_list <- list("CA", "NZ", "US", "GB", "AU", "IE")

pokemon_data <- lapply(country_list, gtrend_by_country) 

pokemon_data <- Reduce(function(x, y) merge(x, y, all=TRUE), pokemon_data) #I got this code from: https://www.statology.org/merge-multiple-data-frames-in-r/

pokemon_data <- pokemon_data %>% 
  select(-c("interest_over_time.keyword", 'interest_over_time.gprop', 'interest_over_time.category'))
pokemon_data$interest_over_time.geo <- countrycode::countrycode(pokemon_data$interest_over_time.geo, "iso2c", "country.name") # got this from here: https://stackoverflow.com/questions/26818257/how-to-convert-country-codes-into-country-names-in-a-column-within-a-data-frame

pokemon_data$interest_over_time.date <-format(as.Date(pokemon_data$interest_over_time.date, tz = "PST8PDT", '%Y-%m-%d'), "%Y-%m-%d",) #I got this line of code from here: https://stackoverflow.com/questions/31798599/change-date-from-y-m-d-to-m-d-y-format-in-r

# Define UI ----
ui <- navbarPage(theme = shinythemes::shinytheme("united"), 
                 "Shiny Pokémon",
                 tabPanel("Homepage",
                          div(img(src = "pokelogo.png", width = "50%", height = "15%"),
                              style = "text-align: center;"), #This idea came from the 20 questions Pokémon shiny app: https://github.com/ashbaldry/Pokemon_20Q/blob/master/ui.R
                          div(img(src = "profoak.png", width = "50%", height = "15%"),
                              style = "text-align: center;"),
                          div(
                            h2("Hello There!"),
                            p("This app was created as a part of the final project of Cal Poly's Stat 431: Advanced Statistical Computing with R course taught by Dr. Kelly Bodwin. This app allows you to explore data related to Pokémon. This app will allow you to explore the popularity of Pokémon in all 50 US states and all high income English speaking countries. Additionally this app allows you to explore how good different Pokémon base stats are at predicing the evolutionary stage of all 898 Pokémon plus their regional variants! With this app, you can Explore Em' All!"), style = "text-align: center;"),
                          div(
                            p("The data in this app come from two sources: A Google spreadsheet that was created as a part of an online Pokémon data analysis challenge and Google trends.",
                              p("Click ", 
                                a(href = 'https://docs.google.com/spreadsheets/d/1kb5IkxQlJoWKGPVxmHb_D7POaAKFXUl8/edit#gid=1425790249', 'Here'), "for the link to the google spreadsheet.")),
                            p("The names of the tabs were inspired by SEMIONKORCHEVSKIY's name for their kaggle dataset. You can find it ", a(href = 'https://www.kaggle.com/datasets/semioniy/predictemall', 'Here'),".")), style = "text-align: center;"),
                 
                 tabPanel("Map Em' All",
                          sidebarLayout(
                            sidebarPanel(h4("Pokémon Popularity Map"), 
                                         p("This map shows the popularity score of Pokémon in each US State over the past day along with comparing the popularity between Ash Ketchum and Pikachu, the two main characters of the Pokémon TV show. Popularity Score is determined by how many Google searches a certain keyword. The scale ranges from 0 to 100 with 0 indicating no popularity and 100 indicating high popularity"),
                                         p('To see the popularity score of Pokémon in a particular state, hover over that state.'), 
                                         p("Hover over the pokéball to see which character was more popular today. Click on the pokéball to see each character's respective popularity score.")),
                            mainPanel(leafletOutput("map"),
                                      textOutput("map_directions")))),
                 
                 tabPanel("Vizualize Em' All",
                          sidebarLayout(
                            sidebarPanel(
                              sliderInput("date",
                                          "Pokémon Popularity Score Per Day By Country:",
                                          min = ymd("2022-01-01"),
                                          max = ymd("2022-06-01"),
                                          value = ymd("2022-01-28")),
                              selectInput(inputId = "Country1", label = "Select a Country", choices = c("Canada", "New Zealand", "United States", "United Kingdom", "Australia", "Ireland")),
                              selectInput(inputId = "Country2", label = "Select a Country", choices = c("New Zealand", "United States", "United Kingdom", "Australia", "Ireland", 'Canada'))),
                            mainPanel(plotOutput("plot"),
                                      textOutput("description")))), 
                 tabPanel("Group Em' All",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput('X', 'X Variable', c('HP', 'Attack', 'Defense', 'SpecialAttack', 'SpecialDefense', 'Speed', 'Total')),
                              selectInput('Y', 'Y Variable', c('Total', 'Attack', 'Defense', 'SpecialAttack', 'SpecialDefense', 'Speed', 'HP'))),
                            mainPanel(
                              plotlyOutput('plot2'), 
                              textOutput("description2")))))



# Define server logic ----
server <- function(input, output) {
  
  icon <- makeIcon(iconUrl = "https://www.freeiconspng.com/thumbs/pokeball-png/file-pokeball-png-0.png",
                   iconWidth = 15, iconHeight = 15) #pokeball
  
  labels <- paste("Who is more popular in", states@data[["name"]], "today?",
                  "<br>", states@data[["ash_pikachu_pop"]])
  
  
  popups <- paste("Popularity Score for Pikachu today: ", states@data[["Pikachu"]], 
                  "<br>", "Popularity Score for Ash Ketchum today: ", states@data[["Ash Ketchum"]])
  
  output$`map` <- renderLeaflet({
    
    bins <- c(40, 50, 60, 70, 80, 90, 100)
    pal <- colorBin("YlOrRd", domain = states@data[["Pokemon"]],
                    bins = bins)
    
    #Some of the choropleth code came from here: https://slcladal.github.io/gviz.html
    
    leafmap <- leaflet(data = states) %>% 
      addTiles() %>% 
      addMarkers(lng = ~lon, lat = ~lat,
                 icon = icon,
                 label = lapply(labels, htmltools::HTML),
                 popup = lapply(popups, htmltools::HTML)) %>% 
      addPolygons(
        label=~stringr::str_c('Popularity Score ',
                              name, ': ',
                              formatC(states@data[["Pokemon"]], big.mark = ',', format='d')),
        labelOptions= labelOptions(direction = 'auto'),
        weight=1, color='#333333', opacity=1,
        fillColor = ~pal(states@data[["Pokemon"]]), fillOpacity = 1,
        highlightOptions = highlightOptions(
          color='#000000', weight = 2,
          bringToFront = TRUE, sendToBack = TRUE)
      ) %>% 
      addLegend(
        "topright", pal = pal, values = ~states@data[["Pokemon"]],
        title = htmltools::HTML("Popularity of Pokémon by State"),
        opacity = 1 )
    
    leafmap
    
  })
  output$description <- renderText({
    'This plot allows you to compare the popularity of Pokémon in high income countries whose primary language is English for any day since the beginning of this year (2022). Choose countries by using the dropdown menus and choose a date by using the slider. The date is preset to January 28, 2022 because that was the day that the game Pokémon Legends: Arceus was released.' 
  })
  
  output$plot <- renderPlot({
    
    pokemon_data %>% 
      mutate(interest_over_time.date = ymd(interest_over_time.date)) %>% 
      filter(interest_over_time.geo %in% c(input$Country1, input$Country2)) %>% 
      filter(interest_over_time.date == input$date) %>% 
      ggplot() +
      geom_col(mapping = aes_string(x = 'interest_over_time.geo', y = 'interest_over_time.hits', fill = 'interest_over_time.geo')) +
      labs(y = "Pokémon Popularity Score", x ="Countries", subtitle = "by Day") +
      ggtitle(str_c(c("Pokémon Popularity Comparison between ", input$Country1, " and ", input$Country2), collapse = "")) +
      theme_classic() +
      theme(legend.position = 'none', text=element_text(size=15, family="Trebuchet MS")) +
      scale_fill_manual(values = c("#FAD61C", '#000000'))
    
  })
  
  # I based the kmeans portion of the shiny app off of what this person did: https://github.com/duf59/shiny-kmeans
  output$description2 <- renderText({
    "This plot allows you to test how good different Pokémon base stats are at predicting each Pokémon's evolutionary stage. The three groups are fully evolved Pokémon, Pokémon that are not fully evolved, and legendary Pokémon. I put legendary Pokémon in a different group because they tend to be much stronger than regular Pokémon. Use the dropdown menus to choose base stats. To toggle the visibility of points, click on the different groups in the legend; points (and the toggling options) are separated up by evolutionary stage and assigned cluster."
  })
  compute <- reactive({
    
    data <- subset(pokemon, select=c(input$X,input$Y))
    
    Kclust <- kmeans(data , 3)
    list(kmean.result = data.frame(pokemon, cluster=as.factor(Kclust$cluster)))
  })
  
  output$plot2 <- renderPlotly({
    data=compute()
    plot = ggplot(data=data$kmean.result, aes_string(x = input$X, y = input$Y, label = 'Name')) +
      geom_jitter(size=1, aes_string(shape = "PokemonEvolution", color = 'cluster')) +
      scale_color_manual(values = c("#FF0000", '#222224', "#91AEB6"))+
      ggtitle("Pokémon Clusters via K Means") +
      labs(xlab = paste(input$X), ylab = paste(input$Y)) +
      theme_classic() +
      guides(color = "none")+
      theme(text=element_text(size=15, family="Trebuchet MS"))
    
    ggplotly(plot, tooltip = c("label", 'shape')) #ggplotly stuff was based off of this: https://stackoverflow.com/questions/49454652/add-custom-data-label-in-ggplotly-scatterplot
  })
}    



# Run the app ----
shinyApp(ui = ui, server = server)
