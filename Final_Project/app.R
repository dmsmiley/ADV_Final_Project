library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(plotly)
library(viridis)
library(fmsb)
library(RColorBrewer)
library(shinyWidgets)
library(shinyBS)
library(scales)
library(shinydashboard)

abandoned = st_read("data/Abandoned_Properties/Abandoned_Property_Parcels.shp")

schools = st_read("data/School_Boundaries/School_Boundaries.shp")

districts = st_read("data/City_Council_Districts/City_Council_Districts.shp")

census = st_read("data/2020_CensusData/2020_CensusData.shp")

unique_school_types <- unique(schools$SchoolType)
color_palette <- colorFactor(brewer.pal(n = min(length(unique_school_types), 8), "Set1"), domain = unique_school_types)


# Check and set CRS if needed
if (!identical(st_crs(census), st_crs(abandoned))) {
  abandoned <- st_transform(abandoned, crs = st_crs(census))
}

# Perform a spatial join based on geometry
joined_data <- st_join(abandoned, census, join = st_within)

# Pivot the data to have columns as rows
melted_data <- joined_data %>%
  select(c("GEOID","Structures", "Property_S",
           "Code_Enfor", "Outcome_St")) %>%
  pivot_longer(cols = c("Structures", "Property_S", "Code_Enfor", "Outcome_St"), names_to = "Variable", values_to = "Value")

# Group by "GEOID" and "Variable" and get unique value counts
summary_data <- melted_data %>%
  group_by(GEOID, Variable, Value) %>%
  summarise(Count = n())

new_summary = sf::st_drop_geometry(summary_data) %>%
  filter(Variable %in% c("Code_Enfor", "Outcome_St", "Property_S"),
         !is.na(Value))

# Spread the summarized data back to columns
spread_data <- new_summary %>%
  pivot_wider(names_from = Value, values_from = Count) %>%
  ungroup() %>%
  select(-Variable) %>%
  group_by(GEOID) %>%
  summarise(across(everything(), ~sum(., na.rm = TRUE)))

joined_ab_census <- left_join(census, spread_data, by = "GEOID")

dtsb_coords <- data.frame(
  lon = -86 - 15/60 - 1/3600,
  lat = 41 + 40/60 + 35/3600
)

test_df = joined_ab_census %>%
  filter(!GEOID %in% c("18141012200", "18141012100", "18141012300",
                       "18141012400", "18141012000", "18141011900",
                       "18141010800", "18141011602", "18141010901",
                       "18141010902", "18141011001", "18141011002",
                       "18141011303", "18141011307", "18141011308",
                       "18141011804", "18141011304", "18141011305",
                       "18141011306", "18141011404", "18141011404",
                       "18141011604", "18141011603", "18141011504",
                       "18141011503", "18141010500", "18141011603",
                       "18141011405", "18141011403", "18141011505",
                       "18141011309", "18141011203", "18141011310",
                       "18141011506", "18141010700", "18141010400",
                       "18141011701", "18141011703", "18141011801",
                       "18141011406", "18141010300", "18141011501",
                       "18141010202", "18141010201", "18141001200",
                       "18141010600", "18141001300", "18141010100")) %>%
  mutate(ratio_poor = B13004_4/B13004_1,
         num_abandoned = rowSums(across(49:76)),
         major_owner = if_else(A10060_2 > A10060_3, 1, 0))


code_enfor_unique = sf::st_drop_geometry(new_summary) %>%
  filter(Variable == "Code_Enfor") %>%
  pull(Value) %>%
  unique()


code_outcome_unique = sf::st_drop_geometry(new_summary) %>%
  filter(Variable == "Outcome_St") %>%
  pull(Value) %>%
  unique()

districts_unique = sf::st_drop_geometry(test_df) %>%
  pull(GEOID) %>%
  unique()

#selectInput("variable_selector", "Select Code Enforcement Type:",
#choices = code_enfor_unique,
#selected = code_enfor_unique[1])
outcome_counts <- abandoned %>%
  count(Outcome_St) %>%
  top_n(10, n) %>%
  arrange(desc(n)) %>% # filter out Outcome_St == NA
  filter(!is.na(Outcome_St))


ggplot(test_df, aes(x = A14006_1, y = num_abandoned, 
                    color = factor(major_owner))) +
  geom_point(aes(size=ratio_poor))
# tooltip

# UI
ui <- fluidPage(
  useShinydashboard(),
  titlePanel("South Bend Civic Data Dashboard"),
  # add subtitle
  h3("Advanced Data Visualization - Fall 2023"),
  h4("Team Members: Raymond, David, Sean, and Travis"),
  
  tabsetPanel(
    tabPanel("Introduction", 
             h3("Welcome to the South Bend Civic Data Dashboard"),
             p("This dashboard is designed to provide insights into how various civic datasets interact and their implications for city planning and development."),
             h4("Overview of Datasets"),
             p("The dashboard integrates various datasets including abandoned properties, school boundaries, city council districts, and census data, each offering unique perspectives on the city's infrastructure and demographics."),
             h4("Key Features"),
             p("Explore interactive maps, utilize data filters, and engage with analytical tools to understand the datasets better."),
             h4("Navigating the Dashboard"),
             p("Use the tabs above to navigate between different sections, each dedicated to specific datasets and their interactions."),
             h4("Objective of the Presentation"),
             p("This presentation aims to demonstrate the value of data-driven insights in urban planning and decision-making."),
             #creating seperation from text and image
             br(), br(), br(),
             #placing image in bottom middle
             div(style = "text-align: center;",
                 img(src = "South_Bend_seal.svg", width = "266px", height = "266px")
             )),
    tabPanel("District Summary",
             sidebarLayout(
               sidebarPanel(
                 selectInput("district_selector", "District:",
                             choices = districts_unique,
                             selected = districts_unique[1])
               ),
               mainPanel(
                 fluidRow(valueBoxOutput("districtIdBox", width = 12)),
                 fluidRow(
                   valueBoxOutput("populationBox", width = 6),
                   valueBoxOutput("medianIncomeBox", width = 6)
                 ),
                 fluidRow(
                   valueBoxOutput("povertyRatioBox", width = 6),
                   valueBoxOutput("abandonedPropertyBox", width = 6)
                 ),
               )
             )
    ),
    tabPanel("Abandoned Property Outcomes",
             sidebarLayout(
               sidebarPanel(
                 selectInput("variable_selector", "Outcome Type:",
                             choices = code_outcome_unique,
                             selected = code_outcome_unique[1])
               ),
               mainPanel(
                 leafletOutput("map")
               )
             )
    ),
    
    tabPanel("District Metrics",
             sidebarLayout(
               sidebarPanel(
                 h4("Map Legend"),
                 p(strong("Blue:"), " Majority of Properties Inhabited by Owners"),
                 p(strong("Gold:"), " Majority of Properties Inhabited by Renters"),
                 p(strong("Size:"), " Poverty Rate")
               ),
               mainPanel(plotlyOutput("topOutcomesPlot")
               )
             )
    ),
    
    tabPanel("Abandoned Properties & Schools",
             sidebarLayout(
               sidebarPanel(
                 h4("Summary"),
                 p("In our analysis, it appears that a smaller number of schools are located in close proximity to abandoned properties. Notably, public schools are more frequently found near these abandoned sites compared to private schools. This pattern may have significant implications for urban planning and community development."),
                 h4("Map Legend"),
                 p(strong("Blue:"), " Public Schools"),
                 p(strong("Gold:"), " Private Schools"),
                 p(strong("Green:"), " Abandoned Properties")
                 # Additional interactive elements can be added here if needed
               ),
               mainPanel(
                 leafletOutput("correlationMap")
               )
             )
    )
    # More tabs can be added if needed
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  filtered_data <- reactive({
    selected_variable <- input$variable_selector
    st_transform(test_df, crs = 4326)  # Reproject to WGS84
    
    selected_district <- input$district_selector
    st_transform(test_df, crs = 4326)  # Reproject to WGS84
  })
  
  output$text1 <- renderText({paste(input$district_selector)})
  
  output$districtIdBox <- renderValueBox({
    valueBox(
      input$district_selector, "District ID", icon = icon("tree-city"),
      color = "blue"
    )
  })
  
  output$populationBox <- renderValueBox({
    valueBox(
      census$A00001_1[census$GEOID == input$district_selector], "Total Population", icon = icon("users-viewfinder"),
      color = "yellow"
    )
  })
  
  output$povertyRatioBox <- renderValueBox({
    valueBox(
      paste(format(round(test_df$ratio_poor[test_df$GEOID == input$district_selector] * 100, 2), nsmall = 2), "%",sep = ""), "% Population Poor or Struggling", icon = icon("square-person-confined"),
      color = "red"
    )
  })
  
  output$medianIncomeBox <- renderValueBox({
    valueBox(
      sprintf("%sK", format(round(census$A14006_1[census$GEOID == input$district_selector]/1000, 1), dec=".")), "Median Income", icon = icon("sack-dollar"),
      color = "green"
    )
  })
  
  output$abandonedPropertyBox <- renderValueBox({
    valueBox(
      test_df$num_abandoned[test_df$GEOID == input$district_selector], "# Abandoned Properties", icon = icon("house-circle-xmark"),
      color = "purple"
    )
  })
  
  # Render Leaflet map
  output$map <- renderLeaflet({
    variable_values <- filtered_data()[[input$variable_selector]]
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(data = filtered_data(),
                  fillColor = ~colorFactor("Blues", levels = sort(unique(variable_values)))(variable_values),
                  fillOpacity = 0.8,
                  color = "white",  # Border color
                  weight = 1,        # Border weight
                  highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                  ),
                  popup = ~paste("District: ", GEOID, "<br>",
                                 input$variable_selector, ":", variable_values))
  })
  
  output$summary_map <- renderLeaflet({
    district_values <- filtered_data()[[input$district_selector]]
    
    leaflet() %>%
      addTiles() %>%
      
      # Outline all shapes with opaque grey color
      addPolygons(data = test_df,
                  fillColor = "grey",  # Opaque grey color
                  fillOpacity = 1,     # Opaque
                  color = "white",     # Border color
                  weight = 1,          # Border weight
                  highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                  )) %>%
      
      # Highlight selected district with blue color
      addPolygons(data = filtered_data() %>%
                    filter(GEOID == district_values),
                  fillColor = "blue",  # Blue color
                  fillOpacity = 0.8,   # Adjust opacity as needed
                  color = "white",     # Border color
                  weight = 2,          # Border weight
                  highlightOptions = highlightOptions(
                    color = "white",
                    weight = 2,
                    bringToFront = TRUE
                  ))
  })
  
  output$topOutcomesPlot <- renderPlotly({
    p <- plot_ly(data = test_df, 
                 x = ~A14006_1, 
                 y = ~num_abandoned,
                 type = 'scatter', 
                 #mode = 'markers',
                 size = ~ratio_poor,
                 text = ~paste(" District: ", GEOID, "<br>",
                               "Median Income: ", dollar_format()(A14006_1), "<br>",
                               "Abandoned Properties: ", num_abandoned, "<br>",
                               "Poverty Rate: ", percent_format()(ratio_poor))) %>%
      add_markers(marker = list(color = ifelse(test_df$major_owner == 1, "#0C2340", "#AE9142"))) %>%
      layout(xaxis = list(title = "Median Household Income"),
             yaxis = list(title = "Abandoned Properties"),
             showlegend = FALSE)
    
    p
  })
  
  output$correlationMap <- renderLeaflet({
    public_schools = schools[schools$SchoolType == "Public",]
    private_schools = schools[schools$SchoolType == "Private",]
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  # Using CartoDB's Positron tiles for a grey-toned base map
      addPolygons(data = public_schools, 
                  color = '#0C2340', weight = 1, 
                  fillOpacity = 0.5) %>%
      addPolygons(data = private_schools, 
                  color = '#AE9142', weight = 1, 
                  fillOpacity = 0.5) %>%
      addPolygons(data = abandoned, weight = 3, color = "#00843D")
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

# subset schools with school_type equal to "public"
#schools <- schools[schools$school_type == "public",]
