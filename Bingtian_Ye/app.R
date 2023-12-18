library(shiny)
library(sf)
library(maps)
library(ggplot2)
library(rnaturalearth)
library(DT)
library(dplyr)
library(rnaturalearthdata)
library(plotly)
library(raster)


generateIntroductionContent <- function() {
  div(
    h3("Introduction"),
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/Flag_of_Grenada.svg/2560px-Flag_of_Grenada.svg.png",
      style = "max-height: 200px; height: 100%; width: auto;"
    ),
    p("The flag of Grenada features a red field with a green border and seven gold stars. The stars represent the seven administrative divisions of the country. The green color symbolizes the lush vegetation of Grenada, and the red color represents the courage and vitality of the people. The nutmeg at the center of the flag is a symbol of Grenada's status as the 'Isle of Spice.'"),
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/0/03/Coat_of_arms_of_Grenada.svg/1920px-Coat_of_arms_of_Grenada.svg.png",
      style = "max-height: 200px; height: 100%; width: auto;"
    ),
    p('The official coat of arms of Grenada is a shield divided into four parts by a golden cross. In the centre of this cross is the Santa Maria, Columbus\' flagship. A lion passant guardant on a red field is shown in the upper left and lower right sections of the shield, with a golden crescent moon out of which a lily grows in the upper right and lower left sections. Above the shield there is a golden helmet, topped with a garland of bougainvillea branches. Within the garland are seven red roses, which stand for the seven communities of Grenada (six parishes and the Southern Grenadines). Holding the shield on the dexter side is a nine-banded armadillo which stands before a corn stalk; on the sinister side is a Grenada dove, which stands before a banana plant. The base represents Mount St. Catherine with the Grand Etang Lake at the centre. A ribbon displays the national motto: "Ever conscious of God we aspire, build and advance as one people."'),
    tags$p(
      style = "font-size: small; color: blue;",
      "Source: Information obtained from Wikipedia. https://en.wikipedia.org/wiki/Grenada#Administrative_divisions"
    )
  )
}

generateGeographyContent <- function() {
  div(
    h3("Geography Content"),
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/6/6e/Grenada_on_the_globe_%28Americas_centered%29.svg/500px-Grenada_on_the_globe_%28Americas_centered%29.svg.png"
    ),
    p("Grenada is a country located in the Caribbean region, situated in the northeastern part of South America. It comprises the island of Grenada and some smaller islands, being part of the Eastern Caribbean. The geographical coordinates of Grenada are approximately between 12 degrees north latitude and 61 degrees west longitude. The main island of Grenada is positioned between approximately 11.5 degrees and 12.5 degrees north latitude and between 61 degrees and 61.5 degrees west longitude. Grenada Island is the largest and primary island of the country, as well as the seat of the national government. Grenada also includes some smaller islands such as Carriacou and Petit Martinique, which are located to the northeast of Grenada Island and are part of Grenada. Grenada's geographical location makes it a beautiful tourist destination, known for its spectacular beaches, coral reefs, and tropical landscapes. It is also an independent country in the Caribbean region, with its capital located in St. George's on the island of Grenada."),
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Grenada-map.png/440px-Grenada-map.png"
    ),
    p("Grenada's terrain is characterized by a rugged and diverse landscape. The island features a series of mountain ranges, with the highest peak being Mount Saint Catherine, rising to approximately 840 meters (2,760 feet) above sea level. These mountains provide stunning natural scenery and opportunities for hiking and mountain climbing. In addition to the mountains, Grenada has numerous hills and rolling terrain covered with farmland and vegetation. The coastal areas are dotted with beautiful beaches and coral reefs, making it a popular destination for tourists seeking water-related activities. Overall, Grenada's topography ranges from high peaks to hilly and coastal regions, offering a rich and varied landscape for outdoor enthusiasts and nature lovers."),
    tags$img(
      src = "https://upload.wikimedia.org/wikipedia/commons/f/fc/Grenada_parishes_named.png"
    ),
    p("Administrative divisions"),
    tags$p(
      style = "font-size: small; color: blue;",
      "Source: Information obtained from Wikipedia. https://en.wikipedia.org/wiki/Grenada#Administrative_divisions"
    )
  )
}

generateIntroductionOfCARICOMContent <- function() {
  p1 <- tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/1/18/Flag_of_CARICOM.svg/2560px-Flag_of_CARICOM.svg.png",style = "max-height: 450px; height: 100%; width: auto; margin: 0px; padding: 0px;")
  p2 <- tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c3/Caricom_Member_states_fully_labeled.png/2560px-Caricom_Member_states_fully_labeled.png",style = "max-height: 450px; height: 100%; width: auto; margin: 0px; padding: 0px;")
  intro_text <- "CARICOM, also called The Caribbean Community.The Caribbean Community (CARICOM or CC) is an intergovernmental organisation that is a political and economic union of 15 member states (14 nation-states and one dependency) throughout the Americas and Atlantic Ocean. They have primary objectives to promote economic integration and cooperation among its members, ensure that the benefits of integration are equitably shared, and coordinate foreign policy. The organisation was established in 1973, with its four founding members signing the Treaty of Chaguaramas. 
The secretariat headquarters is in Georgetown, Guyana. It's full member country as follows:"
  country_data <- read.csv("country in C.csv", header = FALSE)
  colnames(country_data) <- c("country name", "join time")
  country_table <- datatable(
    country_data,
    options = list(
      searching = FALSE,
      ordering = FALSE
    )
  )
  
  list(p1, p2,tags$br(),tags$br(), intro_text, tags$br(), country_table, tags$br(), plotOutput("plot1"), tags$br(),tags$p(
    style = "font-size: small; color: blue;",
    "Source: Information obtained from Wikipedia. https://en.wikipedia.org/wiki/Grenada#Administrative_divisions"
  ))
}

generatePopulationOfCARICOMContent <- function(){
  
  list(
    h3("Population Data"),
    div(
      column(width = 12, DTOutput("dataTable")),
      column(width = 12, plotOutput("mapPlot")),
      p("According to UNSD results, Haiti has a large population in CARICOM, while Grenada has a smaller population of about 125,000"),
      tags$p(
        style = "font-size: small; color: blue;",
        "Source: Information obtained from UNdata. https://data.un.org/default.aspx"
      )
    )
  )
}
generateFinanceOfCARICOMContent <- function(){
  div(
    h3("GDP"),
    column(width = 12, DTOutput("GDPdataTable")),
    column(width = 12, plotOutput("GDPPlot")),
    h3("PPP"),
    column(width = 12, DTOutput("PPPdataTable")),
    column(width = 12, plotOutput("PPPPlot")),
  p("Grenada's relatively small geographical area and population size, as well as the structure of its economy, make it lower compared to some other Caribbean countries in terms of GDP and PPP. However, this does not necessarily mean that Grenada’s economy is unhealthy or unsustainable, as each country has its own unique economic characteristics and challenges. The Grenadian government and social stakeholders can take steps to improve the country’s economy and sustainability."),
  tags$p(
    style = "font-size: small; color: blue;",
    "Source: Information obtained from UNdata. https://data.un.org/default.aspx"
  )
  )}

generateOtherOfCARICOMContent <- function(){
  div(
  column(width = 12, plotOutput("FoodPlot")),
  tags$br(),
  p("It can be seen from the figure that Grenada's food is highly dependent on imports compared to countries in the same region. The impact of food price fluctuations may be greater, meaning its economy is vulnerable. And food safety issues will pose a greater threat."),
  tags$br(),
  column(width = 12, plotOutput("LandPlot")),
  tags$br(),
  p("Although Grenada's land altitude is at a medium level compared to other countries in the same region, considering the rising sea levels caused by global climate warming, this puts the country's future development at risk."),
  tags$br(),
  tags$p(
    style = "font-size: small; color: blue;",
    "Source: Information obtained from The world bank. https://www.worldbank.org/en/home"
  )
  )
}

generateStrengthContent <- function() {
  div(
    column(width = 12, plotOutput("GGDPPlot")),
    tags$br(),
    p("Grenada's economy is highly dependent on tourism. During the epidemic, international travel has been severely restricted, which may lead to a significant decrease in its GDP. As the epidemic eases and the tourism industry gradually recovers, we can see the return to GDP growth in the chart."),
    tags$br(),
    p("Grenada's natural environment and agricultural resources are its main strengths, and its economy relies mainly on its tourism and spice exports."),
    tags$br(),
    fluidRow(
      column(
        width = 6, 
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/8c/Grand_Anse_Beach_Grenada.jpg/2560px-Grand_Anse_Beach_Grenada.jpg", style = "width:100%;"),
        tags$p(style = "text-align: center; font-weight: bold;", "Grenada's beaches") 
      ),
      column(
        width = 6, 
        tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/7/7c/Fleur_de_muscade.jpg", style = "width:100%;"),
        tags$p(style = "text-align: center; font-weight: bold;", "Grenada’s main spice, nutmeg") 
        
      )
    ),
    tags$p(
      style = "font-size: small; color: blue;",
      "Source: Picture obtained from Wikipedia. https://en.wikipedia.org/wiki/Demographics_of_Grenada"
    )
  )
}



generateWeaknessContent <- function(){
  div(column(width = 12, plotlyOutput("weakPlot")),
      tags$br(),
      p("In my opinion, the main weakness of Grenada is that it is highly dependent on tourism (service industry). This makes its economic risk higher, affected by relations with other countries and other major events (such as COVID-19). This makes its economy highly unstable."),
      tags$br(),
      tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/8/87/Grenada_single_age_population_pyramid_2020.png/1920px-Grenada_single_age_population_pyramid_2020.png", style = "width: 100%; max-width: 12; height: auto;"),
      tags$br(),
      p("In addition, its birth population is showing a downward trend. Although its current proportion of working population is relatively high, Grenada does not have the advantage of attracting immigrants to settle, which leads to the possibility that its population will age in the future."),
      tags$br()
  )
}


generateOpportunitiesContent <- function() {
  div(
    fluidRow(
      column(6, 
             tags$img(src = "sunny.png", style = "width:100%; display: block; margin-left: auto; margin-right: auto; padding-bottom: 10px;")),
      column(6, 
             tags$img(src = "wind.png", style = "width:100%; display: block; margin-left: auto; margin-right: auto; padding-bottom: 10px;"))
    ),
    fluidRow(
      column(6, 
             tags$img(src = "energy_sun.jpg", style = "width:100%; display: block; margin-left: auto; margin-right: auto; padding-top: 10px;")),
      column(6, 
             tags$img(src = "energy_wind.jpg", style = "width:100%; display: block; margin-left: auto; margin-right: auto; padding-top: 10px;"))
    ),
    tags$br(),
    p("The development of clean energy will be an opportunity for Grenada. Grenada has the natural conditions to develop solar and wind energy."),
    tags$br(),
    p("Grenada is located in the tropics and has abundant sunlight most of the year, which provides ideal conditions for solar power generation. Longer periods of sunshine mean solar panels are able to produce more electricity, which increases the reliability and economics of solar power as a source of electricity."),
    tags$br(),
    p("Additionally, Grenada's location benefits from stable wind speeds, especially near the coastline. These wind speed conditions are suitable for establishing wind turbines, especially during high wind speed seasons, when wind power can provide large amounts of electricity."),
    tags$br(),
    p("Developing clean energy can help Grenada free itself from dependence on fossil fuels, reduce energy imports, improve energy security, and promote economic diversification. And in the long run, solar and wind energy can reduce the country's energy costs, especially as the related technologies mature and are produced on a large scale, the cost of clean energy is falling."),
    tags$br(),
    tags$p(
      style = "font-size: small; color: blue;",
      "Source: Picture of sunny day and wind speed from meteoblue. https://www.meteoblue.com/en/weather/historyclimate/climatemodelled/grenada_united-states_4428539"
    )
    )
}


generateThreatsContent <- function(){
  div(column(width = 12, plotlyOutput("ThreatsPlot")),
      tags$br(),
  p("Grenada has low sea levels, which could lead to land loss. Seawater can erode coastlines and destroy beaches and land, posing a threat to the tourism industry that relies on beautiful beaches to attract tourists. Saltwater can invade freshwater supplies, affecting the quality of drinking and irrigation water. Agricultural land may become unsuitable for farming due to increased salt concentrations. And infrastructure in low-lying areas, including housing, roads, schools and hospitals, are at risk from flooding and storm surges, which can lead to economic losses and expensive costs to rebuild infrastructure."),
  tags$br(),
  tags$p(
    style = "font-size: small; color: blue;",
    "Source: GEOTIFF from globalsolaratlas. https://globalsolaratlas.info/download/grenada"
  )
  )
}


ui <- fluidPage(
  titlePanel("Grenada", "Bingtian Ye"),
  fluidRow(
    column(
      width = 12, 
      navlistPanel(
        id = "tabset",
        tabPanel("Introduction",
                 uiOutput("introductionTabContent")
        ),
        tabPanel("Geography",
                 uiOutput("geographyTabContent")
        ),
        tabPanel("In CARICOM", 
                 navlistPanel(
                   id = "caribbeanTabset",
                   tabPanel("Introduction of CARICOM"),
                   tabPanel("Population"),
                   tabPanel("Finance"),
                   tabPanel("Other")
                 )
        ),
        tabPanel("SWOT",
                 navlistPanel(
                   id = "swotTabset",
                   tabPanel("Strength"),
                   tabPanel("Weakness"),
                   tabPanel("Opportunities"),
                   tabPanel("Threats")
                 )
        )
      )
    ),
    column(
      width = 12, 
      uiOutput("mainPanelUI")
    )
  )
)

server <- function(input, output, session) {
  library(rnaturalearth)
  library(rnaturalearthdata)
  output$introductionTabContent <- renderUI({
    generateIntroductionContent()
  })
  
  output$geographyTabContent <- renderUI({
    generateGeographyContent()
  })
  

  output$plot1 <- renderPlot({
    countries <- c("Antigua and Barbuda", "The Bahamas", "Barbados", "Belize", 
                   "Dominica", "Grenada", "Guyana", "Haiti", "Jamaica", 
                   "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                   "Suriname", "Trinidad and Tobago")
    
    world_map <- rnaturalearth::ne_countries(returnclass = "sf")
    CC <- world_map[world_map$name %in% countries, ]
    
    ggplot() +
      geom_sf(data = world_map, fill = "grey", color = "white", show.legend = FALSE) +
      geom_sf(data = CC, fill = "blue", color = "white", show.legend = FALSE) +
      coord_sf(xlim = c(-100, -50), ylim = c(0, 30), expand = FALSE) +
      theme_minimal() +
      labs(x = "Longitude", y = "Latitude")
    
  })
  population_data <- read.csv("population.csv", stringsAsFactors = FALSE)
  countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                 "Dominica", "Grenada", "Guyana", "Haiti", "Jamaica", 
                 "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                 "Suriname", "Trinidad and Tobago")
  latest_population <- population_data %>% 
    filter(country %in% countries) %>%
    arrange(desc(Year)) %>%
    group_by(country) %>%
    slice(1) %>%
    ungroup()
  latest_population$Population <- as.numeric(gsub(" ", "", latest_population$Population))
  world <- ne_countries(scale = "medium", returnclass = "sf")
  selected_countries <- world %>% 
    filter(name %in% countries) %>% 
    st_centroid(of_largest_polygon = TRUE)
  country_coords <- as.data.frame(st_coordinates(selected_countries))
  country_coords$country <- selected_countries$name
  merged_data <- merge(latest_population, country_coords, by.x = "country", by.y = "country")
  
  map_plot <- ggplot() +
    geom_sf(data = world, fill = "white", color = "grey") +
    geom_point(data = merged_data, aes(x = X, y = Y, color = Population), size = 3) +
    scale_color_gradient(limits = c(0, 3000), low = "blue", high = "red") +  
    theme_minimal() +
    coord_sf(xlim = c(-100, -50), ylim = c(0, 30), expand = FALSE) +
    labs(title = "Population of Selected Countries in the Latest Year",
         x = "Longitude",
         y = "Latitude")
  result_table <- latest_population %>%
    dplyr::select("country", "Population")
  colnames(result_table) <- c("county", "population(*1000)")
  output$dataTable <- DT::renderDataTable({
    DT::datatable(result_table, options = list(pageLength = 10,
      searching = FALSE,
      ordering = FALSE
    ))
  })
  output$mapPlot <- renderPlot({
    map_plot
  })
  gdp_data <- read.csv("GDP.csv", stringsAsFactors = FALSE)
  ppp_data <- read.csv("PPP.csv", stringsAsFactors = FALSE)
  countries <- c("Antigua and Barbuda", "Bahamas", "Barbados", "Belize", 
                 "Dominica", "Grenada", "Guyana", "Haiti", "Jamaica", 
                 "Saint Kitts and Nevis", "Saint Lucia", "Saint Vincent and the Grenadines", 
                 "Suriname", "Trinidad and Tobago")
  latest_gdp <- subset(gdp_data, `Country.Name` %in% countries)
  latest_ppp <- subset(ppp_data, `Country.Name` %in% countries)
  world <- ne_countries(scale = "medium", returnclass = "sf")
  selected_countries <- world %>% 
    filter(name %in% countries) %>% 
    st_centroid()
  country_coords <- as.data.frame(st_coordinates(selected_countries))
  country_coords$country <- selected_countries$name
  merged_gdp <- merge(latest_gdp, country_coords, by.x = "Country.Name", by.y = "country")
  merged_ppp <- merge(latest_ppp, country_coords, by.x = "Country.Name", by.y = "country")
  map_gdp <- ggplot() +
    geom_sf(data = world, fill = "white", color = "grey") +
    geom_point(data = merged_gdp, aes(x = X, y = Y, size = GDP), color = "lightblue") +
    theme_minimal() +
    coord_sf(xlim = c(-100, -50), ylim = c(0, 30), expand = FALSE) +
    labs(title = "GDP of CARICOM",
         x = "Longitude",
         y = "Latitude")
  map_ppp <- ggplot() +
    geom_sf(data = world, fill = "white", color = "grey") +
    geom_point(data = merged_ppp, aes(x = X, y = Y, size = PPP), color = "lightgreen") +
    theme_minimal() +
    coord_sf(xlim = c(-100, -50), ylim = c(0, 30), expand = FALSE) +
    labs(title = "PPP of CARICOM",
         x = "Longitude",
         y = "Latitude")
  gdp_table <-merged_gdp|>dplyr::select("Country.Name","GDP")
  ppp_table <-merged_ppp|>dplyr::select("Country.Name","PPP")
  output$GDPPlot <- renderPlot({
    map_gdp
  })
  output$PPPPlot <- renderPlot({
    map_ppp
  })
  output$GDPdataTable <- DT::renderDataTable({
    DT::datatable(gdp_table, options = list(pageLength = 10,
                                               searching = FALSE,
                                               ordering = FALSE,
                                              paging = FALSE
    ))
  })
  output$PPPdataTable <- DT::renderDataTable({
    DT::datatable(ppp_table, options = list(pageLength = 10,
                                               searching = FALSE,
                                               ordering = FALSE,
                                              paging = FALSE
    ))
  })
  
  other <- read.csv("other.csv")
  plot_other_food <- other %>%
    filter(!is.na(food.import)) %>%
    ggplot() +
    aes(x = Country.Name, weight = food.import) +
    geom_bar(fill = "#0C4C8A") +
    labs(
      x = "Country",
      y = "Rate",
      title = "Proportion of food imports in total imports"
    ) +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title.y = element_text(size = 15L),
      axis.title.x = element_text(size = 15L)
    )
  plot_other_land <- other %>%
    filter(!is.na(land.5m)) %>%
    ggplot() +
    aes(x = Country.Name, weight = land.5m) +
    geom_bar(fill = "lightgreen") +
    labs(
      x = "Country",
      y = "Rate",
      title = "Proportion of land less than 5 meter in total area"
    ) +
    coord_flip() +
    theme_classic() +
    theme(
      axis.title.y = element_text(size = 15L),
      axis.title.x = element_text(size = 15L)
    )
  output$FoodPlot <- renderPlot({
    plot_other_food
  })
  output$LandPlot <- renderPlot({
    plot_other_land
  })
  
  G_GDP <- read.csv("Grenada GDP.csv")
  plot_G <- ggplot(G_GDP) +
    aes(x = Year, y = GDP) +
    geom_line(colour = "#228B22") +
    labs(title = "GDP of Grenada") +
    theme_minimal() +
    theme(plot.title = element_text(size = 17L))
  output$GGDPPlot <- renderPlot({
    plot_G
  })
  
  weak <- data.frame(
    Sector = c("Agriculture", "Industry", "Services"),
    Proportion = c(0.062, 0.143, 0.795)
  )
  weak_plot <- plot_ly(weak, labels = ~Sector, values = ~Proportion, type = 'pie',
          marker = list(colors = colors, line = list(color = 'white', width = 2))) %>%
    layout(title = "Grenada’s economic proportion of different sectors")
  output$weakPlot <- renderPlotly({
    weak_plot
  })
  
  dem_data <- raster("Grenada.tif")
  dem_min_valid <- min(dem_data[dem_data > -3.402823e+38], na.rm = TRUE)
  dem_max_valid <- max(dem_data[], na.rm = TRUE)
  levels <- seq(from = floor(dem_min_valid / 100) * 100, 
                to = ceiling(dem_max_valid / 100) * 100, 
                by = 100)
  contour_data <- rasterToContour(dem_data, levels = levels)
  contour_sf <- st_as_sf(contour_data)
  contour_sf$level <- as.numeric(contour_sf$level)
  ggplot_object <- ggplot() +
    geom_sf(data = contour_sf, aes(color = level)) +
    scale_color_viridis_c() +
    labs(title = "Grenada Contour Map", x = "Longitude", y = "Latitude") +
    theme_minimal()
  plotly_object <- ggplotly(ggplot_object)
  output$ThreatsPlot <- renderPlotly({
    plotly_object
  })
  
  output$mainPanelUI <- renderUI({
    selectedTab <- input$tabset
    if (selectedTab == "In CARICOM") {
      selectedCaribbeanTab <- input$caribbeanTabset
      if (selectedCaribbeanTab == "Introduction of CARICOM") {
        generateIntroductionOfCARICOMContent()
      } else if(selectedCaribbeanTab == "Population") {
        generatePopulationOfCARICOMContent()
      } else if (selectedCaribbeanTab == "Finance") {
        generateFinanceOfCARICOMContent()
      } else if (selectedCaribbeanTab == "Other"){
        generateOtherOfCARICOMContent()
      } else{
        ""
      }
    } else if (selectedTab == "SWOT") {
      selectedSwotTab <- input$swotTabset
      if (selectedSwotTab == "Strength") {
        generateStrengthContent()
      } else if (selectedSwotTab == "Weakness") {
        generateWeaknessContent()
      } else if (selectedSwotTab == "Opportunities") {
        generateOpportunitiesContent()
      } else if (selectedSwotTab == "Threats") {
        generateThreatsContent()
      } else {
        ""
      }
    } else {
      ""
    }
  })
}

shinyApp(ui, server)
