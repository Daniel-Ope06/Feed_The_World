#
# Testing script

# Useful Packages --------------------------------------------------------------
library(tidyverse)
library(plotly)
library(colorspace)
library(RColorBrewer)
library(ggsci)
library(viridis)
library(data.tree)
library(networkD3)
library(ndtv)
library(visNetwork)
#library(leaflet)

# Creating 3D globe ------------------------------------------------------------

codes <- read_csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")

states <- geojsonio::geojson_read("json/us-states.geojson", what = "sp")

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
df[,2] <- as.numeric(df[,2])
df[,2] <- log(df[,2])

#Set country boundaries as light grey
l <- list(color = toRGB("#335a98"), width = 0.5)
#Specify map projection and options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'orthographic'),
  resolution = '100',
  showcountries = TRUE,
  countrycolor = '#768991',
  showocean = TRUE,
  oceancolor = '#064273',
  showlakes = TRUE,
  lakecolor = '#76b6c4',
  showrivers = TRUE,
  rivercolor = '#76b6c4')



p <- plot_geo(df) %>%
  add_trace(z = ~GDP..BILLIONS., color = ~GDP..BILLIONS., colors = sequential_hcl(9, "Mint") ,#viridis_pal(option = "cividis")(5),
            text = ~COUNTRY, locations = ~CODE, marker = list(line = l),reversescale = F) %>%
  colorbar(title = 'GDP (log)') %>%
  layout(title = '', geo = g) 

p
plot_ly(countries_of_the_world, type='choropleth',
        locations = ~CODE,
        z = ~Population, #countries_of_the_world[[input$general_category]],
        text = ~Country,
        colors = sequential_hcl(12, "ag_GrnYl")) %>% layout(geo = g)
# Mint
# ag_GrnYl
# Emrld
plot_geo(countries_of_the_world, type='choropleth', z = ~Population, text = ~Country,colorscale = "Viridis")



# Selecting colors -------------------------------------------------------------

pal <- choose_palette()

# For fixing and cleaning data -------------------------------------------------

C_dirty <- full_join(data_1, codes, by = c("Country" = "COUNTRY"))
C_fix <- C_dirty[is.na(C_dirty$CODE),]
C_fix$Country
f <- C_dirty %>% filter(CODE == 'GMB')
f$Country

create_help_files(files = c("Clusters", "Columns", "PlotHelp"), 
                  help_dir = "helpfiles")

# Creating and Testing Game ----------------------------------------------
game_data_edges <- data_frame(
  from   = c("Aupra"),
  to     = c("Buruta"),
  weight = c(50),
  type   = c("Uthite")
)

game_data_edges_2 <- data_frame(
  source    = c("Aupra"),
  target = c("Buruta"),
  weight = c(50),
  type   = c("Uthite")
)

network_2 <- graph.data.frame(game_data_edges, gameData_nodes, directed=T)

data <- toVisNetworkData(network)
visNetwork(nodes = data$nodes, edges = data$edges, background = "black") %>%
  visEdges(arrows = "to")

visNetwork(game_data_edges, game_data_nodes, directed=T)
E(network)$color <- "#CD6155"
visIgraph(network) %>%
  visIgraphLayout(layout = "layout_in_circle", physics = T, smooth=T) %>% 
  visPhysics(solver = 'forceAtlas2Based',
             forceAtlas2Based = list(gravitationalConstant = -100,
                                     springLength = 150,
                                     avoidOverlap = 0.5))
# 'repulsion'
interaction <- c("Aupra","Buruta")

shiny::runApp(system.file("shiny", package = "visNetwork"))

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(rowspan = 2, 'Species'),
      th(colspan = 2, 'Sepal'),
      th(colspan = 2, 'Petal')
    ),
    tr(
      lapply(rep(c('Length', 'Width'), 2), th)
    )
  )
))

headerCallback <- "function( thead, data, start, end, display ) {
  $(thead).closest('thead').find('th').eq(0).css('background-color', 'green');
  $(thead).closest('thead').find('th').eq(1).css('background-color', 'red');
  $(thead).closest('thead').find('th').eq(2).css('background-color', 'blue');
  $(thead).closest('thead').find('th').eq(3).css('background-color', 'red');
  $(thead).closest('thead').find('th').eq(4).css('background-color', 'red');
  $(thead).closest('thead').find('th').eq(5).css('background-color', 'blue');
  $(thead).closest('thead').find('th').eq(6).css('background-color', 'blue');
}"

datatable(head(iris, 10), 
          container = sketch, options = list(
            headerCallback = JS(headerCallback)
          )
)

# ---------------------------------------------------------------------------

sketch = htmltools::withTags(table(
  class = 'display',
  thead(
    tr(
      th(colspan = 1, 'Planets'),
      th(colspan = 1, 'Population'),
      th(colspan = 1, 'Uthite'),
      th(colspan = 1, 'Vefroutrium'),
      th(colspan = 1, 'Wathum')
    )
  )
))

headerCallback <- "function( thead, data, start, end, display ) {
  $(thead).closest('thead').find('th').eq(0).css({'color': '#31b0d5', 'font-size': '200%'});
  $(thead).closest('thead').find('th').eq(1).css({'color': '#31b0d5', 'font-size': '200%'});
  $(thead).closest('thead').find('th').eq(2).css({'color': '#31b0d5', 'font-size': '200%'});
  $(thead).closest('thead').find('th').eq(3).css({'color': '#31b0d5', 'font-size': '200%'});
  $(thead).closest('thead').find('th').eq(4).css({'color': '#31b0d5', 'font-size': '200%'});
}"

headerCallback <- "function( thead, data, start, end, display ) {
  $(thead).closest('thead').find('th').css('color', '#31b0d5').css({'color': '#31b0d5', 'font-size': '150%'});
}"

datatable(game_data_nodes[-6], 
          container = sketch, options = list(
            headerCallback = JS(headerCallback)
          )
)



observeEvent(input$run_network,{
  # Subtract from sender
  game_data_nodes[game_data_nodes$Planets == input$planet_sender, input$send_resource] <- 
    game_data_nodes[game_data_nodes$Planets == input$planet_sender, input$send_resource] -
    input$resource_amount
  ##sender[input$send_resource] <- sender[input$send_resource] - input$resource_amount
  
  # Add to receiver
  receiver <- game_data_nodes[game_data_nodes$Planets == input$planet_receiver,]
  receiver[input$send_resource] <- receiver[input$send_resource] + input$resource_amount
})



















