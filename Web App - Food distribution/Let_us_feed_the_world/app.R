# SHINY APP : Feed The World

# Import required packages -----------------------------------------------------
library(shiny)            # To build and run the app
library(shinyWidgets)     # To design the user interface
library(shinythemes)      # To design the user interface
library(shinycssloaders)  # To signify loading
library(shinyhelper)      # To provide help info
library(colorspace)       # To set colors
library(tidyverse)        # To type cleaner code
library(plotly)           # To make interactive plots
library(DT)               # To build interactive datatable
library(igraph)           # To build igraph Network
library(visNetwork)       # To build vis Network


# Load data sets ---------------------------------------------------------------
data_1 <- read_csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vRKkFNgyrqyaqxTBLnAqEy8Ru-a31QG8doskSI6Zxvqgr9N5ZUwYKwVGr-eBOSWsW9p4zkOYmWH5HjO/pub?output=csv")
codes <- read_csv("https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv")
countries_of_the_world <- inner_join(data_1, codes, by = c("Country" = "COUNTRY"))


# User Interface (UI) for the application --------------------------------------

# Set loading spinner style
options(spinner.color = "#31b0d5", spinner.type = 1, spinner.size = 2)

ui <- fluidPage(
    
    theme = shinythemes::shinytheme('slate'),
    titlePanel("Feed The World"),
    
    # Navigation bar with sections
    navbarPage(title = a("Daniel Ope",
                         # Link to LinkedIn profile
                          href = "https://www.linkedin.com/in/opeoluwa-oyedeji-600399174/",
                         target= "_blank"),
        # DATA TAB ------------------------------------------------------------
        tabPanel("Data",
            # Sidebar with input controls
            sidebarLayout(
                sidebarPanel(
                    h4("General"),
                    pickerInput(
                        inputId = "general_category",
                        label = "Select category",
                        choices = c('Population',
                                    'GDP ($ per capita)',
                                    'Illiteracy (%)',
                                    'Arable (%)',
                                    'Birthrate',
                                    'Deathrate',
                                    'Net migration')
                    ) %>%
                        # Help info on selecting categories
                        helper(
                            type = "markdown",
                            title = "Categories",
                            colour = "#31b0d5",
                            size = "l",
                            buttonLabel = "Close",
                            fade = TRUE,
                            content = "general_category_help_info"
                        ),
                 
                    radioGroupButtons(
                        inputId = "general_view",
                        label = "Select view", 
                        choices = c("Map", "Globe")
                    ),
                    hr(),
                    h4("Food Prices"),
                    h5("Controllers not yet created")
                ),

                # Show plots
                mainPanel(
                    tabsetPanel(
                        tabPanel("General", plotlyOutput("general") %>% 
                                     withSpinner()),
                        tabPanel("Food Prices",
                                 HTML("<h1 style='color: #31b0d5; text-align:center'>
                                       This page is intentionally left blank
                                       </h1>")
                        )
                    )
                )
            )                
        ),
        # SIMULATION TAB -------------------------------------------------------
        tabPanel("Simulation",
                 HTML("<h1 style='color: #31b0d5; text-align:center'>
                      This page is intentionally left blank
                      </h1>")
        ),
        
        # GAME TAB -------------------------------------------------------------
        navbarMenu("Games",
                   # Sharing Game
                   tabPanel("Sharing",
                            HTML("<div style = 'color: #31b0d5; text-align:center'>
                                 <h1>
                                 The 'Sharing' game is still in production.
                                 </h1><br>
                                 <h2>
                                 Check out the 'Logistics' game.
                                 </h2>
                                 </div>")
                    ),
                   
                   # Logistics Game
                   tabPanel("Logistics",
                        h3("Reset to begin"),
                        hr(),
                        visNetworkOutput("net") %>% withSpinner(),
                        hr(),
                        fluidRow(
                            column(4,
                                   pickerInput(
                                       inputId = "senderPlanet",
                                       label   = "Select sender",
                                       choices = c('Aupra', 'Buruta', 'Cauliv', 'Dimanope', 'Ephorth')
                                   ),
                                   br(),
                                   pickerInput(
                                       inputId = "receiverPlanet",
                                       label   = "Select receiver",
                                       choices = c('Aupra', 'Buruta', 'Cauliv', 'Dimanope', 'Ephorth'),
                                       selected= "Buruta" 
                                   )
                            ),
                            column(4,
                                   pickerInput(
                                       inputId = "send_resource",
                                       label   = "Select resource",
                                       choices = c('Uthite', 'Vefroutrium', 'Wathum')
                                   ),
                                   br(),
                                   numericInput(
                                       inputId = "resource_amount",
                                       label   = "Select amount",
                                       value   = 0
                                   )
                           ),
                           column(4,
                                  pickerInput(
                                      inputId = "sizeDependsOn",
                                      label   = "Vary size based on",
                                      choices = c('Population', 'Uthite', 'Vefroutrium', 'Wathum')
                                  ),
                                  br(),
                                  actionBttn(
                                      inputId = "run_network",
                                      label = "Run",
                                      icon  = icon("play"),
                                      style = "fill", 
                                      color = "default"
                                  ),
                                  actionBttn(
                                      inputId = "gameTutorial",
                                      label = "Tutorial",
                                      icon  = icon("info-circle"),
                                      style = "fill", 
                                      color = "default"
                                  ),
                                  actionBttn(
                                      inputId = "reset_network",
                                      label = "Reset",
                                      icon  = icon("sync"),
                                      style = "fill", 
                                      color = "default"
                                  )
                           ),
                        ),
                        hr(),
                        DT::dataTableOutput("net_table") %>% withSpinner(),
                        br()
                    )
                   ),
        
        # ABOUT TAB ------------------------------------------------------------
        tabPanel("About",
                 HTML("<div style= 'text-align:center'>
                      <h1 style='color: #31b0d5'>
                      This page is intentionally left blank.
                      </h1><br>
                      <h2 style='color: #31b0d5'>
                      For now you can check out my LinkedIn Profile.
                      </h2><br>
                      <a style='color: #5499C7; font-size:30px';
                      href = 'https://www.linkedin.com/in/opeoluwa-oyedeji-600399174/';
                      target = '_blank'>
                      Opeoluwa Oyedeji
                      </a><br>
                      <a style='color: #5499C7; font-size:30px';
                      href = 'https://daniel-ope.shinyapps.io/Feed_The_World_Code_Documentation/';
                      target = '_blank'>
                      Code Documentation
                      </a>
                      </div>")
        )
    )
)


# Define server logic ----------------------------------------------------------
server <- function(input, output) {
    
    # Load the help info
    observe_helpers(help_dir = "./helpfiles", withMathJax = TRUE)

    # DATA TAB -----------------------------------------------------------------

    # Settings for the globe
    g <- list(
        showframe = FALSE,
        showcoastlines = FALSE,
        projection = list(type = 'orthographic'),
        resolution = '200',
        showcountries = TRUE,
        countrycolor = '#d1d1d1',
        showland = TRUE,
        landcolor = '#ffffff',
        showocean = TRUE,
        oceancolor = '#064273'
        )
    
    
   # Plot the General Map/Globe    
    output$general <- renderPlotly({
        if(input$general_view == "Map"){
            plot_ly(countries_of_the_world, type='choropleth',
                    locations = ~CODE,
                    z = countries_of_the_world[[input$general_category]],
                    text = ~Country,
                    colors = sequential_hcl(12, "ag_GrnYl"))
        }else if(input$general_view == "Globe"){
            plot_ly(countries_of_the_world, type='choropleth',
                    locations = ~CODE,
                    z = countries_of_the_world[[input$general_category]],
                    text = ~Country,
                    colors = sequential_hcl(12, "ag_GrnYl")) %>%
                layout(geo = g)
        }
    })
    
    # GAME TAB -----------------------------------------------------------------
    
    # Set up Planets' info
    Planets <- c('Aupra', 'Buruta', 'Cauliv', 'Dimanope', 'Ephorth')
    pop <- eventReactive(input$reset_network,{sample(50000:100000, size = 5)})
    Uth <- eventReactive(input$reset_network,{sample(1:100, size = 5)})
    Ve <- eventReactive(input$reset_network,{sample(1:100, size = 5)})
    Wa <- eventReactive(input$reset_network,{sample(1:100, size = 5)})
    
    # Set up Tutorial info
    observeEvent(input$gameTutorial,{
        showModal(modalDialog(
            title = HTML("<h2 style='color: #31b0d5'>
                         Logistics game tutorial
                         </h2>"),
            HTML("<p style='font-size:20px'>
                 This will be published when the game is fully functional
                 </p>"),
            easyClose = TRUE
        ))
    })
    
    # Set up Run info
    observeEvent(input$run_network,{
        showModal(modalDialog(
            title = HTML("<h2 style='color: #31b0d5'>
                         Coming soon
                         </h2>"),
            HTML("<p style='font-size:20px'>
                 Game not fully developed
                 </p>"),
            easyClose = TRUE
        ))
    })
    
    
    # Render data table
    output$net_table <- renderDT({

        # Disclaimer -----------------------------------------------------------
        ## This Code Chunk appears in Data Table & Network Visualization
        # Build the data frame
        gameData_nodes <- data_frame(Planets,
                                     Population = pop(),
                                     Uthite = Uth(),
                                     Vefroutrium = Ve(),
                                     Wathum = Wa(),
                                     title  = paste("<p style='color:#17202A'>",
                                                    "<b style='color:#31b0d5'>Planet</b>:",Planets[1:5],
                                                    "<br><b style='color:#31b0d5'>Population</b>:",Population[1:5],
                                                    "<br><b style='color:#31b0d5'>Uthite</b>:",Uthite[1:5],
                                                    "<br><b style='color:#31b0d5'>Vefroutrium</b>:",Vefroutrium[1:5],
                                                    "<br><b style='color:#31b0d5'>Wathum</b>:", Wathum[1:5],"</p>")
        )

        # Update the data frame based on resource_amount
        ## Update Sender
        gameData_nodes[gameData_nodes$Planets == input$senderPlanet,
                       input$send_resource] <-
            gameData_nodes[gameData_nodes$Planets == input$senderPlanet,
                           input$send_resource] - input$resource_amount
        ## Update Receiver
        gameData_nodes[gameData_nodes$Planets == input$receiverPlanet,
                       input$send_resource] <-
            gameData_nodes[gameData_nodes$Planets == input$receiverPlanet,
                           input$send_resource] + input$resource_amount
        # ----------------------------------------------------------------------
        
        
        # Listing the column headers for styling
        Header = htmltools::withTags(table(
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
        
        # Setting the column header text color and font-size
        ## code written in JavaScript (JS)
        headerCallback <- "function( thead, data, start, end, display ) {
            $(thead).closest('thead').find('th').css({'color': '#31b0d5', 'font-size': '130%'});
        }"
        
        datatable(gameData_nodes[-6], rownames = FALSE,
                  container = Header,
                  options = list(
                      headerCallback = JS(headerCallback),
                      pageLength = 5,
                      dom = 'rt',
                      columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  )) %>% 
            formatStyle(columns = colnames(gameData_nodes[-6]),
                        fontSize = '120%')
    })

    # Plot the network
    output$net <- renderVisNetwork({
        
        # Disclaimer -----------------------------------------------------------
        ## This Code Chunk appears in Data Table & Network Visualization
        # Build the data frame
        gameData_nodes <- data_frame(Planets,
                                     Population = pop(),
                                     Uthite = Uth(),
                                     Vefroutrium = Ve(),
                                     Wathum = Wa(),
                                     title  = paste("<p style='color:#17202A'>",
                                                    "<b style='color:#31b0d5'>Planet</b>:",Planets[1:5],
                                                    "<br><b style='color:#31b0d5'>Population</b>:",Population[1:5],
                                                    "<br><b style='color:#31b0d5'>Uthite</b>:",Uthite[1:5],
                                                    "<br><b style='color:#31b0d5'>Vefroutrium</b>:",Vefroutrium[1:5],
                                                    "<br><b style='color:#31b0d5'>Wathum</b>:", Wathum[1:5],"</p>")
        )
        
        # Update the data frame based on resource_amount
        ## Update Sender
        gameData_nodes[gameData_nodes$Planets == input$senderPlanet,
                       input$send_resource] <-
            gameData_nodes[gameData_nodes$Planets == input$senderPlanet,
                           input$send_resource] - input$resource_amount
        ## Update Receiver
        gameData_nodes[gameData_nodes$Planets == input$receiverPlanet,
                       input$send_resource] <-
            gameData_nodes[gameData_nodes$Planets == input$receiverPlanet,
                           input$send_resource] + input$resource_amount
        # ----------------------------------------------------------------------
        
        
        # Set up Planet links
        gameData_edges <- data_frame(
            from   = c(input$senderPlanet),
            to     = c(input$receiverPlanet),
            weight = c(input$resource_amount),
            type   = c(input$send_resource)
        )
        
        # Create igraph network
        network <- graph.data.frame(gameData_edges, gameData_nodes, directed = TRUE)
        
        # Stylize the network
        ## Edge style
        if(input$send_resource == "Uthite"){E(network)$color <- "#CD6155"}
        if(input$send_resource == "Vefroutrium"){E(network)$color <- "#58D68D"}
        if(input$send_resource == "Wathum"){E(network)$color <- "#633974"}
        E(network)$width <- E(network)$weight/10
        
        ## Vertex style
        interaction <- c(input$senderPlanet, input$receiverPlanet)
        V(network)$color <- "#5499C7"
        V(network)[interaction]$color <- "#1A5276"
        if(input$sizeDependsOn == "Population"){V(network)$size <- V(network)$Population/1000}
        if(input$sizeDependsOn == "Uthite"){V(network)$size <- V(network)$Uthite}
        if(input$sizeDependsOn == "Vefroutrium"){V(network)$size <- V(network)$Vefroutrium}
        if(input$sizeDependsOn == "Wathum"){V(network)$size <- V(network)$Wathum}
        
        #V(network)$size <- V(network)$Population/1000
        
        # Convert igraph to visNetwork
        network_vis <- toVisNetworkData(network)
        visNetwork(nodes = network_vis$nodes,
                   edges = network_vis$edges,
                   background = "#FBFCFC") %>%
            visEdges(arrows = "to") %>% 
            visIgraphLayout(layout = "layout_in_circle",
                            physics = TRUE,
                            smooth = TRUE) %>% 
            visPhysics(solver = 'forceAtlas2Based',
                       forceAtlas2Based = list(gravitationalConstant = -100,
                                               springLength = 150,
                                               avoidOverlap = 0.5))
    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)

