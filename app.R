library(shiny)
library(traviz)

ui <- fluidPage(
    titlePanel("traviz Demo with enviroCar data"),
    sidebarLayout(
        sidebarPanel(
            helpText("Visualize and analyze enviroCar trajectory data in Munster, Germany"),

            selectInput("functions",
                        label = "Choose a traviz method to use",
                        choices = c("Plot trajectories",
                                    "Plot trajectory intersections",
                                    "Rasterize data with value",
                                    "Show heatmap of value",
                                    "Show quadrat heatmap of value"),
                        selected = "Plot trajectories"),

            conditionalPanel(
                condition = "input.functions == 'Plot trajectories'",
                sliderInput("num_tracks", "Number of tracks to plot: ",
                            min = 1, max = 20,
                            value = 1, step = 1)
            ),
            conditionalPanel(
                condition = "input.functions == 'Plot trajectory intersections'",
                sliderInput("intersect_track", "Track to determine if it intersects with: ",
                            min = 2, max = 20,
                            value = 2, step = 1)
            )



        ),
        mainPanel(plotOutput("traj_plot"))

    )

)

server <- function(input, output) {
    load("data/ec.trj.rda")
    #subset data for speed
    ec.trj <- ec.trj[50:70,]
    sftc <- df_to_sfTracks(ec.trj)

    tracks_subset <- reactive({
        tracks <- df_to_sfTracks(ec.trj[1:input$num_tracks,])
        return(tracks)
    })

    tracks_intersection <- reactive({
        intersection <- intersection.sfTrack(sftc@tracks[[1]], sftc@tracks[[input$intersect_track]])
        return(intersection)
    })

    output$traj_plot <- renderPlot({
        if(input$functions == "Plot trajectories") {plot.sfTracks(tracks_subset())}
        else if(input$functions == "Plot trajectory intersections") {tracks_intersection()}
    })
}


shinyApp(ui = ui, server = server)
