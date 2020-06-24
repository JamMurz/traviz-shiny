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
                sliderInput("intersect_track", "Track to find intersection with: ",
                            min = 2, max = 20,
                            value = 2, step = 1)
            ),
            conditionalPanel(
                condition = "input.functions == 'Rasterize data with value'",
                selectInput("raster_value",
                            label = "Choose value to rasterize",
                            choices = c("CO2.value",
                                        "Speed.value",
                                        "Consumption.value")),
                sliderInput("raster_resolution", "Pixel resolution",
                            min = .0001, max = .003,
                            value = .0001, step = .0001),
                sliderInput("time", "Time range",
                            min = as.POSIXct("2019-10-25 15:19:26"),
                            max = as.POSIXct("2020-05-10 01:22:21"),
                            value = c(as.POSIXct("2019-10-25 15:19:26"),
                                      as.POSIXct("2020-05-10 01:22:21")),
                            step = 60)

            ),

            conditionalPanel(
                condition = "input.functions == 'Show heatmap of value'",
                selectInput("heatmap_value",
                            label = "Choose value to rasterize",
                            choices = c("CO2.value",
                                        "Speed.value",
                                        "Consumption.value")),
                sliderInput("heatmap_resolution", "Pixel resolution",
                            min = .0001, max = .003,
                            value = .0001, step = .0001)
            )



        ),
        mainPanel(plotOutput("traj_plot"))

    )
)

server <- function(input, output) {
    load("data/ec.trj.rda")
    #subset data for speed
    ec.trj <- ec.trj[50:70,]
    ec.trj_un <- ec.trj %>% unnest
    sftc <- df_to_sfTracks(ec.trj)
    tracks_subset <- reactive({
        tracks <- df_to_sfTracks(ec.trj[1:input$num_tracks,])
        return(tracks)
    })

    tracks_intersection <- reactive({
        intersection <- intersection.sfTrack(sftc@tracks[[1]], sftc@tracks[[input$intersect_track]])
        return(intersection)
    })

    tracks_rasterize <- reactive({
        return(sf_to_rasterize(ec.trj_un, data = input$raster_value, resolution = input$raster_resolution, from =as.POSIXct(input$time[1]), to =as.POSIXct(input$time[2])))
    })

    tracks_heatmap <- reactive({
        return(density_heatmap(ec.trj_un, input$heatmap_value, resolution = input$heatmap_resolution))
    })

    tracks_quadrat <- reactive({
        return(traj_quadrat(ec.trj_un))
    })

    output$traj_plot <- renderPlot({
        if(input$functions == "Plot trajectories") {plot.sfTracks(tracks_subset())}
        else if(input$functions == "Plot trajectory intersections") {tracks_intersection()}
        else if(input$functions == "Rasterize data with value") {plot(tracks_rasterize())}
        else if(input$functions == "Show heatmap of value") {(tracks_heatmap())}
        else if(input$functions == "Show quadrat heatmap of value") {tracks_quadrat()}

    })
}


shinyApp(ui = ui, server = server)
