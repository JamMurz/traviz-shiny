library(shiny)
library(traviz)
library(raster)
load("data/ec.trj.rda")


ui <- fluidPage(
    titlePanel("traviz Demo with enviroCar data"),
    sidebarLayout(
        sidebarPanel(
            helpText("Visualize and analyze enviroCar trajectory data in Munster, Germany"),

            selectInput("functions",
                        label = "Choose a traviz method to use",
                        choices = c("Plot trajectories",
                                    "Plot trajectories point values",
                                    "Plot trajectory intersections",
                                    "Rasterize data with value",
                                    "Show heatmap of value",
                                    "Show quadrat heatmap of trajectory density",
                                    "Show Frechet distance clusters of trajectories"),
                        selected = "Plot trajectories"),

            conditionalPanel(
                condition = "input.functions == 'Plot trajectories'",
                sliderInput("num_tracks", "Tracks to plot: ",
                            min = 1, max = 20,
                            value = c(1,2), step = 1)
            ),
            conditionalPanel(
                condition = "input.functions == 'Plot trajectory intersections'",
                sliderInput("intersect_track", "Track to find intersection with: ",
                            min = 1, max = 20,
                            value = c(1,2), step = 1),
            ),
            conditionalPanel(
                condition = "input.functions == 'Rasterize data with value'",
                selectInput("raster_value",
                            label = "Choose value to rasterize",
                            choices = c("CO2.value",
                                        "Speed.value",
                                        "Consumption.value")),

                sliderInput("raster_resolution", "Pixel resolution",
                            min = .0001, max = .05,
                            value = .001, step = .0001),

                sliderInput("lat", "Latitude",
                            min = as.numeric(st_bbox(ec.trj)[1]),
                            max = as.numeric(st_bbox(ec.trj)[3]),
                            value = c(as.numeric(st_bbox(ec.trj)[1]),
                                      as.numeric(st_bbox(ec.trj)[3])),
                            step = .01),

                sliderInput("lon", "Longitude",
                            min = as.numeric(st_bbox(ec.trj)[2]),
                            max = as.numeric(st_bbox(ec.trj)[4]),
                            value = c(as.numeric(st_bbox(ec.trj)[2]),
                                      as.numeric(st_bbox(ec.trj)[4])),
                            step = .01),

                checkboxInput("idwi", "Use inverse distance weighted interpolation?", FALSE)

            ),

            conditionalPanel(
                condition = "input.functions == 'Show heatmap of value'",
                selectInput("heatmap_value",
                            label = "Choose value to rasterize",
                            choices = c("CO2.value",
                                        "Speed.value",
                                        "Consumption.value")),
                sliderInput("heatmap_resolution", "Pixel resolution",
                            min = .0001, max = .01,
                            value = .001, step = .0001)
            ),

            conditionalPanel(
                condition = "input.functions == 'Show Frechet distance clusters of trajectories'",
                sliderInput("num_clusters", "Number of clusters",
                            min = 1, max = 20,
                            value = 1, step = 1)

            ),

           conditionalPanel(
               condition = "input.functions == 'Plot trajectories point values'",
               selectInput("point_values",
                           label = "Choose point value",
                           choices = c("CO2.value",
                                       "Speed.value",
                                       "Consumption.value")),
           )



        ),
        mainPanel(plotOutput("traj_plot"))

    )
)

server <- function(input, output) {
    #subset data for speed
    ec.trj_rast <- ec.trj[1:20,]
    ec.trj <- ec.trj[63:83,]
    st_crs(ec.trj) <- 4326
    sftc <- df_to_sfTracks(ec.trj)
    ec.trj_un <- ec.trj %>% unnest
    ec.trj_rast_un <- ec.trj_rast %>% unnest

    tracks_subset <- reactive({
        tracks <- df_to_sfTracks(ec.trj[input$num_tracks[1]:input$num_tracks[2],])
        return(tracks)
    })

    tracks_pv <- reactive({
        return(plot.sfTracks(sftc, input$point_values))
    })

    tracks_intersection <- reactive({
        intersection <- intersection.sfTrack(sftc@tracks[[input$intersect_track[1]]], sftc@tracks[[input$intersect_track[2]]])
        return(intersection)
    })

    tracks_rasterize <- reactive({
        if(input$idwi == FALSE){
            return(aggregate_raster_region(sf_to_rasterize(ec.trj_rast_un, data = input$raster_value, resolution = input$raster_resolution),
                                           xmin = input$lat[1], xmax = input$lat[2], ymin = input$lon[1], ymax = input$lon[2]))
        }
        else{
            library(gstat)
            #TO do: add aggregation to idwi
            return(idwi_raster(ec.trj_un, measurement = input$raster_value, resolution = input$raster_resolution))
        }
    })

    tracks_heatmap <- reactive({
        library(maptools)
        return(density_heatmap(ec.trj_un, input$heatmap_value, resolution = input$heatmap_resolution))
    })

    tracks_quadrat <- reactive({
        library(spatstat)
        return(traj_quadrat(ec.trj_un))
    })

    tracks_cluster <- reactive({
        library(sf)
        return(cluster_traj(ec.trj, input$num_clusters))
    })

    output$traj_plot <- renderPlot({
        if(input$functions == "Plot trajectories") {plot.sfTracks(tracks_subset())}
        else if(input$functions == "Plot trajectories point values") {tracks_pv()}
        else if(input$functions == "Plot trajectory intersections") {tracks_intersection()}
        else if(input$functions == "Rasterize data with value") {plot(tracks_rasterize())}
        else if(input$functions == "Show heatmap of value") {(tracks_heatmap())}
        else if(input$functions == "Show quadrat heatmap of trajectory density") {tracks_quadrat()}
        else if(input$functions == "Show Frechet distance clusters of trajectories") {plot(tracks_cluster())}
    })
}


shinyApp(ui = ui, server = server)
