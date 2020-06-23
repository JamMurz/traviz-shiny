library(shiny)
library(traviz)

ui <- fluidPage(
    titlePanel("traviz Demo with enviroCar data"),
    sidebarLayout(
        sidebarPanel(
            helpText("Visualize and analyze enviroCar trajectory data in Munster, Germany"),

            selectInput("function",
                        label = "Choose a traviz method to use",
                        choices = c("Plot trajectories",
                                    "Plot trajectory intersections",
                                    "Rasterize data with value",
                                    "Show heatmap of value",
                                    "Show quadrat heatmap of value"),
                        selected = "Plot trajectories"),

            conditionalPanel(
                condition = "input.function == 'Plot trajectories'",
                sliderInput("num_tracks", "Number of tracks to plot: ",
                            min = 1, max = 20,
                            value = 1)
            )


        ),
        mainPanel(plotOutput("plot"))

    )

)

server <- function(input, output) {
    load("data/ec.trj.rda")
    track1 <- sfTrack(ec.trj[20,])
    track2 <- sfTrack(ec.trj[21,])
    track3 <- sfTrack(ec.trj[22,])
    output$plot <- renderPlot({
        plot(track1)
    })
}


shinyApp(ui = ui, server = server)
