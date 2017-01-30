library(shiny)
library(leaflet)

reg <- c(
  "BLM"="Bureau of Land Management (BLM)",
  "DOD/DOE"="DOD and DOE",
  "FWS"="Fish and Wildlife Service (FWS)",
  "CA, BC"="Government of British Columbia",
  "CA, PCA"="Government of Canada, Parks Canada Agency",
  "Yukon, DOE"="Government of Yukon, Department of Environment",
  "NPS"="National Park Service (NPS)",
  "AK, DNR"="State Department of Natural Resources"
)
veg <- c("Deciduous", "Black Spruce", "White Spruce")

shinyUI(fluidPage(
  title="TPA stand age",
  fluidRow(column(3, h3("TPA stand age"))),
  tabsetPanel(
    tabPanel("Mean", plotOutput("plotmean", height="auto")),
    tabPanel("95% confidence", plotOutput("plotci", height="auto")),
    tabPanel("Example distributions", plotOutput("plotdist", height="auto")),
    id="tsp"
  ),
  fluidRow(
    column(2, selectInput("reg", "Area", reg, multiple=TRUE, width="100%")),
    column(2, selectInput("veg", "Vegetation", veg, width="100%")),
    conditionalPanel("input.tsp == 'Example distributions'",
      column(2, selectInput("fctby", "Facet by", c("Region"="Location", "Period"), width="100%"))),
    conditionalPanel("input.tsp != 'Example distributions'",
      column(2, selectInput("x", "X-axis", c("Year", "Decade"), width="100%"))),
    column(2, selectInput("fctscales", "Axis scales", c("Fixed"="fixed", "Free"="free_y"), width="100%")),
    conditionalPanel("input.tsp == 'Example distributions'",
      column(2, sliderInput("adj", "Smoothing", 0.2, 2, 1, 0.2, width="100%"))),
    conditionalPanel("input.tsp != 'Example distributions'",
      conditionalPanel("input.tsp == 'Mean'",
        column(2, selectInput("pos", "Bars", c("Stacked"="stack", "Proportions"="fill"), width="100%"))
      ),
      column(2, sliderInput("span", "Smoothing", 0, 1, 0, 0.1, width="100%"))
    )
  )
))
