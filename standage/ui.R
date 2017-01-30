library(shiny)
library(leaflet)

reg <- c(
  "DNR"="State Department of Natural Resources",
  "BLM"="Bureau of Land Management (BLM)",
  "DOD/DOE"="DOD and DOE",
  "FWS"="Fish and Wildlife Service (FWS)",
  "NPS"="National Park Service (NPS)",
  "CA, BC"="Government of British Columbia",
  "CA, PCA"="Government of Canada, Parks Canada Agency",
  "Yukon, DOE"="Government of Yukon, Department of Environment"
)
veg <- c("Deciduous", "Black Spruce", "White Spruce")

text_mean <- "Total area for each mean stand age bin is the percentage of the total vegetation cover area distribution mean
where stand age falls within each age bin given the corresponding stand age probability distribition."

text_ci <- "Stand age confidence bands are based on age variability in space as well as uncertainty in total vegetation cover area across Alfresco simulations.
Confidence limits are computed for the total vegetation cover area probability distribution and are used to bound mean stand age."

text_dist1 <- "Three example aggregate periods (2010-2039, 2040-2069, and 2070-2099) show nonparametric estimated probability density functions
for stand age.
Stand age distributions are estimated across space and simulation replicates at an annual temporal resolution for each vegetation class and spatial domain.
Total vegetation cover area probability distributions are similarly derived across simulations but not across space 
(total cover area is already spatially aggregated by definition)."

text_dist2 <- "These estimated annual probability distributions can be integrated over levels of other variables such as time to obtain marginal distributions.
For each period here, 30 annual distributions are integrated to show stand age densities at a coarser time scale.
These density plots are included to provide context regarding where mean stand age estimates and uncertainty in the other two plots are derived from."

shinyUI(fluidPage(
  title="TPA stand age",
  fluidRow(column(12, h3("Terrestrial protected areas stand age summary"))),
  tabsetPanel(
    tabPanel("Mean", plotOutput("plotmean", height="auto")),
    tabPanel("95% confidence", plotOutput("plotci", height="auto")),
    tabPanel("Example distributions", plotOutput("plotdist", height="auto")),
    id="tsp"
  ),
  fluidRow(
    column(2, selectInput("reg", "Area", reg, reg[1], multiple=TRUE, width="100%")),
    column(2, selectInput("veg", "Vegetation", veg, width="100%")),
    conditionalPanel("input.tsp == 'Example distributions'",
      column(2, selectInput("fctby", "Facet by", c("Region"="Location", "Period"), width="100%"))),
    conditionalPanel("input.tsp != 'Example distributions'",
      column(2, selectInput("x", "X-axis", c("Year", "Decade"), width="100%"))),
    column(2, selectInput("fctscales", "Axis scales", c("Fixed"="fixed", "Free"="free_y"), width="100%")),
    conditionalPanel("input.tsp == 'Example distributions'",
      column(2, sliderInput("adj", "Density smoothing", 0.2, 2, 1, 0.2, width="100%"))),
    conditionalPanel("input.tsp != 'Example distributions'",
      conditionalPanel("input.tsp == 'Mean'",
        column(2, selectInput("pos", "Bars", c("Stacked"="stack", "Proportions"="fill"), width="100%"))
      ),
      column(2, sliderInput("span", "Series smoothing", 0, 1, 0, 0.1, width="100%"))
    )
  ),
  fluidRow(
    column(12,
      conditionalPanel("input.tsp == 'Mean'", p(em(text_mean))),
      conditionalPanel("input.tsp == '95% confidence'", p(em(text_ci))),
      conditionalPanel("input.tsp == 'Example distributions'", p(em(text_dist1)), p(em(text_dist2)))
    )
  )
))
