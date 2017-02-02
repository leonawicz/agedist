library(shiny)
library(leaflet)
library(dplyr)
library(purrr)
library(ggplot2)

mult <- 1000
d <- readRDS("age.rds") %>% map(~mutate(.x, Mean=Mean/mult, LB=LB/mult, UB=UB/mult))
xscale <- scale_x_continuous(breaks=seq(1900, 2100, by=10), expand=c(0, 0))
yscale <- scale_y_continuous(expand=c(0,0))
ylbs <- c(expression("Area"~(1000~km^2)~""), expression("Area"~(symbol("\045"))~""))
plottheme <- theme(panel.grid.major = element_line(size = .5, color = "grey"),
                   plot.title=element_text(hjust=0.5),
                   axis.line=element_line(size=.7, color="black"),
                   axis.ticks.length=unit(0.35,"cm"),
                   legend.position="bottom",
                   text = element_text(size=14),
                   panel.spacing.x=unit(1,"cm"),
                   plot.margin=unit(c(0.5, 1, 0.5, 0.5),"cm"),
                   strip.text=element_text(size=14))
d2 <- readRDS("distributions.rds")

shinyServer(function(input, output, session) {

  steps <- reactive(data.frame(element=c(
    "#tb", "a[data-value=\"Example distributions\"]", "#plotmean", "#reg", "#veg", "#x", "#fctscales", "#pos", "#span"),
     intro=c(
       "Stand age summaries are available as means and interval estimates
       at multiple plot tabs offering several different views of the data.
       These are based on annual estimated continuous stand age probability distributions.",
       "A few temporaly aggregated examples of these distributions are available on the last tab.",
       "Mean cover area through time is shown here for each age bin.",
       "Choose one or more areas.",
       "Choose a vegetation class.",
       "Set the temporal resolution to annual or decadal.",
       "If comparing multiple areas, the y-axis scales can be fixed across plot panels or free.
       Free axes can be helpful when comparing vastly different vegetation cover area",
       "For mean stand age (bar plot only), age bins can be stacked or displayed as proportions.",
       "There is no smoothing applied when the slider is at zero. Some trends may be more
       visible with some degree of smoothing applied, especially when there are several 
       overlapping data groups."
     ),
     position=c("bottom", "left", "bottom", rep("bottom", 6))
  ))
  
  # observeEvent(input$help, {
  #   introjs(session, options=list(
  #     steps=steps(), "showProgress"="true", "showStepNumbers"="false", "showBullets"="false"),
  #     events=list(
  #       "onchange" =
  #         "if (this._currentStep==0) {
  #         $('a[data-value=\"age\"]').trigger('click');
  #         }"
  #     ))
  # })
  
  observeEvent(input$map, {
    showModal(modalDialog(
      title="Terrestrial protected areas",
      img(src="tpa_map.png", style="width: 100%;"),
      size="l", easyClose=TRUE, footer=NULL
    ))
  })
  
  span <-reactive({
    x <- input$span
    if(is.null(x) || x==0) NULL else x
  })

  data <- reactive({
    x <- if(is.null(span())) d else
      map(d, ~mutate(.x, Mean=loess.smooth(Year, Mean, span=span(), evaluation=n())$y,
        LB=loess.smooth(Year, LB, span=span(), evaluation=n())$y,
        UB=loess.smooth(Year, UB, span=span(), evaluation=n())$y))
    x <- filter(x[[input$veg]], Location %in% input$reg)
    if(input$x=="Decade")
      x <- filter(x, Year < 2100) %>%
        mutate(Decade=factor(paste0(Decade, "s"))) %>%
        group_by(Location, Vegetation, Age, Decade) %>%
        summarise(Mean=mean(Mean), LB=mean(LB), UB=mean(UB))
    x
  })

  ylb <- reactive({
    if(input$pos=="stack") ylbs[1] else ylbs[2]
  })

  ph <- reactive({
    if(is.null(input$reg)) return(0)
    base <- 600
    n <- length(input$reg)
    if(input$tsp=="Example distributions" && !is.null(input$fctby) &&
       input$fctby=="Period") return(base)
    if(n < 5) base else base*ceiling(n/2)/2
  })

  output$plotmean <- renderPlot({
    g <- ggplot(data(), aes_string(input$x, "Mean", colour="Age", fill="Age")) +
      plottheme + labs(y=ylb())
    if(input$x=="Year"){
      g <- g + geom_bar(stat="identity", position=input$pos) + xscale
    } else {
      g <- g + geom_bar(stat="identity", position=input$pos, colour="gray20")
    }
    if(length(input$reg) > 1)
      g <- g + facet_wrap(~Location, ncol=2, scales=input$fctscales)
    g + yscale
  }, height=function() ph())

  output$plotci <- renderPlot({
    if(input$x=="Year"){
        g <- ggplot(data(), aes(Year, Mean, colour=Age, fill=Age)) +
          geom_ribbon(aes(ymin=LB, ymax=UB), alpha=0.4) +
          plottheme + labs(y=ylbs[1]) + xscale
    } else {
      g <- ggplot(data(), aes(Decade, Mean, colour=Age, fill=Age)) +
        geom_crossbar(aes(ymin=LB, ymax=UB), alpha=0.4) +
        plottheme + labs(y=ylbs[1])
    }
    if(length(input$reg) > 1)
      g <- g + facet_wrap(~Location, ncol=2, scales=input$fctscales)
    g + yscale
  }, height=function() ph())

  adj <-reactive({
    x <- input$adj
    if(is.null(x)) 1 else x
  })

  data2 <- reactive({ filter(d2, Location %in% input$reg & Vegetation==input$veg) })

  output$plotdist <- renderPlot({
    clrby <- if(input$fctby=="Location") "Period" else "Location"
    g <- ggplot(data2(), aes_string("Val", colour=clrby, fill=clrby)) +
      geom_density(alpha=0.4, adjust=adj()) + plottheme + labs(x="Age", y="Density")
    if(length(input$reg) > 1)
      g <- g + facet_wrap(as.formula(paste0("~", input$fctby)), ncol=2, scales=input$fctscales)
    g + scale_x_continuous(expand=c(0,0)) + yscale
  }, height=function() ph())
  
  outputOptions(output, "plotmean", suspendWhenHidden=FALSE)
  outputOptions(output, "plotci", suspendWhenHidden=FALSE)
  outputOptions(output, "plotdist", suspendWhenHidden=FALSE)
})
