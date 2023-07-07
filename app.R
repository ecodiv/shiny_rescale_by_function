#
# This is a app to rescale values according to a user-defined function
# Ecodiv.earth
#

# Install required packages if not already installed
#-------------------------------------------------------------------------------
if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(ggplot2)) {
  install.packages("ggplot2")
}
if (!require(terra)) {
  install.packages("terra")
}
if (!require(tidyterra)) {
  install.packages("tidyterra")
}
library(shiny)
library(ggplot2)
library(tidyterra)
library(terra)

# Functions plot
#-------------------------------------------------------------------------------

# Linear rescale function
linear_rescale <-
  function(x, range_min, range_max, lower_bound, upper_bound, target_min, target_max) {
    if (lower_bound < upper_bound) {
      vals <- ifelse(x < lower_bound, 0, 
                     ifelse(x > upper_bound, 1,
                            (x - lower_bound) / (upper_bound - lower_bound)
                     ))
    } else{
      vals <- ifelse(x > lower_bound, 0,
                     ifelse(x < upper_bound, 1,
                            (x - lower_bound) / (upper_bound - lower_bound)
                     ))
    }
    
    vals <- vals * (target_max - target_min) + target_min
    return(vals)
  }

linres <-
  function(r, s, lower_bound, upper_bound, target_min, target_max) {
    if (lower_bound < upper_bound) {
      s[r < lower_bound] = target_min
      s[r > upper_bound] = target_max
      s[r >= lower_bound & r <= upper_bound] = 
        ((r - lower_bound) / (upper_bound - lower_bound) * 
           (target_max - target_min) + target_min)
      return(s)
    } else {
      s[r > lower_bound] = target_min
      s[r < upper_bound] = target_max
      s[r <= lower_bound & r >= upper_bound] = 
        ((r - lower_bound) / (upper_bound - lower_bound) * 
           (target_max - target_min) + target_min)
      return(s)      
    }
  }

power_rescale <-
  function(x, lower_bound, upper_bound, a, target_min, target_max) {
    if (lower_bound < upper_bound) {
      vals <- ifelse(x < lower_bound, 0,
                     ifelse(x > upper_bound, 1,
                            ((x - lower_bound) / (upper_bound - lower_bound)
                            ) ^ a))
    } else{
      vals <- ifelse(x > lower_bound, 0,
                     ifelse(x < upper_bound, 1,
                            ((x - lower_bound) / (upper_bound - lower_bound)
                            ) ^ a))
    }
    vals <- vals * (target_max - target_min) + target_min
    return(vals)
  }

powres <-
  function(r, s, lower_bound, upper_bound, target_min, target_max, a) {
    if (lower_bound < upper_bound) {
      s[r < lower_bound] = target_min
      s[r > upper_bound] = target_max
      s[r >= lower_bound & r <= upper_bound] = 
        (((r - lower_bound) / (upper_bound - lower_bound))^a * 
           (target_max - target_min) + target_min)
      return(s)
    } else {
      s[r > lower_bound] = target_min
      s[r < upper_bound] = target_max
      s[r <= lower_bound & r >= upper_bound] = 
        (((r - lower_bound) / (upper_bound - lower_bound))^a * 
           (target_max - target_min) + target_min)
      return(s)      
    }
  }

# Gaussian rescale function
gaussian_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- exp(-f1 * (x - f2)^2)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

gaures <- function(r, f1, f2, target_min, target_max) {
    s = exp(-f1 * (r - f2)^2) * (target_max - target_min) + target_min
    return(s)
  }

# Small membership rescale function
small_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + (x/f2)^f1)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

smares <- function(r, f1, f2, target_min, target_max) {
    s = (1 / (1 + (r/f2)^f1)) * (target_max - target_min) + target_min
    return(s)
  }

# Large membership rescale function
large_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + (x/f2)^-f1)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

larres <- function(r, f1, f2, target_min, target_max) {
    s = (1 / (1 + (r/f2)^-f1)) * (target_max - target_min) + target_min
    return(s)
  }

# Near membership rescale function
near_rescale <- function(x, f1, f2, target_min, target_max){
  vals <- 1 / (1 + f1*(x - f2)^2)
  vals <- vals * (target_max - target_min) + target_min
  return(vals)
}

neares <- function(r, f1, f2, target_min, target_max) {
    s = (1 / (1 + f1*(r - f2)^2)) * (target_max - target_min) + target_min
    return(s)
  }

# UI
#-------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Rescale Function Visualization"),
  sidebarLayout(
    sidebarPanel(
      fluidRow(column(6, numericInput("range_min", "Range Min:", min = NA, max = NA, value = 0)),
               column(6,numericInput("range_max", "Range Max:", min = NA, max = NA, value = 50))),
      fluidRow(column(6, numericInput("target_min", "target range min:", min = NA, max = NA, value = 0)),
               column(6,numericInput("target_max", "Target range max:", min = NA, max = NA, value = 1))),
      
      selectInput(
        "rescale_type",
        "Rescale Type:",
        choices = c("Linear", "Gaussian", "Power",
                    "Small", "Large", "Near"),
        selected = "Linear"
      ),
      
      conditionalPanel(
        condition = "input.rescale_type == 'Linear'", 
        fluidRow(
          column(6,numericInput("linear_lb", "Lower bound:", min = NA, max = NA, value = 0)),
          column(6, numericInput("linear_ub", "Upper bound:", min = NA, max = NA, value = 50))
        )
      ),
      conditionalPanel(
        condition = "input.rescale_type == 'Power'", 
        fluidRow(
          column(6, numericInput("power_lb", "Lower bound:", min = NA, max = NA, value = 0)),
          column(6, numericInput("power_ub", "Upper bound:", min = NA, max = NA, value = 50))),
        numericInput("power_f1", "Exponent:", min = NA, max = NA, value = 2),
      ),
      conditionalPanel(
        condition = "input.rescale_type == 'Gaussian'",
        fluidRow(
          column(6, numericInput("gaus_f2",  "Midpoint:", min = NA, max = NA, value = 25)),
          column(6, numericInput("gaus_f1","Spread:", min = NA, max = NA, value = 0.01))
        )
      ),
      conditionalPanel(
        condition = "input.rescale_type == 'Small' || 
                     input.rescale_type == 'Large'",
        fluidRow(
          column(6, numericInput("mem_f1", "Spread:", min = NA, max = NA, value = 4)),
          column(6, numericInput("mem_f2", "Midpoint:", min = NA, max = NA, value = 15))
        )
      ),
      conditionalPanel(
        condition = "input.rescale_type == 'Near'",
        fluidRow(
          column(6, numericInput("near_f1", "Spread:", min = NA, max = NA, value = 0.06)),
          column(6, numericInput("near_f2", "Midpoint:", min = NA, max = NA, value = 25))
        )
      ),
      hr(),
      
      fluidRow(
        column(3, actionButton("plot_btn", "Plot")),
        column(4, checkboxInput("compareplots", "Compare plots", FALSE)),
        column(5, checkboxInput("scale01", "Constraint y-axis to 0-1", FALSE))
      ),
      
      conditionalPanel(
        condition = "input.compareplots",
        hr(),
        fluidRow(column(6, numericInput("target_min2", "target range min:", min = NA, max = NA, value = 0)),
                 column(6,numericInput("target_max2", "Target range max:", min = NA, max = NA, value = 1))),
        
          selectInput(
            "rescale_type2",
            "Rescale Type:",
            choices = c("Linear", "Gaussian", "Power",
                        "Small", "Large", "Near"),
            selected = "Power"
          ),
          
          conditionalPanel(
            condition = "input.rescale_type2 == 'Linear'", 
            fluidRow(
              column(6,numericInput("linear_lb2", "Lower bound:", min = NA, max = NA, value = 0)),
              column(6, numericInput("linear_ub2", "Upper bound:", min = NA, max = NA, value = 50))
            )
          ),
          conditionalPanel(
            condition = "input.rescale_type2 == 'Power'", 
            fluidRow(
              column(6, numericInput("power_lb2", "Lower bound:", min = NA, max = NA, value = 0)),
              column(6, numericInput("power_ub2", "Upper bound:", min = NA, max = NA, value = 50))),
            numericInput("power_f12", "Exponent:", min = NA, max = NA, value = 2),
          ),
          conditionalPanel(
            condition = "input.rescale_type2 == 'Gaussian'",
            fluidRow(
              column(6, numericInput("gaus_f22",  "Midpoint:", min = NA, max = NA, value = 25)),
              column(6, numericInput("gaus_f12","Spread:", min = NA, max = NA, value = 0.01))
            )
          ),
          conditionalPanel(
            condition = "input.rescale_type2 == 'Small' || 
                               input.rescale_type2 == 'Large'",
            fluidRow(
              column(6, numericInput("mem_f12", "Spread:", min = NA, max = NA, value = 4)),
              column(6, numericInput("mem_f22", "Midpoint:", min = NA, max = NA, value = 15))
            )
          ),
          conditionalPanel(
            condition = "input.rescale_type2 == 'Near'",
            fluidRow(
              column(6, numericInput("near_f12", "Spread:", min = NA, max = NA, value = 0.06)),
              column(6, numericInput("near_f22", "Midpoint:", min = NA, max = NA, value = 25))
            )
          ),
      ),
    ),
    mainPanel(
      fluidRow(column(4, plotOutput("rescale_plot")),
               column(6,
                      div(
                      p("Part of a spatial multicriteria decision analysis is to rescale the different criteria layers to a common scale of e.g., 0 to 1. In QGIS, this can be done using e.g., the fuzzify functions."),
                      p("The problem, especially if you are not familiar with the different fuzzify or rescale functions, is to decide which parameter settings to use."),
                      p("This app aims to provide a visual aid by allowing you to try out the different rescale functions and their parameter settings. In addition, you can compare to different combinations of functions and settings."),
                      tags$a(href='https://github.com/ecodiv/shiny_rescale_by_function/', 'Click here to open the Github Repository')))),
      hr(),
      fluidRow(
        column(4, plotOutput("raster")),
        column(4, plotOutput("rescale_raster")),
        column(4, plotOutput("rescale_raster2"))
        )
      )
    )
  )

# Server
#-------------------------------------------------------------------------------
server <- function(input, output) {
  
  observeEvent(input$plot_btn, {
    range_min <- input$range_min
    range_max <- input$range_max
    target_min <- input$target_min
    target_max <- input$target_max
    target_min2 <- input$target_min2
    target_max2 <- input$target_max2
    
    # Generate data
    x <- seq(input$range_min, input$range_max, length.out = 1000)
    
    # Create demo raster layers
    r <- s <- rast(ncol=100, nrow=100, crs="epsg:28992", extent=c(0,100, 0, 100))
    values(r) <- seq(range_min, range_max, length.out=100)
    values(s) <- NA
    minmaxr <- minmax(r, compute=TRUE)
    
    # Rescale data graph 1 
    if (input$rescale_type == "Linear") {
      rescaled_x <- linear_rescale(x, range_min, range_max, input$linear_lb, input$linear_ub, target_min, target_max)
      rescale_r <- linres(r, s, input$linear_lb, input$linear_ub, target_min, target_max)
      
    } else if (input$rescale_type == "Gaussian") {
      rescaled_x <- gaussian_rescale(x, input$gaus_f1, input$gaus_f2, target_min, target_max)
      rescale_r <- gaures(r, input$gaus_f1, input$gaus_f2, target_min, target_max)
      
    } else if (input$rescale_type == "Power") {
      rescaled_x <- power_rescale(x, input$power_lb, input$power_ub, input$power_f1, target_min, target_max)
      rescale_r <- powres(r, s, input$power_lb, input$power_ub, target_min, target_max, input$power_f1)

    } else if (input$rescale_type == "Large") {
      rescaled_x <- large_rescale(x, input$mem_f1, input$mem_f2, target_min, target_max)
      rescale_r <- larres(r, input$mem_f1, input$mem_f2, target_min, target_max)
      
    } else if (input$rescale_type == "Small") {
      rescaled_x <- small_rescale(x, input$mem_f1, input$mem_f2, target_min, target_max)
      rescale_r <- smares(r, input$mem_f1, input$mem_f2, target_min, target_max)
      
    } else {
      rescaled_x <- near_rescale(x, input$near_f1, input$near_f2, target_min, target_max)
      rescale_r <- neares(r, input$near_f1, input$near_f2, target_min, target_max)
    }
    # Rescale data graph 2
    if (input$rescale_type2 == "Linear") {
      rescaled_x2 <- linear_rescale(x, range_min, range_max, input$linear_lb2, input$linear_ub2, target_min2, target_max2)
      rescale_r2 <- linres(r, s, input$linear_lb2, input$linear_ub2, target_min2, target_max2)
    } else if (input$rescale_type2 == "Gaussian") {
      rescaled_x2 <- gaussian_rescale(x, input$gaus_f12, input$gaus_f22, target_min2, target_max2)
      rescale_r2 <- gaures(r, input$gaus_f12, input$gaus_f22, target_min2, target_max2)
    } else if (input$rescale_type2 == "Power") {
      rescaled_x2 <- power_rescale(x, input$power_lb2, input$power_ub2, input$power_f12, target_min2, target_max2)
      rescale_r2 <- powres(r, s, input$power_lb2, input$power_ub2, target_min2, target_max2, input$power_f12)
    } else if (input$rescale_type2 == "Large") {
      rescaled_x2 <- large_rescale(x, input$mem_f12, input$mem_f22, target_min2, target_max2)
      rescale_r2 <- larres(r, input$mem_f12, input$mem_f22, target_min2, target_max2)
    } else if (input$rescale_type2 == "Small") {
      rescaled_x2 <- small_rescale(x, input$mem_f12, input$mem_f22, target_min2, target_max2)
      rescale_r2 <- smares(r, input$mem_f12, input$mem_f22, target_min2, target_max2)
    } else {
      rescaled_x2 <- near_rescale(x, input$near_f12, input$near_f22, target_min2, target_max2)
      rescale_r2 <- neares(r, input$near_f12, input$near_f22, target_min2, target_max2)
    }

    # Create data frame and plot
    tarmin <- ifelse(input$scale01, 0, target_min)
    tarmax <- ifelse(input$scale01, 1, target_max)
    output$rescale_plot <- renderPlot({
      if(input$compareplots){
        df1 <- data.frame(x = x, rescaled_x = rescaled_x, Plots = "upper")
        df2 <- data.frame(x = x, rescaled_x = rescaled_x2, Plots = "lower")
        df <- rbind(df1, df2)
        ggplot(df, aes(x, rescaled_x, color=Plots)) +
          geom_line(line=1.3) +
          ylim(tarmin, tarmax) +
          xlim(range_min, range_max) +
          labs(x = "Original data values", y = "Rescaled data values")  +
          theme_bw(base_size = 12) +
          theme(legend.position="none",
                plot.margin = unit(c(0,0,0,0), "lines")) +
          scale_color_manual(values = c(upper = "#5279CB", lower = "#1F8354"))
      } else {
        df <- data.frame(x = x, rescaled_x = rescaled_x)
        ggplot(df, aes(x, rescaled_x)) +
          geom_line(color="#5279CB", linewidth=1.3) +
          theme_bw(base_size = 12) +
          ylim(tarmin, tarmax) +
          xlim(range_min, range_max) +
          labs(x = "Original data values", y = "Rescaled data values")
      }
    })
    output$raster <- renderPlot({
      ggplot() +
        geom_spatraster(data = r) +
        #geom_spatraster_contour(data = rescale_r, bins=10, colour="white", alpha=0.2) +
        scale_fill_viridis_c() +
        ggtitle("Original raster") +
        theme_void() +
        theme(legend.position = "bottom", 
              legend.text=element_text(size=10),
              legend.key.width = unit(1, 'cm'))
        
    })
    output$rescale_raster <- renderPlot({
      ggplot() +
        labs(title="Rescaled raster layer") +
        geom_spatraster(data = rescale_r) +
        #geom_spatraster_contour(data = rescale_r, bins=10, colour="white", alpha=0.2) +
        scale_fill_viridis_c() +
        theme_void() +
        theme(legend.position = "bottom", 
              legend.text=element_text(size=10),
              legend.key.width = unit(1, 'cm'),
              plot.title = element_text(colour = "#5279CB", face="bold"))
      })
      output$rescale_raster2 <- renderPlot({
        if(input$compareplots){
          ggplot() +
            labs(title="2nd rescaled raster layer") +
            geom_spatraster(data = rescale_r2) +
            #geom_spatraster_contour(data = rescale_r, bins=10, colour="white", alpha=0.2) +
            scale_fill_viridis_c() +
            theme_void() +
            theme(legend.position = "bottom", 
                  legend.text=element_text(size=10),
                  legend.key.width = unit(1, 'cm'),
                  plot.title = element_text(colour = "#1F8354", face="bold"))
          } else {
            ggplot() + geom_blank() + theme_void()
          }
      })
  })
}

# Run the app
shinyApp(ui, server)
