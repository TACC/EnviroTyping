library(shiny)

# contains the data and functions to plot the graphs
source("shiny_function.R")


# Server ####


server <- function(input, output) {
    
    # Compute the formula text ----
    # This is in a reactive expression since it is shared by the
    # output$caption and output$mpgPlot functions
    
    
    # Generate a plot of the requested variable against mpg ----
    # and only exclude outliers if requested
    output$Plot <- renderPlot({
        # conditions to make diff plots
        if (input$scaled_input == TRUE) {
            
            # the process to use for scaled variables
            if (input$input_var == "all_vars") {
                scaled %>% ggplot(aes(fct_inorder(factor(var)), value)) +
                    geom_point(aes(color = stat), size = 5) +
                    facet_wrap(~ group, scales = "free_y") + 
                    labs(title = "Scaled Weather Variables by Group",
                         x = "Weather Variables with Month Number as Suffix",
                         color = "Statistic") +
                    theme(plot.title = element_text(face = "bold", size = 32, margin = margin(b = 20)),
                          axis.text.x = element_text(face = "bold", size = 14, angle = 90, hjust = 1, margin = margin(t = 5)),
                          axis.text.y = element_text(face = "bold", size = 16),
                          axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 30)),
                          axis.title.y = element_blank(),
                          panel.background = element_rect(fill = "white", colour = NA), 
                          panel.border = element_rect(fill = NA, colour = "grey20"), 
                          panel.grid.major = element_line(colour = "grey92"), 
                          panel.grid.minor = element_line(colour = "grey92",size = 0.25), 
                          strip.background = element_rect(fill = "grey85", colour = "grey20"), 
                          legend.key = element_rect(fill = "white", colour = NA),
                          legend.text = element_text(size = 16),
                          legend.title = element_text(size = 20, face = "bold"), 
                          complete = TRUE, 
                          strip.text = element_text(face = "bold", size = 18)) + 
                    scale_color_viridis(discrete = TRUE)
            } else {
                
                # changes based on variable
                if (input$input_var == "dew") {
                    data <- scaled_dew
                    title <- "Scaled Dew Point"
                    ylab <- "Dew Point [C]"
                } else if (input$input_var == "humid") {
                    data <- scaled_humid
                    title <- "Scaled Humidity"
                    ylab <- "Relative Humidity [%]"
                } else if (input$input_var == "temp") {
                    data <- scaled_temp
                    title <- "Scaled Temperature"
                    ylab <- "Temperature [C]"
                } else if (input$input_var == "windDir") {
                    data <- scaled_windDir
                    title <- "Scaled Wind Direction"
                    ylab <- "Wind Direction [degrees]"
                } else if (input$input_var == "windGust") {
                    data <- scaled_windGust
                    title <- "Scaled Wind Gust"
                    ylab <- "Wind Gust [m/s]"
                } else {
                    data <- scaled_windSpd
                    title <- "Scaled Wind Speed"
                    ylab <- "Wind Speed [m/s]"
                }
                data %>% ggplot(aes(fct_inorder(factor(var)), value)) +
                    geom_point(aes(color = stat), size = 5) +
                    facet_wrap(~ group, scales = "free_y") + 
                    labs(title = paste(title, "Profile by Group", sep = " "), 
                         x = "Weather Variables with Month Number as Suffix", 
                         y = ylab, 
                         color = "Statistic") +
                    theme(plot.title = element_text(face = "bold", size = 32, margin = margin(b = 20)),
                          axis.text.x = element_text( face = "bold", size = 16, angle = 45),
                          axis.text.y = element_text(face = "bold", size = 16),
                          axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 30)),
                          axis.title.y = element_text(face = "bold", size = 20, angle = 90, margin = margin(r = 10)),
                          panel.background = element_rect(fill = "white", colour = NA), 
                          panel.border = element_rect(fill = NA, colour = "grey20"), 
                          panel.grid.major = element_line(colour = "grey92"), 
                          panel.grid.minor = element_line(colour = "grey92",size = 0.25), 
                          strip.background = element_rect(fill = "grey85", colour = "grey20"), 
                          legend.key = element_rect(fill = "white", colour = NA),
                          legend.text = element_text(size = 16),
                          legend.title = element_text(size = 20, face = "bold"), 
                          complete = TRUE,
                          strip.text = element_text(face = "bold", size = 18)) +
                    scale_color_viridis(discrete = TRUE)
            }
            
            # for data not being scaled
        } else {
            
            # changes based on variable
            if (input$input_var == "dew") {
                data <- dew2
                title <- "Dew Point"
                ylab <- "Dew Point [C]"
            } else if (input$input_var == "humid") {
                data <- humid2
                title <- "Humidity"
                ylab <- "Relative Humidity [%]"
            } else if (input$input_var == "temp") {
                data <- temp2
                title <- "Temperature"
                ylab <- "Temperature [C]"
            } else if (input$input_var == "windDir") {
                data <- windDir2
                title <- "Wind Direction"
                ylab <- "Wind Direction [degrees]"
            } else if (input$input_var == "windGust") {
                data <- windGust2
                title <- "Wind Gust"
                ylab <- "Wind Gust [m/s]"
            } else {
                data <- windSpd2
                title <- "Wind Speed"
                ylab <- "Wind Speed [m/s]"
            }
            data %>% ggplot(aes(fct_inorder(factor(var)), value)) +
                geom_point(aes(color = stat), size = 5) +
                facet_wrap(~ group, scales = "free_y") + 
                labs(title = paste(title, "Profile by Group", sep = " "), 
                     x = "Weather Variables with Month Number as Suffix", 
                     y = ylab, 
                     color = "Statistic") +
                theme(plot.title = element_text(face = "bold", size = 32, margin = margin(b = 20)),
                      axis.text.x = element_text( face = "bold", size = 16, angle = 45),
                      axis.text.y = element_text(face = "bold", size = 16),
                      axis.title.x = element_text(face = "bold", size = 20, margin = margin(t = 30)),
                      axis.title.y = element_text(face = "bold", size = 20, angle = 90, margin = margin(r = 10)),
                      panel.background = element_rect(fill = "white", colour = NA), 
                      panel.border = element_rect(fill = NA, colour = "grey20"), 
                      panel.grid.major = element_line(colour = "grey92"), 
                      panel.grid.minor = element_line(colour = "grey92",size = 0.25), 
                      strip.background = element_rect(fill = "grey85", colour = "grey20"), 
                      legend.key = element_rect(fill = "white", colour = NA),
                      legend.text = element_text(size = 16),
                      legend.title = element_text(size = 20, face = "bold"), 
                      complete = TRUE, 
                      strip.text = element_text(face = "bold", size = 18)) +
                scale_color_viridis(discrete = TRUE)
        }
    })
    
    output$table <- DT::renderDataTable({
        DT::datatable(hyb_mon_groups)
    })
    
}






# UI ####


ui <- fluidPage(
    
    title = "Post Hoc Grouping Explorer",
    fluidRow(
        column(3,
               h4("Post Hoc Grouping Explorer"),
               # Input: Selector for variable to plot against mpg ----
               selectInput("input_var", "Variable:", 
                           c("Dew Point" = "dew",
                             "Relative Humidity" = "humid",
                             "Temperature" = "temp",
                             "Wind Direction" = "windDir",
                             "Wind Gust" = "windGust",
                             "Wind Speed" = "windSpd",
                             "All Scaled Variables" = "all_vars")),
               
               # Input: Checkbox for whether outliers should be included ----
               checkboxInput("scaled_input", "Use Scaled Data", TRUE)
        )
    ),
    
    
    hr(),
    
    plotOutput("Plot", height = "800px"),
    
    hr(), 
    
    fluidRow(
        column(12,
               h4("Hybrids by Grouping Variables"),
               
               DT::dataTableOutput("table"))
    )
)


shinyApp(ui, server)
