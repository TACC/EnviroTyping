library(shiny)
library(tidyverse)
library(gridExtra)

hyb_by_mon_posthoc <- read_rds("data/posthocgroup.rds")
clms1 = colnames(hyb_by_mon_posthoc)

for (i in 2:9) {
    clms1 = gsub(paste0(i),paste0(0,i,sep = ""), clms1)
}

colnames(hyb_by_mon_posthoc) = clms1

group_number <- sort(unique(hyb_by_mon_posthoc$group))
code_group <- c(1:length(unique(hyb_by_mon_posthoc$group)))
ph_groups <- data.frame(group_number,code_group)

choicesGroups <- setNames(ph_groups$code_group,ph_groups$group_number)

min_max_scale <- function(x){
    return ((x - min(x)) / (max(x) - min(x)))
}

# Define UI ---
ui <- fluidPage(
    titlePanel(h1("Post Hoc Weather Profiles",align="center")),
    sidebarLayout(
       sidebarPanel(
            helpText("Select options to explore 
                    post hoc group profiles."),
           
            selectInput("groups",
                        h4("Group:"),
                        choices = sort(names(choicesGroups)),
                        selected = 1,
                        multiple = FALSE),
            
            selectInput("weather_var",
                        label = h4("Weather Variable(s):"),
                        choices = c("Dew Point" = "Dew",
                                    "Relative Humidity" = "Humid",
                                    "Temperature" = "Temp",
                                    "Wind Direction" = "windDir",
                                    "Wind Gust" = "windGust",
                                    "Wind Speed" = "windSpd"),
                        selected = "Dew",
                        multiple = TRUE),
            
            selectInput("scale",
                        label = h4("Scaling:"),
                        choices = c("Levels" = "levels",
                                    "Min-Max" = "min_max",
                                    "Standardize" = "standard"),
                        selected = "Levels",
                        multiple = FALSE),
            
            helpText("If you would like to see better 
                    scaling of Levels, click the box 
                    below. However, don't click it when 
                    using the other scaling types."),
            h4("Treat NAs as 0s?"),
            checkboxInput("missing", "Yes", value = FALSE),
            
            submitButton("Submit")
            
      ),

      mainPanel(
         plotOutput("profiles")
      )
   )
)

# Define server logic ----
server <- function(input, output) {
 
    weather_var_names = reactive({
        weather_names = c(input$weather_var)
    })
    
    group_var_names = reactive({
        group_names = c(input$groups)
    })
    
    hyb_by_mon_posthoc_new = reactive({
        if(input$missing == TRUE){
            hyb_by_mon_posthoc[hyb_by_mon_posthoc == -999] <- 0
            return(hyb_by_mon_posthoc)
        }else{
            return(hyb_by_mon_posthoc)
        }
    })
    
    df_subset = reactive({
        if(input$scale == "levels"){
            a = hyb_by_mon_posthoc_new() %>% select(group,matches(paste(weather_var_names(),collapse = "|"))) %>% 
            filter(group %in% group_var_names())
            x = ncol(a)
            a = a %>% summarise_at(2:x, funs(min, max, median), na.rm = TRUE) %>%
                gather(key,value) %>%
                separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
            return(a)
        }else if(input$scale == "min_max"){
            a = hyb_by_mon_posthoc_new() %>% mutate_at(6:length(hyb_by_mon_posthoc_new()), funs(c(min_max_scale(.))))
            a = a %>% select(group,matches(paste(weather_var_names(),collapse = "|"))) %>% 
                filter(group == input$groups)
            x = ncol(a)
            a = a %>% summarise_at(2:x, funs(min, max, median), na.rm = TRUE) %>%
                gather(key,value) %>%
                separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
            return(a)
        }else{
            hyb_by_mon_posthoc = hyb_by_mon_posthoc_new() %>% mutate_at(6:length(hyb_by_mon_posthoc_new()), funs(c(scale(.))))
            a = hyb_by_mon_posthoc %>% select(group,matches(paste(weather_var_names(),collapse = "|"))) %>% 
                filter(group == input$groups)
            x = ncol(a)
            a = a %>% summarise_at(2:x, funs(min, max, median), na.rm = TRUE) %>%
                gather(key,value) %>%
                separate(key, into = c("var", "stat"), sep = "_(?!.*_)")
            return(a)
        }
    })

   output$profiles <- renderPlot({

        if(input$scale == "levels"){
            df_subset() %>% ggplot(aes(var,value)) +
                    geom_point(aes(color = stat), size = 5) +
                    #facet_wrap(~ group, scales = "free_y") + 
                    labs(title = paste("Weather Variable Levels for Group",input$groups),
                         x = "Weather Variables with Month Number as Suffix",
                         color = "Statistic") +
                    #scale_fill_manual(values=group.colors) +
                    theme_bw() +
                    theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))
        }else if(input$scale == "min_max"){
            df_subset() %>% ggplot(aes(var,value)) +
                geom_point(aes(color = stat), size = 5) +
                #facet_wrap(~ group, scales = "free_y") + 
                labs(title = paste("Min-Max Scaled Weather Variables for Group",input$groups),
                     x = "Weather Variables with Month Number as Suffix",
                     color = "Statistic") +
                #scale_fill_manual(values=group.colors) +
                theme_bw() +
                theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5)) 
            
        }else{
            df_subset() %>% ggplot(aes(var,value)) +
                geom_point(aes(color = stat), size = 5) +
                #facet_wrap(~ group, scales = "free_y") + 
                labs(title = paste("Standardized Weather Variables for Group",input$groups),
                     x = "Weather Variables with Month Number as Suffix",
                     color = "Statistic") +
                #scale_fill_manual(values=group.colors) +
                theme_bw() +
                theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5)) 
        }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

