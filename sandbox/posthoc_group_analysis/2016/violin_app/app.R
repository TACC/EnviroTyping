setwd("~/EnviroTyping/sandbox/posthoc_group_analysis/2016/violin_app/")
library(shiny)
library(ggplot2)
library(tidyr)
library(readr)

hyb_by_mon_posthoc <- read_rds("data/posthocgroup.rds")

title <- unique(hyb_by_mon_posthoc$Pedi)
code <- c(1:length(unique(hyb_by_mon_posthoc$Pedi)))
hybrids <- data.frame(title,code)

choicesHybrids <- setNames(hybrids$code,hybrids$title)
group.colors = c("1"="#E69F00","2"="#57B4E9","3"="#019E73","4"="#F0E442")

# Define UI  ----
ui <- fluidPage(
    titlePanel(h1("Hybrid Yield Distributions",align="center")),
    sidebarLayout(
        sidebarPanel(
            helpText("Choose your options to view 
                     2016 hybrid Yield distributions."),
            
            selectInput("hybrids",
                        h3("Choose up to 8 Hybrids"),
                        choices = names(choicesHybrids), # will change to vector of unique Pedi
                        selected = NULL,
                        multiple = TRUE),
            
            h3("By Group?"),
            checkboxInput("group", "Yes", value = FALSE),
            
            submitButton("Submit")
        ),
        mainPanel(
            plotOutput("distributions")
        )
    )
)

# Define server logic ----
server <- function(input, output) {
    
    df = hyb_by_mon_posthoc
    
    df_subset = reactive({
        a = df[which(as.character(factor(Pedi,level = input$hybrids)) != 'NA'),]
        return(a)
    })
    
    output$distributions <- renderPlot({
        
    # Violin by Pedi  
    p <- ggplot(df_subset(),aes(Pedi,Yield)) + 
        labs(title = "Yield by Pedigree",x = "Pedi",y = "Yield") +
        geom_violin(fill = "#ADD8E6") + 
        geom_boxplot(width=0.1) + 
        scale_fill_manual(values=group.colors) +
        theme_bw() +
        theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))
    
    if(input$group == FALSE){
        p
    }
    
    # Violin by group 
    else if(input$group == TRUE){
        p <- ggplot(df_subset(),aes(Pedi, Yield, fill = group)) + 
            geom_violin() + 
            scale_fill_manual(values=group.colors,name="Group") + 
            labs(title = "Yield by Pedigree and Group",x = "Pedi",y = "Yield") + 
            theme_bw() + 
            theme(axis.text.x = element_text(angle=45,hjust=1), plot.title = element_text(hjust = 0.5))
        p
    }
# need to connect data to function
# need to create funtion to draw violins
# need to make option to separate by groups
    })
}

# Run the app ----
shinyApp(ui = ui, server = server)