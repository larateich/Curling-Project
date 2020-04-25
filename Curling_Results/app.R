#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("cerulean"),
    "Curling",
    tabPanel("About", 
             titlePanel("About"),
             column(6,
             h3("Why I chose curling"),
             p("Hello! 
               As a college curler, one of the things that I know intimately
                well is the difficulty of 'playing from behind', or losing
                a big end in the beginning of the game. I wanted to test and 
               see if wins in the first end actually do affect the game more 
               than wins in the other ends. I wanted to compare professional 
               curlers, on whom there is more data anyway, to college curlers.
               Due to the lack of a National College Tournament this year, my 
               data was limited for college curling, but I used all of the 
               olympic curling games, mens and womens, since 2006 in my 
               analysis."),
             h3("About Me"),
             p("My name is Lara Teich and I study Government with
             a secondary in East Asian Studies. I'm a junior at the college, 
             and I fell in love with curling in the fall of my freshman year.
             You can reach me at larateich@college.harvard.edu."), 
             p("personal website: larateich.art"), 
             p("linkedIn : https://www.linkedin.com/in/lara-teich-76ba61172/"),
             p("Other sites: Instagram: @loose_leaf_lichen; 
               Scholar: https://scholar.harvard.edu/larateich"))),
    tabPanel("Model",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")),
    tabPanel("About2", 
             titlePanel("About"),
             h3("Project Background and Motivations"),
             p("Hello! 
               As a college curler, one of the things that I know intimately
                well is the difficulty of 'playing from behind', or losing
                a big end in the beginning of the game. I wanted to test and 
               see if wins in the first end actually do affect the game more 
               than wins in the other ends. I wanted to compare professional 
               curlers, on whom there is more data anyway, to college curlers.
               Due to the lack of a National College Tournament this year, my 
               data was limited for college curling, but I used all of the 
               olympic curling games, mens and womens, since 2006 in my 
               analysis."),
             h3("About Me"),
             p("My name is Lara Teich and I study Government with
             a secondary in East Asian Studies. I'm a junior at the college, 
             and I fell in love with curling in the fall of my freshman year.
             You can reach me at larateich@college.harvard.edu."), 
             p("personal website: larateich.art"), 
             p("linkedIn : https://www.linkedin.com/in/lara-teich-76ba61172/"),
             p("Other sites: Instagram: @loose_leaf_lichen; 
               Scholar: https://scholar.harvard.edu/larateich")))
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$line_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_type == "a",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- faithful[, 2],
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- faithful[, 1]
        )
        
        # Draw the histogram with the specified number of bins
        
        hist(x, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
