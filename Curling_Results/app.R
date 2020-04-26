#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinythemes)

olympics<-readRDS("data/olympics.rds")
college<- readRDS("data/college.rds")
props<-readRDS("data/prop_winners.rds")

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
               than wins in the other ends. I also compared professional 
               curlers, on whom there is more data anyway, to college curlers.
               I began by looking only at the importance of winning the first
               end, but I also looked at the importance of other ends, as well."),
             h3("Data"), 
             p("Due to the lack of a National College Tournament this year, my 
               data was limited for college curling, but I used all of the 
               olympic curling games, mens and womens, since 2006 in my 
               analysis. I used data from curlingresults (",
                a("olympics",
                    href = "https://results.worldcurling.org/Championship/Type/4"),
                    ") and softpeelr (", 
                a("2018", 
                    href = "https://softpeelr.com/en/tournaments/537"),
                    " and ",
                a("2019", 
                    href = "https://softpeelr.com/en/tournaments/769"),
                    "yale college spiels) for my analysis.
               From these sites, I scraped the end-by-end information on the 
               results. However, it's important to bear in mind that the results 
               of curling games often don't capture the true balance of teams, 
               or which team was 'in control' of the game."),
             h3("About Me!"),
             p("My name is Lara Teich and I study Government with
             a secondary in East Asian Studies. I'm a junior at the college, 
             and I fell in love with curling in the fall of my freshman year.
             You can reach me at larateich@college.harvard.edu."), 
             p("personal website: larateich.art"), 
             p("linkedIn : https://www.linkedin.com/in/lara-teich-76ba61172/"),
             p("Other sites: Instagram: @loose_leaf_lichen"),
             p("Scholar: https://scholar.harvard.edu/larateich")),
             column(2,
                    imageOutput("curlinglara", height = "100%"),
                    imageOutput("lara", height = "100%"))),
    tabPanel("First End  = Decisive Victory?",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_measure",
                             "Separated By",
                             c("Men/Women" = "competition", 
                               "Finals/Pool" = "team_group")
                         )),
                     mainPanel(plotOutput("prelim_plot")))
             ), 
             fluidPage(
                 titlePanel("Model Title numero dos"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line_plot"))) 
             )),
    tabPanel("Importance of other ends",
             fluidPage(
                 titlePanel("Model Title"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_type",
                             "Plot Type",
                             c("Option A" = "a", "Option B" = "b")
                         )),
                     mainPanel(plotOutput("line2_plot")))
             )),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")))
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
    
    output$prelim_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_measure == "competition",
            
            # If input$plot_type is "a", plot histogram of "waiting" column 
            # from the faithful dataframe
            
            x   <- all_years_olympics %>% 
                group_by(year,competition) %>% 
                summarize(prop_double_win = mean(first_end_first_game, na.rm = T)),
            
            # If input$plot_type is "b", plot histogram of "eruptions" column
            # from the faithful dataframe
            
            x   <- all_years_olympics %>% 
                group_by(year, team_group) %>% 
                summarize(prop_double_win = mean(first_end_first_game, na.rm = T)))
        ifelse(
            input$plot_measure == "competition",
                
                # If input$plot_type is "a", plot histogram of "waiting" column 
                # from the faithful dataframe
                
                y <- x$competition,
                
                # If input$plot_type is "b", plot histogram of "eruptions" column
                # from the faithful dataframe
                
                y   <- x$team_group)

        
        ggplot(x, aes(year, prop_double_win, fill = y, label = round(prop_double_win, digits = 3))) + 
            geom_bar(stat = "identity", position = "dodge")+ 
            theme_classic()+ 
            labs(
                title = "Proportion of Curling Games 'decided' in the First End", 
                subtitle = "teams who made it to the final round are less affected 2018\nbut more affected in all other years", 
                x = "type of game", 
                y = "Proportion of total games won \nthat were won in the first end", 
                fill = 
            )+ geom_text(position = position_dodge(width=1))
        
    })
    # Curling Photo
    
    output$curlinglara <- renderImage({
        # Return a list containing the filename
        list(src = "curling.jpg",
             contentType = 'image/jpg'
             # width = 400,
             # height = 300,
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
    
    # Headshot of me
    
    output$lara <- renderImage({
        # Return a list containing the filename
        list(src = "laraedit.jpg",
             contentType = 'image/jpg',
             width = 300,
             height = 200
             # alt = "This is alternate text"
        )}, deleteFile = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
