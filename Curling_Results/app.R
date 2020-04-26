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
library(gganimate)
library(tidymodels)
library(shinythemes)

olympics<-readRDS("data/olympics.rds")
college<- readRDS("data/college.rds")
props<-readRDS("data/prop_winners.rds")
tidy_props<- readRDS("tidy_props.rds")

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
                 titlePanel("How Many Points in the First end to guarantee a win?"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "who",
                             "Competition Category",
                             c("men", "women", "college")
                         )),
                     mainPanel(plotOutput("chance_plot"))) 
             )),
    tabPanel("Importance of other ends",
             h1("Importance of other ends"),
             h3("Proportion of Winning Score: The shapes of games"),
             fluidPage(
                 titlePanel("Importance of other ends"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "gif_type",
                             "Separated By",
                             c("Men/Women" = "competition", 
                               "Finals/Pool" = "team_group")
                         )),
                     mainPanel(imageOutput("gif", height = "100%")))
             ), 
             h3("Logistic regression"),
             fluidPage(
                 titlePanel("Importance of other ends"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "reg_type",
                             "Regression of:",
                             c("All Olympic" = "all", 
                               "Olympic Women" = "olympic_women",
                               "Olympic Men" = "olympic_men"#, 
                              # "college" = "college",
                               )
                         )),
                     mainPanel(plotOutput("log_plot"))))),
    tabPanel("Discussion",
             titlePanel("Discussion Title"),
             p("Tour of the modeling choices you made and 
              an explanation of why you made them")))
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    # Plots on men/women vs finals/pool
    
    output$prelim_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_measure == "competition",
            
            
            x   <- all_years_olympics %>% 
                group_by(year,competition) %>% 
                summarize(prop_double_win = mean(first_end_first_game, na.rm = T)),
            
           
            x   <- all_years_olympics %>% 
                group_by(year, team_group) %>% 
                summarize(prop_double_win = mean(first_end_first_game, na.rm = T)))
        ifelse(
            input$plot_measure == "competition",

                y <- x$competition,
                
                y <- x$team_group)
        ifelse(
            input$plot_measure == "competition",
            
            legendtitle <- paste("Competition"),
            
            legendtitle <- paste("Team in Finals?"))

        
        ggplot(x, aes(year, prop_double_win, fill = y, label = round(prop_double_win, digits = 3))) + 
            geom_bar(stat = "identity", position = "dodge")+ 
            theme_classic()+ 
            labs(
                title = "Proportion of Curling Games 'decided' in the First End", 
                subtitle = "Women less affect than Men, and finals potentially more affected", 
                x = "Year", 
                y = "Proportion of total games won \nthat were won in the first end", 
                fill = legendtitle
            )+ geom_text(position = position_dodge(width=1))
        
    })
    
    # Points in the first end plot
    
    output$chance_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        chance_everyone %>% filter(category == input$who) %>%
            ggplot(aes(score, value)) +
            geom_col(fill = "turquoise") + 
            theme_classic() +
            labs(
                title = "Score in the first end & chance of winning", 
                x = "score in the first end", 
                y = "chance of winning"
            )
    })
    
    # fade plot
    
    output$fade_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        fdata <- tidy_props
          
        fdata$value <- replace_na(fdata$value, 0)
    
        ifelse(
            input$plot_type == "competition",
            
            y <- fdata$competition,
            
            y <- fdata$team_group)
        
        ifelse(
            input$plot_type == "competition",
            
            legendtitle <- paste("Competition"),
            
            legendtitle <- paste("Team in Finals?"))
        
        
        ggplot(fdata, aes(prop, value)) + 
                geom_col(aes(fill = y)) +
            facet_wrap(vars(year)) +
            transition_states(y,
                              transition_length = 2,
                              state_length = 1) + enter_fade() + exit_fade() +
            theme_classic() +
            labs(
                title = "Proportion of points scored by winners in Olympic Curling Games", 
                y = "Proportion of points", 
                x = "end", 
                fill = legendtitle
            ) + scale_x_discrete(labels = c(1:10))
            
    })
    output$gif <- renderImage({
        ifelse(input$gif_type == "competition", 
        
                (list(src = "plot_comp.gif",
                 contentType = 'image/gif')),
        
                (list(src = "profinals.gif",
                 contentType = 'image/gif')))
        
    }, deleteFile = FALSE)
    # logistic regression
    
    output$log_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        clean_all_years_olympics <- olympics %>% 
            mutate(win = ifelse(ind_game_winner == T, 1, 0)) %>% 
            select(id, year, competition, team_group, country, win, 
                   starts_with("x"), -x, -x_2, -x10, -x9)
        
        clean_all_college<- college %>% 
            mutate(win = ifelse(ind_game_winner == T, 1, 0)) %>% 
            select(id, year, competition, team_group, school, win, 
                   starts_with("x"))
        
        logistic_mod <- logistic_reg() %>%
            set_engine("glm") 
        
        ifelse(
            input$reg_type == "all", 
            filt_clean_all_years_olympics<- clean_all_years_olympics, 
            ifelse(
                input$reg_type == "olympic_men", 
                filt_clean_all_years_olympics <- clean_all_years_olympics %>% 
                    filter(competition == "olympic_men"), 
                filt_clean_all_years_olympics <- clean_all_years_olympics %>% 
                    filter(competition == "olympic_women")
            )
        )
        
        logistic_fit <- fit(logistic_mod,
                            
                            factor(win) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8,
                            
                            data = filt_clean_all_years_olympics)
        
    
        tibble_logistic_fit <- logistic_fit %>%
            tidy(conf.int = TRUE) %>%
            slice(2:8) %>% tibble()
        
        ggplot(tibble_logistic_fit, aes(term, estimate)) + 
            geom_bar(stat = "identity") + 
            geom_errorbar(aes(x = term, ymin = conf.low, ymax = conf.high)) + 
            labs(
                title = "Logistic regression coefficients for points per end on winning",
                subtitle = "with 95% confidence interval", 
                x = "end number", 
                y = "Logistic Regression Coefficient"
            )  + 
            scale_x_discrete(labels = c(1:10))
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
