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
library(broom)
library(tidyr)
library(gganimate)
library(tidymodels)
library(shinythemes)


# data
olympics<-readRDS("data/olympics.rds")
college<- readRDS("data/college.rds")
props<-readRDS("data/prop_winners.rds")
tidy_props<- readRDS("data/tidy_props.rds")

clean_all_college<- college %>% 
    mutate(win = ifelse(ind_game_winner == T, 1, 0))

# models
logistic_mod <- logistic_reg() %>%
    set_engine("glm") 

# function for returning a line equation and r squared as a string
# SOURCE: https://groups.google.com/forum/#!topic/ggplot2/1TgH-kG5XMA

lm_eqn <- function(df){
    m <- lm(x1 ~ win, df);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                     list(a = format(unname(coef(m)[1]), digits = 2),
                          b = format(unname(coef(m)[2]), digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq));
}





# Define UI for application that draws a histogram
ui <- navbarPage(
    theme = shinytheme("cerulean"),
    "Curling",
    tabPanel("Overview",
             titlePanel("Curling: 'Chess on Ice'"),
             h3("Introduction to Curling"), 
             p("In my project, I wanted to explore the differential importance
               of different ends in a game of curling. Curling, like baseball, 
               is played in different rounds called 'ends', and strategy can 
               vary widely between different ends in the same game. A team that's
               playing from behind, for example, will be forced to take riskier
               shots for more points than a team who is in the lead. The leading 
               team might choose to forgo winning points in later ends if they 
               scored by a wide margin earlier."), 
             h3("How is College Curling different?"), 
             p("As a college curler, I know intimately well the differences in 
               gameplay between myself and my team, and what we see on television. 
               The sheer amount of velocity, or 'weight', that adult men can put
               behind a 44-pound curling stone, is just not accessible by 18-21 
               year-old college students. Women, though with notable exceptions 
               like", 
               a("Rachel Homan",
                 href = "https://www.youtube.com/watch?v=_4EVyv3_ch8"), 
               ", who can throw with a ton of weight, are similar 
               to college students in that their stones are typically 'lighter'."), 
             p("Therefore, I hypothesize that though still different, college 
               curling data will more closely resemble women curlers than men in
               the olympics."), 
             h3("Overview of Methods"), 
             p("First, I wanted to see what proportion of games were 'decided', 
               so to speak, in the first end, by finding the proportion of games
               whose winning team also won the first end. Then, I tried using 
               multiple different regressions on the first end and whether the team one, 
               resulting in a positive correlation."), 
             p("I then became interested in the shapes of games more broadly. If 
               not the first end, which end was the most important for scoring points? 
               Which end had the biggest impact on the eventual result? 
               My results in answering these questions are sadly not significant, because 
               the differences in effect are too minute. However, given the volume of data
               used, these differences may serve as a guide for future curling analytics work, 
               and serve as an initial hypothesis."), 
             h3("Preliminary Results"), 
             h4("Percent of the time that the first end winner won the game"), 
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "group_of_curlers",
                             "Curling Level",
                             c("Olympic" = "olympics", 
                               "College" = "college")
                         ), 
                         selectInput(
                             "specification",
                             "Separated By",
                             c("Men/Women (olympics only)" = "competition", 
                               "Finals/Pool" = "team_group", 
                               "Year" = "year")
                         ),
                     ),
                     mainPanel(plotOutput("percentage_plot")), 
                     ), 
                 p("This showed me that winning the first end is incredibly 
                       important, regardless of how the teams are stratified--the
                       team who wins the first end is simply more likely to win the
                       game.")
                 ), 
             h4("Number of Points in the First End and Winning the Game"), 
             fluidPage(
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "country",
                             "Olympic Country",
                             c("all", olympics$country)
                         ), 
                         p("Here you can select a country, or if you 
                           want to look at the entirety of the olympic data, you
                           can select 'all'. 
                           I found it interesting that some countries had 
                           stronger positive relations between first end and winning than
                           others."), 
                         p("As you can see from the R-squared, though, this 
                           positive line is deceiving. This is not super significant, 
                           unfortuneately, but that's okay! half of research is getting
                           bad results and adjusting. These are merely my preliminary 
                           results. Hence, I became more interested in other ends, 
                           as well!"), 
                     ),
                     mainPanel(plotOutput("linreg_olympic"), 
                               plotOutput("linreg_college")), 
                 ), 
                 p("Generally, I noticed a positive relationship between winning more points
                   in the first end and winning the game. This makes sense, but it was interesting
                   to see that it didn't differ by a huge amount in college and olympic games. ")
                     )
            ),
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
             You can reach me at larateich@college.harvard.edu. You can find the 
             entirety of this project, as well as the cleaned datasets that I used, 
             at my", 
                a("Github Repository.", 
                  href = "https://github.com/larateich/Curling-Project"),
               ), 
             p("personal website: ", 
               a("larateich.art",
                 href = "https://larateich.art/"), 
               ), 
             p("linkedIn :", 
                 a("linkedin.com/in/lara-teich",
                     href = "https://www.linkedin.com/in/lara-teich-76ba61172/"), 
                ),
             p("Art Instagram: ", 
               a("@loose_leaf_lichen",
                 href = "https://www.instagram.com/loose_leaf_lichen/"),
               ),
             p("Scholar:", 
                 a("scholar.harvard.edu/larateich", 
                     href = "https://scholar.harvard.edu/larateich"), 
             )),
             column(2,
                    imageOutput("curlinglara", height = "100%"),
                    imageOutput("lara", height = "100%"))),
    tabPanel("First End  = Decisive Victory?",
             fluidPage(
                 titlePanel("Preliminary Plot"),
                 sidebarLayout(
                     sidebarPanel(
                         selectInput(
                             "plot_measure",
                             "Separated By",
                             c("Men/Women" = "competition", 
                               "Finals/Pool" = "team_group")
                         ), 
                         p("Here, we see the proportion of teams who won both
                           the first end and the eventual game. In an attempt to 
                           account for 'goodness' of a team, I separated teams who
                           had made it to the finals from pool playing teams.")),
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
                         ), 
                         p("I hope you'll forgive the rather catchy question. 
                           it's difficult to answer with the data, but according 
                           to this, 100% of the teams accross categories who score 5
                           points in the first end also wind up winning their game. 
                           Interestingly, it appears that college curlers and women 
                           olympians are less influenced by a lost of 1 or 2 in the first
                           end, which might be a sign of more fluctuation in college games. 
                           I am wary of these results, however, since they do not account for
                           the 'strength' of any particular team.")),
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
                         ), 
                         p("Here, I've mapped the shapes of games by looking
                           at the proportion of points won in each end, of the 
                           winners of each game. I basically wanted to know, 
                           where are these winning teams winning the most points? 
                           It appears to fluctuate wildly. Due to a limit of data 
                           for college curling, the sample size of games was too small
                           to be meaningful. The horizontal line represents the null
                           hypothesis, for each bar in each game, since
                           we expect each of the ten ends to be equally important for winning 
                           points. The variation far from this line is indeed a sign
                           that the ends are not all equal among all games. ")),
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
                               "Olympic Men" = "olympic_men", 
                               "College" = "college"
                               )
                         ), 
                         p("I chose to do a logistic regression of multiple 
                           variables, being the points scored in every end.
                           These coefficients, divided by four (labelled in white) can be loosely interpreted 
                           as chances of winning the game, given an increase of one 
                           in any of the ends. You can choose to subset the data 
                           and explore the differences in the regression coefficients. 
                           Due to overfitting, I removed the final ends (9 and 10) from my analysis
                           of olympic curling, since many games do conclude after the
                           8th end. Similarly, in college curling I ommitted the 
                           8th end because most games conclude after 6 or 7 ends, 
                           something that I had noticed while imputting the data. Here
                           you can see, for example, that every point scored in the 
                           first end in college curling games increases the chance of winning
                           by 37 %. That's a big effect!")),
                     mainPanel(plotOutput("log_plot"))))))
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Plot on Overview Page of Percentages
    output$percentage_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        ifelse(
            input$group_of_curlers == "college", 
            x<- college, 
            x<- olympics
        )
        
        ifelse(
            input$specification == "competition",
            
            
            x   <- x %>% 
                group_by(competition, ind_first_end_winner) %>% 
                summarise(pct = mean(ind_game_winner)),
            
            ifelse(
                input$specification == "year", 
                x<- x %>% 
                    group_by(year, ind_first_end_winner) %>% 
                    summarise(pct = mean(ind_game_winner)), 
                x <- x %>% 
                    group_by(team_group, ind_first_end_winner) %>% 
                    summarise(pct = mean(ind_game_winner))
            ))
        
        ifelse(
            input$specification == "competition",
            
            y <- x$competition,
            
            ifelse(
                input$specification == "year", 
                y <- x$year, 
                y <- x$team_group
            )
            
                )
        
    
        
        ggplot(x, aes(y, pct, fill = ind_first_end_winner))+
            geom_col(position = "dodge") +
            theme_classic()+
            labs(
                title = "Percent of the time a team given a win or a loss in the first end", 
                subtitle = "Across the spectrum of year, gender, and curling ability, \nteams who win the first end often win the game", 
                y = "Percentage", 
                x = input$specification, 
                fill = "First end result"
            ) +
            scale_fill_discrete(labels = c("Lost", "Won")) +
            geom_text(aes(label = round(pct * 100, 1), y = pct + 0.025), 
                      color = "black", position = position_dodge(width = 1) 
                      )
    })
    
    output$linreg_college <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ggplot(clean_all_college, aes(x1, win)) + 
            geom_point(position = "jitter") + 
            theme_classic() + 
            geom_smooth(method = "lm", se = F) + 
            ylim(0, 1.5) + xlim(0, 5)+ 
            labs(
                title = "Relationship between score in the First End and Chance of winning the game", 
                subtitle  = "In College Curling", 
                x = "Score in the First End", 
                y = "Chance of Winning"
            ) + 
            geom_text(x = 3, y = .4, label = lm_eqn(clean_all_college), 
                      parse = TRUE, 
                      color = "blue")
        
        
    })
    
    output$linreg_olympic<- renderPlot({
        ifelse(
            input$country == "all", 
            olympic_linreg_data<- olympics %>% 
                mutate(win = ifelse(ind_game_winner == T, 1, 0)), 
            olympic_linreg_data<- olympics %>% 
                filter(country== input$country) %>%
                mutate(win = ifelse(ind_game_winner == T, 1, 0))
        )
        
        ggplot(olympic_linreg_data, aes(x1, win)) + 
            geom_point(position = "jitter") +
            geom_smooth(method = "lm", se = F) + 
            theme_classic() +
            ylim(0, 1.5) + xlim(0, 5) + 
            labs(
                title = "Relationship between score in the First End and Chance of winning the game", 
                subtitle  = paste("in the Olympics for", input$country), 
                x = "Score in the First End", 
                y = "Chance of Winning"
            ) + 
            geom_text(x = 3, y = .4, label = lm_eqn(olympic_linreg_data), 
                      parse = TRUE, 
                      color = "blue")
    })
    
    # Plots on men/women vs finals/pool
    
    output$prelim_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        
        ifelse(
            input$plot_measure == "competition",
            
            
            x   <- olympics %>% 
                group_by(year,competition) %>% 
                summarize(prop_double_win = mean(first_end_first_game, na.rm = T)),
            
           
            x   <- olympics %>% 
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
        props %>% filter(category == input$who) %>%
            ggplot(aes(score, value)) +
            geom_col(fill = "turquoise") + 
            theme_classic() +
            labs(
                title = "Score in the first end & chance of winning", 
                x = "score in the first end", 
                y = "chance of winning"
            )
    })
    
    # fade plot: This is the code for rendering the animated plot. However
    # it takes too long to load. I'm putting it in, as a comment, for future 
    # reference/just in case. However, I used GIFS that I had saved for this portion
    
    # output$fade_plot <- renderPlot({
    #     # Generate type based on input$plot_type from ui
    # 
    #     fdata <- tidy_props
    # 
    #     fdata$value <- replace_na(fdata$value, 0)
    # 
    #     ifelse(
    #         input$plot_type == "competition",
    # 
    #         y <- fdata$competition,
    # 
    #         y <- fdata$team_group)
    # 
    #     ifelse(
    #         input$plot_type == "competition",
    # 
    #         legendtitle <- paste("Competition"),
    # 
    #         legendtitle <- paste("Team in Finals?"))
    # 
    # 
    #     ggplot(fdata, aes(prop, value)) +
    #             geom_col(aes(fill = y)) +
    #         facet_wrap(vars(year)) +
    #         transition_states(y,
    #                           transition_length = 2,
    #                           state_length = 1) + enter_fade() + exit_fade() +
    #         theme_classic() +
    #         labs(
    #             title = "Proportion of points scored by winners in Olympic Curling Games",
    #             y = "Proportion of points",
    #             x = "end",
    #             fill = legendtitle
    #         ) + scale_x_discrete(labels = c(1:10))
    #         
    # })
    output$gif <- renderImage({
        ifelse(input$gif_type == "competition", 
        
                gif <- (list(src = "plot_comp1.gif",
                 contentType = 'image/gif')),
        
                gif<- (list(src = "poolfinals1.gif",
                 contentType = 'image/gif')))
        gif
        
    }, deleteFile = FALSE)
    # logistic regression
    
    output$log_plot <- renderPlot({
        # Generate type based on input$plot_type from ui
        clean_olympics <- olympics %>% 
            mutate(win = ifelse(ind_game_winner == T, 1, 0)) %>% 
            select(id, year, competition, team_group, country, win, 
                   starts_with("x"), -x, -x_2, -x10, -x9)
        
        clean_college<- college %>% 
            mutate(win = ifelse(ind_game_winner == T, 1, 0)) %>% 
            select(id, year, competition, team_group, school, win, 
                   starts_with("x"))
        
        ifelse(
            input$reg_type == "all", 
            filt_olympics<- clean_olympics, 
            ifelse(
                input$reg_type == "olympic_men", 
                filt_olympics <- clean_olympics %>% 
                    filter(competition == "olympic_men"),
                ifelse(input$reg_type == "olympic_women", 
                       filt_olympics <- clean_olympics %>% 
                           filter(competition == "olympic_women"), 
                       filt_college <- clean_college)
                
            )
        )
        
        ifelse(input$reg_type == "college", 
               
               tibble_logistic_fit <- fit(logistic_mod,
                                   factor(win) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7,
                                   data = filt_college) %>% 
                                       tidy(conf.int = TRUE) %>%
                                       slice(2:8),
               
               tibble_logistic_fit <- fit(logistic_mod, 
                                   factor(win) ~ x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8, 
                                   data = filt_olympics)%>% 
                                       tidy(conf.int = TRUE) %>%
                                       slice(2:9)
               )
        
        ggplot(tibble_logistic_fit, aes(term, estimate)) + 
            geom_bar(stat = "identity", fill = "turquoise") + 
            geom_errorbar(aes(x = term, ymin = conf.low, ymax = conf.high), 
                          color = "turquoise4") + 
            labs(
                title = "Logistic regression coefficients for points per end on winning",
                subtitle = "with 95% confidence interval", 
                x = "end number", 
                y = "Logistic Regression Coefficient"
            )  + 
            scale_x_discrete(labels = c(1:10)) + 
            theme_classic() + ylim (0, 2.6) + 
            geom_text(aes(label = round(estimate / 4, 3)), color = "white", vjust = 1)
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
