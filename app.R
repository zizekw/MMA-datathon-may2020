library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(dashboardthemes)
library(tidyverse)
library(DT)
library(plotly)
library(shinymanager)

inactivity <- "function idleTimer() {
var t = setTimeout(logout, 120000);
window.onmousemove = resetTimer; // catches mouse movements
window.onmousedown = resetTimer; // catches mouse movements
window.onclick = resetTimer;     // catches mouse clicks
window.onscroll = resetTimer;    // catches scrolling
window.onkeypress = resetTimer;  //catches keyboard actions

function logout() {
window.close();  //close the window
}

function resetTimer() {
clearTimeout(t);
t = setTimeout(logout, 120000);  // time is in milliseconds (1000 is 1 second)
}
}
idleTimer();"

# data.frame with credentials info
credentials <- data.frame(
    user = c("admin", "rotman"),
    password = c("admin", "data"),
    # comment = c("alsace", "auvergne", "bretagne"), %>% 
    stringsAsFactors = FALSE
)

# Define UI for application that draws a histogram
ui <- secure_app(head_auth = tags$script(inactivity),
    dashboardPagePlus(
    dashboardHeader(title = "Rotman Datathon"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("About Us", tabName = "about", icon = icon("address-card")),
            menuItem("Sample Data", tabName = "data", icon = icon("th")),
            menuItem("Team Shooting Behaviour", tabName = "shots", icon = icon("dashboard")),
            menuItem("Top Performers", tabName = "top", icon = icon("chart-line")),
            menuItem("Roster Recommendations", tabName = "recs", icon = icon("hockey-puck")),
            menuItem("Supplementary Insights", tabName = "supp", icon = icon("chart-pie"))
        )
    ),
    dashboardBody(
        shinyDashboardThemes(
            theme = "boe_website"
        ),
        tabItems(
        tabItem(tabName = "about",
                flipBox(
                    id = 1,
                    main_img = "https://pbs.twimg.com/profile_images/976150368382992384/1xzt6I-H_400x400.jpg",
                    header_img = "https://media-exp1.licdn.com/dms/image/C4E1BAQFWJ3-Y4NP-cw/company-background_10000/0?e=1592352000&v=beta&t=VvspnpAouqETvOiaUDJOi9G-fUWMv-8W-8_KRCoVFgY",
                    front_title = "Rotman Summer Datathon (Team H)",
                    back_title = "More about Stathletes",
                    "Welcome to our dashboard app! We are a group of students at the Rotman School of Management at the University of Toronto. We're participating in the first ever Rotman Summer Datathon as Team H. Thanks to our industry partner Stathletes (click `More` below), we've been given a dataset on the 2018 Olympic Hockey tournament. Team H intends to specifically analyze the Canadian Women's Hockey Team data to identify top performers in the 2018 tournament to help prepare for the next upcoming tournament. This dashboard serves to model key performance metrics and visualize performance behaviour to better inform offensive strategy. We will recommend 5 outstanding players: 3 goal scorers and 2 passers where 2 of the aforementioned players will be a faceoff specialist and one being a takeaway specialist.",
                    back_content = tagList(
                        column(
                            width = 12,
                            align = "center",
                            "We thank Stathletes for providing data for this competition. Stathletes is a Canadian company, founded with the purpose of developing a comprehensive statistical database to drive winning in hockey through real, actionable data.",
                            tags$a(href="https://www.stathletes.com/", "Click here to learn more!")
                        )
                    )
                ),
                tags$br(),
                widgetUserBox(
                    title = "Maggie Hu",
                    subtitle = "Rotman Student (MMA)",
                    width = 6,
                    type = 2,
                    src = "https://media-exp1.licdn.com/dms/image/C5603AQHWBI3KY8xCuA/profile-displayphoto-shrink_400_400/0?e=1596672000&v=beta&t=kXETofJs8L7Ou-vK2ZPvvXFysjTha4A7rdXBZF7hLFo",
                    color = "gray",
                    # background = TRUE,
                    # backgroundUrl = "https://media-exp1.licdn.com/dms/image/C4E1BAQFWJ3-Y4NP-cw/company-background_10000/0?e=1591639200&v=beta&t=fDkwEdH88V4nFGgJgx55WUOGp3Gmnsio14IvN7mr4Ro",
                    # "Some text here!",
                    # footer = "The footer here!",
                    collapsible = FALSE
                ),
                widgetUserBox(
                    title = "Grace Lou",
                    subtitle = "Rotman Student (MBA)",
                    width = 6,
                    type = 2,
                    src = "https://media-exp1.licdn.com/dms/image/C5103AQGGaW5_yV03pg/profile-displayphoto-shrink_800_800/0?e=1597881600&v=beta&t=Trp_vtqbn2N9voyj1X-JjkP7MiNOQZbIlUf7u5u1p7M",
                    color = "gray",
                    # "Some text here!",
                    # footer = "The footer here!",
                    collapsible = FALSE
                ),
                widgetUserBox(
                    title = "Shirley Zhang",
                    subtitle = "Rotman Student (MMA)",
                    width = 6,
                    type = 2,
                    src = "https://media-exp1.licdn.com/dms/image/C4E03AQFNa9ByS0KtKg/profile-displayphoto-shrink_400_400/0?e=1597276800&v=beta&t=UEnQETGprLbMaTcI6xCYIpkH9AjVvYnFa-u0uYWEmSw",
                    color = "gray",
                    # "Some text here!",
                    # footer = "The footer here!",
                    collapsible = FALSE
                ),
                widgetUserBox(
                    title = "Bill Zizek",
                    subtitle = "Rotman Student (MMA)",
                    width = 6,
                    type = 2,
                    src = "https://media-exp1.licdn.com/dms/image/C4D03AQGY6IKql3vhSg/profile-displayphoto-shrink_400_400/0?e=1596672000&v=beta&t=3mS99_Ecrve2pocgtrGu0In0F3k53pZgFx7X-KwJHow",
                    color = "gray",
                    # "Some text here!",
                    # footer = "The footer here!",
                    collapsible = FALSE
                )
        ),
        tabItem(tabName = "data",
                tags$h1("Sample Data"),
                "Note: This is not the full dataset but merely an example of the structure. The full dataset contains many thousands of rows and is owned by Stathletes.",
                tags$br(),
                DTOutput('tbl')
        ),
        tabItem(tabName = "shots",
                tabBox(title = "Plot Options", 
                           tabPanel("Game(s)", 
                                    checkboxGroupInput("game", label = h3("Select Games to Include"), 
                                                               choices = c("2018-02-11 - Olympic Athletes from Russia - Women at Canada - Women", 
                                                                           "2018-02-13 - Finland - Women at Canada - Women", "2018-02-14 - Canada - Women at United States - Women", 
                                                                           "2018-02-19 - Canada - Women at Olympic Athletes from Russia - Women", 
                                                                           "2018-02-21 - Canada - Women at United States - Women", "2019-02-12 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                           "2019-02-14 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                           "2019-02-17 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                           "2019-04-06 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                           "2019-04-13 - Olympic (Women) - Canada at Olympic (Women) - Finland", 
                                                                           "2019-04-14 - Olympic (Women) - United States at Olympic (Women) - Finland"
                                                               ),
                                                               selected = c("2018-02-11 - Olympic Athletes from Russia - Women at Canada - Women", 
                                                                            "2018-02-13 - Finland - Women at Canada - Women", "2018-02-14 - Canada - Women at United States - Women", 
                                                                            "2018-02-19 - Canada - Women at Olympic Athletes from Russia - Women", 
                                                                            "2018-02-21 - Canada - Women at United States - Women", "2019-02-12 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                            "2019-02-14 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                            "2019-02-17 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                            "2019-04-06 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                            "2019-04-13 - Olympic (Women) - Canada at Olympic (Women) - Finland", 
                                                                            "2019-04-14 - Olympic (Women) - United States at Olympic (Women) - Finland"
                                                               ))),
                           tabPanel("Team(s)", checkboxGroupInput("team", label = h3("Select Offensive Teams to Include"), 
                                                                  choices = c("Olympic (Women) - Canada", "Olympic (Women) - Olympic Athletes from Russia", 
                                                                              "Olympic (Women) - Finland", "Olympic (Women) - United States"
                                                                  ),
                                                                  selected = "Olympic (Women) - Canada"))), # make this box width 4
                box(title = NULL, 
                    # plotlyOutput("plot")
                    plotOutput("plot")
                    )),
        
        tabItem(tabName = "top",
                box(title = "Top 7 Scorers", solidHeader = TRUE, width = 6, tableOutput("scorers"), footer = "Goals per Game + Shooting %"),
                box(title = "Top 7 Passers", solidHeader = TRUE, width = 6, tableOutput("passers"), footer = "Passing % + Passing % to Top Scorers"),
                box(title = "Top 7 Faceoff Specialists", solidHeader = TRUE, width = 6, tableOutput("faceoff"), footer = "Total Number of Successes"),
                box(title = "Top 7 Takeaway Specialists", solidHeader = TRUE, width = 6, tableOutput("takeaway"), footer = "Total Number of Successes")),
        
        tabItem(tabName = "recs", 
                
                tags$h2("Roster Composition Recommendations"),
                
                fluidRow(width = 12,
                                           
                                widgetUserBox(
                                    title = "Marie-Philip Poulin",
                                    subtitle = "Scorer + Passer + Takeaway Specialist",
                                    width = 6,
                                    type = 2,
                                    src = "https://lscluster.hockeytech.com/download.php?client_code=hockeycanada&file_path=media/e5e62737e86422633e489032bebbee98.jpg",
                                    color = "gray",
                                    "Marie played in 8 out of 10 matches and holds the highest average goals per game. Marie has one of the highest attempted passes per game and is one of the best passers to other top scorers. She is the top takeaway specialist and the best passer to Melodie Daoust.",
                                    # footer = "Marie has one of the highest attempted passes per game and is one of the best passers to other top scorers. She is the top takeaway specialist and the best passer to Melodie Daoust.",
                                    collapsible = FALSE
                                ),
                                widgetUserBox(
                                    title = "Melodie Daoust",
                                    subtitle = "Scorer + Passer",
                                    width = 6,
                                    type = 2,
                                    src = "https://lscluster.hockeytech.com/download.php?client_code=hockeycanada&file_path=media/6bae05dd6ff018af2b705333411711cd.jpg",
                                    color = "gray",
                                    "Melodie played in 7 out of 10 matches and tied for the second highest average goals per game. Melodie is one of the best passers to other top scorers and is the best passer for Marie-Philip Poulin.",
                                    # footer = "Melodie is one of the best passers to other top scorers and is the best passer for Marie-Philip Poulin.",
                                    collapsible = FALSE
                                ),
                                widgetUserBox(
                                    title = "Jamie Lee Rattray",
                                    subtitle = "Scorer",
                                    width = 6,
                                    type = 2,
                                    src = "https://lscluster.hockeytech.com/download.php?client_code=hockeycanada&file_path=media/6fc6283fc22247024d79cf8f1fb7aba4.jpg",
                                    color = "gray",
                                    "Jamie has the highest shooting percentage among all players as well as tied for the second highest average goals per game.",
                                    # footer = "foo",
                                    collapsible = FALSE
                                ),
                                widgetUserBox(
                                    title = "Brianne Jenner",
                                    subtitle = "Takeaway Specialist + Faceoff Specialist",
                                    width = 6,
                                    type = 2,
                                    src = "https://lscluster.hockeytech.com/download.php?client_code=hockeycanada&file_path=media/205fa34c3041c4a53ee8d3bb9a410c52.jpg",
                                    color = "gray",
                                    "Brigette played in all 10 games and is one of the top faceoff and takeaway specialists. She also has an exceptional passing percentage to top scorers as well as the team in general.",
                                    # footer = "foo",
                                    collapsible = FALSE
                                ),
                                widgetUserBox(
                                    title = "Erin Ambrose",
                                    subtitle = "Passer + Faceoff Specialist",
                                    width = 6,
                                    type = 2,
                                    src = "https://lscluster.hockeytech.com/download.php?client_code=hockeycanada&file_path=media/4855f07adc58ac04f83c2ba762576f75.jpg",
                                    color = "gray",
                                    "Erin has one of the highest average passes per game and is one of the top faceoff specialists. She is the top passer for Jamie Lee Rattray and completed 95% of her passes to Marie-Philip Poulin.",
                                    # footer = "foo",
                                    collapsible = FALSE
                                )),
                tags$br(),
                tags$h2("Shot Activity"),
                fluidRow(width = 12,
                         tabBox(title = "Plot Options",
                                tabPanel("Player(s)", checkboxGroupInput("player_solo", label = h3("Select Players to Include"), 
                                                                       choices = c("Marie-Philip Poulin", "Melodie Daoust", "Jamie Lee Rattray", "Brianne Jenner", "Erin Ambrose"),
                                                                       selected = c("Marie-Philip Poulin", "Melodie Daoust", "Jamie Lee Rattray"))),
                                tabPanel("Game(s)", 
                                         checkboxGroupInput("game_solo", label = h3("Select Games to Include"), 
                                                            choices = c("2018-02-11 - Olympic Athletes from Russia - Women at Canada - Women", 
                                                                        "2018-02-13 - Finland - Women at Canada - Women", "2018-02-14 - Canada - Women at United States - Women", 
                                                                        "2018-02-19 - Canada - Women at Olympic Athletes from Russia - Women", 
                                                                        "2018-02-21 - Canada - Women at United States - Women", "2019-02-12 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                        "2019-02-14 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                        "2019-02-17 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                        "2019-04-06 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                        "2019-04-13 - Olympic (Women) - Canada at Olympic (Women) - Finland", 
                                                                        "2019-04-14 - Olympic (Women) - United States at Olympic (Women) - Finland"
                                                            ),
                                                            selected = c("2018-02-11 - Olympic Athletes from Russia - Women at Canada - Women", 
                                                                         "2018-02-13 - Finland - Women at Canada - Women", "2018-02-14 - Canada - Women at United States - Women", 
                                                                         "2018-02-19 - Canada - Women at Olympic Athletes from Russia - Women", 
                                                                         "2018-02-21 - Canada - Women at United States - Women", "2019-02-12 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                         "2019-02-14 - Olympic (Women) - United States at Olympic (Women) - Canada", 
                                                                         "2019-02-17 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                         "2019-04-06 - Olympic (Women) - Canada at Olympic (Women) - United States", 
                                                                         "2019-04-13 - Olympic (Women) - Canada at Olympic (Women) - Finland"
                                                            )))
                                ), # make this box width 4
                         box(title = NULL, 
                             # plotlyOutput("plot")
                             plotOutput("plot_solo")
                         )),
                ),
        tabItem(tabName = "supp",
                "lorem ipsum"
        )
        )
        )
)
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
    result_auth <- secure_server(check_credentials = check_credentials(credentials))
    
    output$res_auth <- renderPrint({
        reactiveValuesToList(result_auth)
    })
    
    top_players <- c("Marie-Philip Poulin", "Melodie Daoust", "Jamie Lee Rattray", "Brigette Lacquette", "Erin Ambrose")
    
    data <- read_csv("data/Rotman MMA Summer Datathon (men&women olympic).csv")
    
    sample_data <- read_csv("data/sample_data.csv")
    
    # we only want data from the women's team 
    
    game_index <- unique(data$game_name)
    
    game_index <- game_index[c(1:3,7,9,12:17)]
    
    data_women <- data %>% 
                    filter(., data$game_name %in% game_index)
    
    shot_data <- data_women[data_women$event_type == "Shot",]
    
    shot_data$goal <- ifelse(shot_data$event_successful == TRUE, 1, 0)

    shot_model <- glm(shot_data$goal ~ shot_data$x_event + shot_data$y_event + shot_data$shot_type, data = shot_data, family = "binomial")

    summary(shot_model)

    shot_data$goal_predictions <- predict(shot_model, shot_data, type = "response")
    
    # table time
    output$tbl = renderDT(
        # shot_data, options = list(lengthChange = FALSE)
        datatable(
            sample_data,
            rownames = FALSE,
            extensions = c("Buttons", "FixedColumns", "FixedHeader", "Scroller"), 
            options = list(
                # scroller = TRUE,
                # scrollX = TRUE,
                # autoWidth = TRUE,
                # scrollY = "750px",
                # fixedHeader=TRUE,
                # class='cell-border stripe',
                scrollX = TRUE,
                dom = 'Blrtip',
                # buttons = c('colvis', 'copy', 'csv', 'excel', 'print'),
                buttons = c('colvis'),
                lengthMenu = c(10, 25, 50, 100),
                pageLength = 10
            )
        )
    )
    
    # get game from UI
    output$game <- renderPrint({ input$game })
    output$game_solo <- renderPrint({ input$game_solo })
    
    data_plot <- reactive({
        data_women %>% 
            filter(event_type == "Shot") %>% 
            filter(game_name %in% input$game) %>% 
            filter(team_name %in% input$team)
    })
    
    data_plot_solo <- reactive({
        data_women %>% 
            filter(event_type == "Shot") %>% 
            filter(game_name %in% input$game_solo) %>% 
            filter(player_name %in% input$player_solo)
    }) 
    
    
    # subtitle <- reactive({unique(data_plot()$team_name)})
    # caption <- reactive({unique(data_plot()$game_name)})
    
    output$plot <- renderPlot({
        
        # renderPlotly({
        
        img <- png::readPNG("images/ozone.png")
        
        # ggplotly(
            ggplot(data = data_plot(), aes(x = y_event, y = x_event)) + # x and y are location on rink
            
            #  plot rink
            annotation_raster(img, xmin = 0, xmax = 85, ymin = 100, ymax = 200) +
            scale_x_continuous(limits = c(0,85), expand = c(0,0)) +
            scale_y_continuous(limits = c(100,200), expand = c(0,0)) + # base amount should work by here
            
            #  add density
            stat_density_2d(geom = "polygon", aes(alpha = stat(level)), fill = "royalblue4", alpha = 0.2, col = "white", bins = 8) +
            
            geom_point(data = . %>% filter(event_successful == TRUE), fill="#E20778", colour = "white", size = 4, shape = 23, alpha = 0.8) +
                
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x =element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 17),
                  plot.subtitle = element_text(hjust = 0.5, size = 12),
                  legend.position ="none") +
            
            ggtitle("Offensive Zone Shot Activity") +
            # labs(title = "Offensive Zone Shot Activity", subtitle = subtitle(), caption = caption()) +
            # subtitle with team name focused on + game
            
            coord_fixed(xlim = c(0, 85), ylim = c(100, 200)) +
            
            # geom_rect(aes(xmin = 10, xmax = 75, ymin = 103, ymax = 110), colour = "black", size = 1, fill = "lightblue") +
            # annotate("text", label = "Expected Shooting %: 6.3%", x = 42.5, y = 107, size = 5) +
            
            geom_segment(data = data.frame(x = c(38.25, 38.25, 20.75, 20.75, 64.25, 64.25),
                                           y = c(188, 188, 168.5, 153.5, 153.5, 168.5),
                                           xend = c(46.75, 20.75, 20.75, 64.25, 64.25, 46.75),
                                           yend = c(188, 168.5, 153.5, 153.5, 168.5, 188)),
                         aes(x = x, y = y, xend = xend, yend = yend),
                         size = 1, col = "midnightblue", linetype = 2)
            # )
    })
    
    output$plot_solo <- renderPlot({
        
        # renderPlotly({
        
        img <- png::readPNG("images/ozone.png")
        
        # ggplotly(
        ggplot(data = data_plot_solo(), aes(x = y_event, y = x_event)) + # x and y are location on rink
            
            #  plot rink
            annotation_raster(img, xmin = 0, xmax = 85, ymin = 100, ymax = 200) +
            scale_x_continuous(limits = c(0,85), expand = c(0,0)) +
            scale_y_continuous(limits = c(100,200), expand = c(0,0)) + # base amount should work by here
            
            #  add density
            stat_density_2d(geom = "polygon", aes(alpha = stat(level)), fill = "royalblue4", alpha = 0.2, col = "white", bins = 8) +
            
            geom_point(data = . %>% filter(event_successful == TRUE), fill="#E20778", colour = "white", size = 4, shape = 23, alpha = 0.8) +
            
            theme(axis.title.x = element_blank(),
                  axis.text.x = element_blank(),
                  axis.ticks.x =element_blank(),
                  axis.title.y = element_blank(),
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  plot.title = element_text(hjust = 0.5, size = 17),
                  plot.subtitle = element_text(hjust = 0.5, size = 12),
                  legend.position ="none") +
            
            ggtitle("Offensive Zone Shot Activity") +
            # labs(title = "Offensive Zone Shot Activity", subtitle = subtitle(), caption = caption()) +
            # subtitle with team name focused on + game
            
            coord_fixed(xlim = c(0, 85), ylim = c(100, 200)) +
            
            # geom_rect(aes(xmin = 10, xmax = 75, ymin = 103, ymax = 110), colour = "black", size = 1, fill = "lightblue") +
            # annotate("text", label = "Expected Shooting %: 6.3%", x = 42.5, y = 107, size = 5) +
            
            geom_segment(data = data.frame(x = c(38.25, 38.25, 20.75, 20.75, 64.25, 64.25),
                                           y = c(188, 188, 168.5, 153.5, 153.5, 168.5),
                                           xend = c(46.75, 20.75, 20.75, 64.25, 64.25, 46.75),
                                           yend = c(188, 168.5, 153.5, 153.5, 168.5, 188)),
                         aes(x = x, y = y, xend = xend, yend = yend),
                         size = 1, col = "midnightblue", linetype = 2)
        # )
    })
    
    output$scorers <- renderTable({tibble(Rank = c(1:7), Player = c("Marie-Philip Poulin", "Loren Gabel", "Melodie Daoust", "Meghan Agosta", "Jamie Lee Rattray", "Jennifer Wakefield", "Haley Irwin"))}) 
    
    output$passers <- renderTable({tibble(Rank = c(1:7), Player = c("Laura Fortino", "Melodie Daoust", "Marie-Philip Poulin", "Rebecca Johnston", "Jocelyne Larocque", "Brigette Lacquette", "Erin Ambrose"))})
    
    output$faceoff <- renderTable({tibble(Rank = c(1:7), Player = c("Brianne Jenner", "Brigette Lacquette", "Blayre Turnbull", "Emily Clark", "Ann-Sophie Bettez", "Erin Ambrose", "Halli Krzyzaniak"))})
    
    output$takeaway <- renderTable({tibble(Rank = c(1:7), Player = c("Marie-Philip Poulin", "Brianne Jenner", "Renata Fast", "Laura Fortino", "Natalie Spooner", "Jocelyne Larocque", "Brigette Lacquette"))})
    
}

# Run the application 
shinyApp(ui = ui, server = server)
