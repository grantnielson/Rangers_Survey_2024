
# There are some inconsistencies in the data. For example, if you query:
# SELECT * 
# FROM pitch_data 
# WHERE game_pk = 670284
# AND inning = 4
# AND inning_half = 'BOTTOM'
# AND batter_mlb_player_id = 687052;

# You'll see that the base_state and score don't align. I noticed this when spot checking my purple dots.



# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
library(vroom)
library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(rsconnect)


# Sample Data - replace with your actual data
# For demonstration, I'll create a simple df with columns: game_pk, pitch_per_game, home_win_prob_pre
set.seed(123)
df <- vroom('pitch_data.csv')
# df <- df[df$game_pk %in% c(713702, 713708, 713703, 713700, 713701, 713698), ]

player_data <- vroom('player_data.csv')

# Add the Team column based on team_id
team_names <- c('91' = "Bradenton", '94' = "Clearwater", '95' = "Daytona", 
                '97' = "Dunedin", '100' = "Fort Myers", '103' = "Jupiter", 
                '105' = "Lakeland", '110' = "Palm Beach", '115' = "St. Lucie", 
                '117' = "Tampa")
df$home_team <- team_names[as.character(df$home_team_id)]
df$away_team <- team_names[as.character(df$away_team_id)]

df <- df %>%
  # Convert game_date to 'MM-DD' format
  mutate(game_date = gsub("2022-|T.*", "", game_date)) %>%
  # Filter for games where either team is Jupiter (ID 103)
  filter(home_team_id == 103 | away_team_id == 103)

# Determine opponent team
df <- df %>%
  mutate(opponent_team_id = ifelse(home_team_id == 103, away_team_id, home_team_id),
         opponent_team_name = team_names[as.character(opponent_team_id)])

# Create unique identifiers for select input with format 'Opponent Team, MM-DD'
df$game_label <- paste(df$opponent_team_name, df$game_date, sep = ", ")

ui <- fluidPage(
  titlePanel("2022 Jupiter Hammerheads ~ Win Probability Jumps"),
  
  # Subtitle
  tags$h5("Hover over points to observe change in game state", style = "text-align: center; color: gray;"),
  
  fluidRow(
    column(12, uiOutput("team_info"))  # Display home and away team info here
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("game", "Select Game", choices = unique(df$game_label))
    ),
    mainPanel(
      plotOutput("game_plot", hover = hoverOpts(id = "plot_hover")),
      verbatimTextOutput("hover_info")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  selected_game <- reactive({
    # Get game_pk for selected game based on the selected label
    selected_label <- input$game
    game_data <- df %>% filter(game_label == selected_label)
    
    # Get home and away team names for display
    home_team_name <- team_names[as.character(game_data$home_team_id[1])]
    away_team_name <- team_names[as.character(game_data$away_team_id[1])]
    
    # Add pre-event scores by lagging post-event scores
    game_data <- game_data %>%
      arrange(pitch_per_game) %>%
      mutate(
        home_score_pre_event = lag(home_score_post_event, default = 0),
        away_score_pre_event = lag(away_score_post_event, default = 0)
      )
    
    # Join player names for batters and pitchers
    game_data <- game_data %>%
      left_join(player_data, by = c("batter_mlb_player_id" = "player_id")) %>%
      rename(batter_name = name) %>%
      left_join(player_data, by = c("pitcher_mlb_player_id" = "player_id")) %>%
      rename(pitcher_name = name)
    
    list(game_data = game_data, home_team = home_team_name, away_team = away_team_name)
  })
  
  # Display Home and Away Team Info
  # output$team_info <- renderUI({
  #   game <- selected_game()
  #   tags$h4(paste("Home Team:", game$home_team, "| Away Team:", game$away_team))
  # })

  # Game Plot
  output$game_plot <- renderPlot({
    game <- selected_game()
    game_data <- game$game_data
    
    # Game details for title
    game_date <- game_data$game_date[1]
    home_team <- game$home_team
    away_team <- game$away_team
    winner <- ifelse(max(game_data$home_score_post_event) > max(game_data$away_score_post_event), home_team, away_team)
    
    plot_title <- paste("Game:", home_team, "(H) vs.", away_team, "(A) \nWinner:", winner, "| Date:", game_date)
    
    # Calculate size of points, with larger points for top win probability changes
    game_data <- game_data %>%
      mutate(win_prob_change = abs(home_win_prob_post - home_win_prob_pre)) %>%
      arrange(desc(win_prob_change))
    
    top_changes <- game_data$win_prob_change %in% sort(game_data$win_prob_change, decreasing = TRUE)[1:5]
    
    ggplot(game_data, aes(x = batter_per_game, y = home_win_prob_post)) +
      geom_point(aes(size = ifelse(top_changes, 6, 3), color = top_changes)) +
      scale_color_manual(
        name = "Biggest win probability \n changing play?",
        values = c("TRUE" = "purple", "FALSE" = "skyblue")
      ) +
      scale_size_identity() +
      labs(title = plot_title, x = "Batter Number", y = "Home Win Probability") +
      theme_minimal(base_size = 15) +  # Set a larger base font size for the plot
      theme(
        plot.title = element_text(size = 20, face = "bold"),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12)
      )
  })
  
  # Hover Info
  output$hover_info <- renderText({
    hover <- input$plot_hover
    if (is.null(hover)) return(NULL)
    
    game <- selected_game()
    game_data <- game$game_data
    hover_data <- nearPoints(game_data, hover, maxpoints = 1)
    
    if (nrow(hover_data) == 0) return(NULL)
    
    paste("Inning:", hover_data$inning_half, hover_data$inning,
          "\nBatter:", hover_data$batter_name,
          "\nPitcher:", hover_data$pitcher_name,
          "\nHome Score (Pre-Event):", hover_data$home_score_pre_event,
          "\nAway Score (Pre-Event):", hover_data$away_score_pre_event,
          "\nHome Score (Post-Event):", hover_data$home_score_post_event,
          "\nAway Score (Post-Event):", hover_data$away_score_post_event,
          "\nPitch Result:", hover_data$pitch_result,
          "\nPA Result:", hover_data$pa_result,
          "\nBase State (Pre-Event):", hover_data$base_state_pre_event,
          "\nBase State (Post-Event):", hover_data$base_state_post_event,
          "\nWin Prob Change:", abs(hover_data$home_win_prob_pre - hover_data$home_win_prob_post))
  })
}

shinyApp(ui, server)
