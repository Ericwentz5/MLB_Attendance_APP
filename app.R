# 1. Load required libraries
library(shiny)
library(httr)
library(jsonlite)
library(shinyjs)
library(ggplot2)
library(ggimage)
library(dplyr)
library(magick)
library(grid)
library(rsvg)
library(rsconnect)
library(grImport2)




#HUnters stuff ------ rsconnect::setAccountInfo(name='playshotcaller',
#token='39DDBA0A558FE4DD1287DFF2603FFE41',
#secret='YBqzS1Hh4qfYmqHztp5LpeovRayvTKnO2Lw3sLyn')

# 2. Define global variables
api_key <- "NpMK1iObL94oeY6ePh43Bjfz4z14S1fai9tohAW6"

# 3. Define functions





# Function to handle SVG logos
get_svg_logo <- function(url) {
  svg_file <- tempfile(fileext = ".svg")
  download.file(url, svg_file)
  picture <- grImport2::readPicture(svg_file)
  unlink(svg_file)  # Delete the temporary file
  return(picture)
}

# Function 1: Get Attendance Data for a Specific Date
get_attendance_data <- function(year, month, day, team) {
  endpoint <- paste0("https://api.sportradar.com/mlb/trial/v7/en/games/", year, "/", month, "/", day, "/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  games <- json_data$games
  
  date <- paste0(year, "-", month, "-", day)
  games_on_date <- games[grepl(date, games$scheduled), ]
  
  filtered_games <- games_on_date[games_on_date$home.abbr == team | games_on_date$away.abbr == team, ]
  
  if (nrow(filtered_games) == 0) {
    stop("No games found for the given date and team.")
  }
  
  final_data <- data.frame(
    Date = date,
    Home_Team = filtered_games$home.name,
    Away_Team = filtered_games$away.name,
    Attendance = filtered_games$attendance,
    Stadium_Name = filtered_games$venue.name,
    Venue_Capacity = filtered_games$venue.capacity,
    Attendance_Percentage = round((filtered_games$attendance / filtered_games$venue.capacity) * 100, 2)
  )
  
  return(final_data)
}

# Function 2: Get Total Attendance Through a Number of Games
get_total_attendance_through_games <- function(year, team, num_games) {
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  games <- json_data$games
  
  team_games <- games %>% filter(home.abbr == team) %>% arrange(as.Date(scheduled)) %>% slice(1:num_games)
  
  total_attendance <- sum(team_games$attendance, na.rm = TRUE)
  
  return(total_attendance)
}

# Function 3: Get Season Attendance Data for a Team
get_season_attendance <- function(year, team, api_key) {
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  games <- json_data$games
  
  team_games <- games %>% filter(home.abbr == team) %>% arrange(as.Date(scheduled)) %>%
    select(scheduled, attendance) %>%
    mutate(date = as.Date(scheduled)) %>%
    filter(!is.na(attendance)) %>%
    mutate(cumulative_attendance = cumsum(attendance), team = team)
  
  return(team_games)
}

# Function 4: Plot Cumulative Attendance Comparison with Team Logos

get_season_attendance_plot <- function(year, teams) {
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  
  games <- json_data$games
  
  # Ensure we're only considering games from the specified year
  games <- games %>% filter(substr(scheduled, 1, 4) == as.character(year))
  
  # Filter data to only include games up to the current date
  current_date <- Sys.Date()
  games <- games %>% filter(as.Date(scheduled) <= current_date)
  
  # Process data for each team
  all_teams_data <- lapply(teams, function(team) {
    team_data <- games %>% filter(home.abbr == team | away.abbr == team)
    
    if (nrow(team_data) == 0) {
      return(NULL)  # Skip if no data for this team
    }
    
    team_data <- team_data %>%
      mutate(team = team) %>%
      group_by(team, scheduled) %>%
      summarise(attendance = sum(attendance, na.rm = TRUE), .groups = 'drop') %>%
      arrange(as.Date(scheduled)) %>%
      mutate(cumulative_attendance = cumsum(attendance))
    
    return(team_data)
  })
  
  # Combine data for all teams
  all_teams_data <- bind_rows(all_teams_data)
  
  if (nrow(all_teams_data) == 0) {
    stop("No data found for the specified teams.")
  }
  
  # Order teams by their cumulative attendance as of the current date
  final_cumulative <- all_teams_data %>%
    group_by(team) %>%
    summarise(final_cumulative_attendance = max(cumulative_attendance), .groups = 'drop') %>%
    arrange(desc(final_cumulative_attendance))
  
  # Reorder factor levels based on cumulative attendance
  all_teams_data$team <- factor(all_teams_data$team, levels = final_cumulative$team)
  
  # Generate the plot
  plot <- ggplot(all_teams_data, aes(x = as.Date(scheduled), y = cumulative_attendance / 1e6, color = team)) +
    geom_line(size = 1) +
    labs(
      title = paste("Cumulative Attendance Comparison in the", year, "Season"),
      x = "Date",
      y = "Cumulative Attendance (Millions)",
      color = "Team"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b %Y", limits = c(min(as.Date(all_teams_data$scheduled)), max(as.Date(all_teams_data$scheduled)))) +
    scale_y_continuous(labels = scales::comma) +
    guides(color = guide_legend(reverse = TRUE))  # Reverse the legend to match the order of cumulative attendance
  
  # Add text annotation showing the order of leaders
  leader_text <- paste("Current Leaders:", paste(final_cumulative$team, collapse = ", "))
  plot <- plot + annotate("text", x = max(as.Date(all_teams_data$scheduled)), y = max(all_teams_data$cumulative_attendance / 1e6) * 1.05, 
                          label = leader_text, hjust = 1, vjust = 1, size = 3)
  
  return(plot)
}

# Function 5: Get Attendance Data for All Teams on a Specific Date
get_attendance_data_all_teams <- function(year, month, day) {
  # Construct the API endpoint URL with the new format
  endpoint <- paste0("https://api.sportradar.com/mlb/trial/v7/en/games/", year, "/", month, "/", day, "/schedule.json?api_key=", api_key)
  
  # Make the API request
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  games <- json_data$games
  
  # Extract necessary information
  final_data <- data.frame(
    HomeTeam = games$home.name,
    Attendance = games$attendance,
    Capacity = games$venue.capacity,
    AttendancePercentage = pmin(round((games$attendance / games$venue.capacity) * 100, 2), 100)
  )
  
  all_team_logos <- data.frame(
    HomeTeam = c(
      "Orioles", "Red Sox", "Yankees", "Blue Jays", "Rays",
      "White Sox", "Guardians", "Tigers", "Royals", "Twins",
      "Astros", "Angels", "Athletics", "Mariners", "Rangers",
      "Braves", "Marlins", "Mets", "Phillies", "Nationals",
      "Cubs", "Reds", "Brewers", "Pirates", "Cardinals",
      "Diamondbacks", "Rockies", "Dodgers", "Padres", "Giants"
    ),
    Logo = c(
      "https://www.mlbstatic.com/team-logos/110.svg",
      "https://www.mlbstatic.com/team-logos/111.svg",
      "https://www.mlbstatic.com/team-logos/147.svg",
      "https://www.mlbstatic.com/team-logos/141.svg",
      "https://www.mlbstatic.com/team-logos/139.svg",
      "https://www.mlbstatic.com/team-logos/145.svg",
      "https://www.mlbstatic.com/team-logos/114.svg",
      "https://www.mlbstatic.com/team-logos/116.svg",
      "https://www.mlbstatic.com/team-logos/118.svg",
      "https://www.mlbstatic.com/team-logos/142.svg",
      "https://www.mlbstatic.com/team-logos/117.svg",
      "https://www.mlbstatic.com/team-logos/108.svg",
      "https://www.mlbstatic.com/team-logos/133.svg",
      "https://www.mlbstatic.com/team-logos/136.svg",
      "https://www.mlbstatic.com/team-logos/140.svg",
      "https://www.mlbstatic.com/team-logos/144.svg",
      "https://www.mlbstatic.com/team-logos/146.svg",
      "https://www.mlbstatic.com/team-logos/121.svg",
      "https://www.mlbstatic.com/team-logos/143.svg",
      "https://www.mlbstatic.com/team-logos/120.svg",
      "https://www.mlbstatic.com/team-logos/112.svg",
      "https://www.mlbstatic.com/team-logos/113.svg",
      "https://www.mlbstatic.com/team-logos/158.svg",
      "https://www.mlbstatic.com/team-logos/134.svg",
      "https://www.mlbstatic.com/team-logos/138.svg",
      "https://www.mlbstatic.com/team-logos/109.svg",
      "https://www.mlbstatic.com/team-logos/115.svg",
      "https://www.mlbstatic.com/team-logos/119.svg",
      "https://www.mlbstatic.com/team-logos/135.svg",
      "https://www.mlbstatic.com/team-logos/137.svg"
    )
  )
  
  final_data <- merge(final_data, all_team_logos, by = "HomeTeam", all.x = TRUE)
  
  # Remove duplicates
  final_data <- final_data[!duplicated(final_data), ]
  
  final_data <- na.omit(final_data)
  
  # Calculate the average attendance percentage
  avg_attendance_percentage <- mean(final_data$AttendancePercentage, na.rm = TRUE)
  
  # Calculate the delta (distance from the average) in both percentage and number of fans
  final_data$DeltaPercentage <- final_data$AttendancePercentage - avg_attendance_percentage
  final_data$DeltaFans <- round((final_data$DeltaPercentage / 100) * final_data$Capacity)
  
  # Remove the Logo column from the final data for JSON output
  final_data_json <- final_data %>%
    select(-Logo) %>%
    toJSON(pretty = TRUE)
  
  # Plotting
  plot <- ggplot(final_data, aes(x = HomeTeam, y = AttendancePercentage)) +
    geom_point(size = .1) +
    geom_hline(yintercept = avg_attendance_percentage, color = "red", linetype = "dashed") +
    scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 10), labels = scales::comma) +
    labs(
      title = paste("Attendance Capacity Percentage for Home Teams on", paste(year, month, day, sep = "-")),
      x = "Home Team",
      y = "Attendance Percentage"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    geom_image(aes(image = Logo), size = 0.09)  # Adjust image size and position as needed
  
  return(list(plot = plot, json = final_data_json))
}


# Function 6: Get Attendance by Days of the Week for a Season
get_attendance_by_days <- function(year, team) {
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  
  games <- json_data$games
  team_games <- games %>% filter(home.abbr == team | away.abbr == team)
  
  team_games <- team_games %>%
    mutate(day_of_week = weekdays(as.Date(scheduled)))
  
  attendance_by_day <- team_games %>%
    group_by(day_of_week) %>%
    summarize(total_attendance = sum(attendance, na.rm = TRUE))
  
  # Factor the days of the week to ensure they are in order
  attendance_by_day$day_of_week <- factor(attendance_by_day$day_of_week, 
                                          levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
  
  return(attendance_by_day)
}


#Function 7 Average attendance through x home games

# Function to get the average attendance for a team through a specified number of games in a season
get_avg_attendance_through_games <- function(year, team, num_games, game_type) {
  # Construct the API endpoint URL
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  
  # Make the API request
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data:", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  
  games <- json_data$games
  
  # Filter the games to include only games for the specified team
  team_games <- games %>% filter(home.abbr == team | away.abbr == team)
  
  # Sort the games by date to ensure chronological order
  team_games <- team_games %>% arrange(as.Date(scheduled))
  
  # Select the first `num_games` games
  selected_games <- team_games %>% slice(1:num_games)
  
  # Filter games based on the toggle: home or away
  if (game_type == "Home") {
    relevant_games <- selected_games %>% filter(home.abbr == team)
  } else {
    relevant_games <- selected_games %>% filter(away.abbr == team)
  }
  
  # Check if any relevant games are found
  actual_num_games <- nrow(relevant_games)
  
  if (actual_num_games == 0) {
    stop("No games found for the specified team within the selected games.")
  }
  
  # Sum the attendance for the selected games
  total_attendance <- sum(relevant_games$attendance, na.rm = TRUE)
  
  # Calculate the average attendance based on the actual number of games selected
  avg_attendance <- total_attendance / actual_num_games
  
  # Return a list with the necessary information
  return(list(
    Year = year,
    Team = team,
    Number_of_Games_Selected = num_games,
    Number_of_Relevant_Games = actual_num_games,
    Total_Attendance = total_attendance,
    Average_Attendance = avg_attendance
  ))
}
  
  #Function 8 attendance within date range 

get_attendance_home_away <- function(year, team, start_date, end_date) {
  endpoint <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year, "/REG/schedule.json?api_key=", api_key)
  response <- GET(endpoint)
  
  if (response$status_code != 200) {
    stop("Failed to fetch data: ", response$status_code)
  }
  
  data <- content(response, "text")
  json_data <- fromJSON(data, flatten = TRUE)
  
  games <- json_data$games
  games_filtered <- games %>%
    mutate(scheduled_date = as.Date(substr(scheduled, 1, 10))) %>%
    filter(scheduled_date >= as.Date(start_date) & scheduled_date <= as.Date(end_date)) %>%
    filter(home.abbr == team | away.abbr == team)
  
  if (nrow(games_filtered) == 0) {
    return(list(
      message = "No games found within the specified date range.",
      Total_Attendance = NA,
      Average_Attendance = NA,
      Number_of_Games = 0,
      Home_Games = 0,
      Away_Games = 0,
      Total_Home_Attendance = NA,
      Total_Away_Attendance = NA,
      Average_Home_Attendance = NA,
      Average_Away_Attendance = NA
    ))
  }
  
  # Calculating attendance metrics
  total_attendance <- sum(games_filtered$attendance, na.rm = TRUE)
  number_of_games <- nrow(games_filtered)
  average_attendance <- ifelse(number_of_games > 0, total_attendance / number_of_games, NA)
  
  home_games <- games_filtered %>% filter(home.abbr == team)
  away_games <- games_filtered %>% filter(away.abbr == team)
  
  total_home_attendance <- sum(home_games$attendance, na.rm = TRUE)
  total_away_attendance <- sum(away_games$attendance, na.rm = TRUE)
  
  average_home_attendance <- ifelse(nrow(home_games) > 0, total_home_attendance / nrow(home_games), NA)
  average_away_attendance <- ifelse(nrow(away_games) > 0, total_away_attendance / nrow(away_games), NA)
  
  return(list(
    message = "Data successfully retrieved.",
    Total_Attendance = total_attendance,
    Average_Attendance = average_attendance,
    Number_of_Games = number_of_games,
    Home_Games = nrow(home_games),
    Away_Games = nrow(away_games),
    Total_Home_Attendance = total_home_attendance,
    Total_Away_Attendance = total_away_attendance,
    Average_Home_Attendance = average_home_attendance,
    Average_Away_Attendance = average_away_attendance
  ))
}


#Function 9 Year over Year

compare_team_attendance_two_years <- function(team, year1, year2) {
  # API endpoints for both years
  endpoint_year1 <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year1, "/REG/schedule.json?api_key=", api_key)
  endpoint_year2 <- paste0("https://api.sportradar.us/mlb/trial/v7/en/games/", year2, "/REG/schedule.json?api_key=", api_key)
  
  # Fetch data for both years
  response_year1 <- GET(endpoint_year1)
  response_year2 <- GET(endpoint_year2)
  
  if (response_year1$status_code != 200 || response_year2$status_code != 200) {
    stop("Failed to fetch data for one or both years.")
  }
  
  data_year1 <- content(response_year1, "text")
  json_data_year1 <- fromJSON(data_year1, flatten = TRUE)
  games_year1 <- json_data_year1$games
  
  data_year2 <- content(response_year2, "text")
  json_data_year2 <- fromJSON(data_year2, flatten = TRUE)
  games_year2 <- json_data_year2$games
  
  # Process data for the specified team in both years
  team_data_year1 <- games_year1 %>%
    filter(home.abbr == team | away.abbr == team) %>%  # Use | for element-wise comparison
    mutate(year = year1) %>%
    mutate(scheduled = as.Date(substr(scheduled, 1, 10))) %>%
    mutate(month_day = format(scheduled, "%m-%d")) %>%  # Extract month and day
    group_by(month_day, year) %>%
    summarise(attendance = sum(attendance, na.rm = TRUE), .groups = 'drop') %>%
    arrange(month_day) %>%
    mutate(cumulative_attendance = cumsum(attendance))
  
  team_data_year2 <- games_year2 %>%
    filter(home.abbr == team | away.abbr == team) %>%  # Use | for element-wise comparison
    mutate(year = year2) %>%
    mutate(scheduled = as.Date(substr(scheduled, 1, 10))) %>%
    mutate(month_day = format(scheduled, "%m-%d")) %>%  # Extract month and day
    group_by(month_day, year) %>%
    summarise(attendance = sum(attendance, na.rm = TRUE), .groups = 'drop') %>%
    arrange(month_day) %>%
    mutate(cumulative_attendance = cumsum(attendance))
  
  # Combine data from both years
  combined_data <- bind_rows(team_data_year1, team_data_year2)
  
  # Generate the plot
  plot <- ggplot(combined_data, aes(x = as.Date(month_day, format = "%m-%d"), y = cumulative_attendance / 1e6, color = factor(year))) +
    geom_line(size = 1) +
    labs(
      title = paste("Cumulative Attendance Comparison for", team, "in", year1, "and", year2),
      x = "Month-Day",
      y = "Cumulative Attendance (Millions)",
      color = "Year"
    ) +
    theme_minimal() +
    scale_x_date(date_breaks = "1 month", date_labels = "%b") +
    scale_y_continuous(labels = scales::comma) +
    guides(color = guide_legend(reverse = TRUE))
  
  return(plot)
}



#UI


# 4. Define the UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("MLB Attendance App"),
  
  sidebarLayout(
    sidebarPanel(
      actionButton("toggleInputs", "Get Attendance", title = "Retrieve and display attendance data for a specific team on a selected date."),
      hidden(
        div(
          id = "inputFields",
          textInput("year", "Year (e.g., 2018)", value = "", placeholder = "Year (e.g., 2018)"),
          textInput("month", "Month (e.g., 04)", value = "", placeholder = "Month (e.g., 04)"),
          textInput("day", "Day (e.g., 04)", value = "", placeholder = "Day (e.g., 04)"),
          textInput("team", "Team Abbreviation (e.g., PIT)", value = "", placeholder = "Team (e.g., PIT)"),
          actionButton("submit", "Submit")
        )
      ),
      hr(),
      actionButton("togglePlotInputs", "Generate Plot", title = "Generate a scatter plot showing attendance percentages for all MLB teams on a selected date."),
      hidden(
        div(
          id = "plotFields",
          textInput("plot_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("plot_month", "Month (e.g., 08)", value = "", placeholder = "Month (e.g., 08)"),
          textInput("plot_day", "Day (e.g., 01)", value = "", placeholder = "Day (e.g., 01)"),
          actionButton("generatePlot", "Generate Plot")
        )
      ),
      hr(),
      actionButton("toggleTotalAttendanceInputs", "Get Total Attendance", title = "Calculate and display the total attendance for a selected team over a specified number of games in a given year."),
      hidden(
        div(
          id = "totalAttendanceFields",
          textInput("total_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("total_team", "Team Abbreviation (e.g., MIN)", value = "", placeholder = "Team (e.g., MIN)"),
          numericInput("num_games", "Number of Games", value = 30, min = 1),
          actionButton("getTotalAttendance", "Get Total Attendance")
        )
      ),
      hr(),
      actionButton("toggleComparisonPlotInputs", "Compare Teams' Attendance", title = "Compare the cumulative attendance of five MLB teams over the course of a season."),
      hidden(
        div(
          id = "comparisonPlotFields",
          textInput("comparison_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("comparison_team1", "Team 1 (e.g., ATL)", value = "", placeholder = "Team 1 (e.g., ATL)"),
          textInput("comparison_team2", "Team 2 (e.g., PHI)", value = "", placeholder = "Team 2 (e.g., PHI)"),
          textInput("comparison_team3", "Team 3 (e.g., NYM)", value = "", placeholder = "Team 3 (e.g., NYM)"),
          textInput("comparison_team4", "Team 4 (e.g., NYY)", value = "", placeholder = "Team 4 (e.g., NYY)"),
          textInput("comparison_team5", "Team 5 (e.g., WSH)", value = "", placeholder = "Team 5 (e.g., WSH)"),
          actionButton("generateComparisonPlot", "Generate Comparison Plot")
        )
      ),
      hr(),
      actionButton("toggleDaysOfWeekInputs", "Get Attendance by Day of Week", title = "Display total attendance for a selected team, broken down by day of the week over a season."),
      hidden(
        div(
          id = "daysOfWeekFields",
          textInput("days_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("days_team", "Team Abbreviation (e.g., NYY)", value = "", placeholder = "Team (e.g., NYY)"),
          actionButton("getDaysAttendance", "Get Attendance by Day of Week")
        )
      ),
      hr(),
      actionButton("toggleAvgAttendanceInputs", "Get Average Attendance", title = "Calculate and display the average attendance for a selected team over a specified number of games, with options to filter by home or away games."),
      hidden(
        div(
          id = "avgAttendanceFields",
          textInput("avg_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("avg_team", "Team Abbreviation (e.g., MIN)", value = "", placeholder = "Team (e.g., MIN)"),
          numericInput("avg_num_games", "Number of Games", value = 30, min = 1),
          radioButtons("game_type", "Game Type", choices = c("Home", "Away"), selected = "Home"),
          actionButton("getAvgAttendance", "Get Average Attendance")
        )
      ),
      hr(),
      actionButton("toggleAttendanceRangeInputs", "Get Attendance by Date Range"),
      hidden(
        div(
          id = "attendanceRangeFields",
          textInput("range_year", "Year (e.g., 2024)", value = "", placeholder = "Year (e.g., 2024)"),
          textInput("range_team", "Team Abbreviation (e.g., MIN)", value = "", placeholder = "Team (e.g., MIN)"),
          dateInput("start_date", "Start Date (yyyy-mm-dd)", format = "yyyy-mm-dd"),
          dateInput("end_date", "End Date (yyyy-mm-dd)", format = "yyyy-mm-dd"),
          actionButton("getAttendanceRange", "Get Attendance by Date Range")
        )
      ),
      hr(),
      actionButton("toggleYearComparisonInputs", "Compare Attendance Between Years", title = "Compare the cumulative attendance of a team between two years."),
      hidden(
        div(
          id = "yearComparisonFields",
          textInput("comparison_team", "Team Abbreviation (e.g., MIN)", value = "", placeholder = "Team (e.g., MIN)"),
          textInput("year1", "Year 1 (e.g., 2023)", value = "", placeholder = "Year 1 (e.g., 2023)"),
          textInput("year2", "Year 2 (e.g., 2024)", value = "", placeholder = "Year 2 (e.g., 2024)"),
          actionButton("compareYears", "Compare Attendance")
        )
      )
    ),
    mainPanel(
      verbatimTextOutput("json_output"),
      plotOutput("attendance_plot")
    )
  )
)



# 5. Define the server logic
server <- function(input, output, session) {
  session$allowReconnect(TRUE)  # Allow reconnection
  
  observe({
    query <- parseQueryString(session$clientData$url_search)
    
    if (!is.null(query$json) && query$json == "true") {
      result <- NULL
      
      if (!is.null(query$action)) {
        tryCatch({
          result <- switch(query$action,
                           "attendance_date" = get_attendance_data(query$year, query$month, query$day, query$team),
                           "attendance_all_teams" = get_attendance_data_all_teams(query$year, query$month, query$day),
                           "total_attendance" = get_total_attendance_through_games(query$year, query$team, as.numeric(query$num_games)),
                           "compare_teams" = get_season_attendance_plot(query$year, c(query$team1, query$team2, query$team3, query$team4, query$team5)),
                           "average_attendance" = get_avg_attendance_through_games(query$year, query$team, as.numeric(query$num_games), query$game_type),
                           "attendance_by_day" = get_attendance_by_days(query$year, query$team),
                           "attendance_range" = get_attendance_home_away(query$year, query$team, query$start_date, query$end_date),
                           "compare_years" = compare_team_attendance_two_years(query$team, query$year1, query$year2),
                           stop("Invalid action specified.")
          )
        }, error = function(e) {
          result <- list(status = "error", message = e$message)
        })
        
        # Directly render the JSON output without custom messaging
        output$json_output <- renderText({
          toJSON(result, pretty = TRUE)
        })
      }
    }
  })
  
  # UI-related Observers
  observeEvent(input$toggleInputs, {
    toggle("inputFields")
    output$attendance_plot <- renderPlot(NULL)  # Clear any existing plot
  })
  
  observeEvent(input$togglePlotInputs, {
    toggle("plotFields")
    output$attendance_plot <- renderPlot(NULL)  # Clear any existing plot
  })
  
  observeEvent(input$toggleTotalAttendanceInputs, {
    toggle("totalAttendanceFields")
  })
  
  observeEvent(input$toggleComparisonPlotInputs, {
    toggle("comparisonPlotFields")
    output$attendance_plot <- renderPlot(NULL)  # Clear any existing plot
  })
  
  observeEvent(input$toggleDaysOfWeekInputs, {
    toggle("daysOfWeekFields")
    output$attendance_plot <- renderPlot(NULL)  # Clear any existing plot
  })
  
  observeEvent(input$toggleAvgAttendanceInputs, {
    toggle("avgAttendanceFields")
  })
  
  observeEvent(input$toggleAttendanceRangeInputs, {
    toggle("attendanceRangeFields")
  })
  
  observeEvent(input$toggleYearComparisonInputs, {
    toggle("yearComparisonFields")
  })
  
  # UI Handlers for Forms
  observeEvent(input$submit, {
    req(input$year, input$month, input$day, input$team)
    
    final_data <- tryCatch({
      get_attendance_data(input$year, input$month, input$day, input$team)
    }, error = function(e) {
      output$json_output <- renderText({
        toJSON(list(status = "error", message = e$message), pretty = TRUE)
      })
      return(NULL)
    })
    
    if (!is.null(final_data)) {
      output$json_output <- renderText({
        toJSON(final_data, pretty = TRUE)
      })
      hide("inputFields")
    }
  })
  
  # Plot event
  observeEvent(input$generatePlot, {
    req(input$plot_year, input$plot_month, input$plot_day)
    
    tryCatch({
      result <- get_attendance_data_all_teams(input$plot_year, input$plot_month, input$plot_day)
      
      output$attendance_plot <- renderPlot({
        result$plot
      })
      
      output$json_output <- renderText({
        result$json
      })
    }, error = function(e) {
      output$attendance_plot <- renderPlot(NULL)
      showNotification("Failed to fetch data. Please check your inputs.", type = "error")
    })
  })
  
  # Total attendance event
  observeEvent(input$getTotalAttendance, {
    req(input$total_year, input$total_team, input$num_games)
    
    total_attendance <- get_total_attendance_through_games(
      year = input$total_year, 
      team = input$total_team, 
      num_games = input$num_games
    )
    
    result <- list(
      Year = input$total_year,
      Team = input$total_team,
      Number_of_Games = input$num_games,
      Total_Attendance = total_attendance
    )
    
    result_json <- toJSON(result, pretty = TRUE)
    
    output$json_output <- renderText({
      result_json
    })
  })
  
  # Comparison plot event
  observeEvent(input$generateComparisonPlot, {
    req(input$comparison_year, input$comparison_team1, input$comparison_team2, input$comparison_team3, input$comparison_team4, input$comparison_team5)
    
    tryCatch({
      teams <- c(input$comparison_team1, input$comparison_team2, input$comparison_team3, input$comparison_team4, input$comparison_team5)
      
      plot <- get_season_attendance_plot(input$comparison_year, teams)
      
      output$attendance_plot <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$attendance_plot <- renderPlot(NULL)
      showNotification("Failed to fetch data or generate plot.", type = "error")
    })
  })
  
  # Average attendance event
  observeEvent(input$getAvgAttendance, {
    req(input$avg_year, input$avg_team, input$avg_num_games, input$game_type)
    
    tryCatch({
      avg_attendance_data <- get_avg_attendance_through_games(
        year = input$avg_year,
        team = input$avg_team,
        num_games = input$avg_num_games,
        game_type = input$game_type
      )
      
      output$json_output <- renderText({
        toJSON(avg_attendance_data, pretty = TRUE)
      })
    }, error = function(e) {
      output$json_output <- renderText({
        paste("Failed to fetch average attendance data:", e$message)
      })
      showNotification("Failed to fetch average attendance data. Please check your inputs.", type = "error")
    })
  })
  
  # Days of the week attendance event
  observeEvent(input$getDaysAttendance, {
    req(input$days_year, input$days_team)
    
    tryCatch({
      attendance_by_day <- get_attendance_by_days(
        year = input$days_year,
        team = input$days_team
      )
      
      output$json_output <- renderText({
        toJSON(attendance_by_day, pretty = TRUE)
      })
    }, error = function(e) {
      output$json_output <- renderText({
        paste("Failed to fetch attendance data by day of week:", e$message)
      })
      showNotification("Failed to fetch data. Please check your inputs.", type = "error")
    })
  })
  
  # Range attendance event
  observeEvent(input$getAttendanceRange, {
    req(input$range_year, input$range_team, input$start_date, input$end_date)
    
    tryCatch({
      start_date_formatted <- format(as.Date(input$start_date), "%Y-%m-%d")
      end_date_formatted <- format(as.Date(input$end_date), "%Y-%m-%d")
      
      attendance_data <- get_attendance_home_away(
        year = input$range_year,
        team = input$range_team,
        start_date = start_date_formatted,
        end_date = end_date_formatted
      )
      
      output$json_output <- renderText({
        toJSON(attendance_data, pretty = TRUE)
      })
    }, error = function(e) {
      output$json_output <- renderText({
        paste("Failed to fetch attendance data:", e$message)
      })
      showNotification("Failed to fetch data. Please check your inputs.", type = "error")
    })
  })
  
  # Compare years event
  observeEvent(input$compareYears, {
    req(input$comparison_team, input$year1, input$year2)
    
    tryCatch({
      plot <- compare_team_attendance_two_years(input$comparison_team, input$year1, input$year2)
      
      output$attendance_plot <- renderPlot({
        plot
      })
    }, error = function(e) {
      output$attendance_plot <- renderPlot(NULL)
      showNotification("Failed to generate year comparison plot. Please check your inputs.", type = "error")
    })
  })
}



# 6. Run the Shiny app
shinyApp(ui, server)
