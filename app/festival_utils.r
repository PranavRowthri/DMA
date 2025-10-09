# Indian Festival Calendar and Impact Analysis
# This file contains functions to handle Indian festivals and their impact on sales

library(dplyr)
library(lubridate)

# Major Indian Festivals by Quarter/Month
get_indian_festivals <- function() {
  festivals <- data.frame(
    festival_name = c(
      # Q1 (Jan-Mar) - Winter Festivals
      "Makar Sankranti", "Pongal", "Republic Day", "Vasant Panchami", "Maha Shivaratri", "Holi",
      
      # Q2 (Apr-Jun) - Spring/Summer Festivals  
      "Ram Navami", "Hanuman Jayanti", "Akshaya Tritiya", "Buddha Purnima", "Eid al-Fitr",
      
      # Q3 (Jul-Sep) - Monsoon Festivals
      "Rath Yatra", "Guru Purnima", "Raksha Bandhan", "Krishna Janmashtami", "Ganesh Chaturthi", "Onam", "Dussehra",
      
      # Q4 (Oct-Dec) - Autumn/Winter Festivals
      "Navratri", "Dussehra", "Karva Chauth", "Diwali", "Bhai Dooj", "Chhath Puja", "Guru Nanak Jayanti", "Christmas"
    ),
    month = c(
      # Q1
      1, 1, 1, 1, 2, 3,
      # Q2  
      4, 4, 5, 5, 5,
      # Q3
      7, 7, 8, 8, 9, 9, 10,
      # Q4
      10, 10, 10, 10, 10, 11, 11, 12
    ),
    quarter = c(
      # Q1
      1, 1, 1, 1, 1, 1,
      # Q2
      2, 2, 2, 2, 2,
      # Q3
      3, 3, 3, 3, 3, 3, 3,
      # Q4
      4, 4, 4, 4, 4, 4, 4, 4
    ),
    impact_multiplier = c(
      # Q1 - Moderate impact
      1.3, 1.4, 1.1, 1.2, 1.3, 1.5,
      # Q2 - Lower impact
      1.2, 1.2, 1.3, 1.2, 1.4,
      # Q3 - Higher impact
      1.4, 1.2, 1.3, 1.4, 1.6, 1.5, 1.4,
      # Q4 - Highest impact (festival season)
      1.4, 1.5, 1.3, 2.0, 1.4, 1.6, 1.2, 1.3
    ),
    duration_days = c(
      # Q1
      2, 3, 1, 1, 1, 2,
      # Q2
      1, 1, 1, 1, 3,
      # Q3
      2, 1, 1, 1, 10, 3, 2,
      # Q4
      9, 2, 1, 5, 1, 4, 1, 2
    ),
    category = c(
      # Q1
      "Harvest", "Harvest", "National", "Religious", "Religious", "Religious",
      # Q2
      "Religious", "Religious", "Religious", "Religious", "Religious",
      # Q3
      "Religious", "Religious", "Religious", "Religious", "Religious", "Religious", "Religious",
      # Q4
      "Religious", "Religious", "Religious", "Religious", "Religious", "Religious", "Religious", "Religious"
    ),
    stringsAsFactors = FALSE
  )
  
  return(festivals)
}

# Get festivals for a specific month
get_festivals_for_month <- function(month) {
  festivals <- get_indian_festivals()
  return(festivals[festivals$month == month, ])
}

# Get festivals for a specific quarter
get_festivals_for_quarter <- function(quarter) {
  festivals <- get_indian_festivals()
  return(festivals[festivals$quarter == quarter, ])
}

# Calculate festival impact for a given date
calculate_festival_impact <- function(date, location = NULL) {
  # Ensure date is properly formatted
  date_obj <- as.Date(date)
  month_val <- as.numeric(format(date_obj, "%m"))
  day_val <- as.numeric(format(date_obj, "%d"))
  
  festivals <- get_indian_festivals()
  month_festivals <- festivals[festivals$month == month_val, ]
  
  # Base impact
  impact <- 1.0
  
  # Check if date falls within festival period
  if (nrow(month_festivals) > 0) {
    for (i in seq_len(nrow(month_festivals))) {
      festival <- month_festivals[i, ]
      
      # For simplicity, assume festivals occur around mid-month
      # In a real implementation, you'd have exact festival dates
      festival_start <- 10
      festival_end <- festival_start + festival$duration_days - 1
      
      if (!is.na(day_val) && !is.na(festival_start) && !is.na(festival_end) && 
          day_val >= festival_start && day_val <= festival_end) {
        impact <- max(impact, festival$impact_multiplier)
      }
    }
  }
  
  # Regional adjustments for Indian cities
  if (!is.null(location)) {
    indian_cities <- c("Mumbai", "Delhi", "Bangalore", "Chennai", "Kolkata", 
                      "Madurai", "Trichy", "Coimbatore", "Villupuram", "Vellore", 
                      "Kanniyakumari", "Kerala")
    
    if (location %in% indian_cities) {
      # Indian cities have higher festival impact
      impact <- impact * 1.1
    }
  }
  
  return(impact)
}

# Generate festival-adjusted sales forecast
generate_festival_forecast <- function(historical_data, forecast_months = 12) {
  # Get historical monthly data
  monthly_data <- historical_data %>%
    mutate(month = floor_date(date, "month")) %>%
    group_by(month, location) %>%
    summarise(
      sales = sum(sales, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Create forecast data frame
  last_date <- max(monthly_data$month)
  forecast_dates <- seq(from = last_date + months(1), by = "month", length.out = forecast_months)
  
  # Get unique locations
  locations <- unique(monthly_data$location)
  
  # Create forecast grid
  forecast_grid <- expand.grid(
    month = forecast_dates,
    location = locations,
    stringsAsFactors = FALSE
  )
  
  # Calculate base forecast using trend
  forecast_grid$base_forecast <- 0
  
  for (loc in locations) {
    loc_data <- monthly_data[monthly_data$location == loc, ]
    if (nrow(loc_data) >= 3) {
      # Simple linear trend
      lm_model <- lm(sales ~ as.numeric(month), data = loc_data)
      forecast_values <- predict(lm_model, newdata = data.frame(month = forecast_dates))
      forecast_grid$base_forecast[forecast_grid$location == loc] <- forecast_values
    } else {
      # Fallback to average
      avg_sales <- mean(loc_data$sales, na.rm = TRUE)
      forecast_grid$base_forecast[forecast_grid$location == loc] <- avg_sales
    }
  }
  
  # Apply festival adjustments
  forecast_grid$festival_impact <- mapply(
    calculate_festival_impact,
    forecast_grid$month,
    forecast_grid$location
  )
  
  # Calculate final forecast
  forecast_grid$forecast_sales <- forecast_grid$base_forecast * forecast_grid$festival_impact
  
    # Add festival information
    forecast_grid$festival_name <- ""
    forecast_grid$festival_category <- ""
    
    for (i in seq_len(nrow(forecast_grid))) {
      month_val <- as.numeric(format(as.Date(forecast_grid$month[i]), "%m"))
      festivals <- get_festivals_for_month(month_val)
    
    if (nrow(festivals) > 0) {
      # Get the festival with highest impact
      max_impact_festival <- festivals[which.max(festivals$impact_multiplier), ]
      forecast_grid$festival_name[i] <- max_impact_festival$festival_name
      forecast_grid$festival_category[i] <- max_impact_festival$category
    }
  }
  
  return(forecast_grid)
}

# Analyze historical festival performance
analyze_festival_performance <- function(historical_data) {
  # Add festival information to historical data
  historical_data$festival_impact <- mapply(
    calculate_festival_impact,
    historical_data$date,
    historical_data$location
  )
  
  historical_data$month <- as.numeric(format(as.Date(historical_data$date), "%m"))
  historical_data$quarter <- ceiling(historical_data$month / 3)
  
  # Get festival data
  festivals <- get_indian_festivals()
  
  # Create monthly summary with festival info
  monthly_summary <- historical_data %>%
    group_by(month, quarter, location) %>%
    summarise(
      total_sales = sum(sales, na.rm = TRUE),
      avg_festival_impact = mean(festival_impact, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    left_join(
      festivals %>% 
        select(month, quarter, festival_name, impact_multiplier, category) %>%
        group_by(month, quarter) %>%
        summarise(
          festivals = paste(festival_name, collapse = ", "),
          max_impact = max(impact_multiplier),
          .groups = "drop"
        ),
      by = c("month", "quarter")
    )
  
  return(monthly_summary)
}

# Get festival calendar for a specific year
get_festival_calendar <- function(year = 2024) {
  festivals <- get_indian_festivals()
  calendar <- data.frame()
  
  for (i in seq_len(nrow(festivals))) {
    festival <- festivals[i, ]
    
    # For each month, create entries for the festival duration
    for (day in seq_len(festival$duration_days)) {
      # Approximate festival dates (in real implementation, use exact dates)
      festival_date <- as.Date(paste(year, festival$month, 10 + day - 1, sep = "-"))
      
      calendar <- rbind(calendar, data.frame(
        date = festival_date,
        festival_name = festival$festival_name,
        impact_multiplier = festival$impact_multiplier,
        category = festival$category,
        quarter = festival$quarter
      ))
    }
  }
  
  return(calendar[order(calendar$date), ])
}
