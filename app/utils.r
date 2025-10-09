normalize_sales_columns <- function(df) {
  # Standardize column names
  names(df) <- tolower(trimws(names(df)))
  # Rename common variants
  col_map <- c(
    date = "date", order_date = "date", dt = "date",
    location = "location", store = "location", city = "location",
    sales = "sales", amount = "sales", revenue = "sales",
    latitude = "lat", lat = "lat",
    longitude = "lon", long = "lon", lon = "lon"
  )
  std_names <- names(df)
  for (nm in names(col_map)) {
    match_idx <- which(std_names == nm)
    if (length(match_idx) == 1) {
      std_names[match_idx] <- col_map[[nm]]
    }
  }
  names(df) <- std_names

  # Required columns
  stopifnot("date" %in% names(df), "location" %in% names(df), "sales" %in% names(df))

  # Coerce types
  df$date <- as.Date(df$date)
  df$location <- as.character(df$location)
  df$sales <- suppressWarnings(as.numeric(df$sales))
  if (!"lat" %in% names(df)) df$lat <- NA_real_
  if (!"lon" %in% names(df)) df$lon <- NA_real_
  df
}

load_sales_data <- function() {
  # Try multiple possible file paths
  possible_paths <- c(
    file.path("data", "sample_sales.csv"),
    "sample_sales_data.csv",
    file.path("data", "sample_sales_data.csv")
  )
  
  for (sample_path in possible_paths) {
    if (file.exists(sample_path)) {
      df <- readr::read_csv(sample_path, show_col_types = FALSE)
      df <- normalize_sales_columns(df)
      return(df)
    }
  }
  
  # Fallback: generate synthetic data with festival patterns
  set.seed(1)
  dates <- seq(as.Date("2023-01-01"), as.Date("2025-09-30"), by = "day")
  locations <- c("New York", "San Francisco", "Chicago", "Austin", "Seattle", 
                 "Mumbai", "Delhi", "Bangalore", "Chennai", "Kolkata", "Madurai", "Trichy", 
                 "Coimbatore", "Villupuram", "Vellore", "Kanniyakumari", "Kerala", "Pune", "Hyderabad", "Jaipur")
  grid <- expand.grid(date = dates, location = locations, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  
  # Base seasonal pattern
  base <- 100 + 20 * sin(2 * pi * as.numeric(format(grid$date, "%j")) / 365)
  
  # Location effect (Indian cities have higher base sales)
  indian_cities <- c("Mumbai", "Delhi", "Bangalore", "Chennai", "Kolkata", "Madurai", "Trichy", 
                    "Coimbatore", "Villupuram", "Vellore", "Kanniyakumari", "Kerala", "Pune", "Hyderabad", "Jaipur")
  loc_effect <- ifelse(grid$location %in% indian_cities, 150, 50)
  
  # Festival impact for Indian cities
  festival_impact <- 1.0
  if (any(grid$location %in% indian_cities)) {
    indian_rows <- grid$location %in% indian_cities
    month_val <- as.numeric(format(grid$date[indian_rows], "%m"))
    
    # Apply festival multipliers
    festival_multiplier <- rep(1.0, sum(indian_rows))
    
    # Q1 festivals (Jan-Mar)
    festival_multiplier[month_val == 1] <- 1.3  # Makar Sankranti, Pongal
    festival_multiplier[month_val == 2] <- 1.3  # Vasant Panchami, Maha Shivaratri
    festival_multiplier[month_val == 3] <- 1.5  # Holi
    
    # Q2 festivals (Apr-Jun)
    festival_multiplier[month_val == 4] <- 1.2  # Ram Navami, Hanuman Jayanti
    festival_multiplier[month_val == 5] <- 1.3  # Akshaya Tritiya, Buddha Purnima, Eid
    
    # Q3 festivals (Jul-Sep)
    festival_multiplier[month_val == 7] <- 1.4  # Rath Yatra, Guru Purnima
    festival_multiplier[month_val == 8] <- 1.4  # Raksha Bandhan, Krishna Janmashtami
    festival_multiplier[month_val == 9] <- 1.6  # Ganesh Chaturthi, Onam
    
    # Q4 festivals (Oct-Dec) - Peak season
    festival_multiplier[month_val == 10] <- 1.8  # Navratri, Dussehra, Karva Chauth
    festival_multiplier[month_val == 11] <- 2.0  # Diwali, Bhai Dooj, Chhath Puja
    festival_multiplier[month_val == 12] <- 1.4  # Christmas
    
    festival_impact[indian_rows] <- festival_multiplier
  }
  
  # Add some randomness
  noise <- rnorm(nrow(grid), 0, 25)
  
  # Calculate final sales with festival impact
  grid$sales <- pmax(0, (base + loc_effect) * festival_impact + noise)
  # Fake coordinates (approximate city centers)
  coords <- data.frame(
    location = locations,
    lat = c(40.7128, 37.7749, 41.8781, 30.2672, 47.6062,
            19.0760, 28.7041, 12.9716, 13.0827, 22.5726, 9.9252, 10.7905,
            11.0168, 11.9404, 12.9202, 8.0883, 10.8505, 18.5204, 17.3850, 26.9124),
    lon = c(-74.0060, -122.4194, -87.6298, -97.7431, -122.3321,
            72.8777, 77.1025, 77.5946, 80.2707, 88.3639, 78.1198, 78.7047,
            76.9558, 79.4978, 79.1500, 77.5385, 76.2711, 73.8567, 78.4867, 75.7873)
  )
  grid <- dplyr::left_join(grid, coords, by = "location")
  grid
}



