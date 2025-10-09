library(shiny)
library(dplyr)
library(lubridate)
library(readr)
library(DT)
library(plotly)
library(leaflet)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(forecast)

source("utils.R", local = TRUE)
source("festival_utils.R", local = TRUE)

server <- function(input, output, session) {
  sales_data <- reactiveVal(load_sales_data())

  observeEvent(input$file, {
    req(input$file)
    df <- readr::read_csv(input$file$datapath, show_col_types = FALSE)
    df <- normalize_sales_columns(df)
    sales_data(df)
  })

  observe({
    df <- sales_data()
    if (is.null(df) || nrow(df) == 0) return()
    
    # Ensure dates are properly formatted
    min_date <- min(df$date, na.rm = TRUE)
    max_date <- max(df$date, na.rm = TRUE)
    
    if (!is.na(min_date) && !is.na(max_date)) {
      updateDateRangeInput(session, "date_range",
                           start = as.Date(min_date),
                           end = as.Date(max_date))
    }
    
    updateSelectInput(session, "location_filter",
                      choices = sort(unique(df$location)),
                      selected = unique(df$location))
  })

  observeEvent(input$reset_filters, {
    df <- sales_data()
    if (is.null(df) || nrow(df) == 0) return()
    
    min_date <- min(df$date, na.rm = TRUE)
    max_date <- max(df$date, na.rm = TRUE)
    
    if (!is.na(min_date) && !is.na(max_date)) {
      updateDateRangeInput(session, "date_range",
                           start = as.Date(min_date),
                           end = as.Date(max_date))
    }
    
    updateSelectInput(session, "location_filter",
                      choices = sort(unique(df$location)),
                      selected = unique(df$location))
  })

  filtered <- reactive({
    df <- sales_data()
    if (is.null(df) || nrow(df) == 0) return(df)
    df <- df %>% filter(date >= input$date_range[1], date <= input$date_range[2])
    if (!is.null(input$location_filter) && length(input$location_filter) > 0) {
      df <- df %>% filter(location %in% input$location_filter)
    }
    df
  })

  output$total_sales <- renderValueBox({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      valueBox("$0", "Total Sales", icon = icon("money-bill-wave"), color = "green")
    } else {
      valueBox(scales::dollar(sum(df$sales, na.rm = TRUE)), "Total Sales", icon = icon("money-bill-wave"), color = "green")
    }
  })

  output$num_orders <- renderValueBox({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      valueBox("0", "Num Records", icon = icon("hashtag"), color = "aqua")
    } else {
      valueBox(format(nrow(df), big.mark = ","), "Num Records", icon = icon("hashtag"), color = "aqua")
    }
  })

  output$num_locations <- renderValueBox({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      valueBox("0", "Locations", icon = icon("location-dot"), color = "yellow")
    } else {
      valueBox(length(unique(df$location)), "Locations", icon = icon("location-dot"), color = "yellow")
    }
  })

  output$avg_ticket <- renderValueBox({
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      valueBox("$0", "Avg Ticket", icon = icon("receipt"), color = "purple")
    } else {
      valueBox(scales::dollar(mean(df$sales, na.rm = TRUE)), "Avg Ticket", icon = icon("receipt"), color = "purple")
    }
  })

  monthly <- reactive({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(df)
    df %>% mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>% summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")
  })

  output$monthly_plot <- renderPlotly({
    df <- monthly(); if (is.null(df) || nrow(df) == 0) return(NULL)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = month, y = sales)) +
      ggplot2::geom_col(fill = "#2C7FB8") +
      ggplot2::labs(x = "Month", y = "Sales", title = "Monthly Sales") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })

  output$monthly_table <- renderDT({
    df <- monthly(); if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    df %>% arrange(desc(month)) %>%
      mutate(month = format(month, "%Y-%m"), sales = round(sales, 2)) %>%
      datatable(options = list(pageLength = 12), rownames = FALSE, selection = 'single')
  })

  # Store selected month for detailed view
  selected_month <- reactiveVal(NULL)

  observeEvent(input$monthly_table_rows_selected, {
    df <- monthly()
    if (!is.null(input$monthly_table_rows_selected) && nrow(df) > 0) {
      selected_row <- df[input$monthly_table_rows_selected, ]
      selected_month(selected_row$month)
    }
  })

  # Detailed monthly breakdown
  monthly_detail <- reactive({
    if (is.null(selected_month())) return(NULL)
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    df %>% 
      filter(floor_date(date, "month") == selected_month()) %>%
      group_by(location) %>%
      summarise(
        daily_avg = mean(sales, na.rm = TRUE),
        total_sales = sum(sales, na.rm = TRUE),
        num_days = n(),
        .groups = "drop"
      ) %>%
      arrange(desc(total_sales))
  })

  output$monthly_detail_plot <- renderPlotly({
    df <- monthly_detail()
    if (is.null(df) || nrow(df) == 0) {
      # Return empty plot with message
      empty_df <- data.frame(x = 1, y = 1, text = "Select a month to see details")
      p <- ggplot2::ggplot(empty_df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_text(ggplot2::aes(label = text), size = 4, color = "#666") +
        ggplot2::theme_void() +
        ggplot2::theme(panel.background = ggplot2::element_blank())
      return(plotly::ggplotly(p))
    }
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(location, total_sales), y = total_sales)) +
      ggplot2::geom_col(fill = "#2C7FB8") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Location", 
        y = "Total Sales", 
        title = paste("Sales by Location for", format(selected_month(), "%B %Y"))
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })

  output$monthly_detail_table <- renderDT({
    df <- monthly_detail()
    if (is.null(df) || nrow(df) == 0) {
      return(datatable(data.frame(Message = "Select a month to see details"), 
                      options = list(dom = 't'), rownames = FALSE))
    }
    
    df %>% 
      mutate(
        daily_avg = round(daily_avg, 2),
        total_sales = round(total_sales, 2)
      ) %>%
      datatable(
        options = list(pageLength = 10), 
        rownames = FALSE,
        colnames = c("Location", "Daily Average", "Total Sales", "Number of Days")
      )
  })

  # Show trend analysis modal when analyze button clicked
  observeEvent(input$analyze_trends, {
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      showModal(modalDialog(
        title = "Analyze Trends",
        "No data available for analysis. Please upload a CSV or adjust filters.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    # Prepare monthly trend data
    monthly_df <- df %>%
      mutate(month = floor_date(date, "month")) %>%
      group_by(month) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")

    # Save monthly_df into a reactiveVal so the modal's output can access it
    trend_modal_data <- reactiveVal(monthly_df)

    output$trend_modal_plot <- renderPlotly({
      md <- trend_modal_data()
      if (is.null(md) || nrow(md) == 0) return(NULL)
      p <- ggplot2::ggplot(md, ggplot2::aes(x = month, y = sales)) +
        ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
        ggplot2::geom_point(color = "#2C7FB8") +
        ggplot2::labs(title = "Monthly Sales Trend", x = "Month", y = "Sales") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = c("x", "y"))
    })

    showModal(modalDialog(
      title = "Sales Trend Analysis",
      size = "l",
      plotlyOutput("trend_modal_plot", height = "450px"),
      footer = tagList(
        modalButton("Close"),
        actionButton("open_forecast_tab", "Open Forecast Tab", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  # Navigate to forecast tab when user clicks inside modal
  observeEvent(input$open_forecast_tab, {
    removeModal()
    updateTabItems(session, "tabs", "forecast")
  })

  # Location Insights modal
  observeEvent(input$location_insights, {
    df <- filtered()
    if (is.null(df) || nrow(df) == 0) {
      showModal(modalDialog(
        title = "Location Insights",
        "No data available. Upload a CSV or adjust filters.",
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      return()
    }

    loc_summary <- df %>% group_by(location) %>% summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")

    output$location_modal_plot <- renderPlotly({
      p <- ggplot2::ggplot(loc_summary, ggplot2::aes(x = reorder(location, sales), y = sales)) +
        ggplot2::geom_col(fill = "#2C7FB8") +
        ggplot2::coord_flip() +
        ggplot2::labs(title = "Sales by Location", x = "Location", y = "Sales") +
        ggplot2::theme_minimal()
      plotly::ggplotly(p, tooltip = c("x", "y"))
    })

    showModal(modalDialog(
      title = "Location Insights",
      size = "l",
      plotlyOutput("location_modal_plot", height = "450px"),
      footer = tagList(
        modalButton("Close"),
        actionButton("open_locations_tab", "Open Locations Tab", class = "btn-primary")
      ),
      easyClose = TRUE
    ))
  })

  observeEvent(input$open_locations_tab, {
    removeModal()
    updateTabItems(session, "tabs", "locations")
  })

  # Predict Future: go to Forecast tab
  observeEvent(input$predict_future, {
    updateTabItems(session, "tabs", "forecast")
  })

  locations_summary <- reactive({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(df)
    df %>% group_by(location) %>%
      summarise(sales = sum(sales, na.rm = TRUE),
                lat = suppressWarnings(mean(lat, na.rm = TRUE)),
                lon = suppressWarnings(mean(lon, na.rm = TRUE)),
                .groups = "drop")
  })

  output$location_viz <- renderUI({
    df <- locations_summary(); if (is.null(df) || nrow(df) == 0) return(NULL)
    has_geo <- all(c("lat", "lon") %in% names(df)) && any(is.finite(df$lat) & is.finite(df$lon))
    if (has_geo) {
      leafletOutput("location_map", height = 450)
    } else {
      plotlyOutput("location_bar", height = 450)
    }
  })

  output$location_map <- renderLeaflet({
    df <- locations_summary(); if (is.null(df) || nrow(df) == 0) return(NULL)
    df <- df %>% filter(is.finite(lat), is.finite(lon))
    if (nrow(df) == 0) return(NULL)
    pal <- colorNumeric("Blues", df$sales)
    leaflet(df) %>% addProviderTiles(providers$CartoDB.Positron) %>%
      addCircleMarkers(~lon, ~lat, radius = ~pmax(4, 12 * sales / max(sales)),
                       color = ~pal(sales), stroke = FALSE, fillOpacity = 0.7,
                       popup = ~paste0("<b>", location, "</b><br>Sales: ", scales::dollar(round(sales,2)))) %>%
      addLegend("bottomright", pal = pal, values = ~sales, title = "Sales")
  })

  output$location_bar <- renderPlotly({
    df <- locations_summary(); if (is.null(df) || nrow(df) == 0) return(NULL)
    p <- ggplot2::ggplot(df, ggplot2::aes(x = reorder(location, sales), y = sales)) +
      ggplot2::geom_col(fill = "#2C7FB8") +
      ggplot2::coord_flip() +
      ggplot2::labs(x = "Location", y = "Sales", title = "Sales by Location") +
      ggplot2::theme_minimal()
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })

  output$location_table <- renderDT({
    df <- locations_summary(); if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    df %>% arrange(desc(sales)) %>% mutate(sales = round(sales, 2)) %>%
      datatable(options = list(pageLength = 10), rownames = FALSE)
  })

  output$forecast_plot <- renderPlotly({
    df <- filtered(); if (is.null(df) || nrow(df) < 6) return(NULL)
    h <- input$forecast_h
    enable_festivals <- input$enable_festivals
    festival_multiplier <- input$festival_multiplier
    focus_festival <- input$focus_festival
    
    # Generate festival-enhanced forecast
    tryCatch({
      festival_forecast <- generate_festival_forecast(df, h)
      
      # Apply UI controls
      if (!enable_festivals) {
        festival_forecast$festival_impact <- 1.0
      } else {
        festival_forecast$festival_impact <- festival_forecast$festival_impact * festival_multiplier
        
        # Apply festival focus
        if (focus_festival != "All Festivals") {
          festival_forecast$festival_impact <- ifelse(
            grepl(focus_festival, festival_forecast$festival_name) | 
            (focus_festival == "Diwali Season (Q4)" & festival_forecast$month %in% c(10, 11, 12)) |
            (focus_festival == "Ganesh Chaturthi (Q3)" & festival_forecast$month %in% c(7, 8, 9)) |
            (focus_festival == "Holi Season (Q1)" & festival_forecast$month %in% c(1, 2, 3)) |
            (focus_festival == "Monsoon Festivals (Q2)" & festival_forecast$month %in% c(4, 5, 6)),
            festival_forecast$festival_impact,
            1.0
          )
        }
      }
      
      # Recalculate forecast sales
      festival_forecast$forecast_sales <- festival_forecast$base_forecast * festival_forecast$festival_impact
      
      # Aggregate by month for plotting
      monthly_forecast <- festival_forecast %>%
        group_by(month) %>%
        summarise(
          forecast_sales = sum(forecast_sales, na.rm = TRUE),
          festival_name = paste(unique(festival_name[festival_name != ""]), collapse = ", "),
          max_festival_impact = max(festival_impact, na.rm = TRUE),
          .groups = "drop"
        )
      
      # Get historical data
      historical_monthly <- df %>% 
        mutate(month = floor_date(date, "month")) %>%
        group_by(month) %>% 
        summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")
      
      # Create plot data
      plot_df <- data.frame(
        date = c(historical_monthly$month, monthly_forecast$month),
        sales = c(historical_monthly$sales, monthly_forecast$forecast_sales),
        type = c(rep("Historical", nrow(historical_monthly)), rep("Festival-Enhanced Forecast", nrow(monthly_forecast))),
        festival = c(rep("", nrow(historical_monthly)), monthly_forecast$festival_name),
        impact = c(rep(1, nrow(historical_monthly)), monthly_forecast$max_festival_impact)
      )
      
      # Create enhanced plot with festival annotations
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = sales, color = type)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::geom_point(data = plot_df[plot_df$festival != "", ], 
                           ggplot2::aes(size = impact), alpha = 0.7) +
        ggplot2::scale_color_manual(values = c("Historical" = "#2C7FB8", "Festival-Enhanced Forecast" = "#E31A1C")) +
        ggplot2::scale_size_continuous(range = c(3, 8), guide = "none") +
        ggplot2::labs(
          x = "Month", 
          y = "Sales", 
          title = "Monthly Sales Forecast with Indian Festival Impact",
          color = "Type"
        ) +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
      
      # Add festival annotations
      festival_points <- plot_df[plot_df$festival != "", ]
      if (nrow(festival_points) > 0) {
        p <- p + ggplot2::geom_text(
          data = festival_points,
          ggplot2::aes(label = festival, y = sales + max(plot_df$sales, na.rm = TRUE) * 0.05),
          angle = 45, hjust = 0, size = 3, color = "#666666"
        )
      }
      
      plotly::ggplotly(p, tooltip = c("x", "y", "type", "festival", "impact"))
      
    }, error = function(e) {
      # Fallback to simple forecast
      monthly_df <- df %>% 
        mutate(month = floor_date(date, "month")) %>%
        group_by(month) %>% 
        summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")
      
      lm_model <- lm(sales ~ as.numeric(month), data = monthly_df)
      last_month <- max(monthly_df$month)
      fc_dates <- seq(from = last_month + months(1), by = "month", length.out = h)
      fc_values <- predict(lm_model, newdata = data.frame(month = fc_dates))
      
      plot_df <- data.frame(
        date = c(monthly_df$month, fc_dates),
        sales = c(monthly_df$sales, fc_values),
        type = c(rep("Historical", nrow(monthly_df)), rep("Simple Forecast", h))
      )
      
      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = date, y = sales, color = type)) +
        ggplot2::geom_line(linewidth = 1) +
        ggplot2::scale_color_manual(values = c("Historical" = "#2C7FB8", "Simple Forecast" = "#E31A1C")) +
        ggplot2::labs(x = "Month", y = "Sales", title = "Monthly Sales Forecast (Fallback)", 
                     color = "Type") +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "bottom")
      
      plotly::ggplotly(p, tooltip = c("x", "y", "type"))
    })
  })

  # Festival Analysis Tab Outputs
  output$festival_calendar_table <- renderDT({
    festivals <- get_indian_festivals()
    festivals %>%
      select(festival_name, month, quarter, impact_multiplier, duration_days, category) %>%
      mutate(
        impact_multiplier = round(impact_multiplier, 2),
        month_name = month.name[month]
      ) %>%
      select(festival_name, month_name, quarter, impact_multiplier, duration_days, category) %>%
      arrange(quarter, month) %>%
      datatable(
        options = list(pageLength = 15), 
        rownames = FALSE,
        colnames = c("Festival", "Month", "Quarter", "Impact Multiplier", "Duration (Days)", "Category")
      )
  })

  output$festival_quarter_plot <- renderPlotly({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Analyze festival performance
    festival_performance <- analyze_festival_performance(df)
    
    # Aggregate by quarter
    quarter_summary <- festival_performance %>%
      group_by(quarter) %>%
      summarise(
        avg_sales = mean(total_sales, na.rm = TRUE),
        avg_impact = mean(avg_festival_impact, na.rm = TRUE),
        festivals = paste(unique(festivals), collapse = ", "),
        .groups = "drop"
      )
    
    p <- ggplot2::ggplot(quarter_summary, ggplot2::aes(x = as.factor(quarter), y = avg_sales, fill = avg_impact)) +
      ggplot2::geom_col() +
      ggplot2::geom_text(ggplot2::aes(label = paste("Impact:", round(avg_impact, 2))), 
                        vjust = -0.5, size = 3) +
      ggplot2::scale_fill_gradient(low = "#2C7FB8", high = "#E31A1C", name = "Festival\nImpact") +
      ggplot2::labs(
        x = "Quarter", 
        y = "Average Sales", 
        title = "Average Sales by Quarter with Festival Impact"
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(p, tooltip = c("x", "y", "fill"))
  })

  output$festival_location_plot <- renderPlotly({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Analyze festival performance by location
    festival_performance <- analyze_festival_performance(df)
    
    # Filter for Indian locations only
    indian_cities <- c("Mumbai", "Delhi", "Bangalore", "Chennai", "Kolkata", 
                      "Madurai", "Trichy", "Coimbatore", "Villupuram", "Vellore", 
                      "Kanniyakumari", "Kerala")
    
    indian_performance <- festival_performance %>%
      filter(location %in% indian_cities) %>%
      group_by(location) %>%
      summarise(
        avg_sales = mean(total_sales, na.rm = TRUE),
        avg_festival_impact = mean(avg_festival_impact, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_festival_impact))
    
    if (nrow(indian_performance) == 0) {
      # Fallback for non-Indian data
      indian_performance <- festival_performance %>%
        group_by(location) %>%
        summarise(
          avg_sales = mean(total_sales, na.rm = TRUE),
          avg_festival_impact = mean(avg_festival_impact, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(avg_festival_impact))
    }
    
    p <- ggplot2::ggplot(indian_performance, ggplot2::aes(x = reorder(location, avg_festival_impact), y = avg_festival_impact)) +
      ggplot2::geom_col(fill = "#FF6B35") +
      ggplot2::coord_flip() +
      ggplot2::labs(
        x = "Location", 
        y = "Average Festival Impact", 
        title = "Festival Impact by Location"
      ) +
      ggplot2::theme_minimal()
    
    plotly::ggplotly(p, tooltip = c("x", "y"))
  })

  output$festival_impact_table <- renderDT({
    festivals <- get_indian_festivals()
    impact_summary <- festivals %>%
      group_by(category) %>%
      summarise(
        count = n(),
        avg_impact = round(mean(impact_multiplier), 2),
        max_impact = round(max(impact_multiplier), 2),
        .groups = "drop"
      ) %>%
      arrange(desc(avg_impact))
    
    impact_summary %>%
      datatable(
        options = list(pageLength = 10), 
        rownames = FALSE,
        colnames = c("Festival Category", "Number of Festivals", "Average Impact", "Maximum Impact")
      )
  })

  output$festival_trend_plot <- renderPlotly({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(NULL)
    
    # Create monthly trend with festival indicators
    monthly_data <- df %>%
      mutate(
        month = floor_date(date, "month"),
        month_num = as.numeric(format(date, "%m")),
        quarter = ceiling(month_num / 3)
      ) %>%
      group_by(month, month_num, quarter) %>%
      summarise(sales = sum(sales, na.rm = TRUE), .groups = "drop")
    
    # Add festival impact
    monthly_data$festival_impact <- sapply(monthly_data$month_num, function(m) {
      festivals <- get_festivals_for_month(m)
      if (nrow(festivals) > 0) {
        max(festivals$impact_multiplier)
      } else {
        1.0
      }
    })
    
    p <- ggplot2::ggplot(monthly_data, ggplot2::aes(x = month, y = sales)) +
      ggplot2::geom_line(color = "#2C7FB8", linewidth = 1) +
      ggplot2::geom_point(ggplot2::aes(size = festival_impact, color = as.factor(quarter)), alpha = 0.7) +
      ggplot2::scale_color_manual(values = c("1" = "#FF6B35", "2" = "#4ECDC4", "3" = "#45B7D1", "4" = "#96CEB4")) +
      ggplot2::scale_size_continuous(range = c(2, 8), name = "Festival\nImpact") +
      ggplot2::labs(
        x = "Month", 
        y = "Sales", 
        title = "Monthly Sales Trend with Festival Impact Indicators",
        color = "Quarter"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "bottom")
    
    plotly::ggplotly(p, tooltip = c("x", "y", "size", "color"))
  })

  output$data_table <- renderDT({
    df <- filtered(); if (is.null(df) || nrow(df) == 0) return(datatable(data.frame()))
    df %>% arrange(desc(date)) %>%
      mutate(date = as.character(date), sales = round(sales, 2)) %>%
      datatable(options = list(pageLength = 25), rownames = FALSE)
  })
}

shinyServer(server)
