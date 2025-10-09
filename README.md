## Sales Prediction Shiny App with Indian Festival Analysis (R)

This Shiny app provides:

- Monthly sales graph and table with detailed breakdowns
- Location sales report with interactive map and table
- **Festival-Enhanced Sales Forecast** with Indian festival impact analysis
- **Indian Festival Calendar & Impact Analysis** - New dedicated tab
- Festival performance analysis by quarter and location
- Interactive controls for adjusting festival impact multipliers

### Quick start

1) Install R (4.2+) and RStudio (optional).
2) Install packages:

```r
source("scripts/install_dependencies.R")
```

3) Run the app:

```r
shiny::runApp("app")
```

The app ships with a sample dataset in `data/sample_sales.csv`. You can also upload your own CSV with columns: `date` (YYYY-MM-DD), `location` (string), `sales` (numeric), `lat` (optional), `lon` (optional).

### Project structure

```
app/
  global.R
  server.R
  ui.R
  utils.R
data/
  sample_sales.csv
scripts/
  install_dependencies.R
```

### New Features - Indian Festival Analysis

- **Festival Calendar**: Comprehensive database of major Indian festivals by quarter
- **Impact Multipliers**: Realistic sales impact factors for each festival (Diwali: 2.0x, Holi: 1.5x, etc.)
- **Enhanced Forecasting**: Forecast model now incorporates festival effects for more accurate predictions
- **Interactive Controls**: Adjust festival impact multipliers and focus on specific festival seasons
- **Festival Analysis Tab**: Dedicated analysis of how festivals affect sales patterns
- **Quarter-wise Analysis**: Visual breakdown of sales impact by festival seasons

### Festival Categories Included

- **Q1 (Jan-Mar)**: Makar Sankranti, Pongal, Holi, Republic Day
- **Q2 (Apr-Jun)**: Ram Navami, Hanuman Jayanti, Akshaya Tritiya, Buddha Purnima, Eid
- **Q3 (Jul-Sep)**: Rath Yatra, Raksha Bandhan, Krishna Janmashtami, Ganesh Chaturthi, Onam, Dussehra
- **Q4 (Oct-Dec)**: Navratri, Diwali, Bhai Dooj, Chhath Puja, Christmas

### Technical Notes

- Forecast uses festival-enhanced models with seasonal adjustments
- Festival impact is calculated based on historical patterns and cultural significance
- Enhanced sample data includes realistic festival sales patterns for Indian cities
- Mapping uses `leaflet` when latitude/longitude are available; otherwise a bar chart by location is shown



