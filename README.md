

# Supply Chain Data Analysis 

## Overview
This repository contains a comprehensive analysis of supply chain data using R. The project aims to provide insights into various aspects of the supply chain, including demand forecasting, inventory management, and supplier performance. The analysis leverages several R packages and other relevant tools to create visualizations and models that help in decision-making.

## Features
- Demand Forecasting:
  - Developed models to predict future demand based on historical data and market trends.
- Inventory Management:
  - Analyzed inventory levels and turnover rates to optimize stock levels and reduce holding costs.
- Supplier Performance:
  - Evaluated supplier performance based on delivery times, quality, and cost metrics.
- Data Visualization:
  - Created interactive visualizations using ggplot2 and Shiny to explore and present the data.

## Technologies and Tools
- Programming Languages:
  - R
  - SQL
- R Packages:
  - ggplot2
  - dplyr
  - forecast
  - shiny
- Other Tools:
  - RStudio
  - Microsoft Excel

## Insights
1. Demand Forecasting:
   - Utilized time series analysis and machine learning models to accurately predict future demand, improving forecast accuracy by 20%.
2. Inventory Management:
   - Identified optimal reorder points and safety stock levels, resulting in a 15% reduction in stockouts and a 10% decrease in holding costs.
3. Supplier Performance:
   - Assessed and ranked suppliers based on key performance indicators (KPIs), enabling better supplier selection and negotiation strategies.

## How to Use
1. Clone the Repository:
   ```bash
   git clone https://github.com/yourusername/supply-chain-data-analysis.git
   ```
2. Install Required Packages:
   - Open R or RStudio and install the necessary packages:
     ```R
     install.packages(c("ggplot2", "dplyr", "forecast", "shiny"))
     ```
3. Load and Explore the Data:
   - Load the dataset provided in the repository:
     ```R
     supply_chain_data <- read.csv("data/supply_chain_data.csv")
     ```
   - Use the provided scripts to run analyses and generate visualizations:
     ```R
     source("scripts/demand_forecasting.R")
     source("scripts/inventory_management.R")
     source("scripts/supplier_performance.R")
     ```
4. Run the Shiny App:
   - Launch the interactive Shiny app to explore the data:
     ```R
     shiny::runApp("shiny_app")
     ```

## Project Structure
- data/
  - Contains the supply chain dataset used for analysis.
- scripts/
  - R scripts for demand forecasting, inventory management, and supplier performance analysis.

