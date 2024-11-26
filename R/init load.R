# Load necessary libraries
library(leaflet)
library(crosstalk)
library(DT)
library(dplyr)
library(stringr)
library(sf)
library(lubridate)
library(htmlwidgets)
library(htmltools)
library(janitor)
library(purrr)
library(summarywidget)

twelve_months_ago <- today() %m-% months(12)

# Load Czech districts shapefile
slovak_districts <- read_sf("data/districts/lau1-current-iz-shp.shp") |>
  filter(str_detect(tolower(LAU), "sk")) |>
  clean_names() |>
  mutate(
    long = unlist(map(st_centroid(geometry), 1)),
    lat = unlist(map(st_centroid(geometry), 2))
  )

# Assign specialists to districts
set.seed(123)
slovak_districts <- slovak_districts |>
  mutate(
    specialist = case_when(
      substr(lau, 4, 4) == "1" ~ "Miloš",
      substr(lau, 4, 4) == "2" ~ "Jana",
      substr(lau, 4, 4) == "3" ~ "Jan",
      substr(lau, 4, 4) == "4" ~ "Lenka",
      TRUE ~ NA_character_ # Default case if no match
    ),
    label = paste0("Okres: ", name, "; ", "Obchodník: ", specialist)
  )

specialists <- unique(slovak_districts$specialist)

# Color palettes
pal_specialist <- colorFactor("Set1", slovak_districts$specialist)

# Create dummy customers
customer_list <- list()
set.seed(1)
for (spec in specialists) {
  districts_spec <- slovak_districts |> filter(specialist == spec)
  
  # For each district assigned to the specialist
  for (i in seq_len(nrow(districts_spec))) {
    district <- districts_spec[i, ]
    district_code <- district$lau
    
    # Generate a set number of customers per district, say 2 customers per district
    num_customers <- 3
    points <- st_sample(district$geometry, num_customers)
    
    # Convert to data frame
    coords <- st_coordinates(st_centroid(points))
    
    # Create customer types
    customer_types <- c("interný", "akvírovaný", "prospekt")
    
    # Randomly assign customer types
    cust_types <- sample(customer_types, num_customers, replace = TRUE)
    
    # Generate first order dates within last 24 months
    first_order_dates <- sample(seq(as_date("2022-10-01"), as_date("2024-10-01"), by = "day"), num_customers, replace = TRUE)
    
    # Generate sales amounts
    sales_amounts <- runif(num_customers, min = 1000, max = 10000)
    
    customers_spec <- data.frame(
      customer_id = paste0(district_code, 
                           sprintf("%08d", 
                                   sample(1e7:1e8 - 1, 
                                          num_customers, 
                                          replace = FALSE)
                           )
      ),
      specialist = spec,
      district = district_code,
      customer_type = cust_types,
      first_order_date = first_order_dates,
      sales_amount = sales_amounts,
      lat = coords[, 2],
      lng = coords[, 1],
      stringsAsFactors = FALSE
    )
    
    customer_list[[paste0(spec, "_", district_code)]] <- customers_spec
  }
}

customers <- do.call(rbind, customer_list) |>
  mutate(first_order_date = ifelse(customer_type == "prospekt", NA, first_order_date),
         first_order_date = as_date(first_order_date),
         sales_amount = ifelse(customer_type == "prospekt", 0, sales_amount))

# Convert date to character for JSON serialization in JavaScript
customers$label <- paste0("Zákaznícke ID: ", customers$customer_id, "; ", "typ: ", customers$customer_type)
customers$marker_color <- ifelse(customers$customer_type == "interný",
                                 "#D3AF37",
                                 ifelse(customers$customer_type == "akvírovaný",
                                        "#3D729E",
                                        "#C4A484"
                                 )
)


# Create SharedData object
sd_customers <- SharedData$new(customers, key = ~customer_id)