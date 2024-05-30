# Libraries ----
library(shiny)
library(tidyverse)
library(sf)
library(patchwork)
library(scales)
library(showtext)
library(here)

# data wrangling ----
happy <- read.csv(here("data/World-happiness-report-updated_2024.csv")) |>
  janitor::clean_names()

countries <- happy |>
  group_by(country_name) |>
  distinct(country_name) |>
  as.vector()

# adding new non-standard font from google
font_add_google("Noto Serif", "noto serif")
showtext_auto() # use showtext package to render text

# Define UI for application that draws a histogram
ui <- fluidPage(# Application title
  titlePanel("Happiness Data"),
  
  # Sidebar with a slider input for number of bins, a selecter input, and a radio buttons input
  sidebarLayout(
    sidebarPanel(
      img(
        src = "kidney.png",
        height = 100,
        width = 100
      ),
      p(
        "This app investigates the differences between those with Chronic Kidney Disease (CKD) or not",
        "in regard to important metrics in the field of nephrology and the diagnosis of kidney disease.",
        "You can also compare differences between other conditions",
        "to see if having other conditions exacerbates the measurements of those with CKD."
      ),
      h4("Instructions:"),
      p(
        "Select the feature and secondary condition you would like to examine.",
        "You can adjust the number of bins using the slider"
      ),
      h4("Data:"),
      p(
        'Visualizations were made from the "Pre-processed Chronic Kidney Disease Dataset"',
        "from Kaggle, based on the Chronic Kidney Disease dataset from",
        "UC Irvine's Machine Learning Repository.",
        "It contains 400 patient observations taken over a 2-month period in India."
      ),
      
      # Pick which country
      selectInput(
        "country1",
        label = "Choose first country to display:",
        choices = c(
          "Afghanistan",
          "Albania",
          "Algeria",
          "Angola",
          "Argentina",
          "Armenia",
          "Australia",
          "Austria",
          "Azerbaijan",
          "Bahrain",
          "Bangladesh",
          "Belarus",
          "Belgium",
          "Belize",
          "Benin",
          "Bhutan",
          "Bolivia",
          "Bosnia and Herzegovina",
          "Botswana",
          "Brazil",
          "Bulgaria",
          "Burkina Faso",
          "Burundi",
          "Cambodia",
          "Cameroon",
          "Canada",
          "Central African Republic",
          "Chad",
          "Chile",
          "China",
          "Colombia",
          "Comoros",
          "Congo (Brazzaville)",
          "Congo (Kinshasa)",
          "Costa Rica",
          "Croatia",
          "Cuba",
          "Cyprus",
          "Czechia",
          "Denmark",
          "Djibouti",
          "Dominican Republic",
          "Ecuador",
          "Egypt",
          "El Salvador",
          "Estonia",
          "Eswatini",
          "Ethiopia",
          "Finland",
          "France",
          "Gabon",
          "Gambia",
          "Georgia",
          "Germany",
          "Ghana",
          "Greece",
          "Guatemala",
          "Guinea",
          "Guyana",
          "Haiti",
          "Honduras",
          "Hong Kong S.A.R. of China",
          "Hungary",
          "Iceland",
          "India",
          "Indonesia",
          "Iran",
          "Iraq",
          "Ireland",
          "Israel",
          "Italy",
          "Ivory Coast",
          "Jamaica",
          "Japan",
          "Jordan",
          "Kazakhstan",
          "Kenya",
          "Kosovo",
          "Kuwait",
          "Kyrgyzstan",
          "Laos",
          "Latvia",
          "Lebanon",
          "Lesotho",
          "Liberia",
          "Libya",
          "Lithuania",
          "Luxembourg",
          "Madagascar",
          "Malawi",
          "Malaysia",
          "Maldives",
          "Mali",
          "Malta",
          "Mauritania",
          "Mauritius",
          "Mexico",
          "Moldova",
          "Mongolia",
          "Montenegro",
          "Morocco",
          "Mozambique",
          "Myanmar",
          "Namibia",
          "Nepal",
          "Netherlands",
          "New Zealand",
          "Nicaragua",
          "Niger",
          "Nigeria",
          "North Macedonia",
          "Norway",
          "Oman",
          "Pakistan",
          "Panama",
          "Paraguay",
          "Peru",
          "Philippines",
          "Poland",
          "Portugal",
          "Qatar",
          "Romania",
          "Russia",
          "Rwanda",
          "Saudi Arabia",
          "Senegal",
          "Serbia",
          "Sierra Leone",
          "Singapore",
          "Slovakia",
          "Slovenia",
          "Somalia",
          "Somaliland region",
          "South Africa",
          "South Korea",
          "South Sudan",
          "Spain",
          "Sri Lanka",
          "State of Palestine",
          "Sudan",
          "Suriname",
          "Sweden",
          "Switzerland",
          "Syria",
          "Taiwan Province of China",
          "Tajikistan",
          "Tanzania",
          "Thailand",
          "Togo",
          "Trinidad and Tobago",
          "Tunisia",
          "Turkmenistan",
          "Türkiye",
          "Uganda",
          "Ukraine",
          "United Arab Emirates",
          "United Kingdom",
          "United States",
          "Uruguay",
          "Uzbekistan",
          "Venezuela",
          "Vietnam",
          "Yemen",
          "Zambia",
          "Zimbabwe")
      ),
      
      selectInput(
        "country2",
        label = "Choose second country to display:",
        choices = c(
          "Afghanistan",
          "Albania",
          "Algeria",
          "Angola",
          "Argentina",
          "Armenia",
          "Australia",
          "Austria",
          "Azerbaijan",
          "Bahrain",
          "Bangladesh",
          "Belarus",
          "Belgium",
          "Belize",
          "Benin",
          "Bhutan",
          "Bolivia",
          "Bosnia and Herzegovina",
          "Botswana",
          "Brazil",
          "Bulgaria",
          "Burkina Faso",
          "Burundi",
          "Cambodia",
          "Cameroon",
          "Canada",
          "Central African Republic",
          "Chad",
          "Chile",
          "China",
          "Colombia",
          "Comoros",
          "Congo (Brazzaville)",
          "Congo (Kinshasa)",
          "Costa Rica",
          "Croatia",
          "Cuba",
          "Cyprus",
          "Czechia",
          "Denmark",
          "Djibouti",
          "Dominican Republic",
          "Ecuador",
          "Egypt",
          "El Salvador",
          "Estonia",
          "Eswatini",
          "Ethiopia",
          "Finland",
          "France",
          "Gabon",
          "Gambia",
          "Georgia",
          "Germany",
          "Ghana",
          "Greece",
          "Guatemala",
          "Guinea",
          "Guyana",
          "Haiti",
          "Honduras",
          "Hong Kong S.A.R. of China",
          "Hungary",
          "Iceland",
          "India",
          "Indonesia",
          "Iran",
          "Iraq",
          "Ireland",
          "Israel",
          "Italy",
          "Ivory Coast",
          "Jamaica",
          "Japan",
          "Jordan",
          "Kazakhstan",
          "Kenya",
          "Kosovo",
          "Kuwait",
          "Kyrgyzstan",
          "Laos",
          "Latvia",
          "Lebanon",
          "Lesotho",
          "Liberia",
          "Libya",
          "Lithuania",
          "Luxembourg",
          "Madagascar",
          "Malawi",
          "Malaysia",
          "Maldives",
          "Mali",
          "Malta",
          "Mauritania",
          "Mauritius",
          "Mexico",
          "Moldova",
          "Mongolia",
          "Montenegro",
          "Morocco",
          "Mozambique",
          "Myanmar",
          "Namibia",
          "Nepal",
          "Netherlands",
          "New Zealand",
          "Nicaragua",
          "Niger",
          "Nigeria",
          "North Macedonia",
          "Norway",
          "Oman",
          "Pakistan",
          "Panama",
          "Paraguay",
          "Peru",
          "Philippines",
          "Poland",
          "Portugal",
          "Qatar",
          "Romania",
          "Russia",
          "Rwanda",
          "Saudi Arabia",
          "Senegal",
          "Serbia",
          "Sierra Leone",
          "Singapore",
          "Slovakia",
          "Slovenia",
          "Somalia",
          "Somaliland region",
          "South Africa",
          "South Korea",
          "South Sudan",
          "Spain",
          "Sri Lanka",
          "State of Palestine",
          "Sudan",
          "Suriname",
          "Sweden",
          "Switzerland",
          "Syria",
          "Taiwan Province of China",
          "Tajikistan",
          "Tanzania",
          "Thailand",
          "Togo",
          "Trinidad and Tobago",
          "Tunisia",
          "Turkmenistan",
          "Türkiye",
          "Uganda",
          "Ukraine",
          "United Arab Emirates",
          "United Kingdom",
          "United States",
          "Uruguay",
          "Uzbekistan",
          "Venezuela",
          "Vietnam",
          "Yemen",
          "Zambia",
          "Zimbabwe")
      ),
      # choose which var to explore
      radioButtons(
        "x_var",
        "Variable of Interest:",
        choices = c(
          "log_gdp_per_capita",
          "social_support",
          "healthy_life_expectancy_at_birth",
          "freedom_to_make_life_choices",
          "generosity",
          "perceptions_of_corruption",
          "positve_affect",
          "negative_affect"
        )
      ),
      
      p("Source: Kidney Image from Stanford Medicine and Shutterstock")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot", height = 1000)
              # change height so the plot is larger and more readable)
    )))
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    data1 = reactive({
      # define subset of data only looking at one country
      if (input$country1 == "Afghanistan"){happy_subset1 <- happy |> filter(country == "Afghanistan")}
      if (input$country1 == "Albania"){happy_subset1 <- happy |> filter(country == "Albania")}
      if (input$country1 == "Algeria"){happy_subset1 <- happy |> filter(country == "Algeria")}
      happy_subset1
    })  
        output$plot <- renderPlot({
      happy_subset2 <- happy |> 
        filter(country_name == "Afghanistan")
      
      # define the x-axis variable that changes depending on the selecter input
      x_variable <- case_when(
        input$x_var == "log_gdp_per_capita" ~ happy_subset$log_gdp_per_capita,
        input$x_var == "social_support" ~ happy_subset$social_support,
        input$x_var == "healthy_life_expectancy_at_birth" ~ happy_subset$healthy_life_expectancy_at_birth,
        input$x_var == "freedom_to_make_life_choices" ~ happy_subset$freedom_to_make_life_choices,
        input$x_var == "generosity" ~ happy_subset$generosity,
        input$x_var == "perceptions_of_corruption" ~ happy_subset$perceptions_of_corruption,
        input$x_var == "positve_affect" ~ happy_subset$positive_affect,
        input$x_var == "negative_affect" ~ happy_subset$negative_affect
      )
      
      x_title <- case_when(
        input$x_var == "Actual Weight" ~ "Actual Weight in Pounds", 
        input$x_var == "Desired Weight" ~ "Desired Weight in Pounds", 
        input$x_var == "Height" ~ "Height in Inches")
      
      x <- x_variable
      ######################################################################################################
      
      plot1 <- ggplot(happy_subset, aes(x = x_variable, y = life_ladder)) +
        geom_point(color = "black", show.legend = FALSE) +
        theme_minimal() +
        labs() +
        # label x and y axes, and title and subtitle
        theme(
          plot.title = element_text(
            size = 16,
            face = "bold",
            family = "noto serif"
          ),
          plot.subtitle = element_text(size = 12, family = "noto serif"),
          axis.text = element_text(size = 12, family = "noto serif"),
          axis.title = element_text(size = 13, family = "noto serif"),
          strip.text = element_text(
            face = "bold",
            size = 13,
            family = "noto serif"
          ),
          panel.border = element_blank(),
          strip.background = element_rect(color = "grey"),
          axis.line = element_line(color = "black")
        )
      
      plot2 <- ggplot(happy_subset2, aes(x = x_variable, y = life_ladder)) +
        geom_point(color = "black", show.legend = FALSE) +
        theme_minimal() +
        labs() +
        # label x and y axes, and title and subtitle
        theme(
          plot.title = element_text(
            size = 16,
            face = "bold",
            family = "noto serif"
          ),
          plot.subtitle = element_text(size = 12, family = "noto serif"),
          axis.text = element_text(size = 12, family = "noto serif"),
          axis.title = element_text(size = 13, family = "noto serif"),
          strip.text = element_text(
            face = "bold",
            size = 13,
            family = "noto serif"
          ),
          panel.border = element_blank(),
          strip.background = element_rect(color = "grey"),
          axis.line = element_line(color = "black")
        )
      
      # use patchwork to stack the two plots 
      (plot1/plot2) 
    })
  }
  
  # Run the application
  shinyApp(ui = ui, server = server)
  