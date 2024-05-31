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
        src = "happyface.jpg",
        height = 100,
        width = 110
      ),
      p(
        "This app investigates the differences in life ladder world happiness report score",
        "across different countries. You can compare two countries with one another and",
        "for each country you can select a variable to view in relation to that country's",
        "life ladder score. The life ladder ranges from 0 - 10 with higher being happier."
      ),
      h4("Instructions:"),
      p(
        "Select the country and secondary variable you would like to examine."
      ),
      h4("Data:"),
      tags$li(a(href = 'https://www.kaggle.com/datasets/jainaru/world-happiness-report-2024-yearly-updated',
                icon("fa-sharp fa-solid fa-magnifying-glass"),
                title = "Kaggle Dataset"),
              class = "dropdown"),
      p(
        "The data comes from the World Happiness Report data collected from 2005 - 2024."
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
      
      # choose first variable to explore
      radioButtons(
        "x_var",
        "First Variable of Interest:",
        choices = c(
          "log(GDP) per Capita",
          "Social Support",
          "Healthy Life Expectancy at Birth",
          "Freedom to Make Life Choices",
          "Generosity",
          "Perceptions of Corruption",
          "Positive Affect",
          "Negative Affect"
        )
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
      
      # choose second variable to explore
      radioButtons(
        "x_var2",
        "Second Variable of Interest:",
        choices = c(
          "log(GDP) per Capita",
          "Social Support",
          "Healthy Life Expectancy at Birth",
          "Freedom to Make Life Choices",
          "Generosity",
          "Perceptions of Corruption",
          "Positive Affect",
          "Negative Affect"
        )
      ),
      
      p("Source: Smiley face comes from istockphoto.com")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(plotOutput("plot", height = 1000)
              # change height so the plot is larger and more readable)
    )))
  
  # Define server logic required to draw a histogram
  server <- function(input, output) {
    
    # plot 1 data --------------------------------------------------------------
    data1 <- reactive({
      happy_subset <- happy
      # define subset of data only looking at one country----
      if (input$country1 == "Afghanistan"){happy_subset <- happy |> filter(country_name == "Afghanistan")}
      if (input$country1 == "Albania"){happy_subset <- happy |> filter(country_name == "Albania")}
      if (input$country1 == "Algeria"){happy_subset <- happy |> filter(country_name == "Algeria")}
      if (input$country1 == "Angola"){happy_subset <- happy |> filter(country_name == "Angola")}
      if (input$country1 == "Argentina"){happy_subset <- happy |> filter(country_name == "Argentina")}
      if (input$country1 == "Armenia"){happy_subset <- happy |> filter(country_name == "Armenia")}
      if (input$country1 == "Australia"){happy_subset <- happy |> filter(country_name == "Australia")}
      if (input$country1 == "Austria"){happy_subset <- happy |> filter(country_name == "Austria")}
      if (input$country1 == "Azerbaijan"){happy_subset <- happy |> filter(country_name == "Azerbaijan")}
      if (input$country1 == "Bahrain"){happy_subset <- happy |> filter(country_name == "Bahrain")}
      if (input$country1 == "Bangladesh"){happy_subset <- happy |> filter(country_name == "Bangladesh")}
      if (input$country1 == "Belarus"){happy_subset <- happy |> filter(country_name == "Belarus")}
      if (input$country1 == "Belgium"){happy_subset <- happy |> filter(country_name == "Belgium")}
      if (input$country1 == "Belize"){happy_subset <- happy |> filter(country_name == "Belize")}
      if (input$country1 == "Benin"){happy_subset <- happy |> filter(country_name == "Benin")}
      if (input$country1 == "Bhutan"){happy_subset <- happy |> filter(country_name == "Bhutan")}
      if (input$country1 == "Bolivia"){happy_subset <- happy |> filter(country_name == "Bolivia")}
      if (input$country1 == "Bosnia and Herzegovina"){happy_subset <- happy |> filter(country_name == "Bosnia and Herzegovina")}
      if (input$country1 == "Botswana"){happy_subset <- happy |> filter(country_name == "Botswana")}
      if (input$country1 == "Brazil"){happy_subset <- happy |> filter(country_name == "Brazil")}
      if (input$country1 == "Bulgaria"){happy_subset <- happy |> filter(country_name == "Bulgaria")}
      if (input$country1 == "Burkina Faso"){happy_subset <- happy |> filter(country_name == "Burkina Faso")}
      if (input$country1 == "Burundi"){happy_subset <- happy |> filter(country_name == "Burundi")}
      if (input$country1 == "Cambodia"){happy_subset <- happy |> filter(country_name == "Cambodia")}
      if (input$country1 == "Cameroon"){happy_subset <- happy |> filter(country_name == "Cameroon")}
      if (input$country1 == "Canada"){happy_subset <- happy |> filter(country_name == "Canada")}
      if (input$country1 == "Central African Republic"){happy_subset <- happy |> filter(country_name == "Central African Republic")}
      if (input$country1 == "Chad"){happy_subset <- happy |> filter(country_name == "Chad")}
      if (input$country1 == "Chile"){happy_subset <- happy |> filter(country_name == "Chile")}
      if (input$country1 == "China"){happy_subset <- happy |> filter(country_name == "China")}
      if (input$country1 == "Colombia"){happy_subset <- happy |> filter(country_name == "Colombia")}
      if (input$country1 == "Comoros"){happy_subset <- happy |> filter(country_name == "Comoros")}
      if (input$country1 == "Congo (Brazzaville)"){happy_subset <- happy |> filter(country_name == "Congo (Brazzaville)")}
      if (input$country1 == "Congo (Kinshasa)"){happy_subset <- happy |> filter(country_name == "Congo (Kinshasa)")}
      if (input$country1 == "Costa Rica"){happy_subset <- happy |> filter(country_name == "Costa Rica")}
      if (input$country1 == "Croatia"){happy_subset <- happy |> filter(country_name == "Croatia")}
      if (input$country1 == "Cuba"){happy_subset <- happy |> filter(country_name == "Cuba")}
      if (input$country1 == "Cyprus"){happy_subset <- happy |> filter(country_name == "Cyprus")}
      if (input$country1 == "Czechia"){happy_subset <- happy |> filter(country_name == "Czechia")}
      if (input$country1 == "Denmark"){happy_subset <- happy |> filter(country_name == "Denmark")}
      if (input$country1 == "Djibouti"){happy_subset <- happy |> filter(country_name == "Djibouti")}
      if (input$country1 == "Dominican Republic"){happy_subset <- happy |> filter(country_name == "Dominican Republic")}
      if (input$country1 == "Ecuador"){happy_subset <- happy |> filter(country_name == "Ecuador")}
      if (input$country1 == "Egypt"){happy_subset <- happy |> filter(country_name == "Egypt")}
      if (input$country1 == "El Salvador"){happy_subset <- happy |> filter(country_name == "El Salvador")}
      if (input$country1 == "Estonia"){happy_subset <- happy |> filter(country_name == "Estonia")}
      if (input$country1 == "Eswatini"){happy_subset <- happy |> filter(country_name == "Eswatini")}
      if (input$country1 == "Ethiopia"){happy_subset <- happy |> filter(country_name == "Ethiopia")}
      if (input$country1 == "Finland"){happy_subset <- happy |> filter(country_name == "Finland")}
      if (input$country1 == "France"){happy_subset <- happy |> filter(country_name == "France")}
      if (input$country1 == "Gabon"){happy_subset <- happy |> filter(country_name == "Gabon")}
      if (input$country1 == "Gambia"){happy_subset <- happy |> filter(country_name == "Gambia")}
      if (input$country1 == "Georgia"){happy_subset <- happy |> filter(country_name == "Georgia")}
      if (input$country1 == "Germany"){happy_subset <- happy |> filter(country_name == "Germany")}
      if (input$country1 == "Ghana"){happy_subset <- happy |> filter(country_name == "Ghana")}
      if (input$country1 == "Greece"){happy_subset <- happy |> filter(country_name == "Greece")}
      if (input$country1 == "Guatemala"){happy_subset <- happy |> filter(country_name == "Guatemala")}
      if (input$country1 == "Guinea"){happy_subset <- happy |> filter(country_name == "Guinea")}
      if (input$country1 == "Guyana"){happy_subset <- happy |> filter(country_name == "Guyana")}
      if (input$country1 == "Haiti"){happy_subset <- happy |> filter(country_name == "Haiti")}
      if (input$country1 == "Honduras"){happy_subset <- happy |> filter(country_name == "Honduras")}
      if (input$country1 == "Hong Kong S.A.R. of China"){happy_subset <- happy |> filter(country_name == "Hong Kong S.A.R. of China")}
      if (input$country1 == "Hungary"){happy_subset <- happy |> filter(country_name == "Hungary")}
      if (input$country1 == "Iceland"){happy_subset <- happy |> filter(country_name == "Iceland")}
      if (input$country1 == "India"){happy_subset <- happy |> filter(country_name == "India")}
      if (input$country1 == "Indonesia"){happy_subset <- happy |> filter(country_name == "Indonesia")}
      if (input$country1 == "Iran"){happy_subset <- happy |> filter(country_name == "Iran")}
      if (input$country1 == "Iraq"){happy_subset <- happy |> filter(country_name == "Iraq")}
      if (input$country1 == "Ireland"){happy_subset <- happy |> filter(country_name == "Ireland")}
      if (input$country1 == "Israel"){happy_subset <- happy |> filter(country_name == "Israel")}
      if (input$country1 == "Italy"){happy_subset <- happy |> filter(country_name == "Italy")}
      if (input$country1 == "Ivory Coast"){happy_subset <- happy |> filter(country_name == "Ivory Coast")}
      if (input$country1 == "Jamaica"){happy_subset <- happy |> filter(country_name == "Jamaica")}
      if (input$country1 == "Japan"){happy_subset <- happy |> filter(country_name == "Japan")}
      if (input$country1 == "Jordan"){happy_subset <- happy |> filter(country_name == "Jordan")}
      if (input$country1 == "Kazakhstan"){happy_subset <- happy |> filter(country_name == "Kazakhstan")}
      if (input$country1 == "Kenya"){happy_subset <- happy |> filter(country_name == "Kenya")}
      if (input$country1 == "Kosovo"){happy_subset <- happy |> filter(country_name == "Kosovo")}
      if (input$country1 == "Kuwait"){happy_subset <- happy |> filter(country_name == "Kuwait")}
      if (input$country1 == "Kyrgyzstan"){happy_subset <- happy |> filter(country_name == "Kyrgyzstan")}
      if (input$country1 == "Laos"){happy_subset <- happy |> filter(country_name == "Laos")}
      if (input$country1 == "Latvia"){happy_subset <- happy |> filter(country_name == "Latvia")}
      if (input$country1 == "Lebanon"){happy_subset <- happy |> filter(country_name == "Lebanon")}
      if (input$country1 == "Lesotho"){happy_subset <- happy |> filter(country_name == "Lesotho")}
      if (input$country1 == "Liberia"){happy_subset <- happy |> filter(country_name == "Liberia")}
      if (input$country1 == "Libya"){happy_subset <- happy |> filter(country_name == "Libya")}
      if (input$country1 == "Lithuania"){happy_subset <- happy |> filter(country_name == "Lithuania")}
      if (input$country1 == "Luxembourg"){happy_subset <- happy |> filter(country_name == "Luxembourg")}
      if (input$country1 == "Madagascar"){happy_subset <- happy |> filter(country_name == "Madagascar")}
      if (input$country1 == "Malawi"){happy_subset <- happy |> filter(country_name == "Malawi")}
      if (input$country1 == "Malaysia"){happy_subset <- happy |> filter(country_name == "Malaysia")}
      if (input$country1 == "Maldives"){happy_subset <- happy |> filter(country_name == "Maldives")}
      if (input$country1 == "Mali"){happy_subset <- happy |> filter(country_name == "Mali")}
      if (input$country1 == "Malta"){happy_subset <- happy |> filter(country_name == "Malta")}
      if (input$country1 == "Mauritania"){happy_subset <- happy |> filter(country_name == "Mauritania")}
      if (input$country1 == "Mauritius"){happy_subset <- happy |> filter(country_name == "Mauritius")}
      if (input$country1 == "Mexico"){happy_subset <- happy |> filter(country_name == "Mexico")}
      if (input$country1 == "Moldova"){happy_subset <- happy |> filter(country_name == "Moldova")}
      if (input$country1 == "Mongolia"){happy_subset <- happy |> filter(country_name == "Mongolia")}
      if (input$country1 == "Montenegro"){happy_subset <- happy |> filter(country_name == "Montenegro")}
      if (input$country1 == "Morocco"){happy_subset <- happy |> filter(country_name == "Morocco")}
      if (input$country1 == "Mozambique"){happy_subset <- happy |> filter(country_name == "Mozambique")}
      if (input$country1 == "Myanmar"){happy_subset <- happy |> filter(country_name == "Myanmar")}
      if (input$country1 == "Namibia"){happy_subset <- happy |> filter(country_name == "Namibia")}
      if (input$country1 == "Nepal"){happy_subset <- happy |> filter(country_name == "Nepal")}
      if (input$country1 == "Netherlands"){happy_subset <- happy |> filter(country_name == "Netherlands")}
      if (input$country1 == "New Zealand"){happy_subset <- happy |> filter(country_name == "New Zealand")}
      if (input$country1 == "Nicaragua"){happy_subset <- happy |> filter(country_name == "Nicaragua")}
      if (input$country1 == "Niger"){happy_subset <- happy |> filter(country_name == "Niger")}
      if (input$country1 == "Nigeria"){happy_subset <- happy |> filter(country_name == "Nigeria")}
      if (input$country1 == "North Macedonia"){happy_subset <- happy |> filter(country_name == "North Macedonia")}
      if (input$country1 == "Norway"){happy_subset <- happy |> filter(country_name == "Norway")}
      if (input$country1 == "Oman"){happy_subset <- happy |> filter(country_name == "Oman")}
      if (input$country1 == "Pakistan"){happy_subset <- happy |> filter(country_name == "Pakistan")}
      if (input$country1 == "Panama"){happy_subset <- happy |> filter(country_name == "Panama")}
      if (input$country1 == "Paraguay"){happy_subset <- happy |> filter(country_name == "Paraguay")}
      if (input$country1 == "Peru"){happy_subset <- happy |> filter(country_name == "Peru")}
      if (input$country1 == "Philippines"){happy_subset <- happy |> filter(country_name == "Philippines")}
      if (input$country1 == "Poland"){happy_subset <- happy |> filter(country_name == "Poland")}
      if (input$country1 == "Portugal"){happy_subset <- happy |> filter(country_name == "Portugal")}
      if (input$country1 == "Qatar"){happy_subset <- happy |> filter(country_name == "Qatar")}
      if (input$country1 == "Romania"){happy_subset <- happy |> filter(country_name == "Romania")}
      if (input$country1 == "Russia"){happy_subset <- happy |> filter(country_name == "Russia")}
      if (input$country1 == "Rwanda"){happy_subset <- happy |> filter(country_name == "Rwanda")}
      if (input$country1 == "Saudi Arabia"){happy_subset <- happy |> filter(country_name == "Saudi Arabia")}
      if (input$country1 == "Senegal"){happy_subset <- happy |> filter(country_name == "Senegal")}
      if (input$country1 == "Serbia"){happy_subset <- happy |> filter(country_name == "Serbia")}
      if (input$country1 == "Sierra Leone"){happy_subset <- happy |> filter(country_name == "Sierra Leone")}
      if (input$country1 == "Singapore"){happy_subset <- happy |> filter(country_name == "Singapore")}
      if (input$country1 == "Slovakia"){happy_subset <- happy |> filter(country_name == "Slovakia")}
      if (input$country1 == "Slovenia"){happy_subset <- happy |> filter(country_name == "Slovenia")}
      if (input$country1 == "Somalia"){happy_subset <- happy |> filter(country_name == "Somalia")}
      if (input$country1 == "Somaliland region"){happy_subset <- happy |> filter(country_name == "Somaliland region")}
      if (input$country1 == "South Africa"){happy_subset <- happy |> filter(country_name == "South Africa")}
      if (input$country1 == "South Korea"){happy_subset <- happy |> filter(country_name == "South Korea")}
      if (input$country1 == "South Sudan"){happy_subset <- happy |> filter(country_name == "South Sudan")}
      if (input$country1 == "Spain"){happy_subset <- happy |> filter(country_name == "Spain")}
      if (input$country1 == "Sri Lanka"){happy_subset <- happy |> filter(country_name == "Sri Lanka")}
      if (input$country1 == "State of Palestine"){happy_subset <- happy |> filter(country_name == "State of Palestine")}
      if (input$country1 == "Sudan"){happy_subset <- happy |> filter(country_name == "Sudan")}
      if (input$country1 == "Suriname"){happy_subset <- happy |> filter(country_name == "Suriname")}
      if (input$country1 == "Sweden"){happy_subset <- happy |> filter(country_name == "Sweden")}
      if (input$country1 == "Switzerland"){happy_subset <- happy |> filter(country_name == "Switzerland")}
      if (input$country1 == "Syria"){happy_subset <- happy |> filter(country_name == "Syria")}
      if (input$country1 == "Taiwan Province of China"){happy_subset <- happy |> filter(country_name == "Taiwan Province of China")}
      if (input$country1 == "Tajikistan"){happy_subset <- happy |> filter(country_name == "Tajikistan")}
      if (input$country1 == "Tanzania"){happy_subset <- happy |> filter(country_name == "Tanzania")}
      if (input$country1 == "Thailand"){happy_subset <- happy |> filter(country_name == "Thailand")}
      if (input$country1 == "Togo"){happy_subset <- happy |> filter(country_name == "Togo")}
      if (input$country1 == "Trinidad and Tobago"){happy_subset <- happy |> filter(country_name == "Trinidad and Tobago")}
      if (input$country1 == "Tunisia"){happy_subset <- happy |> filter(country_name == "Tunisia")}
      if (input$country1 == "Turkmenistan"){happy_subset <- happy |> filter(country_name == "Turkmenistan")}
      if (input$country1 == "Türkiye"){happy_subset <- happy |> filter(country_name == "Türkiye")}
      if (input$country1 == "Uganda"){happy_subset <- happy |> filter(country_name == "Uganda")}
      if (input$country1 == "Ukraine"){happy_subset <- happy |> filter(country_name == "Ukraine")}
      if (input$country1 == "United Arab Emirates"){happy_subset <- happy |> filter(country_name == "United Arab Emirates")}
      if (input$country1 == "United Kingdom"){happy_subset <- happy |> filter(country_name == "United Kingdom")}
      if (input$country1 == "United States"){happy_subset <- happy |> filter(country_name == "United States")}
      if (input$country1 == "Uruguay"){happy_subset <- happy |> filter(country_name == "Uruguay")}
      if (input$country1 == "Uzbekistan"){happy_subset <- happy |> filter(country_name == "Uzbekistan")}
      if (input$country1 == "Venezuela"){happy_subset <- happy |> filter(country_name == "Venezuela")}
      if (input$country1 == "Vietnam"){happy_subset <- happy |> filter(country_name == "Vietnam")}
      if (input$country1 == "Yemen"){happy_subset <- happy |> filter(country_name == "Yemen")}
      if (input$country1 == "Zambia"){happy_subset <- happy |> filter(country_name == "Zambia")}
      if (input$country1 == "Zimbabwe"){happy_subset <- happy |> filter(country_name == "Zimbabwe")}
      happy_subset
    })  
    
    x_variable1 <- reactive({
      case_when(
        input$x_var == "log(GDP) per Capita" ~ data1()$log_gdp_per_capita,
        input$x_var == "Social Support" ~ data1()$social_support,
        input$x_var == "Healthy Life Expectancy at Birth" ~ data1()$healthy_life_expectancy_at_birth,
        input$x_var == "Freedom to Make Life Choices" ~ data1()$freedom_to_make_life_choices,
        input$x_var == "Generosity" ~ data1()$generosity,
        input$x_var == "Perceptions of Corruption" ~ data1()$perceptions_of_corruption,
        input$x_var == "Positive Affect" ~ data1()$positive_affect,
        input$x_var == "Negative Affect" ~ data1()$negative_affect
      )
    })
    
    
    # plot 2 data --------------------------------------------------------------
    data2 <- reactive({
      happy_subset <- happy
      # define subset of data only looking at one country----
      if (input$country2 == "Afghanistan"){happy_subset <- happy |> filter(country_name == "Afghanistan")}
      if (input$country2 == "Albania"){happy_subset <- happy |> filter(country_name == "Albania")}
      if (input$country2 == "Algeria"){happy_subset <- happy |> filter(country_name == "Algeria")}
      if (input$country2 == "Angola"){happy_subset <- happy |> filter(country_name == "Angola")}
      if (input$country2 == "Argentina"){happy_subset <- happy |> filter(country_name == "Argentina")}
      if (input$country2 == "Armenia"){happy_subset <- happy |> filter(country_name == "Armenia")}
      if (input$country2 == "Australia"){happy_subset <- happy |> filter(country_name == "Australia")}
      if (input$country2 == "Austria"){happy_subset <- happy |> filter(country_name == "Austria")}
      if (input$country2 == "Azerbaijan"){happy_subset <- happy |> filter(country_name == "Azerbaijan")}
      if (input$country2 == "Bahrain"){happy_subset <- happy |> filter(country_name == "Bahrain")}
      if (input$country2 == "Bangladesh"){happy_subset <- happy |> filter(country_name == "Bangladesh")}
      if (input$country2 == "Belarus"){happy_subset <- happy |> filter(country_name == "Belarus")}
      if (input$country2 == "Belgium"){happy_subset <- happy |> filter(country_name == "Belgium")}
      if (input$country2 == "Belize"){happy_subset <- happy |> filter(country_name == "Belize")}
      if (input$country2 == "Benin"){happy_subset <- happy |> filter(country_name == "Benin")}
      if (input$country2 == "Bhutan"){happy_subset <- happy |> filter(country_name == "Bhutan")}
      if (input$country2 == "Bolivia"){happy_subset <- happy |> filter(country_name == "Bolivia")}
      if (input$country2 == "Bosnia and Herzegovina"){happy_subset <- happy |> filter(country_name == "Bosnia and Herzegovina")}
      if (input$country2 == "Botswana"){happy_subset <- happy |> filter(country_name == "Botswana")}
      if (input$country2 == "Brazil"){happy_subset <- happy |> filter(country_name == "Brazil")}
      if (input$country2 == "Bulgaria"){happy_subset <- happy |> filter(country_name == "Bulgaria")}
      if (input$country2 == "Burkina Faso"){happy_subset <- happy |> filter(country_name == "Burkina Faso")}
      if (input$country2 == "Burundi"){happy_subset <- happy |> filter(country_name == "Burundi")}
      if (input$country2 == "Cambodia"){happy_subset <- happy |> filter(country_name == "Cambodia")}
      if (input$country2 == "Cameroon"){happy_subset <- happy |> filter(country_name == "Cameroon")}
      if (input$country2 == "Canada"){happy_subset <- happy |> filter(country_name == "Canada")}
      if (input$country2 == "Central African Republic"){happy_subset <- happy |> filter(country_name == "Central African Republic")}
      if (input$country2 == "Chad"){happy_subset <- happy |> filter(country_name == "Chad")}
      if (input$country2 == "Chile"){happy_subset <- happy |> filter(country_name == "Chile")}
      if (input$country2 == "China"){happy_subset <- happy |> filter(country_name == "China")}
      if (input$country2 == "Colombia"){happy_subset <- happy |> filter(country_name == "Colombia")}
      if (input$country2 == "Comoros"){happy_subset <- happy |> filter(country_name == "Comoros")}
      if (input$country2 == "Congo (Brazzaville)"){happy_subset <- happy |> filter(country_name == "Congo (Brazzaville)")}
      if (input$country2 == "Congo (Kinshasa)"){happy_subset <- happy |> filter(country_name == "Congo (Kinshasa)")}
      if (input$country2 == "Costa Rica"){happy_subset <- happy |> filter(country_name == "Costa Rica")}
      if (input$country2 == "Croatia"){happy_subset <- happy |> filter(country_name == "Croatia")}
      if (input$country2 == "Cuba"){happy_subset <- happy |> filter(country_name == "Cuba")}
      if (input$country2 == "Cyprus"){happy_subset <- happy |> filter(country_name == "Cyprus")}
      if (input$country2 == "Czechia"){happy_subset <- happy |> filter(country_name == "Czechia")}
      if (input$country2 == "Denmark"){happy_subset <- happy |> filter(country_name == "Denmark")}
      if (input$country2 == "Djibouti"){happy_subset <- happy |> filter(country_name == "Djibouti")}
      if (input$country2 == "Dominican Republic"){happy_subset <- happy |> filter(country_name == "Dominican Republic")}
      if (input$country2 == "Ecuador"){happy_subset <- happy |> filter(country_name == "Ecuador")}
      if (input$country2 == "Egypt"){happy_subset <- happy |> filter(country_name == "Egypt")}
      if (input$country2 == "El Salvador"){happy_subset <- happy |> filter(country_name == "El Salvador")}
      if (input$country2 == "Estonia"){happy_subset <- happy |> filter(country_name == "Estonia")}
      if (input$country2 == "Eswatini"){happy_subset <- happy |> filter(country_name == "Eswatini")}
      if (input$country2 == "Ethiopia"){happy_subset <- happy |> filter(country_name == "Ethiopia")}
      if (input$country2 == "Finland"){happy_subset <- happy |> filter(country_name == "Finland")}
      if (input$country2 == "France"){happy_subset <- happy |> filter(country_name == "France")}
      if (input$country2 == "Gabon"){happy_subset <- happy |> filter(country_name == "Gabon")}
      if (input$country2 == "Gambia"){happy_subset <- happy |> filter(country_name == "Gambia")}
      if (input$country2 == "Georgia"){happy_subset <- happy |> filter(country_name == "Georgia")}
      if (input$country2 == "Germany"){happy_subset <- happy |> filter(country_name == "Germany")}
      if (input$country2 == "Ghana"){happy_subset <- happy |> filter(country_name == "Ghana")}
      if (input$country2 == "Greece"){happy_subset <- happy |> filter(country_name == "Greece")}
      if (input$country2 == "Guatemala"){happy_subset <- happy |> filter(country_name == "Guatemala")}
      if (input$country2 == "Guinea"){happy_subset <- happy |> filter(country_name == "Guinea")}
      if (input$country2 == "Guyana"){happy_subset <- happy |> filter(country_name == "Guyana")}
      if (input$country2 == "Haiti"){happy_subset <- happy |> filter(country_name == "Haiti")}
      if (input$country2 == "Honduras"){happy_subset <- happy |> filter(country_name == "Honduras")}
      if (input$country2 == "Hong Kong S.A.R. of China"){happy_subset <- happy |> filter(country_name == "Hong Kong S.A.R. of China")}
      if (input$country2 == "Hungary"){happy_subset <- happy |> filter(country_name == "Hungary")}
      if (input$country2 == "Iceland"){happy_subset <- happy |> filter(country_name == "Iceland")}
      if (input$country2 == "India"){happy_subset <- happy |> filter(country_name == "India")}
      if (input$country2 == "Indonesia"){happy_subset <- happy |> filter(country_name == "Indonesia")}
      if (input$country2 == "Iran"){happy_subset <- happy |> filter(country_name == "Iran")}
      if (input$country2 == "Iraq"){happy_subset <- happy |> filter(country_name == "Iraq")}
      if (input$country2 == "Ireland"){happy_subset <- happy |> filter(country_name == "Ireland")}
      if (input$country2 == "Israel"){happy_subset <- happy |> filter(country_name == "Israel")}
      if (input$country2 == "Italy"){happy_subset <- happy |> filter(country_name == "Italy")}
      if (input$country2 == "Ivory Coast"){happy_subset <- happy |> filter(country_name == "Ivory Coast")}
      if (input$country2 == "Jamaica"){happy_subset <- happy |> filter(country_name == "Jamaica")}
      if (input$country2 == "Japan"){happy_subset <- happy |> filter(country_name == "Japan")}
      if (input$country2 == "Jordan"){happy_subset <- happy |> filter(country_name == "Jordan")}
      if (input$country2 == "Kazakhstan"){happy_subset <- happy |> filter(country_name == "Kazakhstan")}
      if (input$country2 == "Kenya"){happy_subset <- happy |> filter(country_name == "Kenya")}
      if (input$country2 == "Kosovo"){happy_subset <- happy |> filter(country_name == "Kosovo")}
      if (input$country2 == "Kuwait"){happy_subset <- happy |> filter(country_name == "Kuwait")}
      if (input$country2 == "Kyrgyzstan"){happy_subset <- happy |> filter(country_name == "Kyrgyzstan")}
      if (input$country2 == "Laos"){happy_subset <- happy |> filter(country_name == "Laos")}
      if (input$country2 == "Latvia"){happy_subset <- happy |> filter(country_name == "Latvia")}
      if (input$country2 == "Lebanon"){happy_subset <- happy |> filter(country_name == "Lebanon")}
      if (input$country2 == "Lesotho"){happy_subset <- happy |> filter(country_name == "Lesotho")}
      if (input$country2 == "Liberia"){happy_subset <- happy |> filter(country_name == "Liberia")}
      if (input$country2 == "Libya"){happy_subset <- happy |> filter(country_name == "Libya")}
      if (input$country2 == "Lithuania"){happy_subset <- happy |> filter(country_name == "Lithuania")}
      if (input$country2 == "Luxembourg"){happy_subset <- happy |> filter(country_name == "Luxembourg")}
      if (input$country2 == "Madagascar"){happy_subset <- happy |> filter(country_name == "Madagascar")}
      if (input$country2 == "Malawi"){happy_subset <- happy |> filter(country_name == "Malawi")}
      if (input$country2 == "Malaysia"){happy_subset <- happy |> filter(country_name == "Malaysia")}
      if (input$country2 == "Maldives"){happy_subset <- happy |> filter(country_name == "Maldives")}
      if (input$country2 == "Mali"){happy_subset <- happy |> filter(country_name == "Mali")}
      if (input$country2 == "Malta"){happy_subset <- happy |> filter(country_name == "Malta")}
      if (input$country2 == "Mauritania"){happy_subset <- happy |> filter(country_name == "Mauritania")}
      if (input$country2 == "Mauritius"){happy_subset <- happy |> filter(country_name == "Mauritius")}
      if (input$country2 == "Mexico"){happy_subset <- happy |> filter(country_name == "Mexico")}
      if (input$country2 == "Moldova"){happy_subset <- happy |> filter(country_name == "Moldova")}
      if (input$country2 == "Mongolia"){happy_subset <- happy |> filter(country_name == "Mongolia")}
      if (input$country2 == "Montenegro"){happy_subset <- happy |> filter(country_name == "Montenegro")}
      if (input$country2 == "Morocco"){happy_subset <- happy |> filter(country_name == "Morocco")}
      if (input$country2 == "Mozambique"){happy_subset <- happy |> filter(country_name == "Mozambique")}
      if (input$country2 == "Myanmar"){happy_subset <- happy |> filter(country_name == "Myanmar")}
      if (input$country2 == "Namibia"){happy_subset <- happy |> filter(country_name == "Namibia")}
      if (input$country2 == "Nepal"){happy_subset <- happy |> filter(country_name == "Nepal")}
      if (input$country2 == "Netherlands"){happy_subset <- happy |> filter(country_name == "Netherlands")}
      if (input$country2 == "New Zealand"){happy_subset <- happy |> filter(country_name == "New Zealand")}
      if (input$country2 == "Nicaragua"){happy_subset <- happy |> filter(country_name == "Nicaragua")}
      if (input$country2 == "Niger"){happy_subset <- happy |> filter(country_name == "Niger")}
      if (input$country2 == "Nigeria"){happy_subset <- happy |> filter(country_name == "Nigeria")}
      if (input$country2 == "North Macedonia"){happy_subset <- happy |> filter(country_name == "North Macedonia")}
      if (input$country2 == "Norway"){happy_subset <- happy |> filter(country_name == "Norway")}
      if (input$country2 == "Oman"){happy_subset <- happy |> filter(country_name == "Oman")}
      if (input$country2 == "Pakistan"){happy_subset <- happy |> filter(country_name == "Pakistan")}
      if (input$country2 == "Panama"){happy_subset <- happy |> filter(country_name == "Panama")}
      if (input$country2 == "Paraguay"){happy_subset <- happy |> filter(country_name == "Paraguay")}
      if (input$country2 == "Peru"){happy_subset <- happy |> filter(country_name == "Peru")}
      if (input$country2 == "Philippines"){happy_subset <- happy |> filter(country_name == "Philippines")}
      if (input$country2 == "Poland"){happy_subset <- happy |> filter(country_name == "Poland")}
      if (input$country2 == "Portugal"){happy_subset <- happy |> filter(country_name == "Portugal")}
      if (input$country2 == "Qatar"){happy_subset <- happy |> filter(country_name == "Qatar")}
      if (input$country2 == "Romania"){happy_subset <- happy |> filter(country_name == "Romania")}
      if (input$country2 == "Russia"){happy_subset <- happy |> filter(country_name == "Russia")}
      if (input$country2 == "Rwanda"){happy_subset <- happy |> filter(country_name == "Rwanda")}
      if (input$country2 == "Saudi Arabia"){happy_subset <- happy |> filter(country_name == "Saudi Arabia")}
      if (input$country2 == "Senegal"){happy_subset <- happy |> filter(country_name == "Senegal")}
      if (input$country2 == "Serbia"){happy_subset <- happy |> filter(country_name == "Serbia")}
      if (input$country2 == "Sierra Leone"){happy_subset <- happy |> filter(country_name == "Sierra Leone")}
      if (input$country2 == "Singapore"){happy_subset <- happy |> filter(country_name == "Singapore")}
      if (input$country2 == "Slovakia"){happy_subset <- happy |> filter(country_name == "Slovakia")}
      if (input$country2 == "Slovenia"){happy_subset <- happy |> filter(country_name == "Slovenia")}
      if (input$country2 == "Somalia"){happy_subset <- happy |> filter(country_name == "Somalia")}
      if (input$country2 == "Somaliland region"){happy_subset <- happy |> filter(country_name == "Somaliland region")}
      if (input$country2 == "South Africa"){happy_subset <- happy |> filter(country_name == "South Africa")}
      if (input$country2 == "South Korea"){happy_subset <- happy |> filter(country_name == "South Korea")}
      if (input$country2 == "South Sudan"){happy_subset <- happy |> filter(country_name == "South Sudan")}
      if (input$country2 == "Spain"){happy_subset <- happy |> filter(country_name == "Spain")}
      if (input$country2 == "Sri Lanka"){happy_subset <- happy |> filter(country_name == "Sri Lanka")}
      if (input$country2 == "State of Palestine"){happy_subset <- happy |> filter(country_name == "State of Palestine")}
      if (input$country2 == "Sudan"){happy_subset <- happy |> filter(country_name == "Sudan")}
      if (input$country2 == "Suriname"){happy_subset <- happy |> filter(country_name == "Suriname")}
      if (input$country2 == "Sweden"){happy_subset <- happy |> filter(country_name == "Sweden")}
      if (input$country2 == "Switzerland"){happy_subset <- happy |> filter(country_name == "Switzerland")}
      if (input$country2 == "Syria"){happy_subset <- happy |> filter(country_name == "Syria")}
      if (input$country2 == "Taiwan Province of China"){happy_subset <- happy |> filter(country_name == "Taiwan Province of China")}
      if (input$country2 == "Tajikistan"){happy_subset <- happy |> filter(country_name == "Tajikistan")}
      if (input$country2 == "Tanzania"){happy_subset <- happy |> filter(country_name == "Tanzania")}
      if (input$country2 == "Thailand"){happy_subset <- happy |> filter(country_name == "Thailand")}
      if (input$country2 == "Togo"){happy_subset <- happy |> filter(country_name == "Togo")}
      if (input$country2 == "Trinidad and Tobago"){happy_subset <- happy |> filter(country_name == "Trinidad and Tobago")}
      if (input$country2 == "Tunisia"){happy_subset <- happy |> filter(country_name == "Tunisia")}
      if (input$country2 == "Turkmenistan"){happy_subset <- happy |> filter(country_name == "Turkmenistan")}
      if (input$country2 == "Türkiye"){happy_subset <- happy |> filter(country_name == "Türkiye")}
      if (input$country2 == "Uganda"){happy_subset <- happy |> filter(country_name == "Uganda")}
      if (input$country2 == "Ukraine"){happy_subset <- happy |> filter(country_name == "Ukraine")}
      if (input$country2 == "United Arab Emirates"){happy_subset <- happy |> filter(country_name == "United Arab Emirates")}
      if (input$country2 == "United Kingdom"){happy_subset <- happy |> filter(country_name == "United Kingdom")}
      if (input$country2 == "United States"){happy_subset <- happy |> filter(country_name == "United States")}
      if (input$country2 == "Uruguay"){happy_subset <- happy |> filter(country_name == "Uruguay")}
      if (input$country2 == "Uzbekistan"){happy_subset <- happy |> filter(country_name == "Uzbekistan")}
      if (input$country2 == "Venezuela"){happy_subset <- happy |> filter(country_name == "Venezuela")}
      if (input$country2 == "Vietnam"){happy_subset <- happy |> filter(country_name == "Vietnam")}
      if (input$country2 == "Yemen"){happy_subset <- happy |> filter(country_name == "Yemen")}
      if (input$country2 == "Zambia"){happy_subset <- happy |> filter(country_name == "Zambia")}
      if (input$country2 == "Zimbabwe"){happy_subset <- happy |> filter(country_name == "Zimbabwe")}
      happy_subset
    })  
    
    x_variable2 <- reactive({
      case_when(
        input$x_var2 == "log(GDP) per Capita" ~ data2()$log_gdp_per_capita,
        input$x_var2 == "Social Support" ~ data2()$social_support,
        input$x_var2 == "Healthy Life Expectancy at Birth" ~ data2()$healthy_life_expectancy_at_birth,
        input$x_var2 == "Freedom to Make Life Choices" ~ data2()$freedom_to_make_life_choices,
        input$x_var2 == "Generosity" ~ data2()$generosity,
        input$x_var2 == "Perceptions of Corruption" ~ data2()$perceptions_of_corruption,
        input$x_var2 == "Positive Affect" ~ data2()$positive_affect,
        input$x_var2 == "Negative Affect" ~ data2()$negative_affect
      )
    })
    
        output$plot <- renderPlot({
      
      ######################################################################################################
      
      plot1 <- ggplot(data1(), aes(x = x_variable1(), y = life_ladder)) +
        geom_point(color = "black", show.legend = FALSE, size = 3) +
        geom_line() +
        theme_minimal() +
        labs(x = input$x_var,
             y = "Life Ladder",
             title = input$country1) +
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
      
      plot2 <- ggplot(data2(), aes(x = x_variable2(), y = life_ladder)) +
        geom_point(color = "black", show.legend = FALSE, size = 3) +
        geom_line() +
        theme_minimal() +
        labs(x = input$x_var2,
             y = "Life Ladder",
             title = input$country2) +
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
  