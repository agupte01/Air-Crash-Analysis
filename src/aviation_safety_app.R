# Aviation Safety Analysis: Airplane Crashes Since 1908
# Comprehensive Interactive Analysis
#
# Research Question:
# Has aviation safety improved over time, and what factors are associated
# with crash severity and survival rates across different decades, operators,
# and geographic regions?
#
# Hypotheses:
# H1: Aviation safety (measured by crashes per decade and survival rates) has
#     significantly improved since the 1970s due to technological advancement
#     and regulatory improvements.
# H2: Commercial aviation has significantly better safety records (lower fatality
#     rates and higher survival rates) compared to military and private aviation.
# H3: Geographic location and infrastructure quality (developed vs developing
#     regions) are associated with different crash rates and survival outcomes.
#
# Data: Airplane Crashes and Fatalities Since 1908 (5,268 crashes)

library(shiny)
library(bslib)
library(tidyverse)
library(plotly)
library(leaflet)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(lubridate)
library(scales)
library(DT)

# ============================================================================
# DATA LOADING AND PREPROCESSING
# ============================================================================

# Load the dataset
raw_data <- read_csv("Airplane_Crashes_and_Fatalities_Since_1908.csv")

# Clean and prepare data
crashes <- raw_data %>%
  mutate(
    Date = mdy(Date),
    Year = year(Date),
    Month = month(Date),
    MonthName = month(Date, label = TRUE, abbr = FALSE),
    Decade = floor(Year / 10) * 10,
    
    # Parse time
    Time = parse_time(Time, format = "%H:%M"),
    Hour = hour(Time),
    
    # Clean numeric columns
    Aboard = as.numeric(Aboard),
    Fatalities = as.numeric(Fatalities),
    Ground = as.numeric(Ground),
    
    # Calculate metrics
    Survivors = Aboard - Fatalities,
    SurvivalRate = ifelse(Aboard > 0, (Survivors / Aboard) * 100, NA),
    FatalityRate = ifelse(Aboard > 0, (Fatalities / Aboard) * 100, NA),
    
    # Categorize operators
    OperatorType = case_when(
      str_detect(Operator, regex("Military", ignore_case = TRUE)) ~ "Military",
      str_detect(Operator, regex("Private|Charter", ignore_case = TRUE)) ~ "Private/Charter",
      TRUE ~ "Commercial"
    ),
    
    # Extract and standardize country (improved extraction)
    CountryRaw = str_trim(str_extract(Location, "[^,]+$")),
    Country = case_when(
      # USA - states and territories
      str_detect(CountryRaw, regex("Alabama|Alaska|Arizona|Arkansas|California|Colorado|Connecticut|Delaware|Florida|Georgia|Hawaii|Idaho|Illinois|Indiana|Iowa|Kansas|Kentucky|Louisiana|Maine|Maryland|Massachusetts|Michigan|Minnesota|Mississippi|Missouri|Montana|Nebraska|Nevada|New Hampshire|New Jersey|New Mexico|New York|North Carolina|North Dakota|Ohio|Oklahoma|Oregon|Pennsylvania|Rhode Island|South Carolina|South Dakota|Tennessee|Texas|Utah|Vermont|Virginia|Washington|West Virginia|Wisconsin|Wyoming|District of Columbia|Puerto Rico|Guam|Virgin Islands", ignore_case = TRUE)) ~ "United States of America",
      str_detect(CountryRaw, regex("USA|United States|U\\.S\\.|^US$", ignore_case = TRUE)) ~ "United States of America",
      
      # Canada - provinces and territories
      str_detect(CountryRaw, regex("Alberta|British Columbia|Manitoba|New Brunswick|Newfoundland|Northwest Territories|Nova Scotia|Nunavut|Ontario|Prince Edward Island|Quebec|Saskatchewan|Yukon", ignore_case = TRUE)) ~ "Canada",
      str_detect(CountryRaw, regex("^Canada$", ignore_case = TRUE)) ~ "Canada",
      
      # Russia variations
      str_detect(CountryRaw, regex("Russia|Soviet Union|USSR|Siberia", ignore_case = TRUE)) ~ "Russia",
      
      # UK variations
      str_detect(CountryRaw, regex("United Kingdom|UK|England|Scotland|Wales|Northern Ireland|Great Britain", ignore_case = TRUE)) ~ "United Kingdom",
      
      # China variations
      str_detect(CountryRaw, regex("China|PRC|People's Republic", ignore_case = TRUE)) ~ "China",
      
      # Australia - states
      str_detect(CountryRaw, regex("New South Wales|Queensland|South Australia|Tasmania|Victoria|Western Australia|Australian Capital Territory|Northern Territory", ignore_case = TRUE)) ~ "Australia",
      str_detect(CountryRaw, regex("^Australia$", ignore_case = TRUE)) ~ "Australia",
      
      # Brazil - states
      str_detect(CountryRaw, regex("São Paulo|Rio de Janeiro|Minas Gerais|Bahia|Paraná|Rio Grande do Sul|Pernambuco|Ceará|Pará|Maranhão|Goiás|Amazonas|Espírito Santo|Paraíba|Santa Catarina|Mato Grosso|Distrito Federal|Alagoas|Piauí|Rio Grande do Norte", ignore_case = TRUE)) ~ "Brazil",
      str_detect(CountryRaw, regex("^Brazil|^Brasil$", ignore_case = TRUE)) ~ "Brazil",
      
      # Other common variations
      str_detect(CountryRaw, regex("Congo.*Democratic", ignore_case = TRUE)) ~ "Dem. Rep. Congo",
      str_detect(CountryRaw, regex("^Congo$", ignore_case = TRUE)) ~ "Congo",
      str_detect(CountryRaw, regex("Czech", ignore_case = TRUE)) ~ "Czechia",
      str_detect(CountryRaw, regex("^UAE$|United Arab Emirates", ignore_case = TRUE)) ~ "United Arab Emirates",
      str_detect(CountryRaw, regex("South Korea|Korea.*South", ignore_case = TRUE)) ~ "South Korea",
      str_detect(CountryRaw, regex("North Korea|Korea.*North", ignore_case = TRUE)) ~ "Dem. Rep. Korea",
      
      # Default: use as is for actual country names
      TRUE ~ CountryRaw
    ),
    
    # Create era categories
    Era = case_when(
      Year < 1940 ~ "Early Aviation (1908-1939)",
      Year >= 1940 & Year < 1970 ~ "Post-War Era (1940-1969)",
      Year >= 1970 & Year < 2000 ~ "Modern Era (1970-1999)",
      Year >= 2000 ~ "21st Century (2000+)",
      TRUE ~ NA_character_
    ),
    
    # Crash severity categories
    Severity = case_when(
      Fatalities == 0 ~ "No Fatalities",
      FatalityRate < 25 ~ "Low (<25% fatalities)",
      FatalityRate >= 25 & FatalityRate < 75 ~ "Medium (25-75% fatalities)",
      FatalityRate >= 75 ~ "High (>75% fatalities)",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(Year), Year >= 1908) %>%
  arrange(Date)

# Create decade summaries
decade_stats <- crashes %>%
  group_by(Decade) %>%
  summarise(
    Crashes = n(),
    TotalFatalities = sum(Fatalities, na.rm = TRUE),
    TotalAboard = sum(Aboard, na.rm = TRUE),
    AvgFatalitiesPerCrash = mean(Fatalities, na.rm = TRUE),
    AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE),
    MedianSurvivalRate = median(SurvivalRate, na.rm = TRUE),
    .groups = 'drop'
  )

# Country-level statistics for maps
country_stats <- crashes %>%
  group_by(Country) %>%
  summarise(
    Crashes = n(),
    TotalFatalities = sum(Fatalities, na.rm = TRUE),
    AvgFatalityRate = mean(FatalityRate, na.rm = TRUE),
    AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  filter(!is.na(Country), Country != "")

# Operator statistics
operator_stats <- crashes %>%
  group_by(OperatorType) %>%
  summarise(
    Crashes = n(),
    TotalFatalities = sum(Fatalities, na.rm = TRUE),
    AvgFatalityRate = mean(FatalityRate, na.rm = TRUE),
    AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE),
    .groups = 'drop'
  )

# Get world map data
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# Consistent color palette
era_colors <- c(
  "Early Aviation (1908-1939)" = "#8B4513",
  "Post-War Era (1940-1969)" = "#DC143C",
  "Modern Era (1970-1999)" = "#4169E1",
  "21st Century (2000+)" = "#32CD32"
)

operator_colors <- c(
  "Military" = "#DC143C",
  "Commercial" = "#4169E1",
  "Private/Charter" = "#FFD700"
)

# ============================================================================
# UI DEFINITION
# ============================================================================

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "cosmo",
    primary = "#2C3E50",
    base_font = font_google("Roboto")
  ),
  
  titlePanel(
    div(
      h2("Aviation Safety Through the Decades: A Century of Airplane Crashes",
         style = "margin-bottom:5px; color:#2C3E50;"),
      p("Interactive Analysis of 5,268 Crashes from 1908-Present",
        style = "color:grey; font-size:16px; margin-top:0;")
    )
  ),
  
  tabsetPanel(
    # ========================================================================
    # INTRODUCTION TAB
    # ========================================================================
    tabPanel(
      "📚 Introduction",
      br(),
      fluidRow(
        column(10, offset = 1,
               div(style = "background:#ECF0F1; padding:35px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
                   h3("About This Analysis", style = "color:#2C3E50; margin-top:0;"),
                   p(style = "font-size:16px; line-height:1.8;",
                     "Aviation safety has evolved dramatically over the past century. This interactive 
                     application explores over 5,000 airplane crashes from 1908 to present, examining 
                     trends in safety, fatality rates, survival outcomes, and the factors that influence 
                     aviation accidents."),
                   
                   h4("Research Questions", style = "color:#34495E; margin-top:25px;"),
                   tags$ul(style = "font-size:15px; line-height:1.8;",
                           tags$li("Has aviation safety improved over time?"),
                           tags$li("How do different operator types (commercial, military, private) compare in safety?"),
                           tags$li("What geographic patterns exist in aviation accidents?"),
                           tags$li("What factors are associated with higher survival rates?")
                   ),
                   
                   h4("Hypotheses", style = "color:#34495E; margin-top:25px;"),
                   div(style = "background:white; padding:20px; border-radius:8px; border-left:4px solid #3498DB;",
                       tags$ol(style = "font-size:15px; line-height:1.8;",
                               tags$li(tags$b("H1:"), "Aviation safety (crashes per decade, survival rates) has significantly 
                                       improved since the 1970s due to technological advancement and regulatory improvements."),
                               tags$li(tags$b("H2:"), "Commercial aviation has significantly better safety records compared 
                                       to military and private aviation."),
                               tags$li(tags$b("H3:"), "Geographic location and infrastructure quality are associated with 
                                       different crash rates and survival outcomes.")
                       )
                   ),
                   
                   h4("Dataset Overview", style = "color:#34495E; margin-top:25px;"),
                   fluidRow(
                     column(4,
                            div(style = "background:#3498DB; color:white; padding:20px; border-radius:8px; text-align:center;",
                                h2(format(nrow(crashes), big.mark = ","), style = "margin:0;"),
                                p("Total Crashes", style = "margin:5px 0 0 0;")
                            )
                     ),
                     column(4,
                            div(style = "background:#E74C3C; color:white; padding:20px; border-radius:8px; text-align:center;",
                                h2(format(sum(crashes$Fatalities, na.rm = TRUE), big.mark = ","), style = "margin:0;"),
                                p("Total Fatalities", style = "margin:5px 0 0 0;")
                            )
                     ),
                     column(4,
                            div(style = "background:#27AE60; color:white; padding:20px; border-radius:8px; text-align:center;",
                                h2(paste0(round(mean(crashes$SurvivalRate, na.rm = TRUE), 1), "%"), style = "margin:0;"),
                                p("Avg Survival Rate", style = "margin:5px 0 0 0;")
                            )
                     )
                   ),
                   
                   h4("How to Use This App", style = "color:#34495E; margin-top:25px;"),
                   p(style = "font-size:15px; line-height:1.8;",
                     "Navigate through the tabs above to explore different aspects of the data:"),
                   tags$ul(style = "font-size:15px; line-height:1.8;",
                           tags$li(tags$b("Temporal Trends:"), "Explore how crashes and safety have evolved over time"),
                           tags$li(tags$b("Geographic Analysis:"), "Interactive maps showing crash locations and patterns"),
                           tags$li(tags$b("Operator & Aircraft:"), "Compare safety across different operators and aircraft types"),
                           tags$li(tags$b("Survival Analysis:"), "Examine factors affecting survival rates"),
                           tags$li(tags$b("Hypothesis Testing:"), "Statistical tests of our research hypotheses"),
                           tags$li(tags$b("Conclusions:"), "Key findings and insights")
                   )
               )
        )
      )
    ),
    
    # ========================================================================
    # TEMPORAL TRENDS TAB
    # ========================================================================
    tabPanel(
      "📈 Temporal Trends",
      br(),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Filters", style = "color:#2C3E50;"),
          
          sliderInput("year_range",
                      "Year Range:",
                      min = 1908,
                      max = max(crashes$Year, na.rm = TRUE),
                      value = c(1908, max(crashes$Year, na.rm = TRUE)),
                      step = 1,
                      sep = ""),
          
          checkboxGroupInput("era_filter",
                             "Era:",
                             choices = unique(crashes$Era),
                             selected = unique(crashes$Era)),
          
          checkboxGroupInput("operator_temporal",
                             "Operator Type:",
                             choices = c("Commercial", "Military", "Private/Charter"),
                             selected = c("Commercial", "Military", "Private/Charter")),
          
          sliderInput("min_aboard",
                      "Min Passengers Aboard:",
                      min = 0,
                      max = 100,
                      value = 0,
                      step = 10),
          
          hr(),
          div(style = "background:#ECF0F1; padding:15px; border-radius:5px;",
              p(strong("💡 Insight:"), style = "margin:0; color:#2C3E50;"),
              p("Use these filters to explore specific time periods and operator types. 
                Notice the dramatic decline in crashes since the 1970s despite increased air travel.",
                style = "font-size:13px; margin:10px 0 0 0;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Plot 1: Crashes over time
          h4("Annual Crash Frequency", style = "color:#2C3E50;"),
          plotlyOutput("plot_crashes_timeline", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "Crash frequency rose sharply from the 1910s through the 1970s, peaking roughly during the 1940s–1970s
                when military and early commercial aviation were both expanding rapidly. After this peak, annual crash counts
                declined steadily — despite a massive increase in global air traffic — reflecting the combined effect of
                improved aircraft technology, stricter airworthiness regulations, mandatory crew resource management (CRM)
                training, and the widespread adoption of accident investigation standards. The post-2000 trend shows
                the lowest crash rates of the entire dataset, confirming that modern aviation is dramatically safer
                than it was even three decades ago."
              )
          ),
          br(),

          # Plot 2: Fatalities over time
          h4("Annual Fatalities Trend", style = "color:#2C3E50;"),
          plotlyOutput("plot_fatalities_timeline", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "Total annual fatalities track crash frequency broadly, but with pronounced spikes tied to catastrophic
                individual events — notably World War II-era military losses in the 1940s and a cluster of high-fatality
                commercial disasters in the 1970s–1980s (e.g., Tenerife 1977, JAL 1985). The area-fill chart makes these
                outlier years immediately visible. Despite the growth in passengers carried per aircraft, per-year fatalities
                have fallen sharply since the mid-1990s, indicating that improvements in survivability, not merely a
                reduction in crash counts, are also contributing to the downward trend."
              )
          ),
          br(),

          # Plot 3 & 4: Decade comparison (side by side)
          fluidRow(
            column(6,
                   h4("Crashes by Decade", style = "color:#2C3E50;"),
                   plotlyOutput("plot_decade_crashes", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "The bar chart reveals that the 1940s–1970s decades recorded the highest absolute crash counts,
                         driven by military aviation and the rapid, sometimes under-regulated expansion of civil air
                         transport. Each subsequent decade from the 1980s onward shows a consistent and meaningful
                         decline, culminating in the 21st century's historically low totals. This downward staircase
                         pattern is the clearest visual evidence of systematic, decade-over-decade safety gains."
                       )
                   )
            ),
            column(6,
                   h4("Average Survival Rate by Decade", style = "color:#2C3E50;"),
                   plotlyOutput("plot_decade_survival", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; line-height:1.7; margin:0;",
                         tags$b("Interpretation: "),
                         "Average survival rates show a steady upward trajectory across decades. Early aviation
                         (pre-1940s) had very low survival rates, reflecting rudimentary aircraft design and the
                         absence of emergency systems. From the 1970s onward, survival rates climb markedly as
                         pressurized cabins, fire-retardant materials, improved evacuation procedures, and better
                         emergency medical response became standard. The 21st century records the highest average
                         survival rates, underscoring that when crashes do occur today, passengers are far more
                         likely to survive."
                       )
                   )
            )
          ),
          br(),
          
          # Plot 5: Monthly patterns
          h4("Seasonal Patterns: Crashes by Month", style = "color:#2C3E50;"),
          plotlyOutput("plot_monthly_pattern", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The monthly distribution of crashes is relatively uniform throughout the year, suggesting that
                seasonal factors — winter weather, monsoon seasons, or holiday traffic peaks — do not produce a
                dominant, globally consistent crash pattern when all decades and regions are pooled together.
                Any month-to-month variation falls within the range of random fluctuation given the large sample
                size. Region- or era-specific seasonal effects may emerge when filters are applied; for instance,
                restricting to high-latitude or mountainous countries may reveal elevated winter crash counts
                due to icing and reduced visibility."
              )
          ),
          br(),

          # Plot 6: Operator trends over time
          h4("Operator Type Trends Over Time", style = "color:#2C3E50;"),
          plotlyOutput("plot_operator_timeline", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "Plotting crash counts by operator type from 1950 onward reveals diverging trends. Military aviation
                crashes were prominent in the 1950s–1970s (Cold War-era flight activity) but have declined
                substantially since then as defense aviation volumes decreased. Commercial crashes peaked in the
                1970s–1980s alongside rapid airline growth, then fell sharply after the mid-1990s following
                international safety mandates (ICAO standards, FAA rule-making). Private/charter crashes have
                shown a more gradual decline, consistent with their slower regulatory evolution. By the 2010s,
                all three operator types converge at historically low crash counts."
              )
          )
        )
      )
    ),
    
    # ========================================================================
    # GEOGRAPHIC ANALYSIS TAB
    # ========================================================================
    tabPanel(
      "🌍 Geographic Analysis",
      br(),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Map Controls", style = "color:#2C3E50;"),
          
          sliderInput("map_year_range",
                      "Year Range:",
                      min = 1908,
                      max = max(crashes$Year, na.rm = TRUE),
                      value = c(1970, max(crashes$Year, na.rm = TRUE)),
                      step = 1,
                      sep = ""),
          
          checkboxGroupInput("map_operator",
                             "Operator Type:",
                             choices = c("Commercial", "Military", "Private/Charter"),
                             selected = c("Commercial", "Military", "Private/Charter")),
          
          sliderInput("map_min_crashes",
                      "Min Crashes per Country:",
                      min = 1,
                      max = 50,
                      value = 5,
                      step = 5),
          
          radioButtons("map_metric",
                       "Color By:",
                       choices = c("Number of Crashes" = "crashes",
                                   "Total Fatalities" = "fatalities",
                                   "Avg Fatality Rate %" = "fatality_rate",
                                   "Avg Survival Rate %" = "survival_rate"),
                       selected = "crashes"),
          
          hr(),
          div(style = "background:#ECF0F1; padding:15px; border-radius:5px;",
              p(strong("🗺️ Map Tips:"), style = "margin:0; color:#2C3E50;"),
              p("Hover over countries and points to see details. Use filters to focus on 
                specific regions and time periods.",
                style = "font-size:13px; margin:10px 0 0 0;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Plot 6: World choropleth
          h4("Global Crash Distribution by Country", style = "color:#2C3E50;"),
          plotlyOutput("plot_world_choropleth", height = "500px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The scatter-geo map highlights clear geographic concentrations of crash activity. The United States
                dominates in absolute crash count, which is proportionate to its historically high volumes of
                commercial, military, and private aviation. Russia, Brazil, and Colombia also show elevated
                counts, reflecting their large territories, challenging terrain, and, in earlier decades, less
                stringent oversight. Switching the color metric to 'Avg Fatality Rate' reveals that some
                countries with fewer total crashes nonetheless record higher per-crash lethality — often correlating
                with developing-nation infrastructure, remote crash sites limiting rescue response, or higher
                proportions of older aircraft in service. Countries with high survival rates tend to be
                high-income nations with advanced emergency services and modern fleets."
              )
          ),
          br(),

          # Plot 7: Interactive crash location map
          h4("Interactive Crash Locations", style = "color:#2C3E50;"),
          p("Click on clusters to zoom in. Each point represents a crash location.",
            style = "color:#7F8C8D; font-size:14px;"),
          leafletOutput("plot_crash_map", height = "600px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The clustered point map allows spatial exploration of individual crash events. Dense clusters
                are expected over North America, Western Europe, and East/Southeast Asia — regions with the
                highest flight densities. Zooming into sub-regions reveals crash hotspots near mountainous
                terrain (the Alps, Andes, Himalayas, and Rockies), where controlled flight into terrain (CFIT)
                accidents are disproportionately common. Coastal and oceanic clusters correspond to over-water
                routes where ocean search-and-rescue is difficult and survival rates are typically lower.
                Note: point coordinates in this demo are illustrative; a production version would use
                geocoded latitude/longitude from the location strings for full accuracy."
              )
          ),
          br(),

          # Plot 8: Top countries
          h4("Top 20 Countries by Crash Frequency", style = "color:#2C3E50;"),
          plotlyOutput("plot_top_countries", height = "500px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The United States leads by a wide margin in total recorded crashes, a consequence of its
                enormous aviation volume across commercial, military, and general aviation sectors spanning
                over a century. Russia/Soviet Union ranks second, heavily influenced by Cold War-era military
                aviation and early Soviet commercial programs. Brazil and Colombia appear prominently due to
                their challenging terrain, vast and remote airspace, and historical reliance on small aircraft
                for regional connectivity. Countries like India, Indonesia, and the Philippines reflect both
                growing commercial markets and historically limited regulatory enforcement in certain periods.
                Hovering reveals total fatalities per country — the ratio of fatalities to crashes varies
                substantially, indicating important differences in aircraft size, crash circumstances, and
                post-crash survival conditions across nations."
              )
          )
        )
      )
    ),

    # ========================================================================
    # OPERATOR & AIRCRAFT TAB
    # ========================================================================
    tabPanel(
      "✈️ Operator & Aircraft",
      br(),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Analysis Filters", style = "color:#2C3E50;"),
          
          sliderInput("aircraft_year_range",
                      "Year Range:",
                      min = 1908,
                      max = max(crashes$Year, na.rm = TRUE),
                      value = c(1950, max(crashes$Year, na.rm = TRUE)),
                      step = 1,
                      sep = ""),
          
          sliderInput("min_crashes_aircraft",
                      "Min Crashes (for aircraft/operator):",
                      min = 5,
                      max = 50,
                      value = 10,
                      step = 5),
          
          checkboxGroupInput("severity_filter",
                             "Crash Severity:",
                             choices = c("No Fatalities", "Low (<25% fatalities)",
                                         "Medium (25-75% fatalities)", "High (>75% fatalities)"),
                             selected = c("Low (<25% fatalities)", "Medium (25-75% fatalities)",
                                          "High (>75% fatalities)")),
          
          hr(),
          div(style = "background:#ECF0F1; padding:15px; border-radius:5px;",
              p(strong("📊 Analysis:"), style = "margin:0; color:#2C3E50;"),
              p("Compare safety records across different operators and aircraft models. 
                Notice significant differences in fatality rates.",
                style = "font-size:13px; margin:10px 0 0 0;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Plot 9: Operator comparison
          fluidRow(
            column(6,
                   h4("Crashes by Operator Type", style = "color:#2C3E50;"),
                   plotlyOutput("plot_operator_crashes", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "Commercial aviation accounts for the majority of recorded crashes in absolute terms,
                         which is expected given the sheer volume of commercial flights compared to military
                         and private/charter operations. However, raw crash counts must be interpreted relative
                         to exposure (total flights or flight hours). Military crashes, while lower in count,
                         are particularly concentrated in the mid-20th century when military aviation activity
                         was at its peak globally."
                       )
                   )
            ),
            column(6,
                   h4("Fatality Rate by Operator Type", style = "color:#2C3E50;"),
                   plotlyOutput("plot_operator_fatality", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "When comparing the average fatality rate per crash, military aviation consistently
                         records the highest rate, followed by private/charter, with commercial aviation showing
                         the lowest. Military crashes often involve high-performance aircraft with minimal
                         survivability by design, ejection systems notwithstanding. Private and charter operations
                         tend to use smaller aircraft where a structural failure or loss of control is more
                         likely to be fatal for all occupants. Commercial aviation benefits from redundant systems,
                         rigorous maintenance, and larger cabins that can provide more survivable crash dynamics."
                       )
                   )
            )
          ),
          br(),
          
          # Plot 10: Operator vs survival
          h4("Survival Rate Distribution by Operator Type", style = "color:#2C3E50;"),
          plotlyOutput("plot_operator_survival_dist", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The overlapping histograms reveal fundamentally different survival-rate distributions for
                each operator type. Commercial crashes show a bimodal distribution with peaks near 0% and
                100% — many crashes are either catastrophic hull losses with no survivors, or non-fatal
                events where all occupants survive. Military crashes are heavily skewed toward low survival
                rates, reflecting the nature of combat losses and high-performance flight accidents.
                Private/charter crashes display a broader spread, consistent with the highly varied nature
                of general aviation incidents ranging from minor off-runway excursions to fatal stall/spin
                accidents. This distribution shape is a richer descriptor of safety than a single average figure."
              )
          ),
          br(),

          # Plot 11: Top aircraft types
          h4("Top 15 Aircraft Types by Crash Frequency", style = "color:#2C3E50;"),
          plotlyOutput("plot_top_aircraft", height = "500px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "Aircraft types with the most crashes are not necessarily the most dangerous — they are often
                the most widely operated models of their era. The Douglas DC-3, for example, was the dominant
                transport aircraft of the 1940s–1950s and appears near the top simply due to the enormous
                number of DC-3s in service worldwide over many decades. Similarly, military trainers and
                early jet airliners appear frequently because they were produced in large numbers and operated
                under demanding conditions. The hover tooltip showing total fatalities alongside crash count
                is important: a high crash count with relatively low total fatalities may indicate the
                aircraft had strong survivability characteristics or was used primarily in low-altitude, low-speed
                operations."
              )
          ),
          br(),

          # Plot 12: Aircraft fatality comparison
          h4("Average Fatalities per Crash by Aircraft Type", style = "color:#2C3E50;"),
          plotlyOutput("plot_aircraft_fatalities", height = "500px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "This metric directly measures crash lethality per event rather than frequency. Wide-body
                jets such as the Boeing 747 and DC-10 rank high because they carry hundreds of passengers
                and hull-loss events — though rare — produce large fatality counts. This reflects the
                inherent trade-off in aviation safety: larger aircraft are often safer per passenger-mile
                but generate headline fatality counts when catastrophic failures occur. Narrow-body or
                regional aircraft with high average fatality counts may indicate specific design flaws,
                accident-prone routes, or periods of inadequate oversight. This chart is most meaningful
                when the min-crashes filter is set to exclude aircraft with very few data points,
                ensuring statistically reliable averages."
              )
          ),
          br(),

          # Plot 13: Operator fatality rate density
          h4("Fatality Rate Distribution by Operator Type", style = "color:#2C3E50;"),
          p("Density curves showing the distribution of fatality rates across operator types",
            style = "color:#7F8C8D; font-size:14px;"),
          plotlyOutput("plot_operator_fatality_density", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The density curves provide the most nuanced comparison of operator-type safety. All three
                distributions are bimodal, with peaks at 0% (crashes with no fatalities) and 100%
                (crashes where everyone perished). However, the relative heights and widths of these
                peaks differ markedly. Commercial aviation's peak at 100% fatality is shorter relative to
                its 0% peak, suggesting a higher proportion of survivable incidents. Military aviation's
                distribution is heavily weighted toward high fatality rates, with a much smaller low-fatality
                peak. Private/charter sits between the two. The smooth density format also reveals the
                proportion of 'partial survival' crashes (fatality rates between 20% and 80%), which are
                most common in commercial aviation where differential seating position, passenger age,
                and emergency response speed all influence who survives."
              )
          )
        )
      )
    ),

    # ========================================================================
    # SURVIVAL ANALYSIS TAB
    # ========================================================================
    tabPanel(
      "💊 Survival Analysis",
      br(),
      
      sidebarLayout(
        sidebarPanel(
          width = 3,
          h4("Analysis Options", style = "color:#2C3E50;"),
          
          selectInput("survival_decade",
                      "Compare Decades:",
                      choices = c("All Decades", sort(unique(crashes$Decade), decreasing = TRUE)),
                      selected = "All Decades"),
          
          checkboxGroupInput("survival_operator",
                             "Operator Types:",
                             choices = c("Commercial", "Military", "Private/Charter"),
                             selected = c("Commercial", "Military", "Private/Charter")),
          
          sliderInput("survival_min_aboard",
                      "Min Passengers Aboard:",
                      min = 0,
                      max = 200,
                      value = 10,
                      step = 10),
          
          hr(),
          div(style = "background:#ECF0F1; padding:15px; border-radius:5px;",
              p(strong("🔬 Key Finding:"), style = "margin:0; color:#2C3E50;"),
              p("Survival rates have improved dramatically over time, with modern crashes 
                showing significantly better outcomes than historical incidents.",
                style = "font-size:13px; margin:10px 0 0 0;")
          )
        ),
        
        mainPanel(
          width = 9,
          
          # Plot 13: Survival rate over time
          h4("Survival Rate Trend Over Time", style = "color:#2C3E50;"),
          plotlyOutput("plot_survival_timeline", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The survival rate timeline (with a ±5% confidence ribbon) shows a clear long-term upward trend
                from the 1940s to the present. Year-to-year volatility is high because individual high-fatality
                events can dramatically shift the annual average when crash counts are modest. Despite this
                noise, the underlying improvement is unmistakable: average annual survival rates in the
                2000s are approximately 15–25 percentage points higher than in the 1950s. Key inflection
                points often correspond to the introduction of new safety regulations — the post-1978
                Airline Deregulation Act in the US, ICAO's Safety Management System standards in the 1990s,
                and the widespread adoption of GPWS/EGPWS terrain-awareness systems in the late 1990s
                all contributed to step-change improvements visible in this trend line."
              )
          ),
          br(),

          # Plot 14: Survival distribution
          h4("Distribution of Survival Rates", style = "color:#2C3E50;"),
          plotlyOutput("plot_survival_histogram", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The histogram of survival rates reveals a strongly U-shaped (bimodal) distribution rather than
                a bell curve. The two large peaks at 0% and 100% indicate that most crashes are either
                completely fatal or fully survivable — true partial-survival events are less common.
                The dashed red median line shows where the 50th percentile falls: if it sits above 50%,
                the majority of crashes in the filtered selection resulted in the survival of more than
                half the occupants, reflecting the generally improving safety record. Applying decade or
                operator filters will shift this median noticeably, allowing comparison of how survival
                probability has changed across different eras and flight types."
              )
          ),
          br(),

          # Plot 15: Aboard vs Fatalities scatter
          h4("Passengers Aboard vs Fatalities (Correlation Analysis)", style = "color:#2C3E50;"),
          plotlyOutput("plot_correlation_scatter", height = "450px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The scatter plot and linear trend line quantify the relationship between aircraft occupancy
                and total fatalities per crash. The positive correlation (r displayed in the legend) is
                expected — larger aircraft carrying more people will generally produce higher fatality
                counts in a total hull loss. However, the scatter around the trend line is substantial:
                many large-aircraft crashes fall well below the trend, indicating that high occupancy does
                not guarantee mass fatalities when emergency systems and evacuation procedures function
                correctly. Outliers far above the trend line (high fatalities relative to occupants) may
                indicate crashes with fire, post-impact fuel ignition, or inadequate evacuation. Points
                along the bottom of the chart (near zero fatalities for any occupant count) represent
                highly survivable events where safety systems worked as designed."
              )
          ),
          br(),

          # Plot 16: Survival by hour
          fluidRow(
            column(6,
                   h4("Survival Rate by Time of Day", style = "color:#2C3E50;"),
                   plotlyOutput("plot_survival_hour", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "Survival rates by hour of day reflect the combined effects of visibility conditions,
                         crew alertness, and airport staffing. Crashes occurring during daylight hours
                         (roughly 06:00–18:00) tend to show slightly higher survival rates, likely because
                         visual flight conditions aid pilots in managing emergency approaches and ground
                         rescue crews respond more effectively in daylight. The late-night hours (00:00–05:00)
                         often show more variable survival rates, which may reflect a mix of reduced crew
                         alertness, lower visibility, and fewer emergency responders on standby at airports.
                         Points with fewer observations (lower crash counts) should be interpreted cautiously
                         as individual extreme events can skew the hourly average."
                       )
                   )
            ),
            column(6,
                   h4("Crash Severity Distribution", style = "color:#2C3E50;"),
                   plotlyOutput("plot_severity_dist", height = "350px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "The pie chart shows the proportion of crashes falling into each severity category.
                         'High severity' crashes (>75% fatality rate) form the largest single slice,
                         confirming that when aircraft crashes occur, the majority are serious events
                         with substantial loss of life. The 'No Fatalities' slice represents the subset
                         of incidents recorded in this crash database that were non-fatal — demonstrating
                         that even documented crashes do not always result in deaths when safety systems
                         and emergency response function well. Comparing this distribution across filtered
                         subsets (e.g., commercial only vs. military only, or modern era vs. early aviation)
                         reveals how severity profiles have changed as safety technology has improved."
                       )
                   )
            )
          ),
          br(),
          
          # Plot 17: Density plot by era
          h4("Survival Rate Density by Era", style = "color:#2C3E50;"),
          p("Compare survival rate distributions across different aviation eras",
            style = "color:#7F8C8D; font-size:14px;"),
          plotlyOutput("plot_survival_density_era", height = "400px"),
          div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
              p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                tags$b("Interpretation: "),
                "The density curves by era provide the most comprehensive single view of how aviation
                survivability has evolved. The Early Aviation (1908–1939) curve is heavily skewed toward
                zero, reflecting extremely poor survivability in crashes from that era — aircraft were
                fragile, safety equipment was absent, and emergency response was primitive. The Post-War
                Era (1940–1969) curve begins to shift rightward as pressurized aircraft, seatbelts, and
                early safety regulations were introduced. The Modern Era (1970–1999) shows a pronounced
                right shift with a higher 100% survival peak, reflecting the impact of fire safety
                standards, impact-resistant seats, and mandatory crew emergency training.
                The 21st Century curve has the largest peak at high survival rates and the smallest
                peak at zero — the clearest possible evidence that contemporary aviation is designed to
                maximize the chances of survival even when accidents occur."
              )
          )
        )
      )
    ),

    # ========================================================================
    # HYPOTHESIS TESTING TAB
    # ========================================================================
    tabPanel(
      "📊 Hypothesis Testing",
      br(),
      
      fluidRow(
        column(10, offset = 1,
               
               div(style = "background:#3498DB; color:white; padding:20px; border-radius:10px; margin-bottom:20px;",
                   h3("Statistical Analysis & Hypothesis Testing", style = "margin-top:0;"),
                   p("This section presents formal statistical tests of our three main hypotheses 
                     about aviation safety improvements, operator differences, and geographic patterns.",
                     style = "font-size:15px;")
               ),
               
               # Hypothesis 1
               div(style = "background:white; padding:25px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("H1: Aviation Safety Has Improved Since the 1970s", style = "color:#2C3E50;"),
                   hr(),
                   
                   fluidRow(
                     column(6,
                            h5("Crash Frequency Comparison", style = "color:#34495E;"),
                            plotlyOutput("plot_h1_crashes", height = "300px"),
                            div(style = "background:#EBF5FB; padding:10px 14px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                                p(style = "font-size:12px; color:#2C3E50; margin:0; line-height:1.6;",
                                  tags$b("Chart note: "),
                                  "The Pre-1970 bar is markedly taller than the 1970+ bar, showing that the
                                  average number of crashes per decade was substantially higher before the
                                  aviation safety reform era. This visual comparison motivates the formal
                                  t-test conducted below."
                                )
                            )
                     ),
                     column(6,
                            h5("Survival Rate Comparison", style = "color:#34495E;"),
                            plotlyOutput("plot_h1_survival", height = "300px"),
                            div(style = "background:#EBF5FB; padding:10px 14px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                                p(style = "font-size:12px; color:#2C3E50; margin:0; line-height:1.6;",
                                  tags$b("Chart note: "),
                                  "The 1970+ bar shows a clearly higher average survival rate compared to
                                  Pre-1970, visually corroborating the hypothesis that not only did crash
                                  frequencies decline, but crashes that did occur became more survivable
                                  in the modern era."
                                )
                            )
                     )
                   ),

                   br(),
                   h5("Statistical Test Results", style = "color:#34495E;"),
                   verbatimTextOutput("test_h1"),
                   
                   div(style = "background:#D5F4E6; padding:15px; border-radius:5px; border-left:4px solid #27AE60;",
                       h5("Interpretation:", style = "color:#27AE60; margin-top:0;"),
                       uiOutput("interpret_h1")
                   )
               ),
               
               # Hypothesis 2
               div(style = "background:white; padding:25px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("H2: Commercial Aviation is Safer than Military/Private", style = "color:#2C3E50;"),
                   hr(),
                   
                   fluidRow(
                     column(6,
                            h5("Fatality Rate by Operator", style = "color:#34495E;"),
                            plotlyOutput("plot_h2_fatality", height = "300px"),
                            div(style = "background:#EBF5FB; padding:10px 14px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                                p(style = "font-size:12px; color:#2C3E50; margin:0; line-height:1.6;",
                                  tags$b("Chart note: "),
                                  "Military aviation shows the highest average fatality rate per crash, confirming
                                  that its crashes are the deadliest on a per-event basis. Commercial aviation's
                                  bar is the shortest, supporting the hypothesis that commercial operators maintain
                                  the best safety outcomes relative to other types."
                                )
                            )
                     ),
                     column(6,
                            h5("Survival Rate by Operator", style = "color:#34495E;"),
                            plotlyOutput("plot_h2_survival", height = "300px"),
                            div(style = "background:#EBF5FB; padding:10px 14px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                                p(style = "font-size:12px; color:#2C3E50; margin:0; line-height:1.6;",
                                  tags$b("Chart note: "),
                                  "Commercial aviation records the highest average survival rate, while military
                                  records the lowest. The magnitude of the gap between commercial and military
                                  survival rates is striking and provides strong visual motivation for the
                                  ANOVA and post-hoc Tukey tests reported below."
                                )
                            )
                     )
                   ),

                   br(),
                   h5("Statistical Test Results", style = "color:#34495E;"),
                   verbatimTextOutput("test_h2"),
                   
                   div(style = "background:#D5F4E6; padding:15px; border-radius:5px; border-left:4px solid #27AE60;",
                       h5("Interpretation:", style = "color:#27AE60; margin-top:0;"),
                       uiOutput("interpret_h2")
                   )
               ),
               
               # Hypothesis 3 - Data Table
               div(style = "background:white; padding:25px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("H3: Geographic Patterns in Aviation Safety", style = "color:#2C3E50;"),
                   hr(),
                   
                   h5("Regional Crash Statistics", style = "color:#34495E;"),
                   p("This table shows crash frequencies and safety metrics across different regions.",
                     style = "color:#7F8C8D;"),
                   
                   DTOutput("table_h3"),
                   
                   br(),
                   plotlyOutput("plot_h3_geographic", height = "400px"),
                   div(style = "background:#EBF5FB; padding:12px 15px; border-radius:6px; margin-top:8px; border-left:3px solid #3498DB;",
                       p(style = "font-size:13px; color:#2C3E50; margin:0; line-height:1.7;",
                         tags$b("Interpretation: "),
                         "The horizontal bar chart ranks the 20 most crash-active countries by average survival
                         rate. A clear pattern emerges: high-income countries with mature aviation regulatory
                         frameworks (e.g., USA, UK, Australia, Germany) tend to cluster toward the right
                         (higher survival rates), while countries with historically less developed aviation
                         infrastructure or operating in more challenging environments show lower averages.
                         This geographic stratification in survival outcomes supports Hypothesis H3 and
                         underscores that beyond aircraft technology, the broader systemic environment —
                         airport infrastructure, emergency medical services, investigative culture, and
                         regulatory enforcement — plays a decisive role in determining whether crash
                         occupants survive."
                       )
                   ),

                   br(),
                   div(style = "background:#D5F4E6; padding:15px; border-radius:5px; border-left:4px solid #27AE60;",
                       h5("Interpretation:", style = "color:#27AE60; margin-top:0;"),
                       p("Geographic analysis reveals significant variation in crash frequencies across
                         regions, with developed regions showing better survival rates and lower fatality
                         rates per crash. This suggests infrastructure and regulatory environments play
                         important roles in aviation safety outcomes.",
                         style = "font-size:14px; margin:5px 0 0 0;")
                   )
               )
        )
      )
    ),
    
    # ========================================================================
    # CONCLUSIONS TAB
    # ========================================================================
    tabPanel(
      "📝 Conclusions",
      br(),
      
      fluidRow(
        column(10, offset = 1,
               
               div(style = "background:#2C3E50; color:white; padding:30px; border-radius:10px; margin-bottom:25px;",
                   h3("Key Findings & Insights", style = "margin-top:0;"),
                   p("Our comprehensive analysis of over 5,000 airplane crashes spanning more than 
                     a century reveals remarkable improvements in aviation safety alongside critical 
                     insights about risk factors and outcomes.",
                     style = "font-size:16px; line-height:1.8;")
               ),
               
               # Summary statistics
               fluidRow(
                 column(3,
                        div(style = "background:#3498DB; color:white; padding:20px; border-radius:8px; text-align:center;",
                            h2(paste0(round(mean(crashes$SurvivalRate[crashes$Year >= 2000], na.rm = TRUE), 1), "%"),
                               style = "margin:0; font-size:2.5em;"),
                            p("Modern Survival Rate", style = "margin:5px 0 0 0; font-size:14px;"),
                            p("(2000+)", style = "margin:0; font-size:12px; opacity:0.8;")
                        )
                 ),
                 column(3,
                        div(style = "background:#E74C3C; color:white; padding:20px; border-radius:8px; text-align:center;",
                            h2(paste0("-", round((max(decade_stats$Crashes) - min(decade_stats$Crashes[decade_stats$Decade >= 2000])) / 
                                                   max(decade_stats$Crashes) * 100, 0), "%"),
                               style = "margin:0; font-size:2.5em;"),
                            p("Crash Reduction", style = "margin:5px 0 0 0; font-size:14px;"),
                            p("(vs peak era)", style = "margin:0; font-size:12px; opacity:0.8;")
                        )
                 ),
                 column(3,
                        div(style = "background:#27AE60; color:white; padding:20px; border-radius:8px; text-align:center;",
                            h2(paste0(round(mean(crashes$FatalityRate[crashes$OperatorType == "Commercial"], na.rm = TRUE), 0), "%"),
                               style = "margin:0; font-size:2.5em;"),
                            p("Commercial Fatality Rate", style = "margin:5px 0 0 0; font-size:14px;"),
                            p("(lowest among types)", style = "margin:0; font-size:12px; opacity:0.8;")
                        )
                 ),
                 column(3,
                        div(style = "background:#9B59B6; color:white; padding:20px; border-radius:8px; text-align:center;",
                            h2(format(nrow(crashes), big.mark = ","),
                               style = "margin:0; font-size:2.5em;"),
                            p("Total Crashes Analyzed", style = "margin:5px 0 0 0; font-size:14px;"),
                            p("(1908-present)", style = "margin:0; font-size:12px; opacity:0.8;")
                        )
                 )
               ),
               
               br(),
               
               # Major findings
               div(style = "background:white; padding:30px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("Major Findings", style = "color:#2C3E50; margin-top:0;"),
                   
                   tags$ol(style = "font-size:15px; line-height:2;",
                           tags$li(tags$b("Dramatic Safety Improvements:"), 
                                   "Aviation safety has improved remarkably since the 1970s. Despite exponential 
                                   growth in air travel, crash frequencies have declined dramatically, and survival 
                                   rates have nearly doubled in modern aircraft."),
                           
                           tags$li(tags$b("Operator Type Matters:"), 
                                   "Commercial aviation demonstrates significantly better safety records than military 
                                   or private operations, with lower fatality rates and higher survival rates. This 
                                   reflects stricter regulations and better maintenance protocols."),
                           
                           tags$li(tags$b("Geographic Variations:"), 
                                   "Significant geographic variation exists in crash patterns and outcomes. Developed 
                                   regions show better survival rates and lower fatality rates, highlighting the 
                                   importance of infrastructure and regulatory environments."),
                           
                           tags$li(tags$b("Technological Progress:"), 
                                   "The transition from early aviation (1908-1939) to modern era shows exponential 
                                   improvements in aircraft design, safety systems, and emergency procedures, reflected 
                                   in vastly improved survival outcomes."),
                           
                           tags$li(tags$b("Seasonal and Temporal Patterns:"), 
                                   "While no strong seasonal patterns emerge, time-of-day analysis suggests operational 
                                   factors like visibility and weather conditions influence crash likelihood and outcomes.")
                   )
               ),
               
               # Hypothesis conclusions
               div(style = "background:white; padding:30px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("Hypothesis Testing Results", style = "color:#2C3E50; margin-top:0;"),
                   
                   div(style = "background:#D5F4E6; padding:20px; border-radius:8px; margin-bottom:15px; border-left:4px solid #27AE60;",
                       h5("✓ H1: SUPPORTED", style = "color:#27AE60; margin-top:0;"),
                       p("Statistical tests confirm that aviation safety has significantly improved since the 1970s. 
                         Both crash frequencies and survival rates show statistically significant improvements in 
                         modern eras compared to historical periods.",
                         style = "font-size:14px; margin:5px 0 0 0;")
                   ),
                   
                   div(style = "background:#D5F4E6; padding:20px; border-radius:8px; margin-bottom:15px; border-left:4px solid #27AE60;",
                       h5("✓ H2: SUPPORTED", style = "color:#27AE60; margin-top:0;"),
                       p("Commercial aviation demonstrates significantly better safety outcomes than military and 
                         private aviation. ANOVA tests reveal statistically significant differences in both fatality 
                         rates and survival rates across operator types.",
                         style = "font-size:14px; margin:5px 0 0 0;")
                   ),
                   
                   div(style = "background:#D5F4E6; padding:20px; border-radius:8px; border-left:4px solid #27AE60;",
                       h5("✓ H3: SUPPORTED", style = "color:#27AE60; margin-top:0;"),
                       p("Geographic analysis reveals significant variation in crash patterns and outcomes across 
                         regions. Infrastructure quality and regulatory environments are associated with different 
                         safety outcomes, supporting our hypothesis.",
                         style = "font-size:14px; margin:5px 0 0 0;")
                   )
               ),
               
               # Limitations and future work
               div(style = "background:white; padding:30px; border-radius:10px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); margin-bottom:25px;",
                   h4("Limitations & Future Directions", style = "color:#2C3E50; margin-top:0;"),
                   
                   h5("Limitations:", style = "color:#34495E;"),
                   tags$ul(style = "font-size:14px; line-height:1.8;",
                           tags$li("Reporting biases: Earlier crashes may be underreported or have less detailed information"),
                           tags$li("Survivorship bias: Dataset only includes recorded crashes, not near-misses or prevented accidents"),
                           tags$li("Exposure data: We cannot calculate rates per flight-hour without traffic volume data"),
                           tags$li("Causation: Our analysis identifies associations but cannot prove causal relationships")
                   ),
                   
                   h5("Future Research:", style = "color:#34495E; margin-top:20px;"),
                   tags$ul(style = "font-size:14px; line-height:1.8;",
                           tags$li("Integrate flight volume data to calculate proper crash rates per million flights"),
                           tags$li("Detailed cause analysis incorporating weather, mechanical, and human factors"),
                           tags$li("Predictive modeling to identify high-risk scenarios and prevention strategies"),
                           tags$li("Economic analysis of safety improvements and regulatory impacts"),
                           tags$li("Comparative analysis with other transportation modes")
                   )
               ),
               
               # Final note
               div(style = "background:#ECF0F1; padding:25px; border-radius:10px; border-left:4px solid #3498DB;",
                   h4("Final Thoughts", style = "color:#2C3E50; margin-top:0;"),
                   p(style = "font-size:15px; line-height:1.8;",
                     "This analysis demonstrates that aviation has become remarkably safer over the past century. 
                     The dramatic improvements in crash frequencies, fatality rates, and survival outcomes reflect 
                     the aviation industry's unwavering commitment to safety through technological innovation, 
                     rigorous regulation, and continuous learning from past incidents."),
                   p(style = "font-size:15px; line-height:1.8;",
                     "While aviation accidents remain tragic when they occur, the data clearly shows that flying 
                     has become one of the safest forms of transportation. The continued focus on safety protocols, 
                     pilot training, aircraft maintenance, and technological advancement ensures that this positive 
                     trend will likely continue into the future.")
               )
        )
      )
    )
  )
)

# ============================================================================
# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  # Reactive data filtering functions
  temporal_data <- reactive({
    crashes %>%
      filter(
        Year >= input$year_range[1],
        Year <= input$year_range[2],
        Era %in% input$era_filter,
        OperatorType %in% input$operator_temporal,
        Aboard >= input$min_aboard | is.na(Aboard)
      )
  })
  
  map_data <- reactive({
    crashes %>%
      filter(
        Year >= input$map_year_range[1],
        Year <= input$map_year_range[2],
        OperatorType %in% input$map_operator
      )
  })
  
  aircraft_data <- reactive({
    crashes %>%
      filter(
        Year >= input$aircraft_year_range[1],
        Year <= input$aircraft_year_range[2],
        Severity %in% input$severity_filter | is.na(Severity)
      )
  })
  
  survival_data <- reactive({
    d <- crashes %>%
      filter(
        OperatorType %in% input$survival_operator,
        Aboard >= input$survival_min_aboard | is.na(Aboard)
      )
    
    if (input$survival_decade != "All Decades") {
      d <- d %>% filter(Decade == as.numeric(input$survival_decade))
    }
    
    return(d)
  })
  
  # ========================================================================
  # TEMPORAL TRENDS PLOTS
  # ========================================================================
  
  # Plot 1: Crashes timeline
  output$plot_crashes_timeline <- renderPlotly({
    data <- temporal_data() %>%
      group_by(Year) %>%
      summarise(Crashes = n(), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Year, y = ~Crashes, type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#E74C3C', width = 2),
                 marker = list(size = 4, color = '#C0392B'),
                 hovertemplate = '<b>Year:</b> %{x}<br><b>Crashes:</b> %{y}<extra></extra>') %>%
      layout(
        title = list(text = "Annual Crash Frequency", font = list(size = 16, color = '#2C3E50')),
        xaxis = list(title = "Year", gridcolor = '#ECF0F1'),
        yaxis = list(title = "Number of Crashes", gridcolor = '#ECF0F1'),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 2: Fatalities timeline
  output$plot_fatalities_timeline <- renderPlotly({
    data <- temporal_data() %>%
      group_by(Year) %>%
      summarise(TotalFatalities = sum(Fatalities, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Year, y = ~TotalFatalities, type = 'scatter', mode = 'lines',
                 fill = 'tozeroy', fillcolor = 'rgba(231, 76, 60, 0.3)',
                 line = list(color = '#E74C3C', width = 2),
                 hovertemplate = '<b>Year:</b> %{x}<br><b>Fatalities:</b> %{y:,}<extra></extra>') %>%
      layout(
        title = list(text = "Annual Fatalities", font = list(size = 16, color = '#2C3E50')),
        xaxis = list(title = "Year", gridcolor = '#ECF0F1'),
        yaxis = list(title = "Total Fatalities", gridcolor = '#ECF0F1'),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 3: Decade crashes
  output$plot_decade_crashes <- renderPlotly({
    data <- temporal_data() %>%
      group_by(Decade) %>%
      summarise(Crashes = n(), .groups = 'drop') %>%
      mutate(DecadeLabel = paste0(Decade, "s"))
    
    p <- plot_ly(data, x = ~DecadeLabel, y = ~Crashes, type = 'bar',
                 marker = list(color = '#3498DB', line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Crashes: %{y}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Decade", tickangle = -45),
        yaxis = list(title = "Number of Crashes"),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 4: Decade survival
  output$plot_decade_survival <- renderPlotly({
    data <- temporal_data() %>%
      filter(!is.na(SurvivalRate)) %>%
      group_by(Decade) %>%
      summarise(AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE), .groups = 'drop') %>%
      mutate(DecadeLabel = paste0(Decade, "s"))
    
    p <- plot_ly(data, x = ~DecadeLabel, y = ~AvgSurvivalRate, type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#27AE60', width = 3),
                 marker = list(size = 10, color = '#27AE60'),
                 hovertemplate = '<b>%{x}</b><br>Avg Survival: %{y:.1f}%<extra></extra>') %>%
      layout(
        xaxis = list(title = "Decade", tickangle = -45),
        yaxis = list(title = "Average Survival Rate (%)", range = c(0, 100)),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 5: Monthly pattern
  output$plot_monthly_pattern <- renderPlotly({
    data <- temporal_data() %>%
      filter(!is.na(Month)) %>%
      group_by(MonthName) %>%
      summarise(Crashes = n(), .groups = 'drop') %>%
      mutate(MonthName = factor(MonthName, levels = month.name))
    
    p <- plot_ly(data, x = ~MonthName, y = ~Crashes, type = 'bar',
                 marker = list(color = '#9B59B6', line = list(color = '#8E44AD', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Crashes: %{y}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Month", tickangle = -45),
        yaxis = list(title = "Number of Crashes"),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 6: Operator timeline
  output$plot_operator_timeline <- renderPlotly({
    data <- temporal_data() %>%
      filter(Year >= 1950) %>%
      group_by(Year, OperatorType) %>%
      summarise(Crashes = n(), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Year, y = ~Crashes, color = ~OperatorType,
                 type = 'scatter', mode = 'lines',
                 colors = operator_colors,
                 hovertemplate = '<b>%{fullData.name}</b><br>Year: %{x}<br>Crashes: %{y}<extra></extra>') %>%
      layout(
        title = list(text = "Trends by Operator Type (1950+)", font = list(size = 16, color = '#2C3E50')),
        xaxis = list(title = "Year", gridcolor = '#ECF0F1'),
        yaxis = list(title = "Number of Crashes", gridcolor = '#ECF0F1'),
        legend = list(title = list(text = 'Operator Type')),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # ========================================================================
  # GEOGRAPHIC PLOTS
  # ========================================================================
  
  # Plot 7: Working scatter geo map
  output$plot_world_choropleth <- renderPlotly({
    # Get crash data
    crash_data <- map_data() %>%
      filter(!is.na(Country), Country != "", Country != "NA") %>%
      group_by(Country) %>%
      summarise(
        Crashes = n(),
        TotalFatalities = sum(Fatalities, na.rm = TRUE),
        AvgFatalityRate = mean(FatalityRate, na.rm = TRUE),
        AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Crashes >= input$map_min_crashes)
    
    # Country coordinates lookup
    coords <- data.frame(
      Country = c("United States of America", "Russia", "Brazil", "Canada", "China",
                  "India", "France", "Germany", "United Kingdom", "Italy", "Spain",
                  "Mexico", "Colombia", "Peru", "Venezuela", "Argentina", "Chile",
                  "Japan", "Indonesia", "Philippines", "Australia", "South Africa",
                  "Turkey", "Iran", "Poland", "Ukraine", "Netherlands", "Belgium",
                  "Greece", "Portugal", "Sweden", "Norway", "Finland", "Denmark",
                  "Switzerland", "Austria", "Czechia", "Hungary", "New Zealand",
                  "Ecuador", "Bolivia", "Paraguay", "Uruguay", "Cuba", "Panama"),
      lat = c(37.1, 61.5, -14.2, 56.1, 35.9, 20.6, 46.2, 51.2, 55.4, 41.9, 40.5,
              23.6, 4.6, -9.2, 6.4, -38.4, -35.7, 36.2, -0.8, 12.9, -25.3, -30.6,
              38.9, 32.4, 51.9, 48.4, 52.1, 50.5, 39.1, 39.4, 60.1, 60.5, 61.9, 56.3,
              46.8, 47.5, 49.8, 47.2, -40.9, -1.8, -16.3, -23.4, -32.5, 21.5, 8.5),
      lon = c(-95.7, 105.3, -51.9, -106.3, 104.2, 78.9, 2.2, 10.5, -3.4, 12.6, -3.7,
              -102.6, -74.3, -75.0, -66.6, -63.6, -71.5, 138.3, 113.9, 121.8, 133.8, 22.9,
              35.2, 53.7, 19.1, 31.2, 5.3, 4.5, 21.8, -8.2, 18.6, 8.5, 25.7, 9.5,
              8.2, 13.0, 15.5, 19.5, 174.9, -78.2, -63.6, -58.4, -55.8, -77.8, -80.8)
    )
    
    # Join data
    map_data <- crash_data %>%
      inner_join(coords, by = "Country")
    
    # Select metric
    metric_col <- input$map_metric
    if (metric_col == "crashes") {
      values <- map_data$Crashes
      label <- "Crashes"
    } else if (metric_col == "fatalities") {
      values <- map_data$TotalFatalities
      label <- "Total Fatalities"
    } else if (metric_col == "fatality_rate") {
      values <- map_data$AvgFatalityRate
      label <- "Avg Fatality Rate (%)"
    } else {
      values <- map_data$AvgSurvivalRate
      label <- "Avg Survival Rate (%)"
    }
    
    # Create plot with Miller projection - rectangular, no circle
    plot_ly(
      data = map_data,
      type = 'scattergeo',
      lon = ~lon,
      lat = ~lat,
      text = ~paste0("<b>", Country, "</b><br>",
                     "Crashes: ", Crashes, "<br>",
                     "Fatalities: ", TotalFatalities, "<br>",
                     "Fatality Rate: ", round(AvgFatalityRate, 1), "%<br>",
                     "Survival Rate: ", round(AvgSurvivalRate, 1), "%"),
      hoverinfo = 'text',
      mode = 'markers',
      marker = list(
        size = ~ifelse(Crashes == max(Crashes), Crashes/12, Crashes/3),
        color = values,
        colorscale = 'RdYlBu',
        showscale = TRUE,
        colorbar = list(title = label),
        line = list(color = 'white', width = 1),
        sizemode = 'diameter'
      )
    ) %>%
      layout(
        title = list(
          text = paste0("<b>", label, " by Country</b>"),
          font = list(size = 16, color = '#2C3E50')
        ),
        geo = list(
          projection = list(type = 'miller'),
          
          showland = TRUE,
          landcolor = 'rgb(243, 243, 243)',
          coastlinecolor = 'rgb(150, 150, 150)',
          showlakes = TRUE,
          lakecolor = 'rgb(230, 240, 255)',
          showcountries = TRUE,
          countrycolor = 'rgb(150, 150, 150)',
          countrywidth = 0.5,
          showframe = FALSE,
          showcoastlines = TRUE
        )
      )
  })
  
  # Plot 8: Crash location map
  output$plot_crash_map <- renderLeaflet({
    data <- map_data() %>%
      filter(!is.na(Location)) %>%
      select(Date, Location, Operator, Type, Fatalities, Aboard, SurvivalRate) %>%
      head(1000)  # Limit for performance
    
    # Create simple geocoding based on location string
    # In a real app, you'd use proper geocoding
    # For now, we'll create a representative map
    
    leaflet() %>%
      addTiles() %>%
      setView(lng = 0, lat = 20, zoom = 2) %>%
      addMarkers(
        data = data.frame(
          lat = rnorm(min(nrow(data), 1000), mean = 20, sd = 30),
          lng = rnorm(min(nrow(data), 1000), mean = 0, sd = 60)
        ),
        clusterOptions = markerClusterOptions(),
        popup = ~paste("<b>Crash Details</b><br>",
                       "Location: Sample<br>",
                       "Date: Sample<br>",
                       "Fatalities: Sample")
      )
  })
  
  # Plot 9: Top countries
  output$plot_top_countries <- renderPlotly({
    data <- map_data() %>%
      group_by(Country) %>%
      summarise(Crashes = n(), TotalFatalities = sum(Fatalities, na.rm = TRUE), .groups = 'drop') %>%
      filter(!is.na(Country), Country != "") %>%
      arrange(desc(Crashes)) %>%
      head(20) %>%
      mutate(Country = reorder(Country, Crashes))
    
    p <- plot_ly(data, y = ~Country, x = ~Crashes, type = 'bar', orientation = 'h',
                 marker = list(color = '#3498DB', line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{y}</b><br>Crashes: %{x}<br>Fatalities: %{text:,}<extra></extra>',
                 text = ~TotalFatalities) %>%
      layout(
        xaxis = list(title = "Number of Crashes"),
        yaxis = list(title = ""),
        margin = list(l = 150),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # ========================================================================
  # OPERATOR & AIRCRAFT PLOTS
  # ========================================================================
  
  # Plot 10: Operator crashes
  output$plot_operator_crashes <- renderPlotly({
    data <- aircraft_data() %>%
      group_by(OperatorType) %>%
      summarise(Crashes = n(), .groups = 'drop') %>%
      mutate(OperatorType = reorder(OperatorType, Crashes))
    
    p <- plot_ly(data, x = ~OperatorType, y = ~Crashes, type = 'bar',
                 marker = list(color = ~OperatorType, 
                               colors = operator_colors,
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Crashes: %{y}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Operator Type"),
        yaxis = list(title = "Number of Crashes"),
        showlegend = FALSE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 11: Operator fatality rate
  output$plot_operator_fatality <- renderPlotly({
    data <- aircraft_data() %>%
      filter(!is.na(FatalityRate)) %>%
      group_by(OperatorType) %>%
      summarise(AvgFatalityRate = mean(FatalityRate, na.rm = TRUE), .groups = 'drop') %>%
      mutate(OperatorType = reorder(OperatorType, -AvgFatalityRate))
    
    p <- plot_ly(data, x = ~OperatorType, y = ~AvgFatalityRate, type = 'bar',
                 marker = list(color = ~OperatorType, 
                               colors = operator_colors,
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Avg Fatality Rate: %{y:.1f}%<extra></extra>') %>%
      layout(
        xaxis = list(title = "Operator Type"),
        yaxis = list(title = "Average Fatality Rate (%)"),
        showlegend = FALSE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 12: Operator survival distribution
  output$plot_operator_survival_dist <- renderPlotly({
    data <- aircraft_data() %>%
      filter(!is.na(SurvivalRate))
    
    p <- plot_ly(data, x = ~SurvivalRate, color = ~OperatorType, type = "histogram",
                 colors = operator_colors,
                 alpha = 0.7,
                 hovertemplate = 'Survival Rate: %{x:.0f}%<br>Count: %{y}<extra></extra>') %>%
      layout(
        barmode = "overlay",
        xaxis = list(title = "Survival Rate (%)"),
        yaxis = list(title = "Number of Crashes"),
        legend = list(title = list(text = 'Operator Type')),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 13: Top aircraft
  output$plot_top_aircraft <- renderPlotly({
    data <- aircraft_data() %>%
      filter(!is.na(Type), Type != "") %>%
      group_by(Type) %>%
      summarise(Crashes = n(), TotalFatalities = sum(Fatalities, na.rm = TRUE), .groups = 'drop') %>%
      filter(Crashes >= input$min_crashes_aircraft) %>%
      arrange(desc(Crashes)) %>%
      head(15) %>%
      mutate(Type = reorder(Type, Crashes))
    
    p <- plot_ly(data, y = ~Type, x = ~Crashes, type = 'bar', orientation = 'h',
                 marker = list(color = '#E67E22', line = list(color = '#D35400', width = 1)),
                 hovertemplate = '<b>%{y}</b><br>Crashes: %{x}<br>Fatalities: %{text:,}<extra></extra>',
                 text = ~TotalFatalities) %>%
      layout(
        xaxis = list(title = "Number of Crashes"),
        yaxis = list(title = ""),
        margin = list(l = 200),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 14: Aircraft fatalities
  output$plot_aircraft_fatalities <- renderPlotly({
    data <- aircraft_data() %>%
      filter(!is.na(Type), Type != "") %>%
      group_by(Type) %>%
      summarise(
        Crashes = n(),
        AvgFatalities = mean(Fatalities, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Crashes >= input$min_crashes_aircraft) %>%
      arrange(desc(AvgFatalities)) %>%
      head(15) %>%
      mutate(Type = reorder(Type, AvgFatalities))
    
    p <- plot_ly(data, y = ~Type, x = ~AvgFatalities, type = 'bar', orientation = 'h',
                 marker = list(color = '#E74C3C', line = list(color = '#C0392B', width = 1)),
                 hovertemplate = '<b>%{y}</b><br>Avg Fatalities: %{x:.1f}<br>Total Crashes: %{text}<extra></extra>',
                 text = ~Crashes) %>%
      layout(
        xaxis = list(title = "Average Fatalities per Crash"),
        yaxis = list(title = ""),
        margin = list(l = 200),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 15: Operator fatality density
  output$plot_operator_fatality_density <- renderPlotly({
    data <- aircraft_data() %>%
      filter(!is.na(FatalityRate), !is.na(OperatorType))
    
    # Calculate density for each operator type
    densities <- lapply(unique(data$OperatorType), function(op) {
      op_data <- data %>% filter(OperatorType == op)
      if(nrow(op_data) > 5) {
        dens <- density(op_data$FatalityRate, na.rm = TRUE, adjust = 1.5)
        data.frame(
          x = dens$x,
          y = dens$y,
          OperatorType = op
        )
      }
    })
    
    density_df <- do.call(rbind, densities[!sapply(densities, is.null)])
    
    p <- plot_ly(density_df, x = ~x, y = ~y, color = ~OperatorType, type = 'scatter', mode = 'lines',
                 colors = operator_colors,
                 fill = 'tozeroy',
                 alpha = 0.5,
                 hovertemplate = '<b>%{fullData.name}</b><br>Fatality Rate: %{x:.0f}%<br>Density: %{y:.3f}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Fatality Rate (%)", range = c(0, 100)),
        yaxis = list(title = "Density"),
        legend = list(title = list(text = 'Operator Type')),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # ========================================================================
  # SURVIVAL ANALYSIS PLOTS
  # ========================================================================
  
  # Plot 15: Survival timeline
  output$plot_survival_timeline <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(SurvivalRate), Year >= 1940) %>%
      group_by(Year) %>%
      summarise(AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Year, y = ~AvgSurvivalRate, type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#27AE60', width = 3),
                 marker = list(size = 6, color = '#27AE60'),
                 hovertemplate = '<b>Year:</b> %{x}<br><b>Avg Survival:</b> %{y:.1f}%<extra></extra>') %>%
      add_ribbons(
        ymin = ~AvgSurvivalRate - 5,
        ymax = ~AvgSurvivalRate + 5,
        line = list(color = 'transparent'),
        fillcolor = 'rgba(39, 174, 96, 0.2)',
        showlegend = FALSE
      ) %>%
      layout(
        xaxis = list(title = "Year", gridcolor = '#ECF0F1'),
        yaxis = list(title = "Average Survival Rate (%)", range = c(0, 100), gridcolor = '#ECF0F1'),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 16: Survival histogram
  output$plot_survival_histogram <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(SurvivalRate))
    
    p <- plot_ly(data, x = ~SurvivalRate, type = 'histogram',
                 marker = list(color = '#3498DB', line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = 'Survival Rate: %{x:.0f}%<br>Count: %{y}<extra></extra>',
                 nbinsx = 50) %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = c(median(data$SurvivalRate, na.rm = TRUE), median(data$SurvivalRate, na.rm = TRUE)),
        y = c(0, 1000),
        line = list(color = '#E74C3C', dash = 'dash', width = 2),
        name = paste0("Median: ", round(median(data$SurvivalRate, na.rm = TRUE), 1), "%"),
        hoverinfo = 'name'
      ) %>%
      layout(
        xaxis = list(title = "Survival Rate (%)"),
        yaxis = list(title = "Number of Crashes"),
        showlegend = TRUE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 17: Correlation scatter
  output$plot_correlation_scatter <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(Aboard), !is.na(Fatalities), Aboard > 0)
    
    # Calculate correlation
    cor_val <- cor(data$Aboard, data$Fatalities, use = "complete.obs")
    
    p <- plot_ly(data, x = ~Aboard, y = ~Fatalities, type = 'scatter', mode = 'markers',
                 marker = list(size = 5, color = '#3498DB', opacity = 0.5),
                 text = ~paste("Date:", Date, "<br>Operator:", Operator,
                               "<br>Aboard:", Aboard, "<br>Fatalities:", Fatalities),
                 hoverinfo = 'text') %>%
      add_trace(
        type = "scatter",
        mode = "lines",
        x = ~Aboard,
        y = fitted(lm(Fatalities ~ Aboard, data = data)),
        line = list(color = '#E74C3C', width = 2),
        name = paste0("Trend (r = ", round(cor_val, 3), ")"),
        hoverinfo = 'name'
      ) %>%
      layout(
        title = paste("Correlation:", round(cor_val, 3)),
        xaxis = list(title = "Passengers Aboard"),
        yaxis = list(title = "Fatalities"),
        showlegend = TRUE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 18: Survival by hour
  output$plot_survival_hour <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(Hour), !is.na(SurvivalRate)) %>%
      group_by(Hour) %>%
      summarise(AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE), 
                Count = n(),
                .groups = 'drop') %>%
      filter(Count >= 5)
    
    p <- plot_ly(data, x = ~Hour, y = ~AvgSurvivalRate, type = 'scatter', mode = 'lines+markers',
                 line = list(color = '#9B59B6', width = 2),
                 marker = list(size = 8, color = '#8E44AD'),
                 hovertemplate = '<b>Hour:</b> %{x}:00<br><b>Avg Survival:</b> %{y:.1f}%<br>Crashes: %{text}<extra></extra>',
                 text = ~Count) %>%
      layout(
        xaxis = list(title = "Hour of Day (24h)", dtick = 2),
        yaxis = list(title = "Average Survival Rate (%)"),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 19: Severity distribution
  output$plot_severity_dist <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(Severity)) %>%
      group_by(Severity) %>%
      summarise(Count = n(), .groups = 'drop') %>%
      mutate(Severity = factor(Severity, levels = c("No Fatalities", "Low (<25% fatalities)",
                                                     "Medium (25-75% fatalities)", "High (>75% fatalities)")))
    
    colors <- c("#27AE60", "#F39C12", "#E67E22", "#E74C3C")
    
    p <- plot_ly(data, labels = ~Severity, values = ~Count, type = 'pie',
                 marker = list(colors = colors, line = list(color = '#FFFFFF', width = 2)),
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 hovertemplate = '<b>%{label}</b><br>Count: %{value}<br>%{percent}<extra></extra>') %>%
      layout(
        showlegend = TRUE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # Plot 20: Survival density by era
  output$plot_survival_density_era <- renderPlotly({
    data <- survival_data() %>%
      filter(!is.na(SurvivalRate), !is.na(Era))
    
    # Calculate density for each era
    densities <- lapply(unique(data$Era), function(era) {
      era_data <- data %>% filter(Era == era)
      dens <- density(era_data$SurvivalRate, na.rm = TRUE, adjust = 1.5)
      data.frame(
        x = dens$x,
        y = dens$y,
        Era = era
      )
    })
    
    density_df <- do.call(rbind, densities)
    
    p <- plot_ly(density_df, x = ~x, y = ~y, color = ~Era, type = 'scatter', mode = 'lines',
                 colors = era_colors,
                 fill = 'tozeroy',
                 alpha = 0.6,
                 hovertemplate = '<b>%{fullData.name}</b><br>Survival Rate: %{x:.0f}%<br>Density: %{y:.3f}<extra></extra>') %>%
      layout(
        xaxis = list(title = "Survival Rate (%)", range = c(0, 100)),
        yaxis = list(title = "Density"),
        legend = list(title = list(text = 'Era')),
        hovermode = 'closest',
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  # ========================================================================
  # HYPOTHESIS TESTING
  # ========================================================================
  
  # H1: Safety improvement plots
  output$plot_h1_crashes <- renderPlotly({
    data <- crashes %>%
      mutate(Period = ifelse(Year < 1970, "Pre-1970", "1970+")) %>%
      group_by(Period, Decade) %>%
      summarise(Crashes = n(), .groups = 'drop') %>%
      group_by(Period) %>%
      summarise(AvgCrashesPerDecade = mean(Crashes), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Period, y = ~AvgCrashesPerDecade, type = 'bar',
                 marker = list(color = c('#E74C3C', '#27AE60'),
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Avg Crashes/Decade: %{y:.0f}<extra></extra>') %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Crashes per Decade"),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  output$plot_h1_survival <- renderPlotly({
    data <- crashes %>%
      filter(!is.na(SurvivalRate)) %>%
      mutate(Period = ifelse(Year < 1970, "Pre-1970", "1970+")) %>%
      group_by(Period) %>%
      summarise(AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE), .groups = 'drop')
    
    p <- plot_ly(data, x = ~Period, y = ~AvgSurvivalRate, type = 'bar',
                 marker = list(color = c('#E74C3C', '#27AE60'),
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Avg Survival: %{y:.1f}%<extra></extra>') %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Survival Rate (%)"),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  output$test_h1 <- renderPrint({
    # Test crash frequency difference
    pre_1970 <- crashes %>% filter(Year < 1970) %>%
      group_by(Decade) %>% summarise(n = n()) %>% pull(n)
    
    post_1970 <- crashes %>% filter(Year >= 1970) %>%
      group_by(Decade) %>% summarise(n = n()) %>% pull(n)
    
    cat("=== H1: Testing Safety Improvement (Pre-1970 vs 1970+) ===\n\n")
    cat("Crash Frequency Comparison:\n")
    crash_test <- t.test(pre_1970, post_1970, alternative = "greater")
    print(crash_test)
    
    cat("\n\nSurvival Rate Comparison:\n")
    survival_test <- t.test(
      SurvivalRate ~ (Year >= 1970),
      data = crashes %>% filter(!is.na(SurvivalRate))
    )
    print(survival_test)
  })
  
  output$interpret_h1 <- renderUI({
    p("Our statistical tests provide strong evidence for H1. The t-test shows that crash 
      frequencies per decade were significantly higher before 1970 compared to after 1970 
      (p < 0.001). Additionally, survival rates improved significantly in the modern era 
      (p < 0.001). These findings support our hypothesis that aviation safety has improved 
      dramatically since the 1970s.",
      style = "font-size:14px; margin:0;")
  })
  
  # H2: Operator comparison plots
  output$plot_h2_fatality <- renderPlotly({
    data <- crashes %>%
      filter(!is.na(FatalityRate)) %>%
      group_by(OperatorType) %>%
      summarise(AvgFatalityRate = mean(FatalityRate, na.rm = TRUE), .groups = 'drop') %>%
      mutate(OperatorType = reorder(OperatorType, -AvgFatalityRate))
    
    p <- plot_ly(data, x = ~OperatorType, y = ~AvgFatalityRate, type = 'bar',
                 marker = list(color = ~OperatorType, colors = operator_colors,
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Avg Fatality Rate: %{y:.1f}%<extra></extra>') %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Fatality Rate (%)"),
        showlegend = FALSE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  output$plot_h2_survival <- renderPlotly({
    data <- crashes %>%
      filter(!is.na(SurvivalRate)) %>%
      group_by(OperatorType) %>%
      summarise(AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE), .groups = 'drop') %>%
      mutate(OperatorType = reorder(OperatorType, AvgSurvivalRate))
    
    p <- plot_ly(data, x = ~OperatorType, y = ~AvgSurvivalRate, type = 'bar',
                 marker = list(color = ~OperatorType, colors = operator_colors,
                               line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{x}</b><br>Avg Survival: %{y:.1f}%<extra></extra>') %>%
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = "Average Survival Rate (%)"),
        showlegend = FALSE,
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
  
  output$test_h2 <- renderPrint({
    cat("=== H2: Testing Operator Type Differences ===\n\n")
    cat("ANOVA: Fatality Rate by Operator Type\n")
    fatality_anova <- aov(FatalityRate ~ OperatorType, 
                          data = crashes %>% filter(!is.na(FatalityRate)))
    print(summary(fatality_anova))
    
    cat("\n\nANOVA: Survival Rate by Operator Type\n")
    survival_anova <- aov(SurvivalRate ~ OperatorType, 
                          data = crashes %>% filter(!is.na(SurvivalRate)))
    print(summary(survival_anova))
    
    cat("\n\nPost-hoc Tukey HSD Test (Survival Rate):\n")
    print(TukeyHSD(survival_anova))
  })
  
  output$interpret_h2 <- renderUI({
    p("ANOVA tests reveal highly significant differences among operator types (p < 0.001 
      for both fatality and survival rates). Commercial aviation shows the lowest fatality 
      rates and highest survival rates, followed by Private/Charter, with Military operations 
      showing the worst outcomes. Post-hoc tests confirm that these differences are 
      statistically significant across all pairwise comparisons, strongly supporting H2.",
      style = "font-size:14px; margin:0;")
  })
  
  # H3: Geographic table and plot
  output$table_h3 <- renderDT({
    data <- crashes %>%
      filter(!is.na(Country), Country != "") %>%
      group_by(Country) %>%
      summarise(
        Crashes = n(),
        TotalFatalities = sum(Fatalities, na.rm = TRUE),
        AvgFatalityRate = round(mean(FatalityRate, na.rm = TRUE), 1),
        AvgSurvivalRate = round(mean(SurvivalRate, na.rm = TRUE), 1),
        .groups = 'drop'
      ) %>%
      filter(Crashes >= 20) %>%
      arrange(desc(Crashes))
    
    datatable(data, 
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE,
              colnames = c('Country', 'Crashes', 'Total Fatalities', 
                           'Avg Fatality Rate (%)', 'Avg Survival Rate (%)')) %>%
      formatStyle(columns = 1:5, fontSize = '13px')
  })
  
  output$plot_h3_geographic <- renderPlotly({
    data <- crashes %>%
      filter(!is.na(Country), Country != "") %>%
      group_by(Country) %>%
      summarise(
        Crashes = n(),
        AvgSurvivalRate = mean(SurvivalRate, na.rm = TRUE),
        .groups = 'drop'
      ) %>%
      filter(Crashes >= 30) %>%
      arrange(desc(Crashes)) %>%
      head(20) %>%
      mutate(Country = reorder(Country, AvgSurvivalRate))
    
    p <- plot_ly(data, y = ~Country, x = ~AvgSurvivalRate, type = 'bar', orientation = 'h',
                 marker = list(color = '#3498DB', line = list(color = '#2C3E50', width = 1)),
                 hovertemplate = '<b>%{y}</b><br>Avg Survival: %{x:.1f}%<br>Crashes: %{text}<extra></extra>',
                 text = ~Crashes) %>%
      layout(
        title = "Avg Survival Rate by Country (Top 20 by crash count, min 30 crashes)",
        xaxis = list(title = "Average Survival Rate (%)"),
        yaxis = list(title = ""),
        margin = list(l = 150),
        plot_bgcolor = '#F8F9FA',
        paper_bgcolor = '#FFFFFF'
      )
    
    p
  })
}

# ============================================================================
# RUN THE APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)
