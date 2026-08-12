#--------------------------------------------------
# APP: Recurrence forecasting
#--------------------------------------------------

 #--------------------------------------------------
 # Libraries
 #--------------------------------------------------

 if (!requireNamespace("dplyr", quietly = TRUE)) {
   install.packages("dplyr")}

 if (!requireNamespace("ggplot2", quietly = TRUE)) {
   install.packages("ggplot2")}

 if (!requireNamespace("shiny", quietly = TRUE)) {
   install.packages("shiny")}

 if (!requireNamespace("bslib", quietly = TRUE)) {
   install.packages("bslib")}

 if (!requireNamespace("sn", quietly = TRUE)) {
   install.packages("sn")}

 library(dplyr)
 library(ggplot2)
 library(shiny)
 library(bslib)
 library(sn)
 

 #--------------------------------------------------
 # Data
 #--------------------------------------------------

 df_populations <- readRDS("df_populations.RDS")

 #--------------------------------------------------
 # GGplot theme
 #--------------------------------------------------
 # Color palette 
 col_palette <- c(
   "#287233",  # Model 1: clinical
   "#9A3B6A",  # Model 2: age
   "#A6B54A",  # Model 3: ancestry
   "#2E5A8A",  # Model 4: ancestry
   "#A23A44",  # Model 5: PHS5
   "#E69F00"
 )


 theme_app <- function () { 
   theme_classic(base_size=10) %+replace% 
     theme(
       legend.position = "right",
       legend.title = element_text(size = 10, face = "bold"),
       legend.text = element_text(size = 10), 
       plot.title = element_text(size = 10, face = "bold", lineheight = 1.5, 
                                 hjust = 0, margin = margin(0,0,5,0)),
       plot.title.position = "plot", 
       plot.subtitle = element_text(size = 10, face = "italic",
                                    lineheight = 1.5, hjust = 0,
                                    margin = margin(0,0,10,0)),
       plot.caption.position = "plot",
       plot.margin = margin(10,10,10,10),
       axis.line = element_line(linetype = "solid", color = "black"),
       axis.text.y = element_text(size = 10, face = "plain", hjust = 1, margin = margin(0,5,0,0)),
       axis.title.y = element_text(size = 10, face = "bold", hjust = 0.5, angle = 90, margin = margin(0,10,0,0)),
       axis.text.x = element_text(size = 10, face = "plain", margin = margin(5,0,0,0)),
       axis.title.x = element_text(size = 10, face = "bold", hjust = 0.5, margin = margin(10,0,5,0)),
       panel.spacing = unit(1.5, "lines"),
       strip.text = element_text(face = "bold", size = 12, hjust = 0.5),
       strip.text.x = element_text(size = 10, color = "black", face = "bold", hjust = 0.5, margin = margin(t = 4, b = 4)),  
       strip.background = element_rect(fill = "#E69F00", color = "black", linewidth = 1)    
     )
 }


 #--------------------------------------------------
 # Age simulation function
 #--------------------------------------------------

 simulate_age <- function(n, median, min_age, max_age) {
   
   alpha <- -2
   
   omega <- (max_age - min_age) / 4.2
   
   median_z <- -0.584  
   xi_corrected <- median - (median_z * omega)
   
   age <- numeric(0)
   while(length(age) < n) {
     x <- sn::rsn(
       n = n * 2, 
       xi = xi_corrected,
       omega = omega,
       alpha = alpha
     )
     x <- x[x >= min_age & x <= max_age]
     age <- c(age, x)
   }
   
   return(round(age[1:n]))
 }
 

 #--------------------------------------------------
 # Recurrence rate function
 #--------------------------------------------------
 # Timing of recurrences 5 years postoperative
 recurrence_timing <- list(
   "Stage I"   = c("1" = 0.26, "2" = 0.28, "3" = 0.19, "4" = 0.17, "5" = 0.10),
   "Stage II"   = c("1" = 0.30, "2" = 0.34, "3" = 0.19, "4" = 0.13, "5" = 0.03),
   "Stage III"   = c("1" = 0.40, "2" = 0.31, "3" = 0.17, "4" = 0.08, "5" = 0.04)
 )

 timing_df <- tibble(
   stage = names(recurrence_timing),
   timing = recurrence_timing
 ) %>% 
   tidyr::unnest_longer(timing, indices_to = "offset") %>% 
   mutate(offset = as.integer(offset))
 

#--------------------------------------------------
# UI
#--------------------------------------------------

ui <- page_sidebar(
  
  title = "Future recurrence estimator",
  
  sidebar = sidebar(
    width = "350px",
    
    numericInput(
      "n_pt", 
      "Number of patients (n)", 
      value = 2628, 
      min = 100
    ),
    
    sliderInput(
      "median_age",
      "Median age (years)",
      min = 1,
      max = 100,
      value = 74
    ),
    
    sliderInput(
      "agerange",
      "Age range (years)",
      min = 1,
      max = 100,
      value = c(18, 94)
    ),
    
    sliderInput(
      "stage",
      "Stage distribution (I:II:III [%])",
      min = 0,
      max = 100,
      value = c(27, 65)
    ),
    
    selectInput(
      "region", 
      "Country",
      c("Austria", "Belgium", "Bulgaria", "Croatia", "Cyprus", 
        "Czech Republic", "Denmark", "Estonia", "Finland", 
        "France", "Germany", "Greece", "Hungary", "Iceland", 
        "Ireland", "Italy", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Netherlands", "Norway", "Poland", "Portugal", 
        "Romania", "Slovakia", "Slovenia", "Spain", "Sweden", 
        "Switzerland"),
      selected = "Denmark", 
      multiple = FALSE, 
      selectize = TRUE
    ),
    
    sliderInput(
      "stageI_rec_rate",
      "Stage I recurrence rate (%):",
      min = 0,
      max = 100,
      value = 6.5,
      step = 0.5,
      post = "%"
    ),
    
    sliderInput(
      "stageII_rec_rate",
      "Stage II recurrence rate (%):",
      min = 0,
      max = 100,
      value = 15,
      step = 0.5,
      post = "%"
    ),
    
    sliderInput(
      "stageIII_rec_rate",
      "Stage III recurrence rate (%):",
      min = 0,
      max = 100,
      value = 26,
      step = 0.5,
      post = "%"
    ),
    
    sliderInput(
      "rec_rate_change",
      "Annual change in recurrence rate (%):",
      min = -2,
      max = 2,
      value = 0,
      step = 0.25,
      post = "%"
    )
  ),
  
  layout_column_wrap(
    width = 1/3,
    
    card(
      full_screen = TRUE,
      card_header("Cancer population (2025)"),
      plotOutput("age_hist", height = 250),
      ),
    
    card(
      full_screen = TRUE,
      card_header("Country demographics (2025)"),
      plotOutput("region_hist", height = 250),
      ),
    
    card(
      full_screen = TRUE,
      card_header("Incidence of cancer by age group and stage (2025)"),
      plotOutput("inc_rates", height = 250),
    ),
  ),
  
  layout_column_wrap(
    width = 1/3,
    
    card(
      full_screen = TRUE,
      card_header("Incidence of recurrence"),
      plotOutput("rec_rates", height = 250),
    ),
    
    card(
      full_screen = TRUE,
      card_header("Prediction of cancer incidence"),
      plotOutput("cancer_inc", height = 250),
    ),
    
    card(
      full_screen = TRUE,
      card_header("Prediction of recurrences"),
      plotOutput("rec_pred", height = 250),
    ),
  ),
  
  card(
    full_screen = TRUE,
    card_header("Age distribution at recurrence diagnosis"),
    plotOutput("rec_age_dist", height = 250),
  ),
  
  theme = bs_theme(
    bg = "#fff",
    fg = "#000",
    primary = "#E69F00",
    secondary = "#0072B2",
    success = "#009E73",
    base_font = font_google("Inter"),
    code_font = font_google("JetBrains Mono"),
    sidebar_title_font_size = "0.5rem",
    sidebar_font_size = "0.5rem" 
  )
)

#--------------------------------------------------
# Server
#--------------------------------------------------

server <- function(input, output, session){
  
  #--------------------------------------------------
  # Calculation: Stage probabilities (always sum to 1)
  #--------------------------------------------------
  stage_prob <- reactive({
    c(
      input$stage[1],
      input$stage[2] - input$stage[1],
      100 - input$stage[2]
    ) / 100
  })
  
  #--------------------------------------------------
  # Calculation: Patient cohort
  #--------------------------------------------------
  patients <- reactive({
    
    set.seed(1234)
    
    tibble(
      ID = seq_len(input$n_pt),
      age = simulate_age(
        n = input$n_pt,
        median = input$median_age,
        min_age = input$agerange[1],
        max_age = input$agerange[2]),
      stage = sample(
        c("Stage I", "Stage II", "Stage III"),
        size = input$n_pt,
        replace = TRUE,
        prob = stage_prob()
      )
    )
  })

  #--------------------------------------------------
  # Calculation: Age pyramid for selected country
  #--------------------------------------------------
  country <- reactive({
    df_populations %>%
      filter(region == input$region) %>%
      select(`2025`, age)
  })
  
  #--------------------------------------------------
  # Calculation: Incidence rates by age group
  #--------------------------------------------------
  incidence_data <- reactive({
    
    cancer_counts <- patients() %>%
      mutate(age_group = cut(age, breaks = seq(0, 100, by = 5), right = FALSE,
                             labels = paste0(seq(0, 95, by = 5), "-", seq(4, 99, by = 5)))) %>%
      group_by(age_group, stage) %>%
      summarise(cancer_cases = n(), .groups = "drop") %>%
      tidyr::complete(age_group, stage, fill = list(cancer_cases = 0))
    
    population_counts <- country() %>%
      mutate(age_group = cut(age, breaks = seq(0, 100, by = 5), right = FALSE,
                             labels = paste0(seq(0, 95, by = 5), "-", seq(4, 99, by = 5)))) %>%
      group_by(age_group) %>%
      summarise(pop_total = sum(`2025`), .groups = "drop")
    
    population_counts %>%
      left_join(cancer_counts, by = "age_group") %>%
      filter(!is.na(stage)) %>%
      mutate(rate_per_100000 = (cancer_cases / pop_total) * 100000) %>%
      filter(pop_total > 0)
  })
  
  #--------------------------------------------------
  # Calculation: Recurrence rates by stage
  #--------------------------------------------------
  rec_rates_plot_data <- reactive({
    
    base_recurrence_rate <- list(
      "Stage I"   = input$stageI_rec_rate / 100,
      "Stage II"  = input$stageII_rec_rate / 100,
      "Stage III" = input$stageIII_rec_rate / 100
    )
    
    data_list <- lapply(names(recurrence_timing), function(group) {
      timing <- recurrence_timing[[group]]
      base_rate <- base_recurrence_rate[[group]]
      cumulative <- cumsum(timing * base_rate)
      
      df <- data.frame(
        group = group,
        x = as.numeric(names(cumulative)),
        y = cumulative
      )
      
      df <- bind_rows(
        data.frame(group = group, x = 0, y = 0),
        df
      ) %>% arrange(x)
      
      return(df)
    })
    
    bind_rows(data_list)
  })
  
  #--------------------------------------------------
  # Calculation: Predicted number of cancer patients
  #--------------------------------------------------
  # Detailed incidence rates:
  incidence_data_pred <- reactive({
      cancer_counts <- patients() %>%
      group_by(age, stage) %>%
      summarise(cancer_cases = n(), .groups = "drop") %>%
      tidyr::complete(age = 1:99, stage, fill = list(cancer_cases = 0))
    
    population_counts <- country() %>%
      filter(!is.na(age)) %>%
      group_by(age) %>%
      summarise(pop_total = sum(`2025`), .groups = "drop")
    
    population_counts %>%
      left_join(cancer_counts, by = "age") %>%
      filter(!is.na(stage)) %>%
      mutate(rate_per_100000 = (cancer_cases / pop_total) * 100000) %>%
      filter(pop_total > 0)
  })
  
  country_cancer_pred <- reactive({
    pop_long <- df_populations %>%
      filter(region == input$region) %>%
      filter(!is.na(age)) %>%
      tidyr::pivot_longer(
        cols = matches("^20[2-5][0-9]$"),
        names_to = "year",
        values_to = "population"
      ) %>%
      group_by(year, age) %>%
      summarise(pop_total = sum(population, na.rm = TRUE), .groups = "drop")
    
    rates <- incidence_data_pred() %>%
      mutate(age = as.integer(age)) %>%
      select(-pop_total)
    
    pop_long %>%
      left_join(rates, by = "age", relationship = "many-to-many") %>%
      filter(!is.na(stage)) %>%
      mutate(predicted_cases = (rate_per_100000 / 100000) * pop_total) %>%
      select(year, age, stage, predicted_cases) %>%
      mutate(year_num = as.numeric(year)) %>%
      group_by(year_num, stage) %>%
      summarise(total_predicted = sum(predicted_cases, na.rm = TRUE), .groups = "drop")
  })
  
  #--------------------------------------------------
  # Calculation: Predicted number of recurrences
  #--------------------------------------------------
  recurrence_pred <- reactive({
    
    df_surgery <- df_populations %>%
      filter(region == input$region) %>%
      filter(!is.na(age)) %>%
      mutate(age = as.integer(age)) %>% 
      tidyr::pivot_longer(
        cols = matches("^20[2-5][0-9]$"),
        names_to = "year",
        values_to = "population"
      ) %>%
      group_by(year, age) %>%
      summarise(pop_total = sum(population, na.rm = TRUE), .groups = "drop") %>%
      left_join(incidence_data_pred() %>% 
                  mutate(age = as.integer(age)) %>% 
                  select(-pop_total), 
                by = "age", 
                relationship = "many-to-many") %>% 
      filter(!is.na(stage)) %>%
      mutate(predicted_cases = (rate_per_100000 / 100000) * pop_total) %>%
      mutate(year_surgery = as.integer(year)) %>%
      select(year_surgery, age_surgery = age, stage, predicted_cases)
    
    base_rates <- tibble(
      stage = c("Stage I", "Stage II", "Stage III"),
      base_rate = c(input$stageI_rec_rate / 100, 
                    input$stageII_rec_rate / 100, 
                    input$stageIII_rec_rate / 100)
    )
    
    annual_multiplier <- 1 + (input$rec_rate_change / 100)
    
    df_surgery %>%
      left_join(base_rates, by = "stage") %>%
      left_join(timing_df, by = "stage", relationship = "many-to-many") %>%
      mutate(
        years_since_start = year_surgery - 2025,
        dynamic_base_rate = base_rate * (annual_multiplier ^ years_since_start),
        year_recurrence = year_surgery + offset,
        age_recurrence = age_surgery + offset,
        recurrence_cases = predicted_cases * dynamic_base_rate * timing
      ) %>%
      filter(year_recurrence >= 2025 & year_recurrence <= 2050) %>%
      select(year_recurrence, age_recurrence, stage, recurrence_cases)
  })
  
  #--------------------------------------------------
  # Plot: Age and stage distribution
  #--------------------------------------------------
  output$age_hist <- renderPlot({
    
    ggplot(patients(), aes(x = age, fill = stage)) +
      geom_histogram(
        binwidth = 1,
        position = "stack",
        colour = "white",
        linewidth = 0.2) +
      scale_fill_manual(name = "Stage",
                        values = col_palette,
                        labels = c("Stage I", "Stage II", "Stage III")) +
      labs(
        x = "Age (years)",
        y = "Number of patients",
        fill = "Stage") +
      coord_cartesian(xlim = c(0,100)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_app() +
      theme(
        legend.position = c(0.2,0.8)
      )
  })


  #--------------------------------------------------
  # Plot: Country demographics
  #--------------------------------------------------
  output$region_hist <- renderPlot({
    
    ggplot(country(), aes(x = age, y = `2025`)) +
      geom_col(
        colour = "white",
        fill = col_palette[1],
        linewidth = 0.2) +
      labs(
        x = "Age (years)",
        y = "Number of citizens") +
      coord_cartesian(xlim = c(0, 100)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
      scale_x_continuous(expand = c(0, 0)) +
      theme_app()
  })

  #--------------------------------------------------
  # Plot: Incidence rates
  #--------------------------------------------------
  output$inc_rates <- renderPlot({
    
    ggplot(incidence_data(), aes(x = as.numeric(age_group), y = rate_per_100000, color = stage)) +
      geom_line() +
      labs(
        #title = paste("Cancerincidens i", input$region),
        x = "Age group (years)",
        y = "Cancers per 100.000 citizens"
      ) +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0),
                         breaks = seq(from=1, to=20, by=1),
                         labels = c("0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99")) +
      scale_color_manual(name = "Stage",
                         values = col_palette) +
      theme_app() +
      theme(
        legend.position = c(0.2,0.8),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
  })

  #--------------------------------------------------
  # Plot: Cumulative incidence of recurrence
  #--------------------------------------------------
  output$rec_rates <- renderPlot({
    
    ggplot(rec_rates_plot_data(),
           aes(x = x, y = y, color = group)) +
      geom_line(linewidth = 1) +
      labs(
        x = "Year since surgery",
        y = "Cumulative incidence of recurrence",
        color = "Group"
      ) +
      scale_x_continuous(expand = c(0,0), breaks = 0:5) + # Viser pæne årstal 0-5
      scale_y_continuous(expand = c(0,0), limits = c(0, 1.0)) + # Sat til 1.0 (100%) da raterne kan overstige 0.5
      scale_color_manual(name = "Stage",
                         values = col_palette,
                         labels = c("Stage I", "Stage II", "Stage III")) +
      theme_app() +
      theme(
        legend.position = c(0.2,0.8)
      )
  })
  
  #--------------------------------------------------
  # Plot: Cancer incidence
  #--------------------------------------------------
  output$cancer_inc <- renderPlot({
    
    ggplot(country_cancer_pred(), aes(x = year_num, y = total_predicted, fill = stage)) +
      geom_col( 
        position = "stack",
        colour = "white",
        linewidth = 0.2
      ) +
      labs(
        x = "Year",
        y = "Number of cancer patients"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_fill_manual(
        name = "Stage",
        values = col_palette,
        labels = c("Stage I", "Stage II", "Stage III")
      ) +
      theme_app()
  })
  
  
  #--------------------------------------------------
  # Plot: Annual number of recurrences over time
  #--------------------------------------------------
  output$rec_pred <- renderPlot({
    
    plot_data <- recurrence_pred() %>%
      group_by(year_recurrence, stage) %>%
      summarise(total_recurrences = sum(recurrence_cases, na.rm = TRUE), .groups = "drop") %>%
      filter(between(year_recurrence, 2030, 2050))
    
    ggplot(plot_data, aes(x = year_recurrence, y = total_recurrences, fill = stage)) +
      geom_col(
        position = "stack",
        colour = "white",
        linewidth = 0.2
      ) +
      labs(
        x = "Year",
        y = "Number of recurrences"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.10))) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_fill_manual(
        name = "Stage",
        values = col_palette,
        labels = c("Stage I", "Stage II", "Stage III")
      ) +
      theme_app()
  })
  
  #--------------------------------------------------
  # Plot: Age distribution pyramid for 2030, 2040, 2050
  #--------------------------------------------------
  output$rec_age_dist <- renderPlot({
    
    df_rec_prob <- recurrence_pred() %>%
      filter(year_recurrence %in% c(2030, 2040, 2050)) %>%
      mutate(GROUP = cut(age_recurrence, 
                         breaks = c(0, seq(4, 94, by = 5), Inf),
                         labels = c("0-4", paste0(seq(5, 90, by = 5), "-", seq(9, 94, by = 5)), "95+"),
                         include.lowest = TRUE)) %>%
      group_by(year_recurrence, GROUP) %>%
      summarise(n = sum(recurrence_cases, na.rm = TRUE), .groups = "drop") %>%
      tidyr::complete(year_recurrence, GROUP, fill = list(n = 0)) %>%
      filter(!is.na(GROUP)) %>% 
      group_by(year_recurrence) %>%
      mutate(prob = n / sum(n)) %>%
      ungroup() %>%
      mutate(
        label_text = paste0(round(prob * 100, 1), "% / n=", ceiling(n))
      )
    
    ggplot(df_rec_prob, aes(x = GROUP, y = n, fill = as.factor(year_recurrence))) +
      geom_col(width = 0.8, 
               show.legend = FALSE) +
      geom_text(
        aes(label = label_text, y = n + (max(n, na.rm = TRUE) * 0.02)), 
        hjust = 0, 
        size = 3.5, 
        color = "black"
      ) +
      coord_flip() +
      facet_wrap(~year_recurrence, nrow = 1, scales = "free_x") +
      labs(
        x = "Age group at recurrence", 
        y = "Number of recurrences"
      ) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.2))) + 
      scale_fill_manual(values = col_palette) + 
      theme_app() +
      theme(
        panel.grid.major.x = element_line(color = "lightgrey", linewidth = 0.2, linetype = 1)
      )
  })
  
}

#--------------------------------------------------
# Run app
#--------------------------------------------------

shinyApp(ui, server)

