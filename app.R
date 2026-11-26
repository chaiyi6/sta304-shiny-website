library(shiny)
library(bslib)
library(readxl)
library(markdown)   # needed by includeMarkdown() for the About tab

source("content.R")        # labels, prep_data(), interpretations, key findings
source("visualization.R")  # plotting functions

survey <- prep_data(read_excel("cleaned_data.xlsx"))
n_total <- nrow(survey)

max_hours <- max(1, ceiling(max(survey$q2_weekly_hours, na.rm = TRUE)))


# =====================================================================
# UI
# =====================================================================

ui <- page_sidebar(
  title = "Generative AI in Education: What Students Told Us",
  theme = bs_theme(bootswatch = "flatly"),

  sidebar = sidebar(
    width = 300,

    h5("Filter the students"),

    sliderInput(
      "hours", "Weekly AI use (hours):",
      min = 0, max = max_hours, value = c(0, max_hours), step = 0.5
    ),

    checkboxGroupInput(
      "purpose", "Main purpose of AI use:",
      choices  = levels(survey$main_purpose),
      selected = levels(survey$main_purpose)
    ),

    hr(),
    h5("Plot settings"),

    selectInput(
      "colour", "Plot colour:",
      choices = c(
        "Light Blue"                 = "lightblue",
        "Steel Blue"                 = "steelblue",
        "Green"                      = "lightgreen",
        "Purple"                     = "mediumpurple",
        "Orange"                     = "orange",
        "Colour-blind friendly blue" = "#0072B2"
      )
    ),

    sliderInput(
      "axis_max", "Maximum weekly hours shown on the axis:",
      min = 1, max = max_hours + 5, value = max_hours, step = 1
    ),

    hr(),
    h5("Current selection"),
    uiOutput("summary_stats"),

    p(em("The survey had no demographic questions, so students are filtered ",
         "by how much and why they use AI."), style = "font-size: 0.85em;")
  ),

  navset_card_tab(

    nav_panel(
      "Key Findings",
      HTML(key_findings),
      p(em("These findings use all 34 responses. Use the other tabs and the ",
           "filters on the left to explore smaller groups of students."))
    ),

    nav_panel(
      "Usage by Purpose",
      plotOutput("plot1", height = "430px"),
      uiOutput("interp1")
    ),

    nav_panel(
      "Opinions on AI",
      selectInput("var", "Choose a question to display:",
                  choices = rating_choices,
                  selected = "q5_understand_concepts", width = "420px"),
      plotOutput("plot2", height = "400px"),
      uiOutput("interp2")
    ),

    nav_panel(
      "Usage vs Efficiency",
      plotOutput("plot3", height = "430px"),
      uiOutput("interp3")
    ),

    nav_panel(
      "About",
      includeMarkdown("about.md")
    )
  )
)


# =====================================================================
# Server
# =====================================================================

server <- function(input, output, session) {

  filtered_data <- reactive({
    survey[
      !is.na(survey$q2_weekly_hours) &
        survey$q2_weekly_hours >= input$hours[1] &
        survey$q2_weekly_hours <= input$hours[2] &
        survey$main_purpose %in% input$purpose,
      ,
      drop = FALSE
    ]
  })

  # Sidebar summary in plain language
  output$summary_stats <- renderUI({
    df <- filtered_data()
    txt <- interp_sample(df, n_total)
    if (nrow(df) > 0) {
      txt <- paste0(
        txt,
        "<p>Typical weekly use: <strong>", median(df$q2_weekly_hours),
        " hours</strong> (average ", round(mean(df$q2_weekly_hours), 1),
        ").</p>"
      )
    }
    HTML(txt)
  })

  # Tab 1: usage by purpose
  output$plot1 <- renderPlot({
    plot_usage(filtered_data(), colour = input$colour,
               limits = c(0, input$axis_max))
  })
  output$interp1 <- renderUI({
    df <- filtered_data()
    if (nrow(df) == 0) return(HTML(small_n_note(0)))
    # interp_hours() repeats the small-group warning, so show it only once
    hours_txt <- interp_hours(df)
    note <- small_n_note(nrow(df))
    if (nzchar(note)) hours_txt <- sub(note, "", hours_txt, fixed = TRUE)
    HTML(paste0(interp_purpose(df), hours_txt))
  })

  # Tab 2: the rating question chosen by the user
  output$plot2 <- renderPlot({
    plot_rating(filtered_data(), var = input$var, colour = input$colour)
  })
  output$interp2 <- renderUI({
    HTML(interp_likert(filtered_data(), input$var))
  })

  # Tab 3: usage vs efficiency
  output$plot3 <- renderPlot({
    plot_efficiency(filtered_data(), colour = input$colour,
                    xlimits = c(0, input$axis_max))
  })
  output$interp3 <- renderUI({
    HTML(interp_relationship(filtered_data()))
  })
}

shinyApp(ui, server)
