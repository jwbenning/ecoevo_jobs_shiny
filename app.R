# setwd("C:/Users/jwben/Documents/Shiny/")
library(shiny)
library(googlesheets4)
library(tidyverse)
library(Hmisc)
library(ggExtra)

ui <- fluidPage(
  titlePanel("Explore the EEB Job Market"),
  h4(HTML("Data from `Anon Quals` via <a href='https://docs.google.com/spreadsheets/d/1cqTuSeLtH-Zw7X9ZtnhQxzw3r19Rya9nzdqRW9apTmY/edit#gid=1335543736' target='_blank'>EcoEvoJobs.net</a>, for 2021/2022 and 2022/2023 job cycles")),
  fluidRow(
    column(width = 4,
           div(style = "padding: 10px; background-color: #f7f7f7;",
             helpText("Select the variables to plot against one another:"),
             selectInput("x_axis", "X-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "First Author Publication Rate", "Senior Author Pubs", "Total Publications", "Overall Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Phone Interview Rate", "Campus Interviews", "Campus Interview Rate", "Offers", "Offer Rate"), selected = "Overall Publication Rate"),
             selectInput("y_axis", "Y-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "First Author Publication Rate", "Senior Author Pubs", "Total Publications", "Overall Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Phone Interview Rate", "Campus Interviews", "Campus Interview Rate", "Offers", "Offer Rate"), selected = "Offer Rate"),
             selectInput("color_var", "Color by:", choices = c("None", "Job Cycle", "Current Position", "Gender"), selected = "None"),
             checkboxInput("postdocOnly", "Postdoc data only", value = FALSE),
             checkboxInput("noCB", "No confidence bands", value = FALSE)
           ),
           wellPanel(
             titlePanel("Your data"),
             checkboxInput("showDataInputs", "Collapse this section", value = FALSE),
             checkboxInput("useDataInputs", "Plot your data", value = FALSE),
             conditionalPanel(
               condition = "!input.showDataInputs",
               numericInput("phdYear", "PhD Year:", value = NA),
               numericInput("firstAuthorPubs", "First Author Pubs:", value = NA),
               numericInput("seniorAuthorPubs", "Senior Author Pubs:", value = NA),
               numericInput("totalPublications", "Total Publications:", value = NA),
               numericInput("fellowships", "Fellowships:", value = NA),
               numericInput("majorGrants", "Major Grants:", value = NA),
               numericInput("classesTaught", "Classes Taught:", value = NA),
               numericInput("applications", "Applications:", value = NA),
               numericInput("phoneInterviews", "Phone Interviews:", value = NA),
               numericInput("campusInterviews", "Campus Interviews:", value = NA),
               numericInput("offers", "Offers:", value = NA),
             )
           )
    ),
    column(width = 8,
           mainPanel(
             plotOutput("scatter_plot", height = "400px", width = "500px")
           )
    )
  ),
  div(
    style = "padding:10px; background-color: #f7f7f7;",
    HTML("<p>'Offer / Interview Rate' = (Offers or Interviews) / Applications</p>
      <p>'Publication Rate' = Total Publications / Years Since PhD</p>
      <p>'Classes taught' refers to the number of times an applicant was an Instructor of Record</p>
      <p>Loess regression lines have 95% confidence interval bands (geom_smooth function)</p>
      <p>Point ranges for categorical data show mean and 95% confidence limits estimated from bootstrapping (mean_cl_boot function)</p>
      <p>Download the original, cleaned, and R-processed data used in this app <a href='https://docs.google.com/spreadsheets/d/1Wmz6N01mSrpcTfIoMH0Rg42xL64SRKYKzIrAZmDGQ7U/edit#gid=697264455' target='_blank'>here</a>. Data cleaning was necessarily subjective to some extent. All data cleaning steps are recorded in `Data Cleaning` fields for individual records, and on the 'General Cleaning Notes' worksheet.</p>
      <p>Interpret all relationships with a grain of salt -- who knows if the sampling here is representative, and there is inherent messiness (e.g., 'Offers' could encompass R1, SLAC, and even non-academic job offers). </p>
      <p>Email comments / bugs to jwbenning@gmail.com</p>")
  )
)


#Sys.setenv(VROOM_CONNECTION_SIZE = "462144")

csv_export_url <- "https://docs.google.com/spreadsheets/d/1Wmz6N01mSrpcTfIoMH0Rg42xL64SRKYKzIrAZmDGQ7U/gviz/tq?tqx=out:csv&sheet=Clean"

sheet_data <- read_csv(csv_export_url, na = c("", "NULL", "NA")) %>%
#  slice(-2) %>%
  # rename(
  #   `Current Position` = `Current Position`,
  #   `First author pubs` = `1st Author Pubs Qualifications`,
  #   `Senior author pubs` = `Senior Author Pubs`,
  #   `Other pubs` = `Other Pubs (as of 2/1/2023)`,
  #   `Applications` = `Applications Outcomes 2022-2023`
  # ) %>%
  # filter(
  #   grepl("^\\d+$", `First author pubs`),
  #   grepl("^\\d+$", `Senior author pubs`),
  #   grepl("^\\d+$", `Other pubs`),
  #   grepl("^\\d+$", `Applications`),
  #   grepl("^\\d+$", `Offers`),
  #   grepl("^\\d+$", `Instructor of Record`),
  #   grepl("^\\d+$", `1st / Phone Interviews`)
  # ) %>%
  mutate(
    Gender = ifelse(Gender %in% c("Male", "Female"), Gender, "Other"),
    `Current Position` = ifelse(`Current Position` %in% c("Postdoc", "Grad Student", "Non TT faculty", "Asst Prof"), `Current Position`, "Other"),
    `Years Since PhD` = ifelse(`Job Cycle` == "2022/2023", 2023 - as.numeric(`PhD Year`), 2022 - as.numeric(`PhD Year`)),
    `Classes Taught` = as.numeric(`Instructor of Record`),
    #`First author pubs` = as.numeric(`First author pubs`),
    #`Senior author pubs` = as.numeric(`Senior author pubs`),
    #`Other pubs` = as.numeric(`Other pubs`),
    #Applications = as.numeric(Applications),
    #`Phone interviews` = as.numeric(`1st / Phone Interviews`),
    #Offers = as.numeric(Offers), 
    `Total Publications` = `First Author Pubs` + `Senior Author Pubs` + `Other Pubs`
  ) %>%
  mutate(`Phone Interview Rate` = `Phone Interviews` / Applications,
         `Campus Interview Rate` = `Campus Interviews` / Applications,
         `Offer Rate` = Offers / Applications,
         `Overall Publication Rate` = `Total Publications` / `Years Since PhD`,
         `First Author Publication Rate` = `First Author Pubs` / `Years Since PhD`,
         `Current Position` = fct_relevel(`Current Position`, "Grad Student", "Postdoc", "Non TT faculty", "Asst Prof", "Other"),
         `Total Funding` = fct_relevel(as.factor(`Total Funding`), "$1-50,000", "$50,000-100,000", "$100,000-250,000", "$250,000-500,000", "$500,000-$1M", "$1-2M", "$2M+"))





categorical_vars <- c("Gender", "Current Position", "Total Funding")


server <- function(input, output) {
  
  # Define a reactive expression that creates a new dataframe based on the inputs
  ind_data <- reactive({
      # Subtract the PhD year from the current year to calculate the years since PhD
      yearsSincePhD <- 2024 - input$phdYear
      
      tibble(
        `PhD Year` = input$phdYear,
        `First Author Pubs` = input$firstAuthorPubs,
        `First Author Publication Rate` = input$firstAuthorPubs / yearsSincePhD,
        `Senior Author Pubs` = input$seniorAuthorPubs,
        `Total Publications` = input$totalPublications,
        `Overall Publication Rate` = input$totalPublications / yearsSincePhD,
        `Fellowships` = input$fellowships,
        `Major Grants` = input$majorGrants,
        `Classes Taught` = input$classesTaught,
        `Applications` = input$applications,
        `Phone Interviews` = input$phoneInterviews,
        `Phone Interview Rate` = input$phoneInterviews / input$applications,
        `Campus Interviews` = input$campusInterviews,
        `Campus Interview Rate` = input$campusInterviews / input$applications,
        `Offers` = input$offers,
        `Offer Rate` = input$offers / input$applications,
        `Years Since PhD` = yearsSincePhD
      )
  })
  
  data <- reactive({
    filtered_data <- sheet_data[!is.na(sheet_data[[input$y_axis]]), ]
    
    if(is.numeric(sheet_data[[input$y_axis]])) {
      filtered_data <- filtered_data[is.finite(filtered_data[[input$y_axis]]), ]
    }
    
    if(is.numeric(sheet_data[[input$x_axis]])) {
      filtered_data <- filtered_data[is.finite(filtered_data[[input$x_axis]]), ]
    }
    
    if (input$x_axis == "Total Funding") {
      filtered_data <- filtered_data[!is.na(filtered_data$`Total Funding`), ]
    }
    
    if (input$postdocOnly) {
      filtered_data <- filtered_data[filtered_data$`Current Position` == "Postdoc", ]
    }
    
    return(filtered_data)
  })
  
  
  output$scatter_plot <- renderPlot({
    plot_data <- data()
    
    if (input$color_var != "None") {
      color_aes <- aes(color = !!sym(input$color_var))
    } else {
      color_aes <- aes()
    }
    
    if (input$x_axis %in% categorical_vars && input$y_axis %in% categorical_vars && input$x_axis != input$y_axis) {
      # Calculate percentage of each group on the x axis belonging to each group on the y axis
      # percentages <- plot_data %>%
      #   group_by(!!sym(input$x_axis), !!sym(input$y_axis)) %>%
      #   summarise(n = n()) %>%
      #   mutate(percentage = n / sum(n)) %>%
      #   ungroup()
      
      # Calculate counts
      counts <- plot_data %>%
        group_by(!!sym(input$x_axis), !!sym(input$y_axis)) %>%
        summarise(n = n()) 
      
      #print(head(plot_data))
  
      
      p <- ggplot(counts, aes(x = !!sym(input$x_axis), y = n, fill = !!sym(input$y_axis))) +
        geom_bar(stat = 'identity', position = 'dodge') +
        ylab("Count") +
        #scale_y_continuous(labels = scales::percent) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6),
              legend.text = element_text(size = 12),
              legend.position = "bottom") +
        guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
        labs(fill = input$y_axis) +
        annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
      
      print(p)
      
    }
    
    else if (input$x_axis %in% categorical_vars) {
      p <- ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        color_aes +
        geom_jitter(width = 0.3, height = 0, alpha = 0.5)
      
      if(input$color_var != "None") {
        p <- p + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", width = 0.2, size = 1, position = position_dodge(0.2))
      } else {
        p <- p + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", width = 0.2, size = 1) +
          stat_summary(fun.y = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1, size = 6, position = position_nudge(x = -0.2))
      }
      
      p <- p + labs(x = input$x_axis, y = input$y_axis) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6),
              legend.text = element_text(size = 12),
              legend.position = "bottom") +
        guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
        annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
      
      print(p)
      
    } 
    
    else {
      p <- ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        color_aes +
        geom_smooth(method = "loess", alpha = 0.15, se = !input$noCB) +  # Use input$noCB to toggle confidence bands
        geom_point(alpha = 0.5) +
        labs(x = input$x_axis, y = input$y_axis) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text = element_text(size = 14),
              legend.text = element_text(size = 12),
              legend.position = "bottom") +
        guides(color = guide_legend(nrow = 2, byrow = TRUE)) +
        annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
      
      ind_data_value <- ind_data()
      
      # Add vertical and horizontal lines for ind_data
      if (!is.na(ind_data_value[[input$x_axis]]) & input$useDataInputs) {
        p <- p + geom_vline(aes(xintercept = ind_data_value[[input$x_axis]]), linetype = "dotted", color = "red", size = 1)
      }
      
      if (!is.na(ind_data_value[[input$y_axis]]) & input$useDataInputs) {
      p <- p + geom_hline(aes(yintercept = ind_data_value[[input$y_axis]]), linetype = "dotted", color = "red", size = 1)
      }
      
      # Add marginal histograms using ggmarginal
      p <- ggMarginal(p, type = "histogram")
      
      print(p)
    }
  })
}








#Run the Shiny app:
shinyApp(ui = ui, server = server)

