# setwd("C:/Users/jwben/Documents/Shiny/")
library(shiny)
library(googlesheets4)
library(tidyverse)
library(Hmisc)

ui <- fluidPage(
  titlePanel("Explore the EEB Job Market"),
  h4(HTML("Data from <a href='https://docs.google.com/spreadsheets/d/1cqTuSeLtH-Zw7X9ZtnhQxzw3r19Rya9nzdqRW9apTmY/edit#gid=1335543736' target='_blank'>EcoEvoJobs.net</a>, for 2021/2022 and 2022/2023 job cycles")),
  fluidRow(
    column(width = 4,
           div(style = "padding: 10px; background-color: #f7f7f7;",
             helpText("Select the variables to plot against one another:"),
             selectInput("x_axis", "X-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "Senior Author Pubs", "Total Publications", "Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Phone Interview Rate", "Campus Interviews", "Campus Interview Rate", "Offers", "Offer Rate"), selected = "Publication Rate"),
             selectInput("y_axis", "Y-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "Senior Author Pubs", "Total Publications", "Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Phone Interview Rate", "Campus Interviews", "Campus Interview Rate", "Offers", "Offer Rate"), selected = "Offer Rate"),
             selectInput("color_var", "Color by:", choices = c("None", "Job Cycle", "Current Position", "Gender"), selected = "None"),
             checkboxInput("postdocOnly", "Postdoc data only", value = FALSE),
             checkboxInput("noCB", "No confidence bands", value = FALSE)
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
    HTML("<p>Add your data <a href='https://forms.gle/Q7DDngHaVGQDiEZc7' target='_blank'>here</a> if you were on the job market this year!</p>
      <p>'Offer / Interview Rate' = (Offers or Interviews) / Applications</p>
      <p>'Publication Rate' = Total Publications / Years Since PhD</p>
      <p>'Classes taught' refers to the number of times an applicant was an Instructor of Record</p>
      <p>Loess regression lines have 95% confidence interval bands</p>
      <p>Point ranges for categorical data show mean and 95% confidence limits estimated from bootstrapping (mean_cl_boot function)</p>
      <p>Download the original, cleaned, and R-processed data used in this app <a href='https://docs.google.com/spreadsheets/d/1Wmz6N01mSrpcTfIoMH0Rg42xL64SRKYKzIrAZmDGQ7U/edit#gid=697264455' target='_blank'>here</a>. Data cleaning was necessarily subjective to some extent. All data cleaning steps are recorded in `Data Cleaning` fields for individual records, and on the 'General Cleaning Notes' worksheet. App data will be updated semi-regularly to include new records added via EcoEvoJobs.</p>
      <p>Interpret all relationships with a grain of salt -- who knows if the sampling here is representative, and there is inherent messiness (e.g., 'Offers' could encompass R1, SLAC, and even non-academic job offers). </p>
      <p>Email comments/bugs to jwbenning@gmail.com</p>")
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
         `Publication Rate` = `Total Publications` / `Years Since PhD`,
         `Current Position` = fct_relevel(`Current Position`, "Grad Student", "Postdoc", "Non TT faculty", "Asst Prof", "Other"),
         `Total Funding` = fct_relevel(as.factor(`Total Funding`), "$1-50,000", "$50,000-100,000", "$100,000-250,000", "$250,000-500,000", "$500,000-$1M", "$1-2M", "$2M+"))

categorical_vars <- c("Gender", "Current Position", "Total Funding")


server <- function(input, output) {
  data <- reactive({
    filtered_data <- sheet_data[!is.na(sheet_data[[input$y_axis]]), ]
    
    if(is.numeric(sheet_data[[input$y_axis]])) {
      filtered_data <- filtered_data[is.finite(filtered_data[[input$y_axis]]), ]
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
        annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
      
      print(p)
    }
  })
}





# server <- function(input, output) {
#   data <- reactive({
#     filtered_data <- sheet_data[!is.na(sheet_data[[input$y_axis]]) & is.finite(sheet_data[[input$y_axis]]), ]
#     
#     if (input$x_axis == "Total Funding") {
#       filtered_data <- filtered_data[!is.na(filtered_data$`Total Funding`), ]
#     }
#     
#     if (input$postdocOnly) {
#       filtered_data <- filtered_data[filtered_data$`Current Position` == "Postdoc", ]
#     }
#     
#     return(filtered_data)
#   })
#   
#   output$scatter_plot <- renderPlot({
#     plot_data <- data()
#     
#     if (input$color_var != "None") {
#       color_aes <- aes(color = !!sym(input$color_var))
#     } else {
#       color_aes <- aes()
#     }
#     
#     if (input$x_axis %in% categorical_vars) {
#       p <- ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
#         color_aes +
#         geom_jitter(width = 0.3, height = 0, alpha = 0.5)
#       
#       if(input$color_var != "None") {
#         p <- p + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", width = 0.2, size = 1, position = position_dodge(0.2))
#       } else {
#         p <- p + stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", width = 0.2, size = 1) +
#           stat_summary(fun.y = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1, size = 6, position = position_nudge(x = -0.2))
#       }
#       
#       p <- p + labs(x = input$x_axis, y = input$y_axis) +
#         theme_minimal() +
#         theme(axis.title = element_text(size = 18),
#               axis.text.y = element_text(size = 14),
#               axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6),
#               legend.position = "bottom") +
#         annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
#       
#       print(p)
#       
#     } else {
#       p <- ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
#         color_aes +
#         geom_smooth(method = "loess", alpha = 0.1, se = !input$noCB) +  # Use input$noCB to toggle confidence bands
#         geom_point(alpha = 0.5) +
#         labs(x = input$x_axis, y = input$y_axis) +
#         theme_minimal() +
#         theme(axis.title = element_text(size = 18),
#               axis.text = element_text(size = 14),
#               legend.position = "bottom") +
#         annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
#       
#       print(p)
#     }
#   })
# }











#Run the Shiny app:
shinyApp(ui = ui, server = server)

