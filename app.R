# setwd("C:/Users/jwben/Documents/Shiny/")
library(shiny)
library(googlesheets4)
library(tidyverse)
library(Hmisc)

ui <- fluidPage(
  titlePanel("EEB Academic Job Market - 2021/2022 and 2022/2023"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the variables to plot against one another:"),
      selectInput("x_axis", "X-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "Senior Author Pubs", "Total Publications", "Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Offers", "Offer Rate"), selected = "Total Publications"),
      selectInput("y_axis", "Y-axis:", choices = c("Current Position", "Years Since PhD", "Gender", "First Author Pubs", "Senior Author Pubs", "Total Publications", "Publication Rate", "Fellowships", "Major Grants", "Total Funding", "Classes Taught",  "Applications", "Phone Interviews", "Offers", "Offer Rate"), selected = "Offer Rate"),
      selectInput("color_var", "Color by:", choices = c("None", "Job Cycle", "Current Position", "Gender"), selected = "None"),
      checkboxInput("postdocOnly", "Postdoc data only", value = FALSE),
      checkboxInput("noCB", "No confidence bands", value = FALSE),
      HTML("<p>Data from <a href='https://docs.google.com/spreadsheets/d/1cqTuSeLtH-Zw7X9ZtnhQxzw3r19Rya9nzdqRW9apTmY/edit#gid=1335543736' target='_blank'>ecoevojobs</a>. Add your data if you were on the job market this year!</p>
           <p>'Offer Rate' = Offers / Applications</p>
           <p>'Publication Rate' = Total Publications / Years Since PhD</p>
           <p>'Classes taught' refers to the number of times an applicant was an Instructor of Record</p>
           <p>Loess regression lines have 95% confidence interval bands</p>
           <p>Point ranges for categorical data show mean and 95% confidence limits estimated from bootstrapping (mean_cl_boot function)</p>
           <p>Records are removed during data cleaning steps if they contain invalid data types, which is why sample size here may be different from the ecoevojobs sheet</p>
           <p>Email comments/bugs to jwbenning@gmail.com</p>") 
    ),
    mainPanel(
      plotOutput("scatter_plot", height = "400px", width = "400px")
    )
  )
)

#Sys.setenv(VROOM_CONNECTION_SIZE = "462144")

csv_export_url <- "https://docs.google.com/spreadsheets/d/1Wmz6N01mSrpcTfIoMH0Rg42xL64SRKYKzIrAZmDGQ7U/gviz/tq?tqx=out:csv&sheet=Main"

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
  mutate(`Offer Rate` = Offers / Applications,
         `Publication Rate` = `Total Publications` / `Years Since PhD`,
         `Current Position` = fct_relevel(`Current Position`, "Grad Student", "Postdoc", "Non TT faculty", "Asst Prof", "Other"),
         `Total Funding` = fct_relevel(as.factor(`Total Funding`), "$1-50,000", "$50,000-100,000", "$100,000-250,000", "$250,000-500,000", "$500,000-$1M", "$1-2M", "$2M+"))



server <- function(input, output) {
  data <- reactive({
    filtered_data <- sheet_data[!is.na(sheet_data[[input$y_axis]]) & is.finite(sheet_data[[input$y_axis]]), ]
    
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
    
    if (input$x_axis %in% c("Gender", "Current Position", "Total Funding")) {
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
              legend.position = "bottom") +
        annotate("text", x = Inf, y = Inf, label = paste("n =", nrow(plot_data)), hjust = 1, vjust = 1, size = 5)
      
      print(p)
      
    } else {
      p <- ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        color_aes +
        geom_smooth(method = "loess", alpha = 0.1, se = !input$noCB) +  # Use input$noCB to toggle confidence bands
        geom_point(alpha = 0.5) +
        labs(x = input$x_axis, y = input$y_axis) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text = element_text(size = 14),
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
#     if (input$x_axis %in% c("Gender", "Current Position", "Total Funding")) {
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
#         geom_smooth(method = "loess", alpha = 0.) +
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

