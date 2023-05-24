# setwd("C:/Users/jwben/Documents/Shiny/")
library(shiny)
library(googlesheets4)
library(tidyverse)
library(Hmisc)

ui <- fluidPage(
  titlePanel("EEB Academic Job Market - 2022/2023"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select the variables to plot against one another:"),
      selectInput("x_axis", "X-axis:", choices = c("Current position", "Years since PhD", "Gender", "First author pubs", "Senior author pubs", "Total publications", "Classes taught",  "Applications", "Phone interviews", "Offers", "Offer rate"), selected = "Total publications"),
      selectInput("y_axis", "Y-axis:", choices = c("Current position", "Years since PhD", "Gender", "First author pubs", "Senior author pubs", "Total publications", "Classes taught",  "Applications", "Phone interviews", "Offers", "Offer rate"), selected = "Offer rate"),
      #helpText("Filter data by gender:"),
      #checkboxGroupInput("gender_filter", "Gender:", choices = c("Male", "Female", "Other"), selected = c("Male", "Female", "Other"))
      HTML("<p>Data from <a href='https://docs.google.com/spreadsheets/d/1cqTuSeLtH-Zw7X9ZtnhQxzw3r19Rya9nzdqRW9apTmY/edit#gid=1335543736' target='_blank'>ecoevojobs</a>. Add your data if you were on the job market this year!</p>
           <p>'Offer rate' = Offers / Applications</p>
           <p>'Classes taught' refers to the number of times an applicant was an Instructor of Record</p>
           <p>Loess regression lines have 95% confidence interval bands</p>
           <p>Point ranges for categorical data show mean and 95% confidence limits estimated from bootstrapping (mean_cl_boot function)</p>
           <p>Records are removed during data cleaning steps if they contain invalid data types, which is why sample size here may be different from the ecoevojobs sheet</p>
           <p>Email comments/bugs to jwbenning@gmail.com</p>") # lorem ipsum text
    ),
    mainPanel(
      plotOutput("scatter_plot", height = "400px", width = "400px")
    )
  )
)

csv_export_url <- "https://docs.google.com/spreadsheets/d/1cqTuSeLtH-Zw7X9ZtnhQxzw3r19Rya9nzdqRW9apTmY/gviz/tq?tqx=out:csv&sheet=Anon%20Quals"

sheet_data <- read_csv(csv_export_url, na = c("", "NULL", "NA")) %>%
  slice(-2) %>%
  rename(
    `Current position` = `Current Position`,
    `First author pubs` = `1st Author Pubs Qualifications`,
    `Senior author pubs` = `Senior Author Pubs`,
    `Other pubs` = `Other Pubs (as of 2/1/2023)`,
    `Applications` = `Applications Outcomes 2022-2023`
  ) %>%
  filter(
    grepl("^\\d+$", `First author pubs`),
    grepl("^\\d+$", `Senior author pubs`),
    grepl("^\\d+$", `Other pubs`),
    grepl("^\\d+$", `Applications`),
    grepl("^\\d+$", `Offers`),
    grepl("^\\d+$", `Instructor of Record`),
    grepl("^\\d+$", `1st / Phone Interviews`)
  ) %>%
  mutate(
    Gender = ifelse(Gender %in% c("Male", "Female"), Gender, "Other"),
    `Current position` = ifelse(`Current position` %in% c("Postdoc", "Grad Student", "Visiting Asst Prof", "Asst Prof"), `Current position`, "Other"),
    `Years since PhD` = 2022 - as.numeric(`PhD Year`),
    `Classes taught` = as.numeric(`Instructor of Record`),
    `First author pubs` = as.numeric(`First author pubs`),
    `Senior author pubs` = as.numeric(`Senior author pubs`),
    `Other pubs` = as.numeric(`Other pubs`),
    Applications = as.numeric(Applications),
    `Phone interviews` = as.numeric(`1st / Phone Interviews`),
    Offers = as.numeric(Offers), 
    `Total publications` = `First author pubs` + `Senior author pubs` + `Other pubs`
  ) %>%
  mutate(`Offer rate` = Offers / Applications,
         `Current position` = fct_relevel(`Current position`, "Grad Student", "Postdoc", "Visiting Asst Prof", "Asst Prof", "Other"))

server <- function(input, output) {
  data <- reactive({
    filtered_data <- sheet_data 
      #filter(Gender %in% input$gender_filter)
    
    return(filtered_data)
  })
  
  output$scatter_plot <- renderPlot({
    plot_data <- data()
    
    if (input$x_axis %in% c("Gender", "Current position")) {
      ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        #geom_boxplot() +
        geom_jitter(width = 0.3, height = 0, alpha = 0.5) +
        stat_summary(fun.data = "mean_cl_boot", geom = "pointrange", width = 0.2, size = 1, color = "darkcyan") +
        stat_summary(fun.y = mean, geom = "text", aes(label = round(..y.., 2)), vjust = -1, size = 6, color = "darkcyan", position = position_nudge(x = -0.2)) +
        labs(x = input$x_axis, y = input$y_axis) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text.y = element_text(size = 14),
              axis.text.x = element_text(size = 14, angle = 45, vjust = 0.6)) +
        annotate("text", x = Inf, y = Inf, label = paste("n =", length(plot_data[[input$y_axis]])), hjust = 1, vjust = 1, size = 5)
      
    } else {
      ggplot(plot_data, aes(x = !!sym(input$x_axis), y = !!sym(input$y_axis))) +
        geom_smooth(method = "loess", color = "darkcyan") +
        geom_point(alpha = 0.5) +
        labs(x = input$x_axis, y = input$y_axis) +
        theme_minimal() +
        theme(axis.title = element_text(size = 18),
              axis.text = element_text(size = 14)) +
        annotate("text", x = Inf, y = Inf, label = paste("n =", length(plot_data[[input$y_axis]])), hjust = 1, vjust = 1, size = 5)
    }
  })
}

#Run the Shiny app:
shinyApp(ui = ui, server = server)

