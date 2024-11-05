## feed acts and and other information to expert survey...


## Allow the experts to have no opinion...
## A control for prominent and less prominent acts.

acts <- readRDS("data/sentiment/s01_austlii_linked.rds")

# randomise the order of the acts.

set.seed(123)

acts<-acts[sample(1:nrow(acts)), ]

# restricting to acts whose subject we have information...

acts<-acts[!is.na(acts$subject),]

#exporting the first

table(acts$subject)

df<- acts[1:10,]

# Load necessary libraries
library(shiny)
library(shinydashboard)

# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Item Success Survey"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Survey", tabName = "survey")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "survey",
              uiOutput("item_survey_ui"),
              actionButton("next_button", "Next")
      )
    )
  )
)

# Define the server
server <- function(input, output) {
  current_item <- reactiveVal(1)
  num_items <- nrow(df)

   responses <- data.frame(
    modernName = character(),
    id = character(),
    assent = character(),
    subject = character(),
    policy_success_rating= numeric(),
    gov_success_rating = numeric(),
    dont_know=character(),
    feedback = character(),
    stringsAsFactors = FALSE
  )

  output$item_survey_ui <- renderUI({
    item <- df[current_item(), ]
    url <- a("Federal Register of Legislation", href=paste0("https://www.legislation.gov.au/Details/",item$id))
    fluidRow(
      box(
          title =  HTML(paste0("<b>", item$modernName, "</b>")),

          tagList("See a summary of this Act on the ", url),

          tags$br(),tags$br(),

          selectInput("dont_know", "How would you rate your knowledge of this Act?",
                      c("Good" = "good",
                        "Passable" = "passable",
                        "Don't know enough to make a judgment" = "dont_know")),

          sliderInput("policy_success_rating", "How successful was the Act for public policy", min = 1, max = 10, value = 5),
          sliderInput("gov_success_rating", "How much did the Act achieve the government's objectives?", min = 1, max = 10, value = 5),
          textAreaInput("feedback", "Additional Feedback:", rows = 3),

      )
    )
  })

  observeEvent(input$next_button, {
    survey_results <- list(
      modernName = df[current_item(), ]$modernName,
      id = df[current_item(), ]$id,
      assent = df[current_item(), ]$assent,
      subject = df[current_item(), ]$subject,
      policy_success_rating = input$policy_success_rating,
      gov_success_rating = input$gov_success_rating,
      dont_know= input$dont_know,
      feedback = input$feedback
    )
    responses <<- rbind(responses, survey_results)

    # Process the survey results (you can do whatever you want with them here)
    print(survey_results)

    # Move to the next item or end the survey
    if (current_item() < num_items) {
      current_item(current_item() + 1)
    } else {
      showModal(
        modalDialog(
          title = "Survey Completed",
          "Thank you for completing the survey!",
          footer = NULL,
          easyClose = TRUE
        )
      )
      # Save responses to a CSV file
      write.csv(responses, "survey_responses.csv", row.names = FALSE)
    }
  })
}

# Run the application
shinyApp(ui, server)





# # Split the dataframe by 'subject'
# subject_list <- split(acts, acts$subject)
#
# # Create individual dataframes with names as subject names
# for (subject_name in names(subject_list)) {
#   assign(subject_name, subject_list[[subject_name]], envir = .GlobalEnv)
# }
#
# # Output each dataframe to a CSV file
#
# output_folder <- 'data/experts/'
# prefix <- 'e01_'
#
# if (!dir.exists(output_folder)) {
#   dir.create(output_folder, recursive = TRUE)
# }
#
# for (subject_name in names(subject_list)) {
#   filename <- paste0(output_folder, prefix, subject_name, '.csv')
#   write.csv(get(subject_name), file = filename, row.names = FALSE)
# }
#
#
# write.csv(names(subject_list),file=paste0(output_folder, prefix, 'subject_areas.csv'))
