# Load necessary libraries
library(shiny)
library(shinydashboard)
library(googlesheets4)
library(dplyr)

#bring in data
source('data_manip.R')


# google authentication

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = ".secrets"
)

# googledrive::drive_auth()
# googlesheets4::gs4_auth()


# google sheets setup

# googlesheets4::gs4_create(name = "legislation_ratings",
#                           # Create a sheet called main for all data to
#                           # go to the same place
#                           sheets = "main")


sheet_id <- googledrive::drive_get("legislation_ratings")$id


# Define the server
server <- function(input, output, session) {

  current_item <- reactiveVal(1)

  observeEvent(input$start_button, {
     switch(input$tabs,
           "title_page" = "survey"
    )
    updateTabItems(session, "tabs", "survey")
  })


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

    req(input$expertise_type)
    df1<- acts[acts$subject %in% input$expertise_type,]
    df2<- anchor_acts
    df_custom<-rbind(df1[1:15,],df2)

    print(current_item())

    num_items <- nrow(df_custom)

    item <- df_custom[current_item(), ]
    url <- a("Federal Register of Legislation", href=paste0("https://www.legislation.gov.au/Details/",item$id),
             target="_blank")
    fluidRow(
      box(
        title =  HTML(paste0("<b>", item$modernName, "</b>")),

        HTML(paste0("Area of law: ","<b>",item$subject,"</b>")),

        tags$br(),tags$br(),

        tagList("See a summary of this Act on the ", url),

        tags$br(),tags$br(),
        paste("(Act",current_item(), "of 20)"),
        tags$br(),tags$br(),

        selectInput("dont_know", "How would you rate your knowledge of this Act?",
                    c("Good" = "good",
                      "Passable" = "passable",
                      "Don't know enough to make a judgment" = "dont_know")),

        sliderInput("policy_success_rating", "How successful was the Act for public policy?", min = 0, max = 10, value = 5),
        sliderInput("gov_success_rating", "How much did the Act achieve the government's objectives?", min = 0, max = 10, value = 5),
        textAreaInput("feedback", "Additional Feedback:", rows = 3),


      )
    )
  })


  observeEvent(input$next_button, {

    req(input$expertise_type)
    df1<- acts[acts$subject %in% input$expertise_type,]
    df2<- anchor_acts
    df_custom<-rbind(df1[1:15,],df2)
    num_items <- nrow(df_custom)


    survey_results <- list(
      modernName = df_custom[current_item(), ]$modernName,
      id = df_custom[current_item(), ]$id,
      assent = df_custom[current_item(), ]$assent,
      subject = df_custom[current_item(), ]$subject,
      policy_success_rating = input$policy_success_rating,
      gov_success_rating = input$gov_success_rating,
      dont_know= input$dont_know,
      feedback = input$feedback
    )
    responses <<- rbind(responses, survey_results)

    # Process the survey results (you can do whatever you want with them here)
    #print(survey_results)

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

      # file_name<-paste0(gsub(" ","_",df_custom$subject[1]),'_',format(Sys.time(), "%b_%d_%Y"),'.csv')

      now<-Sys.time()
      attr(now, 'tzone') <- "Australia/ACT"
      now<-lubridate::round_date(now, "1 seconds")
      responses$response_time_cbr <- as.character(now)

      # Read our sheet
      values <- read_sheet(ss = sheet_id,
                           sheet = "main")

      # Check to see if our sheet has any existing data.
      # If not, let's write to it and set up column names.
      # Otherwise, let's append to it.

      if (nrow(values) == 0) {
        sheet_write(data = responses,
                    ss = sheet_id,
                    sheet = "main")
      } else {
        sheet_append(data = responses,
                     ss = sheet_id,
                     sheet = "main")
      }

    }
  })
}


