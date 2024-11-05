## feed acts and and other information to expert survey...
# Load necessary libraries
library(shiny)
library(shinydashboard)

#bring in data
source('data_manip.R')


# Define the UI
ui <- dashboardPage(
  dashboardHeader(title = "Legislative Quality Survey"),
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "title_page"),
      menuItem("Survey", tabName = "survey")
    )
  ),
  dashboardBody(

    tabItems(
      tabItem(tabName = "title_page",
              uiOutput("title_page_ui"),
              h2("Welcome to the Legislative Quality Survey"),
              p("Thank you for participating in our survey. We are going to ask you about 15 Acts of Partliament in your fields of expertise. After, we will ask you about five well known Acts across different areas of law. Before we begin, we'd like to learn a bit about your policy expertise."),
              checkboxGroupInput("expertise_type", "Policy Expertise (select all that apply):",
                          choices = acts$subject|>unique()),
              p("If you feel want to start again or have made a mistake, you can return to the start screen by clicking the welcome button in the left-hand menu bar (Warning! your progress will not be saved!)"),

              actionButton("start_button", "Start Survey")
      ),
      tabItem(tabName = "survey",
              uiOutput("item_survey_ui"),
              actionButton("next_button", "Next")
      )
    )
  )
)




