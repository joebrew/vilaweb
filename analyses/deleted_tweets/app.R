library(shiny)
library(shinydashboard)
library(DT)
source('prepare_data.R')
df <- data.frame(df)
df$tweet <- as.character(df$content)

# Define the vector for searching
who <- df %>%
  dplyr::distinct(username) %>%
  left_join(users %>% mutate(username = tolower(screen_name)) %>% dplyr::select(username, name))
who_vec <- who$username
names(who_vec) <- paste0(who$name, ' (@', who$username, ')')
starter <- c('x')
names(starter) <- 'TOTS'
who_vec <- c(starter, who_vec)

searcher <- function(user = NULL,
                     term = NULL,
                     retweets_too = TRUE){
  out <- df
  if(!is.null(user)){
    out <- out %>% filter(username %in% user)
  }
  if(!is.null(term)){
    out <- out %>% filter(grepl(tolower(term), tolower(as.character(tweet))))
  }
  if(!retweets_too){
    out <- out %>% filter(!rt)
  }
  return(out)
}


header <- dashboardHeader(title="Piulets esborrats")
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = 'tabs',
    menuItem(
      text="Cerca",
      tabName="cerca",
      icon=icon("eye")),
    menuItem(
      text="Info",
      tabName="info",
      icon=icon("info")),
    uiOutput('menu_ui')
  )
)

body <- dashboardBody(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  # ),
  tabItems(
    tabItem(
      tabName="cerca",
      fluidPage(
        tabsetPanel(
          tabPanel('Taula de piulets esborrats', 
                   fluidPage(fluidRow(
                     column(12,
                            DT::dataTableOutput('the_table'))
                   ))),
          tabPanel('Visualització', 
                   fluidPage(fluidRow(
                     column(8,
                            plotOutput('the_plot')),
                     column(4,
                            fluidRow(
                              uiOutput('person_ui')
                            )
                     )
                   )))
        )
      )),
    
    tabItem(
      tabName="info",
      fluidPage(
        fluidRow(h3('Info')),
        fluidRow(
          
          p('This application uses data from the ',
            tags$a(href = paste0("https://twitter.com/"), 'Twitter'),
            'platform.'),
          p('The deleted tweets were retrieved from ',
            tags$a(href = 'https://politwoops.eu', 'Politwoops'),'.'),
          p('This app was built to accompany ',
            tags$a(href = self_cite(), 'this article'),'.')
          
        )
      )
    )
  ))

# UI
ui <- dashboardPage(header, sidebar, body, skin="blue")

# Server
server <- function(input, output, session) {
  
  reactive_table <- reactive({
    who <- input$who
    message('who is ', who)
    if(!is.null(who)){
      if(who == 'x'){
        who <- NULL
        message('who is NULL')
      }
    }
    
    
    term <- input$filter
    if(!is.null(term)){
      if(term == ''){
        term <- NULL
      }
    }
    
    
    rt <- input$retweet
    if(is.null(rt)){
      rt <- TRUE
    }
    
    out <- searcher(user = who,
                    term = term,
                    retweets_too = rt)
    return(out)
  })
  
  output$the_table <- DT::renderDataTable({
    
    out <- reactive_table()
    ok <- FALSE
    if(!is.null(out)){
      if(nrow(out) > 0){
        ok <- TRUE
      }
    }
    if(!ok){
      return(tibble(' ' = 'No hi ha cap piulet esborrat amb els filtres seleccionats'))
    } else {
      out <- out %>%
        dplyr::select(user_name,
                      content, created_at,
                      deleted_at, hours_up) %>%
        mutate(hours_up = round(hours_up, digits = 3))
      names(out) <- c('Usuari',
                      'Piulet',
                      'Hora de creació',
                      'Hora d\'eliminació',
                      'Hores abans de ser esborrat')
      return(DT::datatable(out, options = list(dom = 'tp')))
    }
  })
  
  output$the_plot <- renderPlot({
    
    individual <- TRUE
    who <- input$who
    if(is.null(who)){
      individual <- FALSE
    } else if(who %in% c('', ' ', 'x')){
      individual <- FALSE
    }
    if(individual){
      plot_when(user = who, ca = TRUE)
    } else {
      plot_n_deleted(ca = TRUE) +
        theme(plot.caption = element_blank()) +
        labs(subtitle = 'Tots els polítics')
    }
  })
  
  output$person_ui <- 
    renderUI({
      
      # Get the person
      individual <- TRUE
      who <- input$who
      if(is.null(who)){
        individual <- FALSE
      } else if(who %in% c('', ' ', 'x')){
        individual <- FALSE
      }
      
      if(individual){
        
        this_user <- users %>%
          filter(tolower(screen_name) == who)
        
        out_text <- paste0(this_user$statuses_count, ' piulets al seu TL.')
        out_text2 <- paste0(nrow(df %>% filter(username == who)), ' piulets esborrats.')
        out_text3 <- paste0('Compte de Twitter des de: ', as.character(as.Date(this_user$account_created_at)))
        
        fluidPage(
          p(out_text),
          p(out_text2),
          p(out_text3),
          br(),
          tags$a(href = paste0("https://twitter.com/", who), paste0("Pàgina de Twitter de @", who)),br(), 
          tags$a(href = paste0("https://politwoops.eu/user/", who), paste0("Pàgina de Politwoops de @", who)),
        )
      } else {
        fluidPage(
          tags$a(href = "https://twitter.com", "Twitter"), br(), br(),
          tags$a(href = "https://politwoops.eu", "Politwoops")
        )
      }
    })
  
  output$menu_ui <- renderUI({
    tab_name <- input$tabs
    if(tab_name == 'info'){
      fluidPage()
    } else {
      fluidPage(
        selectInput('who', 'Polític',
                    who_vec,
                    multiple = FALSE),
        textInput('filter', 'Filtrar texte'),
        checkboxInput('retweet', 'Inclou repiulets en la taula?', value = TRUE)
      )
    }
    
  })
}

shinyApp(ui, server)
