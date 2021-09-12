#############################################################################################
#############################################################################################
################################## FITBIT WIDGET ############################################
################## EXAMPLE DASHBOARD SEEN BY A/B TEST PARTICIPANT ###########################
#############################################################################################
#############################################################################################

### NOTE: The below code has been adapted to show the dashboard of ONLY one person
### In reality, their is a back-end database that updates the daily responses so that:
# (a) the dashboard text varies based on participant's study condition assignment (Treatment vs. Control)
# (b) it syncs with the open-ended response with which they wrote in their Qualtrics survey response
# (c) a unique instance of each dashboard is created with a unique URL associated with each participant

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyjs)
library(tidyverse)
library(plotly)
library(httr)

### ADAPTED TO ONLY INCLUDE ONE EXAMPLE ACCOUNT (TIMTIM159'S FITBIT)
### TOKEN IS ACTIVE FOR ONE YEAR FROM 9/10/2021
### IN ACTUALITY, 'mturk_df_full' takes the full database of crowdsourced participants (vs manual example shown below)

### mturk_df_full:
# Contains token information about their Fitbit Data ALONG WITH information from their Qualtrics RCT Survey
# In other words, dashboard is interactive based on their:
# (1) condition assignment and (2) their open-ended response

mturk_df_full = data.frame(
        file_name = "timothylee-2020-10-08 00:21:10_success.RDS",
         date_recorded = as.Date("2020-04-18"),
         mturk_id = "5e6bbdddd66d92176dd84d05",
         condition_text = "I always park in the furthest parking spot.",
         condition = "Treatment",
         url_hash_initial = "https://whartonfitbit.shinyapps.io/prolific_research/#access_token=eyJhbGciOiJIUzI1NiJ9.eyJhdWQiOiIyMkJLV1IiLCJzdWIiOiI3VDdLNksiLCJpc3MiOiJGaXRiaXQiLCJ0eXAiOiJhY2Nlc3NfdG9rZW4iLCJzY29wZXMiOiJyYWN0IiwiZXhwIjoxNjYyODcwNzU3LCJpYXQiOjE2MzEzMzQ3NTd9.V-Qz1c47lToBkjBJsbSocsuDUg33eOhqLsi4q8p_F-E&user_id=7T7K6K&scope=activity&token_type=Bearer&expires_in=31536000")


ui<-function(request){
  dashboardPage(dashboardHeader(disable = TRUE), dashboardSidebar(disable = TRUE), 
                dashboardBody(shinyjs::useShinyjs(),
                              
                              ## (Removed from example: A validation portal is provided for participants to enter their Prolific/MTurk ID)
                              ## (Alternatively, researchers can send each participant a unique URL that overrides the validation portal)
                              div(id = "userid_screen",
                                  textInput("userid", "Your Prolific ID")),
                              
                              ## The personalized dashboard itself appears on this page
                              div(id = "body_success",
                                  uiOutput("body")
                              ),
                              
                              ## Page for when Fitbit access token is invalid.
                              shinyjs::hidden(
                                div(
                                  id = "error_msg",
                                  div(
                                    h4(HTML(paste("Error: You do not permission to access this page, or you revoked access to your Fitbit step count history (either manually or automatically after 30 days).", sep = "")))
                                  ))
                              )), skin = "blue")
}

server <- function(input, output, session) {
  shinyjs::hide("userid_screen")
  shinyjs::show("body_success")
  
  output$body <- renderUI({
      
      time <- Sys.time()
      ID <<- trimws(input$userid)
      ID <<- "5e6bbdddd66d92176dd84d05"
      if(ID == "") {
        ID <<- "blank"
      }
      
      ## Back-end to filter to the appropriate MTurk ID / Fitbit token
      id_df = mturk_df_full %>% 
        filter(mturk_id == ID) %>%
        mutate(contains_success = ifelse(grepl("_success", file_name), 1, 0)) %>% 
        filter(contains_success == 1) %>% 
        slice(nrow(.))
      
      print(id_df)
      
      user_condition_text <<- id_df$condition_text
      user_condition_group <<- id_df$condition
      
      ### Display dashboard if it's a valid account, but show Error Page if it's an invalid (bad) account
      is_bad_account = FALSE #initialize bad account as false
      tryCatch(
        expr = {
          if (grepl("access_token=", id_df$url_hash_initial)) {
            full_url = id_df$url_hash_initial[1]
            first_cut_url = strsplit(full_url, "access_token=")[[1]][2]
            key = strsplit(first_cut_url, "&user_id")[[1]][1]
            url_x = "activities/steps"
            start_date = unique(id_df$date_recorded)[1] - 7
            end_date = "today"
            url = paste0("https://api.fitbit.com/1/user/-/", url_x, "/date/", start_date, "/", end_date, ".json")
            
            resp = GET(url, add_headers(Authorization = paste("Bearer", key, sep = " ")))
            json_list = content(resp) #step count data (in json format)
            
            date_recorded = unique(as.Date(id_df$date_recorded, origin='1970-01-01'))
            tracker_item = names(json_list)[1]
            tracker_df = do.call(rbind, json_list[[tracker_item]]) %>% 
              as.data.frame() %>% 
              mutate(tracker_item = tracker_item) %>% 
              mutate(date_recorded = date_recorded)
            
            is_bad_account = nrow(tracker_df) < 1
          }
        },
        error = function(e){ 
          is_bad_account <<- TRUE
          print("error found")
          print(paste0("Is bad account value: ", is_bad_account))
          return(is_bad_account)
        })
      
      fileName <- paste(ID, time, sep="-")
      
      if (is_bad_account) {
        print("Bad account spot")
        shinyjs::hide("body_success")
        shinyjs::show("error_msg")
        fileName = paste0(fileName, "_fail")
        
      } else{
        
        fileName = paste0(fileName, "_success")
        
        set.seed(1996)
        
        ### Format dataset to plot an interactive graph of the steps
        plot_df = tracker_df %>% 
          mutate(activities_steps = as.numeric(unlist(value))) %>%
          mutate(date_time = as.Date(unlist(dateTime))) %>% 
          mutate(day_of_week = weekdays(date_time)) %>% 
          arrange(date_time) %>% 
          # mutate(activities_steps = ifelse(activities_steps > 16000, 12279, activities_steps)) %>% 
          mutate(post_survey = ifelse(date_time >= date_recorded, "After Survey", "Before Survey")) %>% 
          filter(date_time <= date_recorded + 6)
        
        # re-arrange so Before Survey comes first, then After Survey
        plot_df$post_survey <- factor(plot_df$post_survey, levels = c("Before Survey", "After Survey"))
        
        ### Get the "Before Survey" and "After Survey" data
        pre_average_steps = plot_df %>% 
          filter(post_survey == "Before Survey") %>% 
          filter(activities_steps != 0) %>% 
          summarise(mean(.$activities_steps, na.rm = TRUE)) %>% unlist(use.names = FALSE)
        
        during_average_steps = plot_df %>% 
          filter(post_survey != "Before Survey") %>% 
          arrange(date_time) %>% 
          slice(-nrow(.)) %>% 
          filter(activities_steps != 0) %>% 
          summarise(mean(.$activities_steps, na.rm = TRUE)) %>% unlist(use.names = FALSE)
        
        if (Sys.Date() >= (unique(plot_df$date_recorded)[1] + 7)) {
          during_average_steps = plot_df %>% 
            filter(post_survey != "Before Survey") %>% 
            arrange(date_time) %>% 
            filter(activities_steps != 0) %>% 
            summarise(mean(.$activities_steps, na.rm = TRUE)) %>% unlist(use.names = FALSE)
        }
        
        if(is.nan(during_average_steps)){during_average_steps = 0}
        
        days_completed <<- plot_df %>% 
          filter(post_survey != "Before Survey") %>% 
          slice(-nrow(.)) %>% 
          nrow() %>% unlist(use.names = FALSE)
        
        if(days_completed > 7 | Sys.Date() >= (unique(plot_df$date_recorded)[1] + 7)){days_completed <<- 7}
        
        rounded_pre_avg <<- round(pre_average_steps)
        rounded_during_avg <<- round(during_average_steps)
        
        output$show_steps = renderPlotly({
          
          plot_df = plot_df %>%
            mutate(pre_average_steps = pre_average_steps) %>%
            mutate(during_avg_steps = during_average_steps)
          
          fig <- plot_df %>%
            plot_ly(type = 'bar',
                    x = ~date_time,
                    y = ~activities_steps,
                    text = ~pre_average_steps,
                    color = ~post_survey,
                    colors = c("slategrey", "forestgreen"),
                    showlegend = TRUE,
                    hovertemplate = ~paste0('Date: %{x} (', post_survey, ")",
                                            '<br><b>Steps: %{y}</b>',
                                            '<br>Avg Before Survey: %{text: .0f}',
                                            '<br>Avg After Survey: ', round(during_avg_steps, 0),
                                            '<extra></extra>')) %>%
            layout(
              title = list(
                xref = "paper",
                yref = "paper"),
              xaxis = list(
                title = 'Date',
                type = 'date',
                tickformat = '%a, %b %d',
                tickangle = -60,
                tickmode = 'linear'),
              yaxis = list(
                title = 'Steps',
                tickformat = 'digit'
              ))
          
          fig
        })
        
      }
      
      ### Interactive portions based on their Survey Response Data
      tabItem(tabName ="dashboard", class = "active",
              fluidRow(
                
                tags$style(".info-box-icon.bg-green { background-color: #228B22 !important; color: #FFFFFF !important; }"),
                
                infoBox("To increase your step count, you wrote:", 
                        value = tags$p(paste0("\"", user_condition_text, "\""), style = "font-size: 200%;"), 
                        color = "green",
                        icon = icon("user-edit"), width = 12),
                
                tags$style(".small-box.bg-black { background-color: #778899 !important; color: #FFFFFF !important; }"),
                tags$style(".small-box.bg-green { background-color: #228B22 !important; color: #FFFFFF !important; }"),
                
                valueBoxOutput("previousBox"),
                
                valueBoxOutput("currentBox"),
                
                valueBoxOutput("progressBox"),
                
                tags$style(HTML(".box.box-solid.box>.box-header h3.box-title {font-weight: bold; 
                                font-size: 165%; font-family:'Arial'}")),
                tags$style(HTML(".box.box-solid.box>.box-header {text-align: center}")),
                
                box(width = 8,
                    title = HTML("Steps: Before and After Survey"), solidHeader = TRUE,
                    column(width = 12, align = "center",
                           div(plotlyOutput("show_steps", height = "100%"), align = "center"))),
                
                uiOutput("overview"), uiOutput("aboutus"),
                tags$style('#mydiv{font-family:"Arial"; font-size:120%}'),
                tags$style('#mydiv2{font-family:"Arial"; font-size:130%}'),
                tags$style('#mydiv3{font-family:"Arial"; font-size:100%}')
              ))
  })
  
  ### Average Steps Before the Survey
  output$previousBox <- renderValueBox({
    valueBox(
      paste0(format(rounded_pre_avg, big.mark = ","), " Steps"), 
      subtitle = tags$p(HTML("Average <b>Before</b> Survey"), style = "font-size: 175%;"),
      icon = icon("shoe-prints", lib = "font-awesome"),
      color = "black"
    )
  })
  
  ### Average Steps After the Survey
  output$currentBox <- renderValueBox({
    valueBox(
      paste0(format(rounded_during_avg, big.mark = ","), " Steps"), 
      subtitle = tags$p(HTML("Average <b>After</b> Survey"), style = "font-size: 175%;"),
      icon = icon("shoe-prints", lib = "font-awesome"),
      color = "green"
    )
  })
  
  ### How many days are left in the intervention
  output$progressBox <- renderValueBox({
    valueBox(
      paste0(round(days_completed), " of 7 Days"), 
      subtitle = tags$p(HTML("Days Completed"), style = "font-size: 175%;"),
      icon = icon("clock", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  ### Study Overview Reminder (e.g., A/B testing with the text from the Treatment vs. Control Condition)
  output$overview <- renderUI({
    
    personal_rules_text = paste("A personal rule is a <b>principle you vow to stick to <i>without exception</i></b>. It is something you <b>always</b> do. 
                                In the Fitbit survey, you took the initiative to create your own personal rule to help you increase your step count. Good job!",
                                "Sometimes it can be hard to follow a personal rule <i>without exception</i>, but try your best for at least one week.",
                                "After all, every step you take is a step towards a healthier you!", sep = "<br><br>")
    
    plans_text = paste("A plan is <b>something you intend to do</b>. 
                                In the Fitbit survey, you took the initiative to create your own plan to help you increase your step count. Good job!",
                       "Sometimes it can be hard to follow a plan, but try your best for at least one week.",
                       "After all, every step you take is a step towards a healthier you!", sep = "<br><br>")
    
    if(user_condition_group == "Treatment") {
      box( 
        title = HTML(paste0("<div id='mydiv2'>", "<b>Study Overview</b>", "</div>")), 
        closable = FALSE, 
        enable_label = TRUE,
        width = 4,
        solidHeader = FALSE, 
        collapsible = FALSE,
        HTML("<div id='mydiv'>", personal_rules_text, "</div>")
      )
    } else{
      box(
        title = HTML(paste0("<div id='mydiv2'>", "<b>Study Overview</b>", "</div>")), 
        closable = FALSE, 
        enable_label = TRUE,
        width = 4,
        solidHeader = FALSE, 
        collapsible = FALSE,
        HTML("<div id='mydiv'>", plans_text, "</div>")
      ) 
    }
    
  })
  
  ### Additional Box for displaying for troubleshooting the Fitbit numbers
  output$aboutus <- renderUI({
    
    about_us_text = paste0("<i>If these numbers don't look right, try re-syncing your Fitbit with the Fitbit mobile app and refreshing this page.
                           <br><br>
                           Also note that today's steps are excluded from the average, because the day is not finished yet. </i>")
    
    box(
      title = HTML(paste0("<div id='mydiv2'>", "<b>Need Help?</b>", "</div>")), 
      closable = FALSE, 
      enable_label = TRUE,
      width = 4,
      solidHeader = FALSE, 
      collapsible = TRUE,
      HTML("<div id='mydiv3'>", about_us_text, "</div>")
    )
  })
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
