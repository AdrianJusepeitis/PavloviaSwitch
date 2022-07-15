library(tidyr)
library(dplyr)
library(shinyalert)
library(lubridate)

# Define server logic ----
server <- function(input, output, session) {
  
  # generate ui based on query----
  output$Ui <- renderUI({
    if("start" %in% names(getQueryString())
    ){
      # what to do when starting study ----
      # which study ?
      started_study <- getQueryString()$start %>% as.character()
      
      # update participant list and study details
      study_details <- read.csv(paste0("studies/",started_study,"/study_details.csv"))
      participants <- read.csv(paste0("studies/",started_study,"/participants.csv")) %>% separate(SwitchID, sep = "_", into = c("study", "group_tmp", "ID"))
      # set time outs
      participants$status[participants$status == "active" & strptime(participants$started, format = "%Y-%m-%d %H:%M:%S") < Sys.time() - as.numeric(study_details$timeout_threshhold[1])*60] <- "timed out"
      # update study details
      study_details[,c("active_n","timed_out_n","completed_n")] <- table(participants$group %>% factor(levels = study_details$group),
                                                                         participants$status %>% factor(levels = c("active","timed out","completed")))
      # # free places?
      if(!all(study_details$wanted_n == (study_details$completed_n + study_details$active_n))){
        
        # which group ?
        selected_group <- sample(
            study_details %>% 
              group_by(group) %>% 
              summarise(full = (completed_n + active_n)/ wanted_n) %>% 
              filter(full == min(full)) %>% 
              pull(group) %>% 
              as.character(), 1)
        
        # which participant id ?
        ID <- max(c(participants$ID[participants$group == selected_group] %>% as.numeric(),0), na.rm = T) + 1 
        
        # combine to SwitchID
        SwitchID <- paste0(c(started_study, selected_group, ID), collapse = "_")
        
        # update participant list
        participants <- rbind(participants %>% unite(col = "SwitchID", c(1,2,3), sep = "_"),
                              c(SwitchID = SwitchID, group = selected_group, started = Sys.time() %>% as.character, status = "active", time_taken = NA) %>% as.list())
        
        write.csv(participants,
                  paste0("studies/",started_study,"/participants.csv"), row.names = F)
        
        # update study details
        study_details$started_n <- table(participants$group %>% factor(levels = study_details$group))
        study_details$active_n <- table(participants$group %>% factor(levels = study_details$group),
                                        participants$status %>% factor(levels = c("active","timed out","completed")))[,"active"]
        write.csv(study_details, paste0("studies/",started_study,"/study_details.csv"), row.names = F)
        
        # attach query strings
        q <- getQueryString()
        
        qstr <- if(all(names(q) == "start")){""}else{
          q %>% 
            as.data.frame() %>% 
            select(-start) %>% 
            pivot_longer(col = everything()) %>% 
            unite(col = "string", c(name, value), sep = "=") %>% 
            pull(string) %>% 
            paste(collapse = "&")%>%
            paste0("&",.)}
        
        # redirect
        singleton(tags$head(tags$script(HTML("window.location.replace('URL');" %>% 
                                               gsub("URL",
                                                    paste0(study_details$send_link[study_details$group == selected_group], "&SwitchID=", SwitchID,qstr
                                                    ),
                                                    .))
        )))
        
      }else{"This experiment is currently not available."}
      
    }else if("completed" %in% names(getQueryString())){
      
      # what to do when completing study----
      # get id
      completed_id <- getQueryString()$completed %>% as.character()
      # which study
      completed_study <- strsplit(completed_id, split = "_")[[1]][1]
      # update partcipant list
      participants <- read.csv(paste0("studies/",completed_study,"/participants.csv"))
      participants$status[participants$SwitchID == completed_id] <- "completed"
      participants$time_taken[participants$SwitchID == completed_id] <- difftime(Sys.time(), strptime(participants$started[participants$SwitchID == completed_id], format = "%Y-%m-%d %H:%M:%S"), units = "sec") %>% round() %>% seconds_to_period() %>% as.character()
      
      write.csv(participants, paste0("studies/",completed_study,"/participants.csv"), row.names = F)
      
      # update study details
      study_details <- read.csv(paste0("studies/",completed_study,"/study_details.csv"))
      study_details[,c("active_n","timed_out_n","completed_n")] <- table(participants$group %>% factor(levels = study_details$group),
                                                                         participants$status %>% factor(levels = c("active","timed out","completed")))
      # redirect
      if(study_details$completion_link[1] != "none"){
        singleton(tags$head(tags$script(HTML("window.location.replace('URL');" %>% gsub("URL",study_details$completion_link[1],.)))))
      }else{"You may close this window now."}
      
    }else{
      # backend ----
      list(  
        # App title ----
        titlePanel(windowTitle = "PavloviaSwitch",
                   HTML("<p style='font-family:Impact, Charcoal, sans-serif'>PavloviaSwitch <img src='switch-tracks.png' width='80'></p>")),
        # Main panel for displaying outputs ----
        mainPanel(
          
          # Output: Tabset w/ plot, summary, and table ----
          tabsetPanel(id = "tabs",
                      type = "tabs",
                      tabPanel("How it works",
                               htmlOutput("hiw_text")),
                      tabPanel("New switch",
                               fluidRow(column(4,
                                               textInput("username", "Pavlovia username:"),
                                               textInput("studyname", "Name of the experiment:"),
                                               textInput("foldername", "Experiment Settings --> Online --> Output path (may be empty):"),
                                               textInput("endlink", "URL to send subjects to after completion? (may be empty or URL starting with 'https://www.'):"),
                                               numericInput("timeout_threshhold","How many minutes until a subject is timed out?", min = 1, value = 30),
                                               conditionalPanel("input.username != '' &&
                                                                input.studyname != '' &&
                                                                input.n_groups >= 1 &&
                                                                input.n_groups <= 100 &&
                                                                output.all_ngroups_lt1 &&
                                                                input.timeout_threshhold >= 1",
                                                                actionButton("create", "Create new switch", class = "btn-success"))),
                                        column(2, 
                                               numericInput("n_groups","How many groups?", value = 2, min = 1, max = 100),
                                               uiOutput("ninputs"))
                               )
                      ),
                      tabPanel("View existing switch",
                               fluidRow(
                                 column(2,textInput("study_id_view","ID"),
                                        selectInput("view", "View", choices = c("Overview","Participants"), selected = "Overview")),
                                 column(10,conditionalPanel("input.view=='Overview'",
                                                            tableOutput("study_details"),
                                                            htmlOutput("study_info")),
                                        conditionalPanel("input.view=='Participants'",
                                                         dataTableOutput("participants"), 
                                                         uiOutput("download_button")
                                        ))
                               )
                      )
          )
        )
      )
    }
  })
  
  # what to do for the backend----
  # How it works text
  output$hiw_text <- renderText('<p><strong>PavloviaSwitch&nbsp;</strong>is a tool to allow for controlled/deterministic counterbalancing in <a href="http://pavlovia.org" rel="noopener noreferrer" target="_blank">pavlovia</a>-hosted PsychoJS experiments. After creating a switch you may provide your participants with a single URL that will guide them to your study via PavloviaSwitch which will tag them with an ID and a group. After completing your experiment participants are send back to PavloviaSwitch. In this way, PavloviaSwitch knows how many participants completed which condition and can produce a prespecified distribution of participants to your conditions/groups.</p>
<p><strong>How to use it?</strong></p>
<ol>
    <li><u>Create a new Switch:</u> Open the tab &quot;New switch&quot;. <br><br>
        <ol>
            <li>Specify which experiment your participants should be send to by entering your pavlovia <em>username&nbsp;</em>and the<em>&nbsp;experiment name</em>.&nbsp;</li>
            <li>Enter the <em>directory&nbsp;</em>of your .js file, this is empty or &quot;html&quot; in most cases.&nbsp;</li>
            <li>Specify after how many minutes a participant should be<em>&nbsp;timed out</em> so that their place/group can be reassigned to a new participant.&nbsp;</li>
            <li>You may enter a <em>completion&nbsp;</em><em>link</em> that participants are redirected to, after finishing your experiments and coming back to PavloviaSwitch (e.g., a prolific completion link).</li>
            <li>Specify <em>how many different groups</em> your participants should be assigned to.</li>
            <li>Specify <em>how many participants</em> are needed in each group. PavloviaSwitch will not send more participants to your experiment than specified here.</li>
            <li>Click &quot;Create new switch&quot; and note down the ID and the entry link that are displayed. This is the link that will direct your participants to your experiments via PavloviaSwitch.<br><br></li>
        </ol>
    </li>
    <li>&nbsp;<u>Prepare your experiment:</u> Open your experiment in PsychoPy and open the Experiment Settings<br><br>
        <ol>
            <li>Create two &quot;Experiment info variables&quot; called &quot;SwitchID&quot; and &quot;group&quot;.</li>
            <li>Under &quot;Online/Completed URL&quot; enter redirectURL</li>
            <li>Use the group variable (expInfo[&apos;group&apos;]) to design your experiment to be different for each group.</li>
            <li>Sync your experiment with pavlovia<br><br></li>
        </ol>
    </li>
    <li><u>Start collecting data</u> by distributing the entry link to your subjects.<br><br></li>
    <li><u>View the progress</u> of your data collection by entering your study ID in the &quot;View switch&quot; tab.</li>
</ol>
<p>&nbsp;</p>' %>% gsub("redirectURL", paste0("$\'https://", session$clientData$url_hostname, session$clientData$url_pathname, "?completed=\' + expInfo[\'SwitchID\']"),.))
  
  # render inputs for n per group
  output$ninputs <- renderUI({
    if(input$n_groups %>% is.numeric() & input$n_groups >= 1){
      lapply(seq(input$n_groups), function(i) {
        numericInput(inputId = paste0("ngroup",i),
                     label = paste("n in group", i),
                     min = 1, value = 10)
      })}else{"Need more groups!"}
  })
  
  # check whether all n's are larger than 1
  output$all_ngroups_lt1 <- reactive({
    all(reactiveValuesToList(input) %>% as.data.frame() %>% select(contains("ngroup")) %>% unlist() >=1)
  })
  outputOptions(output, "all_ngroups_lt1", suspendWhenHidden = FALSE)
  
  # when creating ----
  observeEvent(input$create,{
    req(input$username, input$studyname)
    
    # which is the next ID for a study?
    new_study_id <- max(c(list.files("studies") %>% as.numeric(),0), na.rm = T) + 1
    
    # create directory
    dir.create(paste0("studies/",new_study_id %>% as.character()))
    
    # write study details
    new_study <- reactiveValuesToList(input) %>% 
      as.data.frame() %>% 
      select(contains("ngroup")) %>%
      pivot_longer(cols = contains("ngroup"), names_to = "group", values_to = "wanted_n", names_prefix = "ngroup") %>%
      arrange(group) %>%
      mutate(study_id = new_study_id, 
             created = Sys.time() %>% as.character(),
             user = input$username %>% tolower(), 
             experiment = input$studyname %>% tolower(),
             folder = input$foldername %>% tolower(),
             timeout_threshhold = input$timeout_threshhold,
             started_n = 0,
             active_n = 0,
             timed_out_n = 0,
             completed_n = 0,
             completion_rate = 0,
             entry_link = paste0(session$clientData$url_hostname, session$clientData$url_pathname,"?start=",new_study_id),
             send_link = paste0("https://run.pavlovia.org/",input$username,"/",input$studyname,"/",input$foldername,"/?group=", group),
             completion_link = ifelse(input$endlink != "", input$endlink, "none")
      ) %>% arrange(group)
    
    write.csv(new_study, paste0("studies/",new_study_id,"/study_details.csv"), row.names = F)
    
    # prepare participants
    new_participants <- data.frame(SwitchID = as.character(), group = as.character(), started = as.character(), status = as.character(), time_taken = as.character())
    
    write.csv(new_participants, paste0("studies/",new_study_id,"/participants.csv"), row.names = F)
    
    # sucess message
    shinyalert("Success!",
               paste0("You created a new switch with the ID <b>", new_study_id,"</b>.<br><br>
                      Please note this ID down. You can use it to view the details of your data collection later.<br><br>
                      Provide your subjects with the following link to access your study: <br><b>",session$clientData$url_hostname, session$clientData$url_pathname,"?start=",new_study_id,"</b>"),
               type = "success",
               html = T, size = "m")
    
    # go to other tab
    updateTextInput(session, "study_id_view", value = new_study_id)
    updateTabsetPanel(session, "tabs", selected = "View existing switch")
    
  })
  
  # when viewing ----
  # produce list of study details and participants
  viewed_study <- reactive({
    if(input$study_id_view %in% list.files("studies")){
      # read
      study_details <- read.csv(paste0("studies/",input$study_id_view,"/study_details.csv")) 
      participants <- read.csv(paste0("studies/",input$study_id_view,"/participants.csv"))
      # update
      # set time outs
      participants$status[participants$status == "active" & strptime(participants$started, format = "%Y-%m-%d %H:%M:%S") < Sys.time() - as.numeric(study_details$timeout_threshhold[1])*60] <- "timed out"
      # update study details
      study_details$started_n <- table(participants$group %>% factor(levels = study_details$group))
      study_details[,c("active_n","timed_out_n","completed_n")] <- table(participants$group %>% factor(levels = study_details$group),
                                                                         participants$status %>% factor(levels = c("active","timed out","completed")))
      study_details$completion_rate <- study_details$completed_n/study_details$started_n
      
      # return
      list("study_details" = study_details, "participants" = participants)
    }
  })
  
  # render table of study details
  output$study_details <- renderTable({
    if(input$study_id_view %in% list.files("studies")){
      table <- viewed_study()$study_details %>%  select(c("group",	"wanted_n", "started_n","active_n", "timed_out_n", "completed_n", "completion_rate"))
      names(table) <- c("group", "n wanted", "n started","n currently active", "n timed out or aborted", "n completed", "completion rate")
      table
    }
  })
  
  #render info text
  output$study_info <- renderText({
    if(input$study_id_view %in% list.files("studies")){
      
      paste0("Related pavlovia project: <b>", viewed_study()$study_details$user[1],"/",viewed_study()$study_details$experiment[1], "</b><br>",
             "Link to access the study: <b>", viewed_study()$study_details$entry_link[1], "</b><br>",
             "Completion link: <b>", viewed_study()$study_details$completion_link[1], "</b>")
    }else{req(input$study_id_view)
      "This ID does not exist."}
  })
  
  # render participant table
  output$participants <- renderDataTable({
    if(input$study_id_view %in% list.files("studies")){
      table <- viewed_study()$participants
      names(table) <- c("SwitchID", "group","started", "status", "time taken")
      table %>% arrange(desc(started))
    }
  })
  
  # download
  output$download_button <- renderUI(
    if(input$study_id_view %in% list.files("studies")){
      downloadButton("download_participants", "Download participant data", class = "btn btn-info")
    }else{req(input$study_id_view)
      "This ID does not exist."}
  )
  
  
  output$download_participants <- downloadHandler(
    filename = function(){
      paste("PavloviaSwitch ", 
            viewed_study()$study_details$user[1],"-",
            viewed_study()$study_details$experiment[1], 
            " (ID = ", viewed_study()$study_details$study_id[1],") ", Sys.time() %>% gsub(":","-",.) , ".csv", sep="")
    },
    content = function(file) {
      table <- viewed_study()$participants
      names(table) <- c("SwitchID", "started", "status", "time taken")
      write.csv(table, file, row.names = F)
    }
  )
  
  # upon closing
  session$onSessionEnded(stopApp)
}