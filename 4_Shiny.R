
#message : 
## https://deanattali.com/blog/shinyalert-package/


#anyLib::anyLib(c("shiny","googleAuthR", "shinydashboard",""shinyalert", "shinyWidgets", "DT", "plotly", "ggplot2", "googleVis", "colourpicker"))
# 
library(googleAuthR)
library(shiny)
library(shinydashboard)
library(shinyalert)
library(DT)
library(tidyverse)
library(readxl)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(googlesheets4)




# tags --------------------------------------------------------------------


tag_bas <- tags$head(
  tags$style(HTML("
      #sidebarItemExpanded > ul > :last-child {
        position: absolute;
        bottom: 0;
        width: 100%;
      }
    ")))



# UI ----------------------------------------------------------------------


ui <- dashboardPage(    ## layout
  dashboardHeader(title = "PRETAG"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Presentation", tabName = "present", icon = icon("readme")), 
      menuItem("Data refresh", tabName = "Actu_data", icon = icon("rotate")), 
      menuItem("Data overview", tabName = "readData", icon = icon("table")),
      menuItem("Focus on products", icon = icon("flask-vial"),
          menuSubItem("Product overview", tabName = "readProd", icon = icon("table")),
          menuSubItem("Top 10", tabName = "Top_prod", icon = icon("chart-simple"))),
      
      br(),
      HTML("<center><a href=''><strong> Suzon Garnier </strong></a><br>
         </center>"),
      tag_bas
    )
  ),
  dashboardBody(

    tags$head(
      tags$style(HTML("
      /* CSS pour centrer l'image */
      .center-image {
        display: flex;
        justify-content: center;
        align-items: center;
        height: 100%;
      }

      /* Ajuster la taille de l'image pour s'adapter à la fenêtre */
      .center-image img {
        max-width: 100%;
        max-height: 100%;
      }
    "))
    ),
    
    
    tabItems(
      
      #present
      tabItem(tabName = "present",
              h1("Presentation"),
              br(),
              p("Welcome to the PRETAG project Rshiny"), 
              p("This application is linked to your google account to access to the product identification table. As well as your Kobo account to access the questionnaire data."),
              p("Here you can see the overview of data and products,
               download the data from the Kobo platform and refresh the the product identification table"),
              p("You can see somme result, like the 10 most used products in different ways"),
              br(),
              p("And you can also rename the products, when the same product was written in different ways."),
              p("link for the tutorial : https://docs.google.com/presentation/d/1eha39gBe4NPhcdNirlOUtSjRdAhRoIz2/edit?usp=sharing&ouid=101342974498218598981&rtpof=true&sd=true"),
              br(),
              p("Explore this tool, and if you have any questions, don't hesitate to send me a message :"),
              p("benjamin.heuclin@cirad.fr  orsuzon.garnier@laposte.net"),
              div(class = "center-image", imageOutput("image_display"))
      ),
      
      #data refresh
      tabItem(tabName = "Actu_data",
              h1("Data refresh"),
              p("As long as the data collection is not finished, I recommend updating the two datasets regularly"),
              br(),
              p("Click on the 'Refresh data' button to refresh the data from the Kobo and see the most recent information."),
              div(style = "text-align: center;",
                  actionButton("btn_run_code", "Refresh data", style = "font-size: 16px;")
              ),
              br(),
              verbatimTextOutput("output_message_telechargement"),
              br(),
              p("Click on the 'Update the Drive' button to refresh the data from the Google Sheet and see the most recent information."),
              div(style = "text-align: center;",
                  actionButton("btn_update_drive", "Update the Drive", style = "font-size: 16px;")
              ),
              verbatimTextOutput("output_message_drive")
      ),
      
     
      
      # Read general data 
      
      tabItem(tabName = "readData",
              h1("Data overview"),
              br(),
              p("Welcome to the data overview. Here you can explore various summaries and previews of the data."),
              br(),  # Sauter une ligne
              div(style = "display: flex; justify-content: center;",
                  actionButton("btn_show_preview_all", "Full Data", style = "margin: 5px;"),
                  actionButton("btn_show_preview_farmer", "Data per Farmer", style = "margin: 5px;"),
                  actionButton("btn_show_preview_cycle", "Data per Cycle", style = "margin: 5px;"),
                  actionButton("btn_show_preview_application", "Data per Application", style = "margin: 5px;")
              ),
              br(),
              div(style = "text-align: center;",  # Centrer le dataTableOutput
                  dataTableOutput(outputId = "preview")
              )
      ),
      
      
      ### Focus on product 
      
      # data overview 
      tabItem(tabName = "readProd",
              br(),
              h1("Focus on products"),
              p("Renome les nom des colones dans le output, et fait que les données sois dans l'ordre decroissant"),
              h2("Data overview"),
              p("Here you have access to the product drive, which lists all the products encountered in our surveys. 
      Our goal is to identify as many products as possible. If you wish to participate in the identification process, 
      you can open the Google Sheet and add the products that you identify."),
              div(style = "text-align: center;",
                  actionButton("btn_open_sheet", "Open Google Sheet", style = "font-size: 16px;")
              ),
              br(),
              p("We recommend using the 'Unknown Product' button to identify unidentified products. 
      Once you are finished, you can refresh the data by going to the 'Data Refresh' tab and clicking on the 'Update the Drive' button."),
              
          br(),
          h3("Rename product"),
          p("If you whant to rename some product, clic here :"),
          div(style = "text-align: center;",
              actionButton("btn_rename_prod", "Rename product", style = "font-size: 16px;")
          ),
          br(),
          p("To see the data renamed you need to go to go to the page data refresh and clic on the button update the drive"),
          
          
              h2("Explore products data"),
              p("You can use the buttons below to explore the product data:"),
              div(style = "display: flex; justify-content: center;",
                  actionButton("btn_show_prod", "Show All Products", style = "margin: 5px;"),
                  actionButton("btn_show_known_prod", "Show Known Products", style = "margin: 5px;"),
                  actionButton("btn_show_unknown_prod", "Show Unknown Products", style = "margin: 5px;"),
                  actionButton("btn_show_common_name", "Show Product Common Names", style = "margin: 5px;"),
                  actionButton("btn_show_sa", "Show Active substance", style = "margin: 5px;")
              ),
              br(),
              div(style = "text-align: center;",
                  tableOutput(outputId = "table_prod")
              )
              
      ),
      
      # TOP 10 
      
      tabItem(tabName = "Top_prod",
              br(),
              h1("Focus on products"),
              h2("Top 10"),
              p("Here you have access to the 10 most used products and active substance."),
              p("You can choose the way to calculate the 10 most used products or SA with the buttons below"),
              
              h3("Product"),
              div(style = "display: flex; justify-content: center;",
                  actionButton("btn_prod_farm", "Percentage of farmer using a product", style = "margin: 5px;"),
                  actionButton("btn_prod_app", "Number of applications with this product in our samples", style = "margin: 5px;"),
                  actionButton("btn_prod_amount", "Average amount applied per application per hectare", style = "margin: 5px;")
              ),
              br(),
              h3("Active substance"),
            
              p(""),
              div(style = "display: flex; justify-content: center;",
                  actionButton("btn_sa_app", "Number of applications with this active substance in our samples", style = "margin: 5px;"),
                  actionButton("btn_sa_quantity", "Average amount (active substance) applied per application per hectare", style = "margin: 5px;")
              ),
  
              br(),
              
              div(style = "text-align: center;",
                           tableOutput(outputId = "table_top")
                       ),
                
              div(style = "text-align: center;",
                          plotOutput(outputId = "graph_top")
                       )
                       )
              
              
      )
    )
  )






# First and automatic action ------------------------------------------------------


sheet_url<-"https://docs.google.com/spreadsheets/d/1hjXlk5_ckY_LBcDL0mu2k2NKmm6s_-OJGu0d_5qQQQo/edit#gid=0"  # ouvrir le Google sheet 


if(try(load("Data/data.Rdata"), silent = TRUE)=="data"){
  data0 <<- data
  source("1_bis_retraitement_name_prod.R", local = TRUE )
  } else {
  source("1_telechargement_mise_en_forme_data.R", local = TRUE)
  load("Data/data.Rdata")
  source("1_bis_retraitement_name_prod.R", local = TRUE )
  data0 <<- data
  
} #download data from kobo if there are not yet on the computer

# Server ------------------------------------------------------------------


server <- function(input, output,session) {
  
  
  
  observe({
    shinyalert("Warning", "To run this application, you must be connected to the CIRAD Project implementation drive. To connect, a page has just opened on your browser. Enter your Google account associated with this drive. Be careful, remember to check the box Consult, modify, create and delete all your Google Sheets spreadsheets. Thank you and good use", type = "info")}) #message warning

  observe({
    gs4_auth("1")

    unknown_prod<<-read_sheet(sheet_url,sheet="name_not_found")
    Common_name<<-read_sheet(sheet_url,sheet="Common_name")
    Name_found<<-read_sheet(sheet_url,sheet=1)


       }) #download data drive




  # Presentation 
 
 
 output$image_display <- renderImage({
   list(src = "Data/image_lina.jpg", alt = "Mon Image")
 }, deleteFile = FALSE)  


  
  #  Data refresh
  observeEvent(input$btn_run_code, {
    source("1_telechargement_mise_en_forme_data.R", local = TRUE)
    
    load("Data/data.Rdata")
    source("1_bis_retraitement_name_prod.R", local = TRUE )
    data0 <<- data
    
    output$output_message_telechargement <- renderPrint({   #  message
      "The data has been refreshed"
    })
  }) #refresh data
  
  
  observeEvent(input$btn_update_drive, {
    
    load("Data/data.Rdata")
    source("1_bis_retraitement_name_prod.R", local = TRUE )
    data0 <<- data
    
    ## Name found (P1)
    Name_found<<-read_sheet(sheet_url,sheet=1)

    ## unknown_prod (P2)
    unknown_prod <<- data0 %>%
      group_by(name_product2) %>%
      filter(name_product2 %in% sort(setdiff(unique(data0$name_product2), unique(Name_found$name_product2))))%>%
      summarise(kind_product = ifelse(any(kind_product == "Other"), NA, unique(na.omit(kind_product))))
    
    write_sheet(unknown_prod, ss = sheet_url, sheet = "name_not_found")
    
    
    ## Common_name (P3)
    Common_name <<- data0 %>%
      group_by(Common_name, name_product2) %>%
      summarise(kind_product = ifelse(any(kind_product == "Other"), NA, kind_product))%>%
      filter(!is.na(Common_name))
    
    write_sheet(Common_name, ss = sheet_url, sheet = "Common_name")
    
 
    ## output 
    
    output$output_message_drive <- renderPrint({"The drive has been updated"}) # message 
    
    
  }) #update drive
  
  # Data overview (general)
  
  observeEvent(input$btn_show_preview_all, { 
    output$preview <-renderDataTable(
      {data0},options = list(scrollX = TRUE ,scrollY = "500px", dom = "lftip"))
    }) # tab all
  
  observeEvent(input$btn_show_preview_farmer, {  
    Farm<-data0 %>%
      group_by(id_farmer) %>%
      summarise(across(everything(), ~ if(length(unique(.)) == 1) {unique(.)} else NA)) %>%
      select(-c(id_cycle,saison_start,saison_harvest, `__version__`,Starting_date,Harvest_date,Freq_irrig,n_per_cycle,cycl,))
    
    Farm <- Farm[, colMeans(is.na(Farm)) < 0.8]
    
    output$preview <-  renderDataTable({Farm},  options = list(scrollX = TRUE ,scrollY = "500px", dom = "lftip" )) 
    
  }) #tab farmer
  
  observeEvent(input$btn_show_preview_cycle, { 
    Cycle<-data0 %>%
      group_by(id_cycle) %>%
      summarise(across(everything(), ~ if(length(unique(.)) == 1) {unique(.)} else NA))  %>%
      select(-c(id_application,`__version__`,id_application,gps_lat,gps_lon, Survey_date, Phone_Number,Gender,Age,
                Nb_household,Nb_active_labor,Education,able_to_read,able_to_read2,household_able_to_read,
                household_able_to_read2,Experience,Beside_career,Other_career,Other_career2,farm_main_income,
                Beside_crops,rice_main_agri_income,Owner_of_the_plot,nb_plot,Total_size,cooperative,cooperative2,
                arrangements,Which_arrangement,target_plot,plot_size,Main_Water_Source,Main_Water_Source2,
                Other_Water_Source,Other_Water_Source2,far_from_water,water_sufficient,different_if_better_water,
                different_if_better_water2,Land_characteristic,bordering_by,crops_bordering,bordering_by2,Synchronous_culture,
                other_crop,which_before,crop_other1,which_after,crop_other2,feel_unwell,symptoms,opinion_health,kind_health_prob,
                Who_safety_training,Who_safety_training2,read_the_instruction,instruction_info,instruction_info_001,
                agri_equipment,agri_equipment2,keep_after,other_after,disposal,other_disposal,own_consumption,
                own_consumption_size,Comment,VA_C1,VA_C1_URL,VA_C2,VA_C2_URL,VA_C3,VA_C3_URL))
    
    Cycle <- Cycle[, colMeans(is.na(Cycle)) < 0.8]
    
    output$preview <-  renderDataTable({Cycle},  options = list(scrollX = TRUE ,scrollY = "500px", dom = "lftip" )) 
    
  })  #tab cycle
  
  observeEvent(input$btn_show_preview_application, {  
    App<-data0 %>%
      group_by(id_application) %>%
      summarise(across(everything(), ~ if(length(unique(.)) == 1) {unique(.)} else NA))%>%
      select(-c(id_produit,gps_lat,gps_lon,ID_code,Survey_date,`__version__`,Phone_Number,Gender,Age,Nb_household,Nb_active_labor,Education,able_to_read,able_to_read2,household_able_to_read,household_able_to_read2,Experience,Beside_career,Other_career,farm_main_income,Beside_crops,Owner_of_the_plot,nb_plot,Total_size,cooperative,arrangements,target_plot,plot_size,Main_Water_Source,Other_Water_Source,far_from_water,water_sufficient,different_if_better_water,different_if_better_water2,Land_characteristic,nb_cycle,bordering_by,Synchronous_culture,other_crop,feel_unwell,symptoms,Who_safety_training,read_the_instruction,instruction_info,agri_equipment,keep_after,disposal,own_consumption,own_consumption_size,Comment,Contract_farming,rice_future,country_sold,Cultivation_practices,If_broadcasting,Kg_seed,land_prep,seed_treatments,seed_treatments2,Starting_date,Harvest_date,Freq_irrig,n_apl_fert,fert_type,How_aply_fert,fertilizer_amount,insects,diseases,weeds,rodents,snails,Cow_buffalo,Bird,method,saison_start,saison_harvest,name_product2))
    App <- App [, colMeans(is.na(App)) < 0.8]
    
    output$preview <-  renderDataTable({App},  options = list(scrollX = TRUE ,scrollY = "500px", dom = "lftip" )) 
    
  }) #tab application
  
  ### Focus on product 
  
  # data overview (product)
  
  observeEvent(input$btn_open_sheet, {
    browseURL(sheet_url)
  }) #open drive
  
  
### POP UP rename 
  
    popupModal <- function() {
      modalDialog(
      br(),
      br(),
      tags$p(
        style = "font-weight: bold; color: red;",
        "WARNING : Put the name product between quotes, and if you whant more than one put comma in between :"), 
      br(),
      # Zone de saisie de texte
      textInput("CN_input", "Current name:", value = ""),
      textInput("NN_input", "new name:", value = ""),
      
      # Bouton pour soumettre le message
      actionButton("submit_button", "submit"),
      modalButton("Close"),
      
      # Zone d'affichage du message saisi
      verbatimTextOutput("message_output"),
      
      br(),
      br(),
      p("Exemple : "),
      
      #image
      
      output$image_display <- renderImage({
        list(src = "Data/image_rename.png", alt = "Mon Image2", style = "border: 2px solid black;")
      }, deleteFile = FALSE) 
    )
  } #creation pop up rename 
  
  observeEvent(input$btn_rename_prod, {
    showModal(popupModal())
  }) # show pop up rename 
  
  observeEvent(input$submit_button, {
    
    message_CN <- input$CN_input
    message_NN <- input$NN_input
    
    #test les condition 
    test<-paste("data$name_product2[data$name_product2 %in% c(", message_CN, ")] = " ,message_NN)
    
    TRY <- tryCatch(eval(parse(text = test)), error = function(e) 0)
  
          
                    if (TRY==0) {
                      
                      output$message_output <- renderPrint({
                        cat("ERROR : Have you put the name product between quotes,\n and if you whant more than one put comma in between ?\n like in the exemple") })
                      
                    } else {
    
    # Mettre à jour l'affichage du message
    output$message_output <- renderPrint({
      cat("Current name :", message_CN, "\n")
      cat("New name :", message_NN, "\n")
    })
    
    # Écrire le message dans un nouveau fichier R
    file_path <- "1_bis_retraitement_name_prod.R"
    file_content <- readLines(file_path)
    file_content <- c(file_content, paste("data$name_product2[data$name_product2 %in% c(", message_CN, ")] = " ,message_NN))
    
    
    writeLines(file_content, file_path)
                    }
  }) #even in the rename
  
 
  
  
  
###
  
  observeEvent(input$btn_show_prod, {
    prod <- data0 %>%
      group_by(name_product2) %>%
      summarise(kind_product = ifelse(any(kind_product == "Other"), NA, kind_product))%>%
    rename("Product Name" = name_product2)
    
    output$table_prod  <-  renderTable({prod}) 
  })  # tab prod
  
  observeEvent(input$btn_show_known_prod, {
    know_prod<<-Name_found[,c(1,3,5,6,8,9,13,15)]
    know_prod<-know_prod%>%
      rename("Product Name" = name_product2)
    
    output$table_prod  <-  renderTable({know_prod})
  })  # tab known_prod
  
  observeEvent(input$btn_show_unknown_prod, {
    unknown_prod<-unknown_prod%>%
      rename("Product Name" = name_product2)
    output$table_prod  <-  renderTable({unknown_prod})
    
  })  # tab unknown_prod
  
  observeEvent(input$btn_show_common_name, {
    Common_name<-Common_name%>%
      rename("Product Name" = name_product2)
    output$table_prod <- renderTable({Common_name})  
  }) # tab Common_name
  
  observeEvent(input$btn_show_sa, {
    
    
    know_prod<<-Name_found[,c(1,3,5,6,8,9,13,15)]
    
    know_prod <<- know_prod %>%
      mutate(`Concentration rate 1` = ifelse(!is.na(`Matching unit`),
                                             case_when(
                                               `Matching unit` == "g/Kg" ~ `Concentration rate 1`,
                                               `Matching unit` == "g/L" ~ `Concentration rate 1`,
                                               `Matching unit` == "%" ~ `Concentration rate 1`*10,
                                               TRUE ~ NA  ),
                                             NA))%>%
      mutate(`Concentration rate 2` = ifelse(!is.na(`Matching unit`),
                                             case_when(
                                               `Matching unit` == "g/Kg" ~ `Concentration rate 2`,
                                               `Matching unit` == "g/L" ~ `Concentration rate 2`,
                                               `Matching unit` == "%" ~ `Concentration rate 2`*10,
                                               TRUE ~ NA  ),
                                             NA))
    
    sa=data0  %>%
      group_by(name_product2) %>% 
      summarise(apply_quantity=mean(Quantity_per_hectare, na.rm=TRUE) ) 
    
    sa_a <- merge(sa, know_prod, by = "name_product2", all.x = TRUE)
    
    sa_a<-sa_a %>% 
      mutate (
        sub_act1=`Concentration rate 1`*apply_quantity,
        sub_act2=`Concentration rate 2`*apply_quantity)  %>%
      pivot_longer(cols = starts_with("Active Substance"),
                   names_to = "substance_active",
                   values_to = "substance_name") %>%
      mutate (Quantity= case_when(
        substance_active == "Active Substance 1" ~ sub_act1,
        substance_active == "Active Substance 2" ~ sub_act2)) %>%
      # select(c(substance_name,Quantity)) %>% 
      filter(!is.na(substance_name) & !is.na(Quantity)) 
    
    
    summarized_data <- sa_a %>%
      group_by(substance_name) %>%
      mutate(prod_column = paste0("Produit ", row_number())) %>%
      pivot_wider(names_from = prod_column, values_from = name_product2, values_fn = list(name_product2 = toString)) 
    
    summarized_data<-summarized_data[,-c(1:9)]
    
    result <- summarized_data %>%
      group_by(substance_name) %>%
      summarize(
        Moyenne_Quantity = mean(Quantity, na.rm = TRUE),
        Produit_1 = first(na.omit(`Produit 1`)),
        Produit_2 = first(na.omit(`Produit 2`)),
        Produit_3 = first(na.omit(`Produit 3`))
      )%>%
      arrange(-Moyenne_Quantity) 
    
    
    output$table_prod <- renderTable({result}) 
    
  })  #tab substance active 
  
  # Top 10 
  
  observeEvent(input$btn_prod_farm, {
    
    p = data0 %>% 
      group_by(name_product2) %>% 
      summarise(freq=n(),
                nb_farmer=length(unique(id_farmer))) %>% 
      mutate(precentage_farmer = nb_farmer / length(unique(data0$id_farmer))*100) %>%
      arrange(-nb_farmer)  
    sub<-p[2:11,]
    
    sub_p <- merge(sub, Name_found, by = "name_product2", all.x = TRUE)
  
    output$table_top  <-  renderTable({sub_p[,c(1,2,3,4,7,8,10,11,15,17)]}) 
    
    graph_p<-ggplot(sub_p, aes(x = reorder(name_product2, -precentage_farmer), y = precentage_farmer, fill = Target)) +
      geom_col() +
      geom_text(aes(label = ifelse(is.na(`Active Substance 2`), `Active Substance 1` , 
                                   paste(`Active Substance 1` , `Active Substance 2` , sep = " + "))),
                vjust = 0, size = 3.5, angle = 90, hjust = 1.1)+
      xlab("Product") +
      ylab("Percent farmer") +
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right") 
    
    output$graph_top <- renderPlot({graph_p})
  }) # % farmer (p)
  
  observeEvent(input$btn_prod_app,{
    
    f = data0 %>% 
      group_by(name_product2) %>% 
      summarise(freq=n(),
                nb_farmer=length(unique(id_farmer))) %>% 
      mutate(precentage_farmer = nb_farmer / length(unique(data0$id_farmer))*100) %>%
      arrange(-freq) 
    
    subf<-f[2:11,]
    sub_f <- merge(subf, Name_found, by = "name_product2", all.x = TRUE)
    
    output$table_top  <-  renderTable({sub_f[,c(1,2,3,4,7,8,10,11,15,17)]}) 
    
    graph_f<-ggplot(sub_f , aes(x=reorder(name_product2, -freq) , y=freq, fill = Target)) +
      geom_col()+
      geom_text(aes(label = ifelse(is.na(`Active Substance 2`), `Active Substance 1` , 
                                   paste(`Active Substance 1` , `Active Substance 2` , sep = " + "))),
                vjust = 0, size = 3.5, angle = 90, hjust = 1.1)+
      xlab("Product") +
      ylab("Number of applications per year")+
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right") 
    
    output$graph_top <- renderPlot({graph_f})
    
  })# nb application (freq) (f)
  
  observeEvent(input$btn_prod_amount, {
   
    
   a=data0  %>%
      group_by(name_product2) %>% 
      summarise(
        quantity_per_hectare=mean(Quantity_per_hectare, na.rm=TRUE),
        nb_farmer=length(unique(id_farmer))
      ) %>% 
      mutate(precentage_farmer = nb_farmer / length(unique(data0$id_farmer))*100) %>%
      arrange(-quantity_per_hectare) 
    
    suba<-a[2:11,]
    
    sub_a <- merge(suba, Name_found, by = "name_product2", all.x = TRUE)
    sub_a<-sub_a %>% arrange(desc(quantity_per_hectare)) %>%
      rename()
    
    
    output$table_top  <-  renderTable({sub_a[,c(1,2,3,4,7,8,10,11,15,17)]}) 
    
    graph_a<-ggplot(sub_a , aes(x=reorder(name_product2, -quantity_per_hectare) , y=quantity_per_hectare, fill = Target)) +
      geom_col()+
      geom_text(aes(label = ifelse(is.na(`Active Substance 2`), `Active Substance 1` , 
                                   paste(`Active Substance 1` , `Active Substance 2` , sep = " + "))),
                vjust = 0, size = 3.5, angle = 90, hjust = 1.1)+
      xlab("Product") +
      ylab("Average amount applied per application per hectare")+
      theme_minimal() + 
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "right") 
    
    output$graph_top <- renderPlot({graph_a})
    
  }) # Amount (a)
  
  observeEvent(input$btn_sa_quantity, {
    
    know_prod<<-Name_found[,c(1,3,5,6,8,9,13,15)]
    
    know_prod <<- know_prod %>%
      mutate(`Concentration rate 1` = ifelse(!is.na(`Matching unit`),
                                             case_when(
                                               `Matching unit` == "g/Kg" ~ `Concentration rate 1`,
                                               `Matching unit` == "g/L" ~ `Concentration rate 1`,
                                               `Matching unit` == "%" ~ `Concentration rate 1`*10,
                                               TRUE ~ NA  ),
                                             NA))%>%
      mutate(`Concentration rate 2` = ifelse(!is.na(`Matching unit`),
                                             case_when(
                                               `Matching unit` == "g/Kg" ~ `Concentration rate 2`,
                                               `Matching unit` == "g/L" ~ `Concentration rate 2`,
                                               `Matching unit` == "%" ~ `Concentration rate 2`*10,
                                               TRUE ~ NA  ),
                                             NA))
    
    sa=data0  %>%
      group_by(name_product2) %>% 
      summarise(apply_quantity=sum(Quantity_per_hectare, na.rm=TRUE) ) 
    
    sa_a <- merge(sa, know_prod, by = "name_product2", all.x = TRUE)
    
    sa_a<-sa_a %>% 
      mutate (
        sub_act1=`Concentration rate 1`*apply_quantity,
        sub_act2=`Concentration rate 2`*apply_quantity)  %>%
      pivot_longer(cols = starts_with("Active Substance"),
                   names_to = "substance_active",
                   values_to = "substance_name") %>%
      mutate (Quantity= case_when(
        substance_active == "Active Substance 1" ~ sub_act1,
        substance_active == "Active Substance 2" ~ sub_act2)) %>%
      select(c(substance_name,Quantity)) %>% 
      filter(!is.na(substance_name) & !is.na(Quantity)) 
    
    top_10_quantite <- sa_a %>%
      group_by(substance_name) %>%
      summarise(quantity_per_hectar = mean(Quantity)) %>%
      arrange(desc(quantity_per_hectar)) %>%
      head(10)
    
    output$table_top  <-  renderTable({top_10_quantite}) 
    
    graph_sa<-ggplot(top_10_quantite , aes(x=reorder(substance_name, -quantity_per_hectar) , y=quantity_per_hectar)) +
      geom_col()+
      xlab("Active substance") +
      ylab("Quantity mean")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    output$graph_top <- renderPlot({graph_sa})
    }) # sa quantity mean
  
  observeEvent(input$btn_sa_app, {
    know_prod<<-Name_found[,c(1,3,5,6,8,9,13,15)]
    
    sa_a <- merge(data0, know_prod, by = "name_product2", all.x = TRUE)
   
    sa_a<- sa_a[, c("Active Substance 1", "Active Substance 2", "name_product2")]
    
    
    sa_a<-sa_a %>%
      filter(!is.na(`Active Substance 1`))%>%
      pivot_longer(cols = starts_with("Active Substance"),
                   names_to = "substance_active",
                   values_to = "substance_name")  %>%
      select(c(name_product2,substance_name)) %>% 
      filter(!is.na(substance_name) ) 
    
    top_10_applications <- sa_a %>%
      group_by(substance_name) %>%
      summarise(number_applications = n()) %>%
      arrange(desc(number_applications)) %>%
      head(10)
    
    
    output$table_top  <-  renderTable({top_10_applications}) 
    
    graph_sa<-ggplot(top_10_applications , aes(x=reorder(substance_name, -number_applications) , y=number_applications)) +
      geom_col()+
      xlab("Active substance ") +
      ylab("Number of applications")+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    output$graph_top <- renderPlot({graph_sa})
  }) #sa nb application
  ###
  
} 

shinyApp(ui, server)

