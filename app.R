library(plyr)
library(dplyr)
library(tidyverse)
library(htmltools)
library(scales)
library(dipsaus)
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyEffects)
library(shinyWidgets)
library(dqshiny)

source("./global.R")
#source("./DT_Function/shiny_module.R")
#source("./DT_Function/DT_app.R")


# UI ----------------------------------------------------------------------

ui <- 
  dashboardPagePlus(md = FALSE,skin = "midnight",
                    # HEADER -------------------------------------------------
                    dashboardHeaderPlus(
                      fixed = TRUE,titleWidth = 300,
                      title = tagList(
                        span(class = "logo-lg", h2("Kombucha Brewing")), 
                        img(src="BK_logo2.png", height='55px', width='70px')),
                      tags$li(class = "dropdown",
                              tags$style(".main-header .logo {height: 65px;}"),
                              tags$style(".sidebar-toggle {height: 65px;}"),
                              tags$style(".navbar {min-height:65px !important}")
                      )),
                    dashboardSidebar(width = 300,
                                     div(class = "inlay", style = "height:15px;width:100%;"),
                      tags$style(".left-side, .main-sidebar {padding-top: 65px}"),
                      sidebarMenu(
                        menuItem("Brew Settings",tabName = "param",icon = icon("table")),
                        menuItem("Cost Analysis",tabName = "reportGen",icon = icon("book"))
                    )),
                    # DashboardBody -------------------------------------------------
                    dashboardBody(
                      setShadow(class = "box"),setZoom(class="box"),
                      tags$head(
                        tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                        ),
                      tabItems(
                        tabItem("param", 
                                div(class = "inlay", style = "height:70px;width:100%;"),
                                div(style = "font-size:16px; margin-left:3em", 
                                fluidRow(column(11,uiOutput("batchSettings"))),
                                fluidRow(column(11,uiOutput("boochSettings"))))
                                ), # End tabItem
                        tabItem("reportGen",
                                div(class = "inlay", style = "height:70px;width:100%;"),
                                div(style = "font-size:16px; margin-left:3em",
                                h4("You will need ~",style="color:white;font-size: 24px;font-weight: bold;"),br(),
                                fluidRow(column(11,uiOutput("summaryReport"))),br(),
                                fluidRow(column(11,uiOutput("pricing"))))
                                ) # End tabItem
                            ) # End tabItems
                      ) # End dashboardBody
  ) # End UI

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  source("./BoochCalcs.R",local = T)
  source("./DT_Function/DT_app.R",local=T)
  
  output$batchSettings <- renderUI({
    boxPlus(width=12, status = "info",title="Overall Booch Settings",
            closable = F,solidHeader = T,collapsible = T,
            div(style = "position: relative; padding: 0px",
                fluidRow(
                  column(4,align="center",
                         div(class = "inlay", style = "height:30px;width:100%;"),
                         numericInput("batchSize","Batch Size (Gallons)",15,min = 1,max=1000,step = 1,width="60%"),
                         tags$style(type="text/css", "#batchSize {font-size:18px;}")),
                  column(4,align="center",
                         sliderInput("Starter_Ratio", "Starter Ratio",value=1/5,min=1/10,max=1,step=0.05),
                         HTML("i.e. The volumetric ratio of previously fermented F1 kombucha, to newly brewed tea.")),
                  column(4,align="center",
                         div(class = "inlay", style = "height:60px;width:100%;"),
                         htmlOutput("SettingsText"),
                         tags$style(type="text/css", "#SettingsText {font-size:22px;color:#ffffff;}")
                  )))
            )
  })
  output$boochSettings <- renderUI({
    boxPlus(width = 12,
            status = "primary",title = "Booch Brewing Specifics",
            closable = F, collapsible = T,solidHeader = T,
            div(style = "position: relative; padding: 0px",
# F1 Ingredients ----------------------------------------------------------
                tabBox(width = "100%",selected="F1 Ingredients",
                       shiny::tabPanel(title = "F1 Ingredients",
                                       tags$style(type="text/css", 
                                       "#F1Ingredients label {color: black;} #ui-id-2 {padding: 0 10px;}
                                       .skin-midnight .main-footer, .skin-midnight .nav-tabs-custom #F1Ingredients {color: black;}"),
                                       dq_accordion("F1Ingredients",bg_color="#133467",style = "color:white;border:1px solid white;",
                                                    icons = c(open="box-open",closed="box"),
                                                    titles = c("Tea Selection","Sugar","Cooling Agent"), 
                                                    contents= list(
                                       tagList(
                                         panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 320px; position:relative; align: center;padding: 10px 10px;",
                                       fluidRow(
                                         column(4,
                                                h4("Tea Selection",style="color:#7F5217;font-weight:520;"),
                                                sliderInput("tea_per_gal", "Tea bags per gallon",value=10,min=0,max=15),
                                                sliderInput("BlackTea_Ratio", "Percent Black Tea",value=70,min=0,max=100,post = "%"),
                                                HTML("<b>Note:</b> The rest is assumed to be Green Tea")),
                                         column(8,
                                                h4("Tea Costs",style="color:green;font-weight:520;"),
                                                column(6,
                                                       textInput("blackTea_price","Total cost of Black Tea",value = "21.00"),
                                                       numericInput("blackTea_amount","Number of Black Tea Bags in Purchase Order",value = 600,step = 1,min = 1),
                                                       htmlOutput("TeaText1"),
                                                       tags$style(type="text/css", "#blackTea_price {color : green;}")),
                                                column(6,
                                                       textInput("greenTea_price","Total cost of Green Tea",value = "3.00"),
                                                       numericInput("greenTea_amount","Number of Green Tea Bags in Purchase Order",value = 40,step = 1,min = 1),
                                                       htmlOutput("TeaText2"),
                                                       tags$style(type="text/css", "#greenTea_price {color : green;}"))
                                                )))),
                                       tagList(
                                         panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 260px; position:relative; align: center;padding: 10px 0px;",
                                       fluidRow(
                                         column(5,
                                                h4("Sugar",style="color:#493D26;font-weight:520;"),
                                                sliderTextInput("sugar_perGallon","Cups of Sugar Per Gallon",choices=c("1/2","3/4","1","5/4"),selected="1",grid = TRUE)),
                                         column(6,offset=1,
                                                h4("Sugar Cost",style="color:green;font-weight:520;"),
                                                textInput("sugar_price","Total cost of Sugar",value = "5.00"),
                                                numericInput("sugar_amount","Sugar Quantity in Purchase Order (cups)",value = 14,step = 1,min = 1),
                                                htmlOutput("sugarText"),
                                                tags$style(type="text/css", "#sugar_price {color : green;}"))
                                       ))),
                                       tagList(
                                         panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 260px; position:relative; align: center;padding: 10px 0px;",
                                       fluidRow(
                                         column(5,
                                                h4("Cooling Agent",style="color:#2B65EC;font-weight:520;"),
                                                radioGroupButtons(
                                                  inputId = "coolingAgent",
                                                  label = "Choose a Cooling Agent :", 
                                                  choices = c(`<i style="color:#2B65EC;" class='fas fa-tint fa-2x'></i><h5 style="display:inline; font-size:18px;color:#2B65EC;">  Water</h5>` = "Water", 
                                                              `<i style="color:#56A5EC;" class='fas fa-icicles fa-2x'></i><h5 style="display:inline;font-size:18px;color:#56A5EC;">  Ice</h5>` = "Ice"),
                                                  justified = TRUE)
                                                ),
                                         column(6,offset=1,
                                                h4("Cooling Agent Cost",style="color:green;font-weight:520;"),
                                                textInput("cooling_price","Total cost of Cooling Agent",value = "3"),
                                                conditionalPanel("input.coolingAgent=='Water'",
                                                                 numericInput("water_amount","Gallons of Distilled/Spring Water",value = 5,step = 1,min = 1)
                                                ),
                                                conditionalPanel("input.coolingAgent=='Ice'",
                                                                 numericInput("ice_amount","Number of Ice bags",value = 1,step = 1,min = 1)
                                                ),
                                                htmlOutput("coolingText"),
                                                tags$style(type="text/css", "#cooling_price {color : green;}")
                                         ))))
                                       ))
                       ),
# F2 Ingredients ----------------------------------------------------------
                       shiny::tabPanel(title = "F2 Ingredients",
                                         panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 550px; position:relative; align: center;padding:0px;",
                                         fluidPage(
                                           ### This is to adjust the width of pop up "showmodal()" for DT modify table 
                                           tags$head(tags$style(HTML('.modal-lg {width: 1200px;}'))),
                                           br(),
                                           tags$head(tags$style(".butt{background-color:#230682;} .butt{color: #e6ebef;}")),
                                           
                                           useShinyalert(), # Set up shinyalert
                                           uiOutput("MainBody_trich"),
                                           actionButtonStyled(inputId = "Updated_trich",label = "Save",type = "success"),
                                           downloadButton("Trich_csv", "Download in CSV", class="butt"),br(),br()
                                         )),
                                       fluidRow(
                                         column(6,
                                                checkboxGroupButtons(
                                                  inputId = "boochTypes",selected = "Regular",
                                                  label = "What Type(s) of Kombucha Are You Making?", checkIcon = list(yes = icon("ok", lib = "glyphicon")),
                                                  choices = c(`<i style="color:#CD7F32;" class='fas fa-glass-whiskey fa-2x'></i><h5 style="display:inline; font-size:18px;color:#CD7F32;">  Regular</h5>` = "Regular", 
                                                              `<i style="color:#728C00;" class='fas fa-beer fa-2x'></i><h5 style="display:inline;font-size:18px;color:#728C00;">  Hard (alcoholic)</h5>` = "Hard"),
                                                  justified = TRUE),
                                                conditionalPanel("input.boochTypes.indexOf('Regular') > -1 && input.boochTypes.indexOf('Hard') > -1",
                                                                 sliderTextInput(inputId = "boochRatio",label = "Percent (%) Regular Booch", post = "%",
                                                                   choices = seq(0, 100,by=5),selected = 50),
                                                                 fluidRow(column(12,align="center",htmlOutput("brewVolRatio_Text")))
                                                )),
                                         column(6,
                                                conditionalPanel("input.boochTypes.indexOf('Regular') > -1",
                                                                 uiOutput("RegChosen")),
                                                conditionalPanel("input.boochTypes.indexOf('Hard') > -1",
                                                                 uiOutput("HardChosen")))
                                       )
                       ),
# Bottling ----------------------------------------------------------------
                       shiny::tabPanel(title = "Bottling",
                                       fluidRow(
                                         column(4,
                                                pickerInput("bottleSize","Bottle Size",selected = 16,choices = c(8,12,16,24,32,64,128),
                                                            choicesOpt = list(content = c("8 oz","12 oz","16 oz","24 oz","32 oz","64 oz","1 gal"))),
                                                numericInput("juice_perBottle","Ounces of Juice per Bottle",value=3,step = .5),
                                                htmlOutput("BoochBottleText")
                                         ),
                                         column(8,
                                                h4("Bottling Costs",style="color:green;font-weight:520;"),
                                                column(6,
                                                       textInput("bottle_price","Cost per Case of Bottles",value = "6.55"),
                                                       textInput("cap_price","Cost per Cap",value = "0.09"),
                                                       tags$style(type="text/css", "#bottle_price {color : green;}"),
                                                       tags$style(type="text/css", "#cap_price {color : green;}")),
                                                column(6,align="center",
                                                       div(class = "inlay", style = "height:20px;width:100%;"),
                                                       actionButtonStyled(inputId='bottleLink1', label="Buy Bottles", icon = icon("wine-bottle"),
                                                                    style='padding:10px; font-size:150%',class="btn-lg",
                                                                    onclick ="window.open('https://www.fillmorecontainer.com/', '_blank')"))      
                                         ))
                       ))))
  })


# Report Generation -------------------------------------------------------

output$summaryReport <- renderUI({
  boxPlus(
    width = 12,
    status = "danger",title = "Cost Analysis",
    closable = F, collapsible = F,solidHeader = T,
    div(style = "position: relative; padding: 0px;",
                 tagList(
                   panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 300px; position:relative; align: center;padding: 0 10px;",
                         htmlOutput("TeaText3"),#HTML("test"),
                         tags$style("#TeaText3{font-size: 16px;color:black}")
                   ),
                   fluidRow(
                   column(10,
                   conditionalPanel("input.boochTypes.indexOf('Regular') > -1 && input.boochRatio != 0",
                   panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 300px; position:relative; align: center;padding: 0 10px;",
                         htmlOutput("RegBooch_SummaryText"),
                         tags$style("#RegBooch_SummaryText{font-size: 16px;color:black}")
                   ))),
                   column(2,
                   conditionalPanel("input.boochTypes.indexOf('Regular') > -1 && input.boochRatio != 0 &&
                                    input.F2_RegChosen !== 'undefined' && input.F2_RegChosen.length > 0",
                                    htmlOutput("RegBooch_DetailedText"),
                                    tags$style("#RegBooch_DetailedText{font-size: 16px;color:black}")
                   ))),
                   fluidRow(
                   column(10,
                   conditionalPanel("input.boochTypes.indexOf('Hard') > -1 && input.boochRatio != 100",
                   panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 300px; position:relative; align: center;padding: 0 10px;",
                         htmlOutput("HardBooch_SummaryText"),
                         tags$style("#HardBooch_SummaryText{font-size: 16px;color:black}")
                   ))),
                   column(2,
                   conditionalPanel("input.boochTypes.indexOf('Hard') > -1 && input.boochRatio != 100 &&
                                    input.F2_HardChosen !== 'undefined' && input.F2_HardChosen.length > 0",
                                    htmlOutput("HardBooch_DetailedText"),
                                    tags$style("#HardBooch_DetailedText{font-size: 16px;color:black}")
                   ))),
                   panel(style="overflow-y: scroll;overflow-x:hidden;max-height: 200px; position:relative; align: center;padding: 0 10px;",
                         htmlOutput("bottlesText"),
                         tags$style("#bottlesText{font-size: 16px;color:black}")
                   ))
  ))
})
  
  output$pricing <- renderUI({
    boxPlus(
      width = 12,
      status = "warning",title = "Pricing",
      closable = F, collapsible = F,solidHeader = T,
      div(style = "position: relative; padding: 0px;",br(),
          fluidRow(
            column(4,style="padding-right:0px;",
                   fluidRow(
                     column(9,style="padding-right:0px;",
                            htmlOutput("PricingText1"),
                            tags$style(type="text/css", "#PricingText1 {font-size:16px;}")),
                     column(3,align="right",style="padding-left:0px;",
                            div(class = "inlay", style = "height:20px;width:100%;"),
                            tags$div(HTML('<i class="fas fa-angle-double-right fa-3x" style = "color:#DA3248;"></i>')))
                   )),
            column(4,align="center",style="padding:0px;",
                   div(class = "inlay", style = "height:20px;width:100%;"),
                   htmlOutput("PricingText2"),
                   tags$style(type="text/css", "#PricingText2 {font-size:18px;padding:0px;}")
                   ),
            column(4,style="padding-left:0px;",
                   fluidRow(
                     column(3,align="left",style="padding-right:0px;",
                            div(class = "inlay", style = "height:20px;width:100%;"),
                            tags$div(HTML('<i class="fas fa-angle-double-right fa-3x" style = "color:#DA3248;padding:0px;"></i>'))),
                     column(9,
                            textInput("sellPrice","Sell Price per Bottle",value = "2.50"),
                            tags$style(type="text/css", "#sellPrice {font-size:18px; color:green;}")))
                   )),hr(),
          fluidRow(align="center",
            column(12,
                   htmlOutput("PricingText3"),
                   tags$style(type="text/css", "#PricingText3 {font-size:20px;padding:0px;}"))
          )
      ))
  })
# Adding dollar signs to costs --------------------------------------------
  observe(priority = 1,{
    shiny::req(input$blackTea_price); shiny::req(input$greenTea_price)
    shiny::req(input$sugar_price); shiny::req(input$cooling_price)
    if (!str_detect(input$blackTea_price, "\\$")) {
      updateTextInput(session, "blackTea_price","Total cost of Black Tea",  
                      value = number(as.numeric(input$blackTea_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
    if (!str_detect(input$greenTea_price, "\\$")) {
      updateTextInput(session, "greenTea_price","Total cost of Green Tea",  
                      value = number(as.numeric(input$greenTea_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
    if (!str_detect(input$sugar_price, "\\$")) {
      updateTextInput(session, "sugar_price","Total cost of Sugar",  
                      value = number(as.numeric(input$sugar_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
    if (!str_detect(input$cooling_price, "\\$")) {
      updateTextInput(session, "cooling_price","Total cost of Cooling Agent",  
                      value = number(as.numeric(input$cooling_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
  })
  observe(priority = 1,{
    shiny::req(input$bottle_price); shiny::req(input$cap_price)
    if (!str_detect(input$bottle_price, "\\$")) {
      updateTextInput(session, "bottle_price","Cost per Case of Bottles",  
                      value = number(as.numeric(input$bottle_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
    if (!str_detect(input$cap_price, "\\$")) {
      updateTextInput(session, "cap_price","Cost per Cap",  
                      value = number(as.numeric(input$cap_price), prefix = "$", big.mark = ",",accuracy=0.01))
    }
  })
  observe(priority = 1,{
    shiny::req(input$sellPrice)
    if (!str_detect(input$sellPrice, "\\$")) {
      updateTextInput(session, "sellPrice","Sell Price per Bottle",  
                      value = number(as.numeric(input$sellPrice), prefix = "$", big.mark = ",",accuracy=0.01))
    }
})
}

shinyApp(ui, server)