library(stringr)

# Constants
oz_per_gal <- 128

# Settings
Batch_Size <- reactive({as.numeric(input$batchSize)}) # Gal
Starter_Ratio <- reactive({as.numeric(input$Starter_Ratio)})# 1/5
bottleSize <- reactive({as.numeric(input$bottleSize)}) # 16 ounces
juice_perBottle <- reactive({as.numeric(input$juice_perBottle)})# 3 ounces
oz_booch_perBottle <- reactive({bottleSize()-juice_perBottle()})# 13 ounces
NewBatch_Vol <- reactive({(1-Starter_Ratio())*Batch_Size()}) # Gal

values <- reactiveValues()

output$SettingsText <- renderUI({
  HTML(paste0("<b>Brew Volume: ",NewBatch_Vol()," gallons</b>"))
})
# Ingredients -------------------------------------------------------------


observe(priority = 2,{

# Tea ---------------------------------------------------------------------

tea_per_gal <- as.numeric(input$tea_per_gal)# 10 #bags
BlackTea_Ratio <- as.numeric(input$BlackTea_Ratio/100); GreenTea_Ratio <- 1-BlackTea_Ratio
# Bags
BlackTea_Bags <- round(tea_per_gal*BlackTea_Ratio*NewBatch_Vol(),0)
GreenTea_Bags <- round(tea_per_gal*GreenTea_Ratio*NewBatch_Vol(),0)
# Dollars/Bag
# Black Tea
blackTea_price <- as.numeric(gsub("\\$", "", input$blackTea_price))
blackTea_amount <- as.numeric(input$blackTea_amount)
BlackTea_Rate <- blackTea_price/blackTea_amount;
output$TeaText1 <- renderUI({
  HTML(paste0(blackTea_amount," black tea bags for ",input$blackTea_price," comes out to ~ <b>",
              dollar(BlackTea_Rate),"</b> per bag."))
})
# Green Tea
greenTea_price <- as.numeric(gsub("\\$", "", input$greenTea_price))
greenTea_amount <- as.numeric(input$greenTea_amount)
GreenTea_Rate <- greenTea_price/greenTea_amount;
output$TeaText2 <- renderUI({
  HTML(paste0(greenTea_amount," green tea bags for ",input$greenTea_price," comes out to ~ <b>",
              dollar(GreenTea_Rate),"</b> per bag."))
})
# Total Cost/Batch
BlackTea_Cost <- BlackTea_Rate*BlackTea_Bags
GreenTea_Cost <- GreenTea_Rate*GreenTea_Bags
values$Tea_Cost <- BlackTea_Cost + GreenTea_Cost
# Text to Print
output$TeaText3 <- renderUI({
  header <- HTML("Tea Summary:")
  Tea_Text <- HTML(
    paste0("<ul><li><b>",BlackTea_Bags," black tea bags</b> (@ ",dollar(BlackTea_Rate)," per bag), for a total cost of ",dollar(BlackTea_Cost)," <u>per batch</u>.</li>"),
    paste0("<li><b>",GreenTea_Bags," green tea bags</b> (@ ",dollar(GreenTea_Rate)," per bag), for a total cost of ",dollar(GreenTea_Cost)," <u>per batch</u>.</li>"),
    paste0("<li><b>",BlackTea_Bags+GreenTea_Bags, " total tea bags</b>, for a total cost of <b>",dollar(values$Tea_Cost),"</b> <u>per batch</u></li></ul>"))
  tagList(
    h5(header,style="font-size: 20px;font-weight: bold;"),
    Tea_Text)
})

# Sugar -------------------------------------------------------------------

sugar_per_gal <- mixedToFloat(as.character(input$sugar_perGallon)) #cups
sugar_vol <- sugar_per_gal*NewBatch_Vol()

sugar_price <- as.numeric(gsub("\\$", "", input$sugar_price))
sugar_amount <- as.numeric(input$sugar_amount)
sugar_Rate <- sugar_price/sugar_amount;

#sugar_Rate <- 5/14 # Dollars/cup
values$sugar_Cost <- round(sugar_Rate*sugar_vol,2)
# Text to Print
output$sugarText <- renderUI({
  Sugar_Text <- HTML(
    paste0("<b>",sugar_vol, " cups</b> of sugar per batch, for a total cost of <b>",dollar(values$sugar_Cost),"</b> <u>per batch</u>.")
  )
  Sugar_Text
})

# Cooling Agent -----------------------------------------------------------

resource_Chosen <- ifelse(input$coolingAgent=="Water",1,2) # Ice
Ratio_Tea_Brewed <- 1/2; IceBags_per_gal <- 1

cooling_price <- as.numeric(gsub("\\$", "", input$cooling_price))
water_amount <- as.numeric(input$water_amount); ice_amount <- as.numeric(input$ice_amount)
Resource_rates <- c(cooling_price/water_amount, cooling_price/ice_amount); # Dollars/gal or dollars/bag
Resource_costs <- c(Resource_rates[1]*water_amount,Resource_rates[2]*ice_amount)

# Total Cost
values$Resource_Cost <- round(Resource_costs[resource_Chosen],2) # Dollars per Batch

output$coolingText <- renderUI({
  if(input$coolingAgent=="Water"){
    Cooling_Text <- HTML(paste0(water_amount," gallon(s) of distilled/spring water, at ",
                                dollar(Resource_rates[1]), " per gallon, for a total of ",
                                dollar(Resource_costs[1])," per batch."))
  }else if(input$coolingAgent=="Ice"){
    Cooling_Text <- HTML(paste0(ice_amount," bag(s) of ice, at ",
                                dollar(Resource_rates[2]), " per bag, for a total of ",
                                dollar(Resource_costs[2])," <u>per batch</u>."))
  }
  Cooling_Text
})
}) # End Observe

# Flavoring ---------------------------------------------------------------

output$RegChosen <- renderUI({
  Ingredients <- vals_trich$Data$Ingredients
  pickerInput("F2_RegChosen","Select F2 Ingredients for Regular Kombucha",multiple = T,
              selected = c("Ginger","Pomegranate Juice","Blue Butterfly Pea Flower"),choices = Ingredients)
})
output$HardChosen <- renderUI({
  Ingredients <- vals_trich$Data$Ingredients
  pickerInput("F2_HardChosen","Select F2 Ingredients for Hard Kombucha",multiple = T,
              selected = c("Mango Juice","Citra Hops","EC-1118 Champagne Yeast"),choices = Ingredients)
})

observe({
  shiny::req(bottleSize()); shiny::req(juice_perBottle()); shiny::req(input$boochTypes)
  
  if("Regular" %in% input$boochTypes && "Hard" %in% input$boochTypes){
    Percent_Regular <- as.numeric(input$boochRatio)/100
  }else if(input$boochTypes=="Regular"){
    Percent_Regular <- 1
    updateSliderTextInput(session = session,inputId = "boochRatio",selected = 100)
  }else if(input$boochTypes=="Hard"){
    Percent_Regular <- 0
    updateSliderTextInput(session = session,inputId = "boochRatio",selected = 0)
  }
  
  RegBooch_Vol <- Percent_Regular*NewBatch_Vol() #gal
  Juice_Ratio <- juice_perBottle()/(juice_perBottle()+oz_booch_perBottle()) 
  OzJuice_perGal <- Juice_Ratio*oz_per_gal
  
  Ingredients <- c("Ginger","Pomegranate Juice", "Mango Juice","Apricot Puree", "Citra Hops","Blue Butterfly Pea Flower","EC-1118 Champagne Yeast")
  Ingredient_units <- c("lb","32oz jar","32oz jar","28oz jar", "1oz bag","oz","5g bag")
  Ingredient_costs <- c(2.98,4.49,3.59,4.20,5.48,2.37,1.4) # Dollars per unit
  Ingredient_amt_perGal <- c(0.125,
                             round(OzJuice_perGal/32,2),
                             round(OzJuice_perGal/32,2),
                             round(OzJuice_perGal/28,2),
                             1/5,
                             (3.2/6),
                             1/5) # Amount in unit
  # Ingred_Dat <- data.frame(Ingredients,Ingredient_amt_perGal,Ingredient_units,Ingredient_costs) #%>%
  # Ingred_Dat$Ingredient_amt_perGal <- round(Ingred_Dat$Ingredient_amt_perGal,3)
  # saveRDS(Ingred_Dat, "./note.rds")
  
  Ingred_Dat <- vals_trich$Data
  Ingredients <- Ingred_Dat$Ingredients; Ingredient_units <- Ingred_Dat$Ingredient_units;
  Ingredient_amt_perGal <- Ingred_Dat$Ingredient_amt_perGal; Ingredient_costs <- Ingred_Dat$Ingredient_costs;
  
  ### Regular Booch ###
  Reg_chosen <- as.character(input$F2_RegChosen) #c("Ginger","Pomegranate Juice","Blue Butterfly Pea Flower")
  Reg_chosen_Loc <- grep(paste(Reg_chosen,collapse = "|"),Ingredients)
  Reg_unit <- Ingredient_units[Reg_chosen_Loc]
  Reg_amt_perGal <- round(Ingredient_amt_perGal[Reg_chosen_Loc],3)
  Reg_rate <- Ingredient_costs[Reg_chosen_Loc]
  Reg_cost <- round(Reg_rate*Reg_amt_perGal*NewBatch_Vol()*Percent_Regular,2)
  values$Reg_TotCost <- sum(Reg_cost)
  
  output$RegBooch_SummaryText <- renderUI({
    shiny::req(input$F2_RegChosen)
    RegBooch_Summary <- data.frame("<li><b>",Reg_amt_perGal*RegBooch_Vol,paste0("</b>",Reg_unit,"(s)"),"of",paste0("<b>",Reg_chosen,"</b>,"),"for a total of<b>",paste0(dollar(Reg_cost),"</b>, per ",RegBooch_Vol," gallon batch of <u>regular</u> booch.</li>"))
    RegBooch_Text <- paste0("<ul>",do.call("paste",c(RegBooch_Summary)),"</ul>") 
    tagList(
      h5("F2 Ingredients (Regular)",style="font-size: 20px;font-weight: bold;"),
      HTML(RegBooch_Text)
    )
  })
  output$RegBooch_DetailedText <- renderUI({
    shiny::req(input$F2_RegChosen)
    RegBooch_Details <- data.frame("<li><b>",Reg_amt_perGal,paste0("</b>",Reg_unit,"(s)"),"of",paste0("<b>",Reg_chosen,"</b>,"),"per gallon, at",dollar(Reg_rate*Reg_amt_perGal),"/ gallon booch",paste0("(",dollar(Reg_rate)," per ",Reg_unit,"),"),"for a total of",paste0(dollar(Reg_cost)," per batch.</li>"))
    RegBooch_Text <- paste0("<ul>",do.call("paste",c(RegBooch_Details)),"</ul>")
    tagList(
      div(class = "inlay", style = "height:45px;width:100%;"),
    dropdown(label="More Details",
      HTML(RegBooch_Text),
      status = "primary",right=T,icon = icon("gear"), 
      width = "600px",style="unite"))
  })
 
  
  ### Hard Booch ###
  Percent_Hard <- 1 - Percent_Regular
  HardBooch_Vol <- Percent_Hard*NewBatch_Vol()

  Hard_chosen <- as.character(input$F2_HardChosen) #c("Mango Juice","Citra Hops","EC-1118 Champagne Yeast")
  Hard_chosen_Loc <- grep(paste(Hard_chosen,collapse = "|"),Ingredients)
  Hard_unit <- Ingredient_units[Hard_chosen_Loc]
  Hard_amt_perGal <- round(Ingredient_amt_perGal[Hard_chosen_Loc],3)
  Hard_rate <- Ingredient_costs[Hard_chosen_Loc]
  Hard_cost <- round(Hard_rate*Hard_amt_perGal*NewBatch_Vol()*Percent_Hard,2)
  values$Hard_TotCost <- sum(Hard_cost)
  
  output$HardBooch_SummaryText <- renderUI({
    shiny::req(input$F2_HardChosen)
    HardBooch_Summary <- data.frame("<li><b>",Hard_amt_perGal*HardBooch_Vol,paste0("</b>",Hard_unit,"(s)"),"of",paste0("<b>",Hard_chosen,"</b>,"),"for a total of<b>",paste0(dollar(Hard_cost),"</b>, per ",HardBooch_Vol," gallon batch of <u>hard</u> booch.</li>"))
    HardBooch_Text <- paste0("<ul>",do.call("paste",c(HardBooch_Summary)),"</ul>");
    tagList(
      h5("F2 Ingredients (Hard)",style="font-size: 20px;font-weight: bold;"),
      HTML(HardBooch_Text)
    )
  })
  output$HardBooch_DetailedText <- renderUI({
    shiny::req(input$F2_HardChosen)
    HardBooch_Details <- data.frame("<li><b>",Hard_amt_perGal,paste0("</b>",Hard_unit,"(s)"),"of",paste0("<b>",Hard_chosen,"</b>,"),"per gallon, at",dollar(Hard_rate*Hard_amt_perGal),"/ gallon booch",paste0("(",dollar(Hard_rate)," per ",Hard_unit,"),"),"for a total of",paste0(dollar(Hard_cost)," per batch.</li>"))
    HardBooch_Text <- paste0("<ul>",do.call("paste",c(HardBooch_Details)),"</ul>")
    tagList(
      div(class = "inlay", style = "height:45px;width:100%;"),
      dropdown(label="More Details",
               HTML(HardBooch_Text),
               status = "primary",right=T,icon = icon("gear"), 
               width = "600px",style="unite"))
  })
  
  output$brewVolRatio_Text <- renderUI({
    HTML(paste0("Brew Ratio: <b>",RegBooch_Vol," gallons</b> <u>Regular</u> : <b>",HardBooch_Vol," gallons</b> <u>Hard</u>"))
  })
  
  
  values$IngredientCost <- values$Tea_Cost + values$sugar_Cost + values$Resource_Cost + values$Reg_TotCost + values$Hard_TotCost# dollar(IngredientCost)
  
}) # End observe

# Bottling ----------------------------------------------------------------

observe({
  ### Links ###
  # Bottles: https://www.fillmorecontainer.com/a16-10c-case12ct-12-oz.html
  # Caps: https://www.fillmorecontainer.com/38-ct-plastic-white-matte-f217.html
  
  bottle_cost <- as.numeric(gsub("\\$", "", input$bottle_price)) # per 12 pack
  cap_cost <- as.numeric(gsub("\\$", "", input$cap_price)) # per cap
  values$Bottles_perBatch <- round(oz_per_gal*NewBatch_Vol()/oz_booch_perBottle(),2)
  neededBottles <- round_any(values$Bottles_perBatch, 12, f = ceiling); cases <- neededBottles/12
  neededCaps <- round_any(values$Bottles_perBatch,1, f = floor)
  values$BottlingCost <- bottle_cost*cases + cap_cost*neededCaps
  
  output$bottlesText <- renderUI({
    header <- HTML("Bottle Summary:")
    if(bottleSize() != 128){
      values$bottleUnit <- paste(bottleSize(),"oz")
    }else{
      values$bottleUnit <- paste("1 gal")
    }
    bottleSize <- c("8 oz","12 oz","16 oz","24 oz","32 oz","64 oz","1 gal")
    bottlesText <- HTML("<ul><li>",paste0("The <b>",NewBatch_Vol(),"</b> gallon batch will yield ~ <b>",values$Bottles_perBatch,"</b> <u>",values$bottleUnit," bottles</u>, which requires an order of <b>",cases," cases</b> (@ ",dollar(bottle_cost)," per case),"),
                        paste0("and <b>",neededCaps," caps</b> (@ ",dollar(cap_cost)," per cap), for a total of <b>",dollar(values$BottlingCost),"</b>.</li></ul>"))
    tagList(
      h5(header,style="font-size: 20px;font-weight: bold;"),
      bottlesText
    )
  })
  output$BoochBottleText <- renderUI({
    HTML(paste0("Ounces of Kombucha per Bottle: <b>",oz_booch_perBottle()," ounces</b>"))
  })
}) # End observe

# Cost Analysis -----------------------------------------------------------

observe({
  TotalCost <- values$IngredientCost + values$BottlingCost
  Bottles_perBatch <- values$Bottles_perBatch
  CostPerBottle <- round(TotalCost/floor(Bottles_perBatch),2)
  output$PricingText1 <- renderUI({
    shiny::req(values$IngredientCost); shiny::req(values$BottlingCost)
    tagList(
      HTML(paste0("Total Cost for ",NewBatch_Vol()," gallon Brew Volume:  <b>",dollar(TotalCost),"</b>.")),
      HTML(paste0("<ul><li>Total Ingredient Cost:  <b>",dollar(values$IngredientCost),"</b></li>")),
      HTML(paste0("<li>Total Bottling Cost:  <b>",dollar(values$BottlingCost),"</b></li></ul>"))
    )
  })
  output$PricingText2 <- renderUI({
    HTML(paste0("Average Cost per <u>",values$bottleUnit," Bottle</u>:  <b>",dollar(CostPerBottle),"</b>."))
  })
  sellPrice <- as.numeric(gsub("\\$", "", input$sellPrice))
  TotalRevenue <- sellPrice*floor(Bottles_perBatch)
  
  TotalProfit <- TotalRevenue - TotalCost
  ProfitPerBottle <- round(TotalProfit/floor(Bottles_perBatch),2)
  
  BreakEven <- ceiling(TotalCost/sellPrice)
  output$PricingText3 <- renderUI({
    shiny::req(input$sellPrice)
    tagList(
      HTML(paste0("Total Revenue:  <b>",dollar(TotalRevenue),"</b>  -- "),
           paste0("Total Profit:  <b>",dollar(TotalProfit),"</b>  -- "),
           paste0("Average Profit per Bottle:  <b>",dollar(ProfitPerBottle),"</b><br>"),
           paste0("<b>You will break even after selling ~ ",BreakEven," bottles</b>"))
    )
  })
})


