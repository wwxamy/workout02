library(shiny)
library(ggplot2)
library(reshape2)

future_value <- function(amount, rate, years){
  amount*(1+rate)^years
}
annuity <- function(contrib, rate, years){
  contrib*(((1+rate)^years-1)/rate)
}
growing_annuity <- function(contrib, rate, growth, years){
  contrib*(((1+rate)^years-(1+growth)^years)/(rate-growth))
}

ui <- fluidPage(
  
  # Application title
  titlePanel("Saving/investment simulations"),
  
  # Sidebar with a slider input for number of bins 
  fluidRow(
  column(4,sliderInput("initial",
                  "Initial amount",
                  min = 1,
                  max = 100000,
                  value = 1000)),
  column(4,sliderInput("return_rate",
                       "Return rate (in %)",
                       min = 0,
                       max = 20,
                       value = 5)), 
  column(4, sliderInput("years",
                        "Years",
                        min = 0,
                        max = 50,
                        value = 10)), 
  column(4, sliderInput("annual",
                        "Annual Contribution",
                        min = 0,
                        max = 50000,
                        value = 2000)), 
  column(4, sliderInput("growth",
                        "Grouth rate (in %)",
                        min = 0,
                        max = 20,
                        value = 2)), 
  column(4, selectInput("facet", "Facet?", choices = c("No", "Yes"))),
  column(12,plotOutput(outputId = "freq")),
  column(12, tableOutput(outputId = "summary"))
  
    ))



# Define server logic required to draw a histogram
    
server <- function(input, output) {
  output$freq <- renderPlot({
    years <- input$years
    initial_amount <- input$initial
    contribution <-  input$annual
    return_rate <- input$return_rate
    growth_rate <-  input$growth 
    
    if(input$facet == "No"){
      modalities <-data.frame(year = 0:years, no_contrib =rep(1000, years+1), fixed_contrib = rep(1000, years+1), growing_contrib =rep(1000, years+1))
      for(y in 1:years) {
        fv <- future_value(initial_amount, return_rate, y)
      fva <- annuity(contribution, return_rate, y)
      fvga <- growing_annuity(contribution, return_rate, growth_rate, y)
      modalities$no_contrib[y+1] <- fv
      modalities$fixed_contrib[y+1] <- fv+fva
      modalities$growing_contrib[y+1] <- fv+fvga}
      
      melt_modalities <-melt(modalities, id.vars = "year")
      ggplot(data = melt_modalities,aes(x = year, y = value)) + geom_point(aes(color =variable)) + geom_line(aes(color = variable)) + ggtitle("Three modes of investing")
    }
    
    else{
      investments <-c('regular savings','high-yield savings','index fund')
      rates <-list(regular = 0.0001,high_yield = 0.0225,index_fund = 0.065)
      scenarios <-list(1:3)
      for(r in 1:length(rates)) {
        balances <-data.frame(
          year = 0:years,
          product = investments[r],
          no_contrib =rep(initial_amount, years+1),
          fixed_contrib =rep(initial_amount, years+1),
          growing_contrib =rep(initial_amount, years+1))
        for(y in 1:years) {
          fv <-future_value(initial_amount, rates[[r]], y)
        fva <-annuity(contribution, rates[[r]], y)
        fvga <-growing_annuity(contribution, rates[[r]], growth_rate, y)
        balances$no_contrib[y+1] <- fv
        balances$fixed_contrib[y+1] <- fv+fva
        balances$growing_contrib[y+1] <- fv+fvga}
        scenarios[[r]] <- balances}
      dat <-do.call("rbind", scenarios)
      melt_dat <-melt(dat,id.vars =c("year", "product"),value.name = "balance")
      names(melt_dat)[3] <- "modality"
      ggplot(data = melt_dat,aes(x = year, y = balance))+geom_point(aes(color = modality), size = 0.5)+geom_line(aes(color = modality))+facet_grid(.~product)+theme_bw()
    } })
    
    output$summary <- renderTable({
      
      years <- input$years
      initial_amount <- input$initial
      contribution <-  input$annual
      return_rate <- input$return_rate
      growth_rate <-  input$growth 
      
      modalities <-data.frame(year = 0:years,no_contrib =rep(1000, years+1),fixed_contrib =rep(1000, years+1),growing_contrib =rep(1000, years+1))
      for(y in 1:years) {
        fv <-future_value(initial_amount, return_rate, y)
        fva <-annuity(contribution, return_rate, y)
        fvga <-growing_annuity(contribution, return_rate, growth_rate, y)
        modalities$no_contrib[y+1] <- fv
        modalities$fixed_contrib[y+1] <- fv+fva
        modalities$growing_contrib[y+1] <- fv+fvga}
      
      modalities
      
  }) 
}



# Run the application 
shinyApp(ui = ui, server = server)

