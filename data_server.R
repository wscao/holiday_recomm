recomdata <- reactive({
  selected_place <- input$place_selection
  
  give_rec(selected_place)
 
})


observeEvent(input$run, {
  
  recomdata <- recomdata()
  
  if(length(input$place_selection) < 1){
    sendSweetAlert(
      session = session,
      title = "Please select 1 place.",
      text = "",
      type = "info")
  } else{
    output$recomm <- renderTable(recomdata) 
  }
  
})