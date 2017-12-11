nx1 <- eventReactive(input$next1,{
  updateNavbarPage(session,"navbar",selected="1")
  return("")
})
output$tnx1 <- renderText({
  nx1()
})

bk0 <- eventReactive(input$back0,{
  updateNavbarPage(session,"navbar",selected="0")
  return("")
})
output$tbk0 <- renderText({
  bk0()
})

nx2 <- eventReactive(input$next2,{
  updateNavbarPage(session,"navbar",selected="2")
  return("")
})
output$tnx2 <- renderText({
  nx2()
})

bk1 <- eventReactive(input$back1,{
  updateNavbarPage(session,"navbar",selected="1")
  return("")
})
output$tbk1 <- renderText({
  bk1()
})

nx3 <- eventReactive(input$next3,{
  updateNavbarPage(session,"navbar",selected="3")
  return("")
})
output$tnx3 <- renderText({
  nx3()
})

bk2 <- eventReactive(input$back2,{
  updateNavbarPage(session,"navbar",selected="2")
  return("")
})
output$tbk2 <- renderText({
  bk2()
})

nx4 <- eventReactive(input$next4,{
  updateNavbarPage(session,"navbar",selected="4")
  return("")
})
output$tnx4 <- renderText({
  nx4()
})

bk3 <- eventReactive(input$back3,{
  updateNavbarPage(session,"navbar",selected="3")
  return("")
})
output$tbk3 <- renderText({
  bk3()
})