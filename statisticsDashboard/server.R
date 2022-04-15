### Generate Distribution Data
zData <- tibble(x = rnorm(10000),
                y = unlist(dnorm(x)))
ggplot2::theme_set(ggplot2::theme_minimal())

server <- function(input,output){
  
# convert Radio Button selection into 2 dummies
  zTwoTail <- reactive({
    ifelse(input$zTail == 0, T, F)
  })
  
  zLeftTail <- reactive({
    ifelse (input$zTail !=0 & input$zTail == 1,
            T,
            F)
  })

# get P value
  zpValue <- reactive({1 - as.numeric(input$zConfInt)})
  
# get critical z value
  zqValue <- reactive({ifelse(zTwoTail() == T,
                             qnorm(zpValue()/2, lower.tail = F),
                             qnorm(zpValue(), lower.tail = F)) * ifelse(zTwoTail() == F & zLeftTail() == F,1,-1)
  })
 
# vector for to fill geom_area()
  zfill <- reactive(`if`(zTwoTail() == T, abs(zData$x)<abs(zqValue()), zData$x<zqValue()))

# render plot
output$zDistPlot <- renderPlot({
  
  zData %>% ggplot(aes(x = x, y = y))+
    # Define Limits of the plot
    ylim(0,0.41)+ scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4),limits = c(-4,4))+
    # Draw the plot
    geom_line()+
    geom_area(aes(fill = zfill()))+
    # show the p value with a segment
    geom_segment(color="forestgreen",size=1,aes(x = zqValue(), y = 0, xend = zqValue(),
                                                yend = dnorm(zqValue())))+
    # add second segment for a two-tailed
    {if (zTwoTail()) 
      list(
        geom_segment(color="forestgreen",size=1,aes(x = -zqValue(), y = 0, xend = -zqValue(),
                                                    yend = dnorm(-zqValue()))))}+
    # create description of the values in the right top corner
    annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
             label = paste("<span style='color: red;'>p-value:",as.character(zpValue()),"</span><br>",
                           "<span style='color: blue;'>conf. Interval:",as.character(input$zConfInt),"</span><br>",
                           "<span style='color: forestgreen;'>z-value:",as.character(round(zqValue(),2),"</span>"))
    )+ xlab("Standard Deviation") + ylab("Probability")+
    theme(legend.position = "none")}) 
  #### t Distribution
  tdf <- reactive({
    input$tdf
  })
  
  tData <- reactive({
    tibble(
      x = rt(10000, df = tdf()),
      y = unlist(dt(x,df = tdf()))
    )
  })
  
  tTwoTail <- reactive({
    ifelse(input$tTail == 0, T, F)
  })
  
  tLeftTail <- reactive({
    ifelse (input$tTail !=0 & input$tTail == 1,
            T,
            F)
  })
  
  tpValue <- reactive({1 - as.numeric(input$tConfInt)})
  
  tqValue <- reactive({ifelse(tTwoTail() == T,
                              qt(tpValue()/2, df = tdf(), lower.tail = F),
                              qt(tpValue(), df = tdf(), lower.tail = F)) * ifelse(tTwoTail() == F & tLeftTail() == F,1,-1)
  })
  
  ztqValue <- reactive({ifelse(tTwoTail() == T,
                               qnorm(tpValue()/2,  lower.tail = F),
                               qnorm(tpValue(), lower.tail = F)) * ifelse(tTwoTail() == F & tLeftTail() == F,1,-1)
  })
  
  
  tfill <- reactive(`if`(tTwoTail() == T, abs(tData()$x)<abs(tqValue()), tData()$x<tqValue()))
  
  
  output$table <- renderTable(zData)  
  output$text <- renderText(ztqValue())
  
  output$tDistPlot <- renderPlot({
    
    tData() %>% ggplot(aes(x = x, y = y))+
      ylim(0,0.41)+ scale_x_continuous(breaks = c(-4,-3,-2,-1,0,1,2,3,4),limits = c(-4,4))+
      geom_line()+
      geom_area(aes(fill = tfill()))+
      # draw z distribtuion on top
      geom_line(data = zData, mapping = aes(x=x, y=y), color ="grey", lwd = 2)+
      geom_segment(color="grey",size=1,aes(x = ztqValue(), y = 0, xend = ztqValue(),
                                           yend = dnorm(ztqValue())))+
      geom_segment(color="forestgreen",size=1,aes(x = tqValue(), y = 0, xend = tqValue(),
                                                  yend = dt(x = tqValue(),df = tdf())))+
      {if (tTwoTail()) 
        list(
          geom_segment(color="grey",size=1,aes(x = -ztqValue(), y = 0, xend = -ztqValue(),
                                               yend = dnorm(-ztqValue()))),
          geom_segment(color="forestgreen",size=1,aes(x = -tqValue(), y = 0, xend = -tqValue(),
                                                      yend = dt(-tqValue(),df = tdf()))))}+
      annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
               label = paste("<span style='color: black;'>df:",as.character(tdf()),"</span><br>",
                             "<span style='color: red;'>p-value:",as.character(tpValue()),"</span><br>",
                             "<span style='color: blue;'>conf. Interval:",as.character(input$tConfInt),"</span><br>",
                             "<span style='color: forestgreen;'>t-value:",as.character(round(tqValue(),2)),"</span><br>",
                             "<span style='color: grey;'>z-value:",as.character(round(ztqValue(),2)),"</span>")) + 
      xlab("Standard Deviation") + ylab("Probability")+
      theme(legend.position = "none")
  })
    ##### chi^2 distribution 
    chipValue = reactive({1 - input$chiConfInt})
    
    chiqValue <- reactive({
      qchisq(chipValue(),df = input$chidf, lower.tail = F)
    })
    
    chiData <- reactive({
      tibble(x = rchisq(10000, df = input$chidf),
             y = unlist(dchisq(x, df = input$chidf))
      )
    })
    chifill = reactive({
      chiData()$x < chiqValue()
    }) 
    
    output$text <- renderText(chiqValue())  
    output$table <- renderTable(chiData())
    
    output$chiDistPlot <- renderPlot({ 
      chiData() %>% ggplot(aes(x = x, y = y)) +
        geom_line() +
        geom_area(aes(fill = chifill()))+
        geom_segment(color="forestgreen",size=1,aes(x = chiqValue(), y = 0, xend = chiqValue(),
                                                    yend = dchisq(chiqValue(), df = input$chidf)))+
        annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
                 label = paste("<span style='color: black;'>df:",as.character(input$chidf),"</span><br>",
                               "<span style='color: red;'>p-value:",as.character(chipValue()),"</span><br>",
                               "<span style='color: blue;'>conf. Interval:",as.character(input$chiConfInt),"</span><br>",
                               "<span style='color: forestgreen;'>chisq:",as.character(round(chiqValue(),2),"</span>")
                 )) + xlab("X^2 Value") + ylab("Probability")+
        theme(legend.position = "none")
    })
    
##### F distribution 
    
    fpValue <- reactive({1 - input$fConfInt})
    fqValue <- reactive({qf(input$fConfInt,df1 = input$fdf1, df2 = input$fdf2, lower.tail = T)})
    fqLimit <- reactive({qf(0.99,df1 = input$fdf1, df2 = input$fdf2, lower.tail = T)})
    fData <- reactive({
      tibble(x = rf(10000, df1 = input$fdf1, df2 = input$fdf2 ),
             y = unlist(df(x, df1 = input$fdf1, df2 = input$fdf2 ))
      )
    })
    
    ffill = reactive({ fData()$x < fqValue() })
    
    output$text = renderText({fqValue()})
    output$table = renderTable({ffill()})
    
    output$fDistPlot = renderPlot({ 
      fData() %>% ggplot(aes(x = x, y = y )) +
        xlim(0,fqLimit()+1) +
        geom_line() +
        geom_area(aes(fill = ffill())) +
        geom_segment(color="forestgreen",size=1.5,aes(x = fqValue(), y = 0, xend = fqValue(),
                                                      yend = df(fqValue(), df1 = input$fdf1, df2 = input$fdf2))) +
        annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
                 label = paste("<span style='color: black;'>df1:",as.character(input$fdf1),"</span><br>",
                               "<span style='color: black;'>df2:",as.character(input$fdf2),"</span><br>",
                               "<span style='color: red;'>p-value:",as.character(fpValue()),"</span><br>",
                               "<span style='color: blue;'>conf. Interval:",as.character(input$fConfInt),"</span><br>",
                               "<span style='color: forestgreen;'>f-value:",as.character(round(fqValue(),2),"</span>"))
        )+ xlab("F Value") + ylab("Probability") +
        theme(legend.position = "none") 
    })
}

