library(tidyverse)
library(ggtext)
dat <- tibble( x = rnorm(10000, 0, 1),
               y = dnorm(x,0,1))

df = 5
confInt = 0.95
pValue = 1 - confInt
qValue <- qchisq(confInt,df = df, lower.tail = T)
chiData <- tibble(x = rchisq(10000, df = df),
                y = unlist(dchisq(x, df = df)),
                fill = x<qValue )

chiData %>% ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_area(aes(fill = fill))+
  geom_segment(color="forestgreen",size=1,aes(x = qValue, y = 0, xend = qValue,
                                              yend = dchisq(qValue, df = df)))+
  annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
           label = paste("<span style='color: black;'>df:",as.character(`df`),"</span><br>",
                         "<span style='color: red;'>p-value:",as.character(`pValue`),"</span><br>",
                         "<span style='color: blue;'>conf. Interval:",as.character(`confInt`),"</span><br>",
                         "<span style='color: forestgreen;'>chisq:",as.character(round(`qValue`,2),"</span>")
           ))+
  theme(legend.position = "none")

####
# f distribution
df1 = 10
df2 = 100
confInt = 0.95
pValue = 1 - confInt
qValue <- qf(confInt,df1 = df1, df2 =df2, lower.tail = T)
fData <- tibble(x = rf(10000, df1 = df1, df2 =df2 ),
                y = unlist(df(x, df1 = df1, df2 =df2 )),
                fill = x<qValue )

fData %>% ggplot(aes(x = x, y = y, fill = fill)) +
  geom_line() +
  geom_area()+
  geom_segment(color="forestgreen",size=1.5,aes(x = qValue, y = 0, xend = qValue,
                                                yend = df(qValue, df1 = df1,df2 = df2)))+
  annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
           label = paste("<span style='color: black;'>df1:",as.character(`df1`),"</span><br>",
                         "<span style='color: black;'>df2:",as.character(`df2`),"</span><br>",
                         "<span style='color: red;'>p-value:",as.character(`pValue`),"</span><br>",
                         "<span style='color: blue;'>conf. Interval:",as.character(`confInt`),"</span><br>",
                         "<span style='color: forestgreen;'>f-value:",as.character(round(`qValue`,2),"</span>")
           )) +
  theme(legend.position = "none")



#######
twoTail = T
leftTail = T
df = 5
confInt = 0.95
pValue = 1 - confInt
qValue <- ifelse(twoTail == T,
                 qt(pValue/2,df = df, lower.tail = F),
                 qt(pValue,df = df, lower.tail = F))
zValue <- ifelse(twoTail == T,
                 qnorm(pValue/2,lower.tail = F),
                 qnorm(pValue, lower.tail = F))
## must be before data, so labels for fill are assigned correctly
if (twoTail == F & leftTail == F) {
  zValue = -zValue
  qValue = -qValue
}

tData <- tibble(x = rt(10000, df = df),
                y = unlist(dt(x, df = df)),
                fill = `if`(twoTail == T,abs(x)<qValue,x<qValue))

zData <- tibble( x = rnorm(10000, 0, 1),
                 y = dnorm(x,0,1))


tData %>% ggplot(aes(x = x, y = y)) +
  ylim(0,0.41)+ scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3),limits = c(-4,4))+
  geom_line() +
  geom_area(aes(fill = fill))+
  geom_line(data = zData, mapping = aes(x=x, y=y), color ="grey")+
  geom_segment(color="grey",size=1,aes(x = zValue, y = 0, xend = zValue,
                                       yend = dnorm(zValue)))+
  geom_segment(color="forestgreen",size=1,aes(x = qValue, y = 0, xend = qValue,
                                              yend = dt(qValue, df = df)))+
  {if (twoTail) 
  list(
  geom_segment(color="grey",size=1,aes(x = -zValue, y = 0, xend = -zValue,
                                       yend = dnorm(-zValue))),
  geom_segment(color="forestgreen",size=1,aes(x = -qValue, y = 0, xend = -qValue,
                                              yend = dt(-qValue, df = df))))}+
  annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
           label = paste("<span style='color: black;'>df:",as.character(`df`),"</span><br>",
                         "<span style='color: red;'>p-value:",as.character(`pValue`),"</span><br>",
                         "<span style='color: blue;'>conf. Interval:",as.character(`confInt`),"</span><br>",
                         "<span style='color: forestgreen;'>t-value:",as.character(round(`qValue`,2)),"</span><br>",
                         "<span style='color: grey;'>z-value:",as.character(round(`zValue`,2),"</span>"))
           )+
  theme(legend.position = "none")

# z dist
twoTail = F
leftTail = T
confInt = 0.95
pValue = 1 - confInt
qValue <- ifelse(twoTail == T,
                 qnorm(pValue/2, lower.tail = F),
                 qnorm(pValue, lower.tail = F))

## must be before data, so labels for fill are assigned correctly
if (twoTail == F & leftTail == F) {
  qValue = -qValue
}

zData <- tibble(x = rnorm(10000),
                y = unlist(dnorm(x)),
                fill = `if`(twoTail == T, abs(x)<qValue, x<qValue))

zData %>% ggplot(aes(x = x, y = y))+
  ylim(0,0.41)+ scale_x_continuous(breaks = c(-3,-2,-1,0,1,2,3),limits = c(-3,3))+
  geom_line()+
  geom_area(aes(fill = fill))+
  geom_line(data = zData, mapping = aes(x=x, y=y), color ="grey")+
  geom_segment(color="forestgreen",size=1,aes(x = qValue, y = 0, xend = qValue,
                                              yend = dnorm(qValue)))+
  {if (twoTail) 
    list(
      geom_segment(color="forestgreen",size=1,aes(x = -qValue, y = 0, xend = -qValue,
                                                  yend = dnorm(-qValue))))}+
  annotate("richtext", Inf, Inf, hjust = 1, vjust = 1,
           label = paste("<span style='color: red;'>p-value:",as.character(`pValue`),"</span><br>",
                         "<span style='color: blue;'>conf. Interval:",as.character(`confInt`),"</span><br>",
                         "<span style='color: forestgreen;'>z-value:",as.character(round(`zValue`,2),"</span>"))
  )+
  theme(legend.position = "none")

