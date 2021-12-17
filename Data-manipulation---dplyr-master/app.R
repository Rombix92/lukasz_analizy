library(dplyr)
library(reshape)
library(tidyr)
library(effsize)
library(shiny)
library(xlsx)
library(ggplot2)
library(DT)
library(stats)
library(colourpicker)
library(rsconnect)
library(V8)


# wczytanie plików

baza<-read.xlsx("baza_lacznie2.xlsx", sheetIndex = 1, sheetName = TRUE)

baza_1 <- baza %>% filter((Manipulation=="Down" | Manipulation=="Up"),Czyodpad!=1, 
                          czynapisalazgodniezmaniupalacja0NIE==1, Wyklucz_gora!=1, Wyklucz_dol!=1)%>% 
  select(Manipulation,Change1:Change20,contains("Intelligence"),contains("Agreeableness"),
         contains("Conscientiousness"),contains("Extraversion"),contains("Neuroticism"),
         -contains("delta"),-contains("Oczkiwana"),contains("change_personality"))


baza_korelacje<-baza %>%   select(Czyodpad, Agreeableness_1,Conscientiousness_1,Extraversion_1,Neuroticism_1,Intelligence_1,Agreeableness_2,Conscientiousness_2,Extraversion_2,Neuroticism_2,Intelligence_2,lp_2) %>%  filter(!is.na(Neuroticism_2)) %>% select(-Czyodpad) %>% gather(key,Value,-lp_2) %>% separate(key,c("Dimension","Measurement"),"_") %>% arrange(lp_2) %>% cast(lp_2+Dimension~Measurement) 

dodge <- position_dodge(width=0.9)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
                 numericInput(inputId="Z_change_personality",label="Engagement",min=0, max=5, value=5, step=0.5)
                 ),
    
    mainPanel(
      navbarPage(
        "Menu:", 
        header= wellPanel( align="center","App and research author: Łukasz Rąbalski.", img(src="okragle.png",width=80, height=80),
                          br(),
                          "Contact: lukaszrabalski@gmail.com"),
        tabPanel(title=tags$span(style="color:green",tags$b("Introduction")),
                 wellPanel(
                   h3(tags$b("Introduction to content")),
                   "On this page, I share with you some methods taken from the dplyr package, which were 
                    extraordinarily helpful during my work on databases containing results from my 
                   Psychological master degree research.",
                   br(),
                   br(),
                   "The object of the study was to check whether personality has network properties, i.e. 
                   whether after activation of one personality components the rest components will behave in
                   a predictable pattern.",
                   
                   br(),
                   "Subjects who take part in the study were filling up a personality questionnaire twice:",
                   br(),
                   "1) in the neutral condition",
                   br(),
                   "2) in the experimental condition",
                   br(),
                   br(),
                   "Experimental condition depends on writing short dissertation praising (Up condition) or 
                   devaluating (Down condition) one particular personality item 'I get chores done right away.",
                   "Below you may see a table containing the data which will be the object of this tutorial.",
                   br(),
                   br(),
                   "Description of variables:",
                   br(),
                   "* Manipulation - information to which condition subject was ascribed",
                   br(),
                   "* Change1-Change20 - change in each of 20 personality item between two conditions",
                   br(),
                   "* Intelligence_1 - mean level of intelligence dimensions (personality dimmension) invthe neutral condition",
                   br(),
                   "* Intelligence_2 - mean level of inteligency dimensions (personality dimension) in the experimental condition",
                   
                   br(),
                   "* Agreeableness, Conscientiousness, Extraversion, and Neuroticism are also personality dimensions",
                   br(),
                   "* change_personality - mean change of personality results between two condition - neutral and experimental",
                   br(),
                   "* Zchange_personality - standardized 'change_personality'",
                   br(),
                   br(),
                   dataTableOutput(outputId="baza"),
                   br(),
                   br(),
                   h4("Please continue reading by switching panel to 'Data manipulation'.")
                                  )),
        tabPanel(title=tags$span(style="color:green",tags$b("Data manipulation")),
                 wellPanel(
                   h3(tags$b("Introduction to content")),
                   "After gathering responses from subjects I faced two challenges:",
                   br(),
                   "1) check how excluding subject suspected of low involvement in a study has an influence on results",
                   br(),
                   "2) transpose collected data into different types depending on analysss requirements.",
                   br(),
                   br(),
                   "First, I will show You how I obtain different data structure and run analysis on them. Then,
                   thanks to a Shiny application, I will be able to diagnose my data with reference to some suspicious subjects,
                   whose response pattern were outstanding.",
                   br(),
                   br(),
                   "At the beginning none, of the subjects were excluded. Below the frequency table is presented.",
                   br(),
                   br(),
                   tags$b("Table 1"),
                   tableOutput(outputId = "frequency"),
                   h3(tags$b("Data manipulation with dplyr")),
                   "In order to create plot presenting Means and Standard Deviations broken down into Manipulation group
                   the original database needed to be transposed. In the tab 'Introduction' you may see original data, below target structure, and under the table I describe how I managed to obtain it.",
                   br(),
                   br(),
                   tags$b("Table 2"),
                   dataTableOutput(outputId = "mean_and_sd"),
                   br(),
                   br(),
                   "Original base is kept in R environment as a 'database()'. Target base will be called 'num' because it will contain numbers of interest.",
                   br(),
                   "I begin with computing mean change of each item broken down into Manipulation group. I do this with the following code:",
                   br(),
                   br(),
                   tags$code("num<-database() %>% group_by(Manipulation) %>% summarize(M_Change01=mean(Change1,na.rm=TRUE), ... ,  M_Change20=mean(Change20,na.rm=TRUE), SD_Change01=sd(Change1,na.rm=TRUE), ... ,SD_Change20=sd(Change20,na.rm=TRUE))"),
                   br(),
                   br(),
                   "In order to transpose columns M_Change01, M_Change02, M_Change03 e.t.c. I had to use ", tags$b(tags$code("gather()"))," function. Following code is telling to the program to transpose all of the columns (except 'Manipulation' variable) into a row, and to name column consisting of column names 'key', and to name column consisting of column values 'values'.",
                   " Additionally, I created two new variables from the variable 'key', with ", tags$b(tags$code("mutate()"))," function. One contains information on whether a value is mean or SD, the second contains information about a number of items.",
                   br(),
                   br(),
                   tags$code(" num<-num %>% gather(key, value, -c(Manipulation)) %>% mutate(name=substr(key, start=0, stop=1), Item=substr(key, nchar(key)-1, nchar(key)))"),
                   br(),
                   br(),
                   "Then I exclude key variable, which was already transformed into two other variables and using ", tags$b(tags$code("cast()"))," function, I transpose 'name' column into to separate columns 'M' denoting 'mean' and 'S' denoting 'standard deviation' ",
                   br(),
                   br(),
                   tags$code("num<-num%>% select(1,3:5)"),
                   br(),
                   tags$code("num<- num %>% cast(Manipulation+Item~name)"),
                   br(),
                   br(),
                   "This is how the above table 2 was created basing on the original one. But what were the purposes of this data manipulation?",
                   br(),
                   "The aim was to obtain data suitable to a particular plot created with the ggplot2 package, which I present below.",
                   tags$code("ggplot(num(),aes(x=Item, y=M, fill=Manipulation, color=Significance)) +",br(),
                     "geom_col(stat='identity', position=dodge, alpha=0.8, size=1.5) +",br(),
                     "geom_errorbar( aes(ymin=M-SE_1_96, ymax=M+SE_1_96), width=0.7, position=dodge, alpha=0.9, size=0.5, color='black') +",br(),
                     "facet_grid(Dimmension~., scales='free',space='free', switch='both')+",br(),
                     "geom_hline(yintercept = 0, linetype='dashed', color = 'red', size=0.8)+",br(),
                     "scale_color_manual(values=c('green','transparent'))+",br(),
                     "labs(y='Change toward higher levels of personality dimmension', fill='Experimental condition')+",br(),
                     "theme(axis.text=element_text(size=12),axis.title=element_text(size=12))+",br(),
                     "coord_flip()"),
                   br(),
                   br(),
                   tags$b("Chart 1"),
                   plotOutput(outputId="chart1", height=800),
                   br(),
                   br(),
                   "My next goal was to calculate a series of statistical tests 
                   which would inform me in which personality items there were
                   the biggest changes between 'Down' and 'Up' conditions",
                   br(),
                   br(),
                   "It was presumed that experimental condition will have the biggest effect on manipulated item '07' of conscientiousness dimension. Secondary highest effect was expected among all other conscientiousness items: '02','12' and '17'.",
                   br(),
                   "In order to calculate those differences I use original dataset (tab 'Introduction') and with the simple use of t.test(), append(), and data.frame() the table was created.",
                   br(),
                   br(),
                   tags$code(
                   "p.value<-t.test(database()$Change1~database()$Manipulation)$p.value",br(),
                   "p.value<-append(p.value,t.test(database()$Change2~database()$Manipulation)$p.value)",br(),
                   "p.value<-append(p.value,t.test(database()$Change3~database()$Manipulation)$p.value)",br()
                   ),
                   "....",
                   br(),
                   br(),
                   tags$code(
                   "d.value<-cohen.d(database()$Change1~database()$Manipulation, na.rm=TRUE)$estimate",br(),
                   "d.value<-append(d.value,cohen.d(database()$Change2~database()$Manipulation, na.rm=TRUE)$estimate)",br(),
                   "d.value<-append(d.value,cohen.d(database()$Change3~database()$Manipulation, na.rm=TRUE)$estimate)",br(),
                   "d.value<-append(d.value,cohen.d(database()$Change4~database()$Manipulation, na.rm=TRUE)$estimate)",br()
                   ),
                   "....",
                   br(),
                   br(),
                   tags$code(
                   "item<-c(1:20)
                   
                   result<-data.frame(item,p=round(p.value,3), Cohen_d=round(d.value,2))
                   "),

                   br(),
                   br(),
                   tags$b("Table 3"),
                   dataTableOutput(outputId = "t_test"),
                   br(),
                   br(),
                   "The last task to do was to prepare the dataset for calculating within-subject correlation indices between results from survey 1 (first pool - neutral condition)
                   and results from survey 2 (second pool - experimental condition). To obtain this aim, I needed data set which would contain 5 rows for each subject, where each row would store information of different dimension and
                    results from different pools (first and second) would be stored in two different columns.
                  ",br(),
                   
                   tags$code("baza_korelacje<-baza %>%",br(),
                  "select(Agreeableness_1,Conscientiousness_1,Extraversion_1,Neuroticism_1,Intelligence_1,",br(),
                  "Agreeableness_2,Conscientiousness_2,Extraversion_2,Neuroticism_2,Intelligence_2,lp_2) %>%",br(),
                  "filter(!is.na(Neuroticism_2))%>%",br(),
                  "select(-Czyodpad) %>%",br(),
                  "gather(key,Value,-lp_2) %>%",br(),
                  "separate(key,c('Dimension','Measurement'),'_') %>%",br(),
                  "arrange(lp_2) %>%",br(),
                  "cast(lp_2+Dimension~Measurement)"),
                   
                   br(),
                   br(),
                   tags$b("Table 4"),
                   dataTableOutput(outputId = "baza_cor"),
                  br(),
                  br(),
                  "Above table allowed me for computing correlation indices between two pools for each subject.",
                  br(),
                  br(),
                  h3(tags$b("Excluding subjects of low involvement")),
                  "Finally, I was able to check how excluding subjects suspected for low involvement has an influence on results.",
                  br(),
                  br(),
                  "In order to check this please feel free to manipulate with input window in the upper left corner. I put engagement of subject in the continuous scale where:",br(),
                  "5 - denotes lowest possible involvement",br(),
                  "0 - denotes highest possible involvement.",br(),
                  "With every change you insert into the input panel, the tables and chart will change accordingly."

            
                    
                 )
        ))
    )
  )
)


server <- function(input, output) { 
  
  database<-reactive({
    database <- baza_1 %>% filter(Zchange_personality <= input$Z_change_personality) 
    database
  })
  
  frequency<-reactive({
    freq<-database() %>% group_by(Manipulation) %>% summarise(N=n())
    freq
  })
  
  t_test<-reactive({
    t.test(database$Osobowosc_1_1,Osobowosc_2_1, paired=TRUE)
  })
  

  
  num<-reactive({
    num<-database() %>% group_by(Manipulation) %>% summarize(M_Change01=mean(Change1,na.rm=TRUE), M_Change02=mean(Change2,na.rm=TRUE), M_Change03=mean(Change3,na.rm=TRUE), M_Change04=mean(Change4,na.rm=TRUE), M_Change05=mean(Change5,na.rm=TRUE), M_Change06=mean(Change6,na.rm=TRUE), M_Change07=mean(Change7,na.rm=TRUE), M_Change08=mean(Change8,na.rm=TRUE), M_Change09=mean(Change9,na.rm=TRUE), M_Change10=mean(Change10,na.rm=TRUE), M_Change11=mean(Change11,na.rm=TRUE), M_Change12=mean(Change12,na.rm=TRUE), M_Change13=mean(Change13,na.rm=TRUE), M_Change14=mean(Change14,na.rm=TRUE), M_Change15=mean(Change15,na.rm=TRUE), M_Change16=mean(Change16,na.rm=TRUE), M_Change17=mean(Change17,na.rm=TRUE), M_Change18=mean(Change18,na.rm=TRUE), M_Change19=mean(Change19,na.rm=TRUE), M_Change20=mean(Change20,na.rm=TRUE), SD_Change01=sd(Change1,na.rm=TRUE),	SD_Change02=sd(Change2,na.rm=TRUE),	SD_Change03=sd(Change3,na.rm=TRUE),	SD_Change04=sd(Change4,na.rm=TRUE),	SD_Change05=sd(Change5,na.rm=TRUE),	SD_Change06=sd(Change6,na.rm=TRUE),	SD_Change07=sd(Change7,na.rm=TRUE),	SD_Change08=sd(Change8,na.rm=TRUE),	SD_Change09=sd(Change9,na.rm=TRUE),	SD_Change10=sd(Change10,na.rm=TRUE),	SD_Change11=sd(Change11,na.rm=TRUE),	SD_Change12=sd(Change12,na.rm=TRUE),	SD_Change13=sd(Change13,na.rm=TRUE),	SD_Change14=sd(Change14,na.rm=TRUE),	SD_Change15=sd(Change15,na.rm=TRUE),	SD_Change16=sd(Change16,na.rm=TRUE),	SD_Change17=sd(Change17,na.rm=TRUE),	SD_Change18=sd(Change18,na.rm=TRUE),	SD_Change19=sd(Change19,na.rm=TRUE),	SD_Change20=sd(Change20,na.rm=TRUE))
    num<-num %>% gather(key, value, -c(Manipulation)) %>% mutate(name=substr(key, start=0, stop=1), Item=substr(key, nchar(key)-1, nchar(key)))
    num<-num%>% select(1,3:5)
    num<- num %>% cast(Manipulation+Item~name)
    num$N<-0
    num$N[num$Manipulation=="Down"]<-frequency()$N[1]
    num$N[num$Manipulation=="Up"]<-frequency()$N[2]
    num <- num %>% mutate(SE_1_96=1.96*S/sqrt(N))
    num$Significance<-""
    num$Significance[abs(num$M)>num$SE_1_96]<-"*<.05"
    num$Significance[abs(num$M)<=num$SE_1_96]<-">.05"
    num$Dimmension<-c("Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ","Agreeableness ","Conscientiousness ","Extraversion ","Neuroticism","Inteligency ")
    num
  })
  
  output$baza<-DT::renderDataTable(baza_1, options=list(
    autowidth=TRUE,scrollX = TRUE))
  
  output$frequency<-renderTable(frequency())
  
  output$mean_and_sd<-DT::renderDataTable(num(), options=list(
    autowidth=TRUE,scrollX = TRUE)
  )
  
  output$t_test<-DT::renderDataTable({
    p.value<-t.test(database()$Change1~database()$Manipulation)$p.value
    p.value<-append(p.value,t.test(database()$Change2~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change3~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change4~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change5~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change6~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change7~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change8~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change9~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change10~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change11~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change12~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change13~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change14~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change15~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change16~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change17~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change18~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change19~database()$Manipulation)$p.value)
    p.value<-append(p.value,t.test(database()$Change20~database()$Manipulation)$p.value)
    
    
    
    
    d.value<-cohen.d(database()$Change1~database()$Manipulation, na.rm=TRUE)$estimate
    d.value<-append(d.value,cohen.d(database()$Change2~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change3~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change4~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change5~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change6~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change7~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change8~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change9~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change10~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change11~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change12~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change13~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change14~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change15~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change16~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change17~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change18~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change19~database()$Manipulation, na.rm=TRUE)$estimate)
    d.value<-append(d.value,cohen.d(database()$Change20~database()$Manipulation, na.rm=TRUE)$estimate)
    
    item<-c(1:20)
    
    result<-data.frame(item,p=round(p.value,3), Cohen_d=round(d.value,2))
    result
  })
  
  output$baza_cor<-DT::renderDataTable(baza_korelacje)
  
  output$chart1<-renderPlot(
    
    ggplot(num(),aes(x=Item, y=M, fill=Manipulation, color=Significance)) +
      geom_col(stat="identity", position=dodge, alpha=0.8, size=1.5) +
      geom_errorbar( aes(ymin=M-SE_1_96, ymax=M+SE_1_96), width=0.7, position=dodge, alpha=0.9, size=0.5, color="black") +
      facet_grid(Dimmension~., scales="free",space="free", switch="both")+
      geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.8)+
      scale_color_manual(values=c("green","transparent"))+
      labs(y="Change toward higher levels of personality dimmension", fill="Experimental condition")+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12))+
      coord_flip()
  )
  output$chart2<-renderPlot(
    ggplot(num(), aes(M))+
      geom_histogram()
  )
  
  
  
  
  
  
  
  
}

shinyApp(ui=ui,server=server)


