library(shiny)
library(xlsx)
library(ggplot2)
library(dplyr)
library(stats)
library(DT)
library(colourpicker)
library(rsconnect)
library(V8)

# wczytanie plików

chart1<-read.xlsx("wykres1.xlsx", sheetIndex = 1, sheetName = TRUE)
t_test_1<-read.xlsx("t_test_1.xlsx", sheetIndex = 1, sheetName = TRUE)
results1<-read.xlsx("results1.xlsx", sheetIndex = 1, sheetName = TRUE)
chart2<-read.xlsx("chart2.xlsx", sheetIndex = 1, sheetName = TRUE)
results2<-read.xlsx("results2.xlsx", sheetIndex = 1, sheetName = TRUE)
results3<-read.xlsx("results3.xlsx", sheetIndex = 1, sheetName = TRUE)
results4<-read.xlsx("results4.xlsx", sheetIndex = 1, sheetName = TRUE)
results4<-read.xlsx("results4.xlsx", sheetIndex = 1, sheetName = TRUE)
machine<-read.xlsx("machine_learning_table.xlsx", sheetIndex = 1, sheetName = TRUE, header = TRUE)


fit<- lm(Abs_Item_7_change~ZM_AOF+ZINA+ZINA*ZM_AOF, data=t_test_1)
fit_2<-lm(Abs_Item_7_change~Z_Item_7+Manipulation_1+Importance+Z_Item_7*Manipulation_1, data=t_test_1)
summary(fit_2)





dodge <- position_dodge(width=0.9)

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=3,
      "Poniższe suwaki są przydatne po kliknięciu zakładki 'Machine learning'",
      hr(), br(),
      sliderInput(inputId="item7",label="Poziom pozycji 7 - warunek neutralny",min=1, max=7, value=7),
      sliderInput(inputId="importance",label="Ocena wazności wymiaru sumienności",min=1, max=10, value=10),
      selectInput("direction","Warunek eksperymentalny", choices = levels(machine$Manipulation)),
      br(),
      br(),
      # Two line breaks for visual separation
      h5("Built with",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
         "by",
         img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
         ".")
      
      
      
    ),
    mainPanel(
      navbarPage(
        "Menu:", 
       footer =  wellPanel("Autor badania i strony: Łukasz Rąbalski.", img(src="okragle.png",width=80, height=80), align="left", "Kontakt: lukaszrabalski@gmail.com"),
        tabPanel(title="Wstęp teoretyczny",
                 wellPanel(
                 br(),
                 br(),
                 h4(tags$b("Cel badania.")),
                 "Celem badania było sprawdzenie czy osobowość trafniej jest opisywana przez teorię cechy latentnej czy teorię modelu sieciowego.",
                 br(),
                 br(),
                 h4(tags$b("Kontekst teoretyczny.")),
                 tags$u(tags$i("Cecha latentna zakłada:")),
                 br(),
                 "* Wskaźniki konstruktu teoretycznego są niezależne od siebie, ich korelacje wynikają z istniejacej
                 cechy latentnej wyjaśniającej współkorelacje między zjawiskami.",
                 br(),
                tags$i("Na przykładzie kwestionariusza osobowości:"),
                br(),
                 "korelacja między pozycjami 'Lubię chodzić na imprezy' i 'Lubię być w centrum uwagi' nie wynika z faktu, 
                 iż oba zachowania są ze sobą przyczynowo, bądź homeostatycznie powiązane, ale wynika z faktu, iż posiadają wspólną przyczynę, 
                 w tym wypadku czynnik Ekstrawersji ulokowanym w każdym z nas. Zgodnie z tym podejściem, to ekstrawersja jest przyczyzną lubienia imprez i chodzenia na imprezy.",
                 br(), br(),
                 tags$u(tags$i("Model sieciowy zakłada:")),
                 br(),
                 "* Wskaźniki konstruktu tworzą dynamiczną sieć. Zmiana w jednym elemencie będzie propagowana na pozostałe.",
                br(),
                tags$i("Na przykładzie kwestionariusza osobowości:"),
                br(),
                "korelacja między pozycjami 'Lubię chodzić na imprezy' i 'Lubię być w centrum uwagi'
                 wynikają z faktu, iż oba te zachowania wzajemnie na siebie wpływają, bądź jedno jest przyczyną drugiego.",
                 br(), 
                 h4(tags$b("Metoda.")),
                 "W pierwszej kolejności badani uzupełniali kwestionariusz osobowości", tags$b("w warunkach neutralnych")," (nic nie wpływało na ich sposób odpowiedzi).",
                 br(),
                 br(),
                 "Następnie po miesiącu część była proszona  o napisanie krótkiej rozprawki zachwalającej wykonywanie obowiązków przy pierwszej nadążającej się okazji (warunenk eksperymentalny: w górę), a część nie popierającej takiej postawy (warunenk eksperymentalny: w dół).
                 Po napisaniu rozprawki badani ponownie byli proszeni o uzupełnienie kwestionariusza osobowości.",
                 br(),
                 br(),
                 "Badani byli proszeni o pisanie rozprawki akurat na taki temat, gdyż wypełnianie obowiązków przy pierwszej nadażającej się okazji jest jedną z pozycji wymiaru sumienności kwestionariusza osobowości. Gdyby na skutek pisania rozprawki zmianie uległby deklarowany poziom badanych tylko na tej pozycji, byłaby to przesłanka
                 za tym, by uznać model cechy latentnej jako trafjniej opisujący osobowość. Gdyby natomiast zmianie w poziomie tej pozycji towarzyszyły zmiany w innych pozycjach wymiaru sumienności np 'Lubię porządek', taki rezultat przemawiałby za modelem sieciowym.",
                 br(),
                 br(),                 
                 
                 "Wyniki badania można zobaczyć w zakładce 'Wyniki'."
        )),
        
        tabPanel(title="Wyniki", 
                 wellPanel(
                 h3(tags$b("Sekcja 1 - wpływ pisania rozprawki na wyniki kwestionariusza osobowości")),
                 "Poniżej zaprezentowano rozkład zmian jakie zaobserwowane w wynikach kwestionariusza osobowości w podziale na rodzaj warunku eksperymentalnego w jakiej brał udział badany.",
                 br(),
                 br(),
                 "Na zielono podświetlono te zmiany w wynikach, które były istotne 
                statystycznie, tzn. badani istotnie inaczej względem warunku neutralnego odpowiedzieli na te pozycje po 
                napisaniu rozprawki.",
                 br(),
                 br(),
                 "Przypomnienie: rozprawki tyczyły się pozycji nr 7: 'I get chores done right away'",
                 br(),br(),
                 "Wykres 1",br(),
                 tags$i("Wykres zmian w poszczególnych pozycjach kwestionariusza osobowości w podziale na grupę warunku eksperymentalnego"),
                 
                 plotOutput(outputId="chart1", height=800), 
                 br(),
                 "Efekt warunku eksperymentalnego wystąpił, tzn. wystąpiła istotna różnica w zmianie 
                w odpowiedziach na pozycje 7 między grupą warunku w górę (M = 0.29, SD = 0.86), a grupą
                warunku w dół (M = -0.44, SD = 0.82) t (72)= 3.74, p <.001, Cohen's d = 0.86. Zaobserwowano wysoką siłę efektu.",
                 br(),br(),
                 "Tabela 1",br(),
                 tags$i("Wyniki testu t studenta dla różnic w zmianie pozycji 7 między grupami warunków eksperymentalnych"),      
                 tableOutput(outputId="table1"),
                 wellPanel(style = "background: #90EE90",tags$b("Najważniejszy wniosek z badania:"),"Pisanie rozprawki spowodowało istotną zmianę w zamierzonej pozycji 7 ('Wykonuję obowiązki przy pierwszej nadażającej się okazji').", tags$u(" Zmiana ta
                  nie rozpropagowała się dalej,"), "tzn. nie było innych istotnych zmian z kwestionariuszu osobowości. Powyższy rezultat jest argumentem za tym, iż model cechy latentnej trafniej opisuje osobowość niż model
                  sieciowy. Innymi słowy, korelacje między pozycjami osobowości nie wynikają z dynamicznym powiązaĹ„ między nimi, a z czynnika osobowościowego który jest ich wspólną przyczyną.")
                
                 ),
                 
                 
                 hr(),hr(),
                 h3(tags$b("Sekcja 2 - wpływ przerwy między pisaniem rozprawki a uzupełnianiem kwestionariusza osobowości na siłę efektu pisania rozprawki")),
                 "W badaniu kontrolowano również, czy przerwa między pisaniem rozprawki a wypełnnianiem kwestionariusza osobowości osłabia siłę wpływu aktu pisania rozprawki.",
                 br(),br(),
                 "Osoby, które przystępowały do uzupełniania kwest. osobowości po przerwie, w mniejszym stopniu ulegały efektowi pisania rozprawki,
                choć różnica ta niebyła istotna statystycznie F = 1.05, p = 0.31.",
                 br(),br(),
                 "Wykres 2",br(),
                 tags$i("Wykres siły efektu pisania rozprawki w zależności od wystąpienia przerwy bądź nie, w podziale na grupy warunków eksperymentalnego"),
                 plotOutput(outputId="chart2", height=400),br(),br(),
                 "Tabela 2",br(),
                 tags$i("Wyniki testu ANOVA dla różnic w sile efektu pisania rozprawki dla czynników: przerwa oraz grupa eksperymentalna"),
                 tableOutput(outputId="table2"),
                 
                 
                 
                 hr(),hr(),
                 wellPanel(
                  h3(tags$b("Sekcja 3 - próba wyjaśnienia zmienności efektu pisania rozprawki")),
                 "Na koniec spróbowano wyjaśnić zmienność siły wpływu pisania rozprawki na zmianę w pozycji 7 przy użyciu dwóch modeli:",
                 br(),br(),
                 "1) model 1 wyjaśniał tę zmienność przy użyciu konstruktów zaczerpniętych z teori PSI ('Psychological System Interaction') Julius Kuhl'a ",
                 br(),
                 "2) model 2 wyjaśniał tę zmienność przy użyciu następujących zmiennych:",br(),
                 "     * wyjściowy poziom pozycji 7 u badanego (w warunkach neutralnych)",br(),
                 "     * ocena ważności wymiaru sumienności",br(),
                 "     * przynależność do warunków eksperymentalnych (up vs down)",
                 hr(),hr(),
                 "Model 1 okazał się być nieistotny statystycznie F = 2.25, p = 0.09, R^2 = 0.09. W związku z powyższym założenia teorii  PSI nie będą omawiane.",
                 
                 br(),br(),
                 "Tabela 3",br(),
                 tags$i("Wartości oszacowaĹ„ regresji dla modelu 1."),
                 tableOutput(outputId="table3"),
                 
                 "Model 2 był istotny statystycznie F = 4.32, p < 0.05, R^2 = 0.16.",br(),
                 "Istotnymi predyktorami tego, jak silnie akt pisania rozprawki zadziała na zmianę w pozycji 7 były dwa efektu, efekt główny poziomu wyjściowego pozycji 7 oraz interakcja poziomu wyjściowego pozycji 7 z grupą warunku eksperymentalnego",br(),
                 "Powyższe wyniki można rozumieć w następujący sposób. To w jak silnym stopniu osoby zmieniały swoją odpowiedź w pozycji 7 
                 pod wpływem pisania rozprawki zależało od poziomu pozycji 7 jakim charakteryzowali się w warunkach neutralnym oraz od warunku eksperymentalnego do którego zostali przydzieleni.",br(),br(),
                 "W grupie osób", tags$b(" piszących rozprawkę o zaletach "), "realizacji obowiązków przy pierwszej nadażającej się okazji, im",tags$b(" niższy "),"był posiadany wyjściowy poziom tej pozycji tym większy efekt pisania rozprawki można było zaobserwować.",br(),br(),
                 "W grupie osób", tags$b(" piszących rozprawkę o wadach "), "realizacji obowiązków przy pierwszej nadażającej się okazji, było na odwrót. Im", tags$b(" wyższy "),"był posiadany wyjściowy poziom tej pozycji tym większy efekt pisania rozprawki można było zaobserwować.",
                 br(),br(),
                 "Wyżej opisane zjawisko zostało zobrazowane na wykresie nr 3. Dodatkowo namacalnemu zrozumieniu tego efektu może pomóc algorytm przygotowany w zakładce 'Machine Learning'.",
                 br(),br(),
                 "Tabela 4",br(),
                 tags$i("Wartości oszacowaĹ„ regresji dla modelu 2."),
                 tableOutput(outputId="table4"),
                 "Wykres 3",br(),
                 tags$i("Wykres siły efektu grupy eksperymentalnej w zależności od poziomu wyjściowego pozycji 7."),
                 plotOutput(outputId="chart5", height=400),
                 br(),
                 "Poniżej dodatkowo przedstawiono wykres rozrzutu reszt względem przewidywanych przez model wartości. Model charakteryuje się homoskedastycznością reszt, co pozwala zaufać wartość oszacowaĹ„ uzyskanym przy użyciu metody najmniejsych kwadratów.",
                  br(),br(),
                 "Wykres 4",br(),
        tags$i("Wykres reszt na przestrzeni różnych przewidywanych wartości dla modelu 2."),
        plotOutput(outputId="chart4", height=400)
        
        )),
        tabPanel(title="Machine Learning",
                 
                 wellPanel(
                 "Parametry z tabeli nr 4 z zakładki 'Results' posłużyły do stworzenia algorytmu
                  umożliwiającego przewidywanie wielkości zmiany w pozycji 7 u przeciętnego badanego
                charakteryzującego się podanym poziomem pozycji 7 w warunkach neutralnych ('Poziom pozycji 7 - warunek neutralny'), deklarowaną 
                ważnością wymiaru sumienności ('Ocena ważności wymiaru sumienności') oraz typem warunku eksperymentalnego w jakim miałby się on znaleźć ('Warunek eksperymentalny').",
                br(), br(),
                
                "Dla parametrów podanych z panelu po lewej stronie przewidywany poziom zmiany w pozycji 7 u badanego wynosi:",
                h3(tags$span(style="color:green",align="center",tags$b(textOutput(outputId="prediction")))),
                br(),
                "Powyższą zmianę wyliczoną na podstawie algorytmu można odnieść do zmiany, jaką zaobserwowano u osób badanych, cechujących się dokładnie takimi samymi charakterstykami jak te z suwaków. Rzeczywiste wartości przedstawia tabela nr 5.",
                br(),
                tags$i("Przykład interpretacji:")," jeżeli ustawimy suwaki:", 
                br(), "* ", tags$i(tags$u('Poziom pozycji 7 - warunek neutralny'))," na wartość 7",
                br(), "* ", tags$i(tags$u('Ocena ważności wymiaru sumienności'))," na wartość 10",
                br(), "* ", tags$i(tags$u('Warunek eksperymentalny'))," na wartość 'Down'",
                br(), "wyliczymy wtedy przy użyciu równania uzyskanego z modelu 2 z zakładki 'Wyniki' przewidywaną zmianę u badanego w pozycji 7, który charakteryzuje się w warunkach neutralnych poziomem pozycji 7 równym '7', ważnością wymiaru sumienności '10' i został ulokowany w warunku eksperymentalnym w dół, tzn. został poproszony o napisanie rozprawki na temat wad realizacji obowiązków przy pierwszej nadażającej się okazji.",
                br(),"Model przewiduje, że osoba o wyżej wymienionej charakterystyce zmieni pod wpływem pisania rozprawki poziom pozycji 7 o",  tags$b(" -1.12."),
                br(),
                br(),"Przewidywana zmiana badanego została wyliczona i przedstawiona przy użyciu zielonych liczb.",
                br(),
                br(),"W Tabeli 5 możemy sprawdzić, jak wyglądała rzeczywista zmiana wśród badanych o danej charakterystyce (w kolumnie 'Size.of.expected.change.'). Wynika  z niej, że było dwóch takich badanych, u jednego na skutek pisania rozprawki w warunku eksperymentalnym w dół poziom pozycji 7 zmalał o 1, u drugiego o 2. Można stwierdzić, że w tym wypadku model nie mylił się znacząco.",
                br(),br(),
                "Tabela 5",br(),
                tags$i("Badani odpowiadający parametrom wprowadzonym w panelu po lewej stronie wraz z odpowiadającymi im wielkościami zmiany."),
                 tableOutput(outputId="table5")
                 )
                 
        )
      )
    )
  )
)

server <- function(input, output) { 
  
  output$chart1<-renderPlot(
  
    ggplot(chart1,aes(x=Item, y=M, fill=Manipulation, color=Significance)) +
    geom_col(stat="identity", position=dodge, alpha=0.8, size=1.5) +
    geom_errorbar( aes(ymin=M-X1.96.SE, ymax=M+X1.96.SE), width=0.7, position=dodge, alpha=0.9, size=0.5, color="black") +
    facet_grid(Dimmension~., scales="free",space="free", switch="both")+
    geom_hline(yintercept = 0, linetype="dashed", color = "red", size=0.8)+
    scale_color_manual(values=c("green","transparent"))+
    labs(y="Change toward higher levels of personality dimmension", fill="Experimental condition")+
      theme(axis.text=element_text(size=12),axis.title=element_text(size=12))+
    coord_flip()
)
  output$table1<-renderTable(results1)
  
  output$chart2<-renderPlot(
    ggplot(chart2, aes(x=Latency, y=Mean_Abs_change, color=Manipulation))+
      geom_line(aes(group=Manipulation))+
      geom_point(size=2)+
      geom_errorbar(aes(ymin=Mean_Abs_change-1.96*SE, ymax=Mean_Abs_change+1.96*SE),  size=0.4, width=0.1, alpha=0.7,position=position_dodge(width=0.1))+
      labs(y="Effect size of writing an essay", color="Experimental Condition")
    
  )
  
  output$table2<-renderTable(results2)
  
  output$chart3<-renderPlot(
    ggplot(fit, aes(x=fit$fitted.values,y=fit$residuals, color=abs(fit$residuals)))+
      geom_jitter(position=position_jitter(0.08))+
      geom_smooth(se=FALSE)+
      xlim(-0.5,1.5)+
      ylim(-2,2)+
      labs(y="Residuals",x="Fitted values", color="Residuals")+
      scale_color_gradient(low="green", high="red")
  )
  
  output$table3<-renderTable(results3)
  
    output$chart4<-renderPlot(ggplot(fit_2, aes(x=fit_2$fitted.values,y=fit_2$residuals, color=abs(fit_2$residuals)))+
    geom_jitter(position=position_jitter(0.08), size=3, alpha=0.6)+
    geom_smooth(se=FALSE)+
    xlim(-0.5,1.5)+
    ylim(-2,2)+
    labs(y="Residuals",x="Fitted values", color="Residuals")+
    scale_color_gradient(low="green", high="red")
    )
    
    output$chart5<-renderPlot(ggplot(t_test_1,aes(x=Item_7,y=Abs_Item_7_change, color=Manipulation))+
                                geom_jitter(width = 0.1, height=0.1, size=3, alpha=0.6)+
                                geom_smooth(se=FALSE, method="lm")+
                                labs(y="Effect size of writing an essay",x="Poziom pozycji 7 - warunek neutralny", color="Experimental Condition")
        )
    
    output$table4<-renderTable(results4)
    
    output$prediction<-reactive({
      
      if(input$direction=="Down") {
        manip=0
        prediction=-(-1.03+0.21*input$item7+1.47*manip+0.068*input$importance-0.40*input$item7*manip)
      }
      else {
        manip=1
        prediction=-1.03+0.21*input$item7+1.47*manip+0.068*input$importance-0.40*input$item7*manip
      }
      
      
    })
    
    machine_t<-reactive({
      data<- subset(machine, Output.level.of.Item_7==input$item7 & Importance.of.Conscientiousness==input$importance & Manipulation==input$direction)
      data
      })
    
    output$num1<-renderPrint({input$item7})
    output$num2<-renderPrint({input$importance})
    output$text<-renderPrint({input$direction})
    
    output$table5<-renderTable({
      machine_t()[,-4]
    })
   
    
    

  

}

shinyApp(ui=ui,server=server)


