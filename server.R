#Rad servera i kreiranje tabela i grafikona

server <- function(input, output, session){
  
  
  ##OPSTI PREGLED TAB
  
  # kreiranje tabele dodeljenih sredstava po godinama u levom uglu 
  
  output$tabelaukupno <- renderTable({yearstable})
  
  
  # kreiranje barcharta za sve institucije koje dodeljuju sredstva u levom uglu
  
  output$barchartdonor <- renderPlotly ({
    
    #pravljenje grafikona
    
    g1<- ggplot (data = barchartorgani, aes (x = reorder(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = c("#9F2A63FF","#F57D15FF","#FAC127FF"))+
      
      xlab ("Organ koji je raspisao konkurs")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Ukupno dodeljena sredstva\n od 2015-2021. godine")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin=unit(c(1,1,1.5,1.2),"cm"))
    
    ggplotly(g1, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0,
                    
                    font = list(size = 9)))}) 
  
  #Linechart za teme u desnom delu taba
  
  output$linechartteme <- renderPlotly ({
    
    #kreinranje linecharta
    
    gg <- ggplot (linechartteme, aes(GODINA, `n`, group = `TEMA PROJEKTA`, label = Info, color =`TEMA PROJEKTA`)) +
      
      geom_line(position = position_dodge(0.1))+
      
      geom_point(position = position_dodge(0.1))+
      
      
      ggtitle("Zastupljenost tema po godinama\n na osnovu broja projekata")+
      
      scale_color_viridis(discrete = TRUE) +
      
      ylab("Broj projekata")+
      
      xlab("")+
      
      theme(legend.position = "bottom",
            
            legend.title = element_blank(),
            
            panel.background = element_rect (fill = "#f5f1ef"),
            
            plot.title = element_text(hjust = 0.5),
            
            axis.ticks.x = element_blank(),
            
            plot.margin=unit(c(1,1,1.5,1.2),"cm"),
            
            plot.caption = element_text()
      )
    
    ggplotly(gg, tooltip = "label") %>%
      
      config(displayModeBar=FALSE) %>%
      
      layout(legend = list(title = list(text = 'Teme projekata'), orientation = "h", x = 0, y =-0.3), annotations = 
               
               list(x = 1, y = -0.31, text = "*Klikom na teme u legendi ispod određujete koja će\n da se pojavi na grafikonu. Dupli klik resetuje grafikon.", 
                    
                    showarrow = F, xref='paper', yref='paper', 
                    
                    xanchor='right', yanchor='auto', xshift=0, yshift=0, font=list(size=10)))}) 
  
  
  
  ##PODNOSIOCI TAB
  
  # Azuriranje sa server strane podnosilaca
 
 updateSelectizeInput(session, "podnosilac",
                     choices = c("SVI PODNOSIOCI",
                                   sort(unique(tabelapodnosioci$`PODNOSILAC PROJEKTA`)))
                      , server = TRUE)

  #Kreiranje velike pretrazive tabele uz uslov sta je izabrano u dropdown meniju
  
  
  tabelapodnosiocireact <- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") { 
    
    selected<-reactive({ tabelapodnosioci <- subset(tabelapodnosioci, `PODNOSILAC PROJEKTA` == input$podnosilac)
    
    tabelapodnosioci<-tabelapodnosioci %>%
      
      select(-`PODNOSILAC PROJEKTA`)
    
    })
    
    selected() 
    
    
    
    
  }else { tabelapodnosioci
  }   
  })
  
  output$tabelapodnosioci <- renderDataTable({
    
    tabelapodnosiocireact()
    
    
    
  }, options = list(language = list(sSearch="Pretraži celu tabelu:", sLengthMenu="Prikaži _MENU_ unosa", info="Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  #Kreiranje male tabele uz uslov sta je izabrano iz padajuceg menija
  
  totalpodnosiocireactive<- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") {
    
    podnosilacitotal <- podnosiocitotal %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac)%>%
      
      select(-`PODNOSILAC PROJEKTA`)
  }})
  
  
  output$tabelaukupnopodnosioci<-renderTable(totalpodnosiocireactive(),align = "c")
  
  
  #grafikoni tab podnosioci bar chart i line chart uz uslov sta je izabrano iz padajuceg menija
  
  podnosiocichartreact <- eventReactive (input$go, { if(input$podnosilac != "SVI PODNOSIOCI")
    
    
  { 
    selected <- reactive({subset(yearssredstvapodnosioci, `PODNOSILAC PROJEKTA` == input$podnosilac)})
    
    
    #Kreiranje linechart za pojedinacne podnosioce
    
    lci <- ggplot (data = selected(), aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`, label = `Info`))+
      
      geom_line (aes(group = 1), colour = "#E95420")+
      
      geom_point(color = "#E95420")+
      
      xlab ("")+
      
      ylab ("")+
      
      ggtitle (paste("Ukupno dobijena sredstva\n za izabranog podnosioca \n po godinama"))+
      
      scale_y_continuous(name="", labels = comma)+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.ticks=element_blank(),
             
             axis.text.x = element_text(angle = 30),
             
             panel.grid.minor = element_line(),
             
             plot.margin=unit(c(2,1.5,1.5,1.2),"cm"))
    
    
    ggplotly(lci, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%layout(annotations = 
                                                 
                                                 list(x = 1.2, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                                                      
                                                      showarrow = F, xref = 'paper', yref = 'paper', 
                                                      
                                                      xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    
  }else{ 
    #Kreiranje barchart za top 4 podnosioca
    
    bci <-  ggplot (data = barcharttop4podnosioci, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = "#E95420")+
      
      xlab ("Podnosilac projekta")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Četiri podnosioca koji\n su dobili najviše sredstava\n od svih organa")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
    
    ggplotly( bci, tooltip = "Info") %>%
      
      config(displayModeBar=FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  }})
  
  
  output$podnosiocilinebar <- renderPlotly ({podnosiocichartreact ()}) 
  
  
  
  
  #piechart teme za pojedinacnog podnosioca uz uslov sta je izabrano iz padajuceg menija
  
  podnosiocipiechartreact <- eventReactive (input$go, { if(input$podnosilac != "SVI PODNOSIOCI")
    
    
  {
    piecharttemepodnosiocimain <- piecharttemepodnosiocimain  %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac) 
    
    #kreiranje pie chart za pojedinacne podnosioce
    
    pcpoj<- piecharttemepodnosiocimain %>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo="label", marker = list(colors =piecharttemepodnosiocimain$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dobijenih sredstava\n po temama za izabranog podnosioca"),
             
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    
    pcpoj%>%layout(margin = list(b = 40))
    
    
    
  } else{
    
    #kreiranje piechart za sve podnosioce 
    
    pcpsvi<-  piecharttemepodnosall%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemepodnosall$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dodeljenih sredstava\n po temama za\n sve podnosioce"),
             
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    pcpsvi%>%layout(margin = list(b = 40)) }
    
  })
  
  output$chartteme<- renderPlotly ({podnosiocipiechartreact ()}) 
  
  
  #Tabela medija za datog podnosioca u APR-u
  
  tobelamedijiaprreactive<- eventReactive (input$go, {if (input$podnosilac != "SVI PODNOSIOCI") {
    
    aprregistrovanimedijifin <- aprregistrovanimedijifin %>%
      
      filter(`PODNOSILAC PROJEKTA` %in% input$podnosilac)%>%
      
      select(-`PODNOSILAC PROJEKTA`)
  }})
  
  
  output$registrovanimedijiapr<- 
  
  renderDataTable({
    
    tobelamedijiaprreactive()
    
    
    
  },options = list(pageLength = 5,lengthMenu =c(3,5,10),  searching=FALSE, info=0, language = list(sSearch="Pretraži celu tabelu:",sLengthMenu="Prikaži _MENU_ unosa",zeroRecords ='Za podnosioca nije nadjen registrovan medij u APR-u', infoEmpty="Nema unosa", info="Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  #MINISTARSTVO KULTURE TAB
  
  # Kreiranje velike pretrazive tabele za ministarstvo uz uslov sta je izabrano iz padajuceg menija
  
  
  tabelakulturareact <- eventReactive (input$go1, {if (input$temakultura != "Sve teme") { 
    
    tabelakultura <- tabelakultura %>%
      
      filter(`TEMA PROJEKTA`%in% input$temakultura) %>%
      
      select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
    tabelakultura
    
  }else {tabelakultura %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
  }   
  })
  
  output$tabelaministarstvo <- renderDataTable({
    
    tabelakulturareact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa",paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  # Kreiranje piechart teme za ministarstvo i  koliko je para  dato pojedinacno po temi po godinama uz uslov
  #sta je izabrano iz padajuceg menija
  
  temekultchartreact <- eventReactive (input$go1, { if(input$temakultura != "Sve teme")
    
    
  { linecharttemekult <- linecharttemekult %>%
    
    filter(`TEMA PROJEKTA`%in% input$temakultura)
  
  #kreiranje linecharta za teme
  
  
  lctk <- ggplot (data = linecharttemekult, aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`,label = `Info`))+
    
    geom_line (aes(group = 1), color = linecharttemekult$boje)+
    
    geom_point(color = linecharttemekult$boje)+
    
    xlab ("")+
    
    ylab ("")+
    
    ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu temu\n po godinama"))+
    
    scale_y_continuous(name="", labels = comma)+
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.ticks = element_blank(),
           
           axis.text.x = element_text(angle = 30),
           
           panel.grid.minor = element_line(),
           
           plot.margin = unit(c(1.5,1,1.5,1.2),"cm"))
  
  
  ggplotly(lctk, tooltip = "Info") %>%
    
    config(displayModeBar = FALSE) %>% layout(annotations = 
                                                list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                     
                                                     showarrow = F, xref = 'paper', yref = 'paper', 
                                                     
                                                     xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  
  }else{ 
    
    #kreiranje piechart za sve teme
    
    pctkul<- piecharttemekult%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label",marker = list(colors = piecharttemekult$boje), textinfo = 'none') %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text = paste( "Prikaz učešća dodeljenih sredstava\n po temama za Ministarstvo kulture")),
             
             margin = list(t = 100, b = 20),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
    
    pctkul%>%layout(margin = list(b = 40))
    
  }})
  
  
  output$chartministarstvoteme <- renderPlotly ({temekultchartreact()}) 
  
  #bar chart top 4 podnosioca kod ministarstva kulture
  
  
  output$barchartministarstvopodnosioci <- renderPlotly( { bcik <-  ggplot (data = barcharttop4podnosiocikult, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`, label = `Info`))+
    
    geom_bar (stat = "identity", fill = "#E95420") +
    
    xlab ("Podnosilac projekta") +
    
    scale_y_continuous(name="", labels = comma)+
    
    ggtitle ("Četiri podnosioca koji\n su dobili najviše sredstava\n od Ministarstva kulture") +
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.text.x = element_blank(),
           
           axis.ticks=element_blank(),
           
           plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
  
  
  ggplotly( bcik, tooltip = "Info") %>%
    
    config(displayModeBar=FALSE) %>%
    
    layout(annotations = 
             
             list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.",
                  
                  showarrow = F, xref = 'paper', yref = 'paper', 
                  
                  xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))})
  
  
  ##POKRAJINSKI TAB
  
  #Kreiranje pretrazive tabela uz uslov sta je izabrano iz padajuceg menija 
  
  tabelapoksekreact <- eventReactive (input$go2, {if (input$temapoksek != "Sve teme") { 
    
    tabelapoksek <- tabelapoksek %>%
      
      filter(`TEMA PROJEKTA` %in% input$temapoksek) %>%
      
      select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
    
    
    tabelapoksek
    
    
    
    
  }else {tabelapoksek %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
  }   
  })
  
  output$tabelapoksek <- renderDataTable({
    
    tabelapoksekreact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa", paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  # chart top cetiri teme poksek i  koliko je para  dato pojedinacno po temi uz uslov sta je izabrano iz  padajuceg menija
  
  temepoksekchartreact <- eventReactive (input$go2, { if(input$temapoksek != "Sve teme")
    
    
  { linecharttemepoksek<- linecharttemepoksek %>%
    
    filter(`TEMA PROJEKTA` %in% input$temapoksek)
  
  
  #kreiranje line charta za pokrajinski sekretarijat za teme
  
  lptk<- ggplot (data = linecharttemepoksek, aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`,label = `Info`))+
    
    geom_line (aes(group = 1), color = linecharttemepoksek$boje)+
    
    geom_point(color = linecharttemepoksek$boje)+
    
    xlab ("")+
    
    ylab ("")+
    
    ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu temu\n po godinama"))+
    
    scale_y_continuous(name="", labels = comma)+
    
    theme (legend.position = "none",
           
           panel.background = element_rect (fill = "transparent"),
           
           plot.background = element_rect (fill = "transparent", color = NA),
           
           plot.title = element_text ( hjust = 0.5),
           
           axis.ticks = element_blank(),
           
           axis.text.x = element_text(angle = 30),
           
           panel.grid.minor = element_line(),
           
           plot.margin = unit(c(1.8,0.7,1.5,1.2),"cm"))
  
  
  ggplotly(lptk, tooltip = "Info") %>%
    
    config(displayModeBar=FALSE) %>% layout(annotations = 
                                              
                                              list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                   
                                                   showarrow = F, xref = 'paper', yref = 'paper', 
                                                   
                                                   xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
  
  }else{ 
    
    
    #Kreiranje pie chart za teme za pokrajinski sekretarijat
    
    pctpoksek<-  piecharttemepoksek%>%
      
      plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label",marker = list(colors = piecharttemepoksek$boje), textinfo = "none") %>%
      
      add_pie() %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(showlegend = F,
             
             title = list(text =  "Prikaz učešća dodeljenih sredstava\n po temama za\n Pokrajinski sekretarijat"),
             margin = list(t = 100),
             
             paper_bgcolor = 'rgba(0,0,0,0)',
             
             plot_bgcolor = 'rgba(0,0,0,0)',
             
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
    pctpoksek%>%layout(margin = list(b = 40))
    
    
    
  }})
  
  
  output$chartpoksekteme <- renderPlotly ({temepoksekchartreact()})
  
  #bar chart top 4 podnosioca kod pokrajinskog sekretarijata
  
  
  output$barchartpoksekpodnosioci<-renderPlotly({ 
    
    bcip<-  ggplot (data = barcharttop4podnosiocipoksek, aes (x = reorder(`PODNOSILAC PROJEKTA`,-`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`,label = `Info`))+
      
      geom_bar (stat = "identity",fill = "#E95420")+
      
      xlab ("Podnosilac projekta")+
      
      scale_y_continuous(name="", labels = comma)+
      
      ggtitle ("Četiri podnosioca koji su\n dobili najviše sredstava od\n Pokrajinskog sekretarijata")+
      
      theme (legend.position = "none",
             
             panel.background = element_rect (fill = "transparent"),
             
             plot.background = element_rect (fill = "transparent", color = NA),
             
             plot.title = element_text ( hjust = 0.5),
             
             axis.text.x = element_blank(),
             
             axis.ticks=element_blank(),
             
             plot.margin = unit(c(1.8,0.7,1.5,1.2),"cm"))
    
    ggplotly( bcip, tooltip = "Info") %>%
      
      config(displayModeBar = FALSE) %>%
      
      layout(annotations = 
               list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                    
                    showarrow = F, xref = 'paper', yref = 'paper', 
                    
                    xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))})
  
  
  ##LOKALNE SAMOUPRAVE TAB
  
  
  # kreiranje male tabele sa top 4 opstine i ukupnom sumom po opstini uz uslov sta je izabrano iz padajuceg menija
  
  tabela4opstinereact <- eventReactive (input$go3, 
                                        
                                        {if(input$opstina != "Sve opštine") {tabelaukupnopstina <- tabelaukupnopstina %>%
                                          
                                          filter(OPŠTINA %in% input$opstina)%>%
                                          
                                          select(-OPŠTINA)} else {tabela4opstine}})
  
  output$top4opstine <- renderTable(tabela4opstinereact(), align = "c")
  
  #Kreiranje velike pretrazive tabele uz uslov sta je izabrano iz padajuceg menija
  
  
  tabelaopstinereact <- eventReactive (input$go3, {if (input$opstina != "Sve opštine") { 
    
    #kreiranje podskupa za tabelu zavisno od toga koja opstina je izabrana iz padajuceg menija
    tabelaopstinee <- tabelaopstinee %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina)
    
    
    if(isTRUE(sum(tabelaopstinee$`SREDSTVA U EVRIMA`) != 0)){
      
      tabelaopstinee<- tabelaopstinee %>% select(-`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)
      
      #Formatiranje kolone sredstva
      
      tabelaopstinee$`SREDSTVA U DINARIMA` <- format( tabelaopstinee$`SREDSTVA U DINARIMA` , big.mark = ',', digits = 0,nsmall = 0,scientific = FALSE)
      
      tabelaopstinee$`SREDSTVA U EVRIMA` <- format( tabelaopstinee$`SREDSTVA U EVRIMA` , big.mark = ',',digits = 0, nsmall = 0, scientific = FALSE)
      
      tabelaopstinee 
      
      #dodavanje jos jednog uslova za jos manje kolona kada sredstva nisu dodeljena nijedne godine
      
    }else{ tabelaopstinee <- tabelaopstinee %>% select(`NAZIV PROJEKTA`,`GODINA`,`SREDSTVA U DINARIMA`)}
    
    tabelaopstinee
    
  }else { 
    
    tabelaopstinee$`SREDSTVA U DINARIMA` <- format( tabelaopstinee$`SREDSTVA U DINARIMA` , big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE)
    
    tabelaopstinee$`SREDSTVA U EVRIMA` <- format( tabelaopstinee$`SREDSTVA U EVRIMA` , big.mark = ',', digits = 0,nsmall = 0, scientific = FALSE)
    
    tabelaopstinee
    
    
    
  }   
  })
  
  output$tabelaopstina <- renderDataTable({
    
    tabelaopstinereact()
    
    
    
  }, options = list(language = list(sSearch = "Pretraži celu tabelu:", sLengthMenu = "Prikaži _MENU_ unosa", info = "Prikazuje od _START_ do _END_ od ukupno _TOTAL_ unosa", paginate = list(previous = 'PRETHODNI', `next` = 'SLEDEĆI'))))
  
  
  #pie  chart teme i line chart  godine lokalne samouprave uz uslov sta je izabrano iz padajuceg menija
  
  
  loksamchartreact <- eventReactive (input$go3, { if(input$opstina != "Sve opštine")
    
    
  {
    piecharttemeloksampoj <- piecharttemeloksampoj %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina) 
    
    
    if(sum(piecharttemeloksampoj$`SREDSTVA U EVRIMA`!=0)){
      #kreiranje piechart za teme za pojedinacne samouprave i pod uslovom da su u opstini dodeljena sredstva
      
      pcloksam<- piecharttemeloksampoj%>%
        
        plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemeloksampoj$boje), textinfo = 'none') %>%
        
        add_pie() %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(showlegend = F,
               
               title = list(text = paste( "Prikaz učešća dodeljenih sredstava\n po temama za\n opštinu", input$opstina,"\n")),
               
               margin = list(t = 100, b = 20),
               
               paper_bgcolor = 'rgba(0,0,0,0)',
               
               plot_bgcolor = 'rgba(0,0,0,0)',
               
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      
      pcloksam%>%layout(margin = list(b = 40))
      
      
    }}else{ 
      #kreiranje pie chart za sve lokalne samouprave
      
      pctsvesam<- piecharttemeloksam%>%
        
        plot_ly(labels = ~Info, values = ~`SREDSTVA U EVRIMA`, hoverinfo = "label", marker = list(colors = piecharttemeloksam$boje), textinfo = "none") %>%
        
        add_pie() %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(showlegend = F,
               
               title = list(text = "Prikaz učešća dobijenih sredstava\n po temama za\n sve lokalne samouprave"),
               
               margin = list(t = 100),
               
               paper_bgcolor = 'rgba(0,0,0,0)',
               
               plot_bgcolor = 'rgba(0,0,0,0)',
               
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
        )
      
      pctsvesam%>%layout(margin = list(b = 40))
      
    }})
  
  
  output$chartopstinetemeilinechart <- renderPlotly ({loksamchartreact()}) 
  
  #bar chart top 4 podnosioca kod lokalnih samouprava i line chart sredstava po godinama za opstinu uz uslov
  #sta je izabrano iz padajuceg menija
  
  loksamchartizdtemreact <- eventReactive (input$go3, { if(input$opstina != "Sve opštine")
    
    
  {
    
    yearssredstvaloksam <- yearssredstvaloksam %>%
      
      filter(`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA` %in% input$opstina) 
    
    if(sum(yearssredstvaloksam$`SREDSTVA U EVRIMA`!=0)){
      
      # Uz uslov da su dodeljena sredstva kreiranje linechart grafikona za dodeljena sredstva po godinama 
      
      lcls <- ggplot (data = yearssredstvaloksam , aes (x = as.factor(GODINA), y = `SREDSTVA U EVRIMA`, label = `Info`))+
        
        geom_line (aes(group = 1), colour = "#E95420")+
        
        geom_point(color = "#E95420")+
        
        xlab ("")+
        
        ylab ("")+
        
        ggtitle (paste("Ukupno dodeljena sredstva\n za izabranu opštinu \n po godinama"))+
        
        scale_y_continuous(name="", labels = comma)+
        
        theme (legend.position = "none",
               
               panel.background = element_rect (fill = "transparent"),
               
               plot.background = element_rect (fill = "transparent", color = NA),
               
               plot.title = element_text ( hjust = 0.5),
               
               axis.ticks=element_blank(),
               
               axis.text.x = element_text(angle = 30),
               
               panel.grid.minor = element_line(),
               
               plot.margin=unit(c(2,1.5,1.5,1.2),"cm"))
      
      
      ggplotly(lcls, tooltip = "Info") %>%
        
        config(displayModeBar = FALSE) %>%layout(annotations = 
                                                   
                                                   list(x = 1.2, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                                                        
                                                        showarrow = F, xref = 'paper', yref = 'paper', 
                                                        
                                                        xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    }} 
    else{
      
      #kreiranje barchart za top4 podnosioca za sve lokalne samouprave
      
      bcils<-  ggplot (data = barcharttop4podnosiociloksam, aes (x = reorder(`PODNOSILAC PROJEKTA`, -`SREDSTVA U EVRIMA`), y = `SREDSTVA U EVRIMA`, label = `Info`))+
        
        geom_bar (stat = "identity",fill = "#E95420")+
        
        xlab ("Podnosilac projekta")+
        
        scale_y_continuous(name="", labels = comma)+
        
        ggtitle ("Četiri podnosioca koji su\n dobili najviše sredstava od\n svih lokalnih samouprava")+
        
        theme (legend.position = "none",
               
               panel.background = element_rect (fill = "transparent"),
               
               plot.background = element_rect (fill = "transparent", color = NA),
               
               plot.title = element_text ( hjust = 0.5),
               
               axis.text.x = element_blank(),
               
               axis.ticks=element_blank(),
               
               plot.margin=unit(c(1.5,1,1.5,1.2),"cm"))
      
      ggplotly( bcils, tooltip = "Info") %>%
        
        config(displayModeBar = FALSE) %>%
        
        layout(annotations = 
                 list(x = 1, y = -0.3, text = "*Prelaskom kursora preko grafikona videćete informacije.\n y osa predstavlja sredstva u evrima.", 
                      
                      showarrow = F, xref = 'paper', yref = 'paper', 
                      
                      xanchor = 'right', yanchor = 'auto', xshift = 0, yshift = 0, font = list(size = 9)))
    }})
  
  
  
  
  output$barchartopstinepodnosioci<-renderPlotly({loksamchartizdtemreact () })
  
  
  
  ##PODACI TAB
  
  # kako se odabere excel ili csv dugme kreira se fajl u tom formatu
  
  output$downloaddatabase <- downloadHandler (
    
    filename = function(){paste("Projektno sufinansiranje.csv",sep = "")},
    
    content = function(file) {
      
      write.csv(Projectmedia, file)}
  )
  
  output$downloaddatabase1 <- downloadHandler(
    
    filename = function(){paste("Projektno sufinansiranje.xlsx", sep = "")},
    
    content = function(file) {
      
      write_xlsx(Projectmedia, path = file)})
  
  # Ukoliko se klikne na metodologiju ili analizu skida se pdf fajl koji se nalazi u folderu www
  
  output$downloaddatadict <- downloadHandler (
    
    filename = "Metodologija.pdf",
    
    content = function(file) {
      
      file.copy("www/metodologijafinal.pdf", file)
      
      
    }
  )
  
  
  output$downloadanalysis <- downloadHandler (
    
    filename = "Publikacija.pdf",
    
    content = function(file) {
      
      file.copy("www/publikacija.pdf", file)
      
      
      
      
    }
  )   
  
  
  output$downloadanalysiseng <- downloadHandler (
    
    filename = "PublicationEnglish.pdf",
    
    content = function(file) {
      
      file.copy("www/publication.pdf", file)
      
      
      
      
    }
  ) 
  
}
