#########KRAJ OBRADE PODATAKA SLEDI UI I SERVER DEO####


#Kreiranje izgleda aplikacije User interface-UI

 
  navbarPage(
    #glavni naslov i izbor teme
    
    "Projektno sufinansiranje medija u Srbiji", theme = shinytheme ("united"),
    
    # izgled prvog taba Opsti pregled sa tekstom i tabelama i grafikonom sa leve strane u sidebar panelu
    
    tabPanel ("Opšti pregled", 
              sidebarPanel ( h4 ("Tabela ukupno dodeljenih\n sredstava po godinama od svih organa vlasti", align="center"),
                            
                            tableOutput("tabelaukupno"),
                            
                            plotlyOutput("barchartdonor") %>% withSpinner(color="#E95420")
              ),
              
              #tekst koji ide u glavni ili desni deo sa nekim informacijama koje izvlaci iz same tabele, dodati su i linkovi
              #za kreiranje novog pasusa koristi se h4
              
              mainPanel ( h4 ("Dobrodošli na stranicu na kojoj se možete bolje upoznati sa procesom projektnog sufinansiranja medijskih
                                  
                                  sadržaja od javnog interesa u Republici Srbiji. Projekat", a(" Centra za održive zajednice",href = "https://odrzivezajednice.org/"), 
                              
                              "u okviru kojeg je izrađena ova veb aplikacija („Otvorenim podacima do kvalitetnijeg projektnog sufinansiranja medijskih sadržaja“) 
                                  
                                  podržali su ", a("Ministarstvo kulture i informisanja Republike Srbije", href = "http://www.kultura.gov.rs"),
                              
                              "i", a("Misija OEBS-a u Srbiji", href = "https://www.osce.org/mission-to-serbia")," sa ciljem da se učini transparentnim ovaj proces 
                                  
                                  koji je važan za medije, medijske profesionalce, ali i građane.", align = "justify"),
                          
                          h4 ("Veb aplikacija koja je pred vama daje mogućnost da pretražujete podatke po podnosiocu projekta,
                                  
                                  prema organu koji je dodelio sredstva, ali i prema temama koje su zastupljene u realizovanim projektima.
                                  
                                  Možete, takođe, da preuzmete podatke, metodologiju vezanu za podatke i njihovo prikupljanje, 
                                  
                                  kao i celokupnu publikaciju u kojoj se pored metodoloških napomena i preporuka, nalazi i tekst 
                                  
                                  o istorijatu konkursnog sufinansiranja u Srbiji.
                                  
                                  Podaci su takođe dostupni i na", a(" Portalu otvorenih podataka.", href = "https://data.gov.rs/sr/datasets/rezultati-konkursa-za-projektno-sufinansiranja-medijskikh-sadrzhaja/"), align ="justify"),
                          
                          h4 (paste("Na ovoj stranici imate sumirane podatke za sve godine trajanja projektnog sufinansiranja, a zastupljeni 
                                        
                                        su svi organi vlasti koji su dodeljivali novac medijima na osnovu Zakona o javnom informisanju i 
                                        
                                        medijima. Jedna od novina je zastupljenost tema koje su definisane na osnovu naslova projekata. 
                                        
                                        Tokom ovog procesa, od 2015. do  2021. godine, dodeljeno je ukupno ", format(sum(Projectmedia$`SREDSTVA U EVRIMA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE),
                                    
                                    "evra za ", format(NROW(tabelapodnosioci$`NAZIV PROJEKTA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE), "projekata koji su podneti od ukupno",
                                    
                                    format(n_distinct(tabelapodnosioci$`PODNOSILAC PROJEKTA`), big.mark = ',', digits = 0, nsmall = 0, scientific = FALSE), " podnosilaca. Predmet prikupljanja podataka 
                                        
                                        bili su konkursi Minstarstva kulture i informisanja Republike Srbije, Pokrajinskog sekretarijata za kulturu, javno informisanje i odnose sa verskim zajednicama i",
                                    
                                    n_distinct(yearssredstvaloksam$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`) ,"lokalne samouprave (gradovi i opštine). Grafikon sa vaše leve strane prikazuje dodelu sredstava prema organu vlasti koji je raspisao konkurse za sve godine, a drugi 
                                  
                                  grafikon na ovoj stranici prikazuje zastupljenost tema po godinama na osnovu broja projekata."), align = "justify"),
                          
                          h4 (strong("Važna napomena:")," pošto su grafikoni interaktivni, potrebno je samo da pređete kursorom preko njih da biste videli informacije.", align ="justify"),
                          
                          #mesto gde ce ici linechart grafikon a ispod njega logoi
                          
                          plotlyOutput("linechartteme", height = 500) %>% withSpinner(color = "#E95420"),
                          h5 (strong("*")," Kreiranje veb aplikacije podržali su Misija OEBS-a u Srbiji i Ministarstvo kulture i informisanja. Stavovi izrečeni pripadaju isključivo autoru i njegovim saradnicima i
                              ne predstavljaju nužno stav Misije OEBS-a u Srbiji i Ministartva kulture i informisanja. Svako spominjanje naziva Kosovo, bez obzira da li upućuje na teritoriju, institucije ili stanovništvo u ovoj aplikaciji i podacima treba da se tumači
                              u potpunosti u skladu sa Rezolucijom saveta bezbednosti UN 1244.", align ="justify"),
                          
                          fluidRow(
                            column(4, img(src="COZ logo.jpg",style="width: 200px;")),
                            
                            column(4, img(src="OEBSlog.png", style="width: 260px; margin-top: 80px;")),
                            
                            column(4, img(src="min.png"))
                          )
                          
                          
                          
                          
              )),
    
    #uredjivanje taba podnosilac projekta
    
    tabPanel("Podnosilac projekta",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke vezane za",strong("podnosioce projekata"), "koji su dobili sredstva u periodu od početka 2015. do 2021. godine.", align = "justify"), 
               
               h4("Ako izaberete sve podnosioce iz padajućeg menija i pritisnete dugme ", strong("“Pretraži”"), "pojaviće vam se grafikon sa informacijama o četiri podnosioca medijskih projekata koji su
                        
                        dobili najviše novca od svih organa koji raspisuju konkurse, grafikon učešća dodeljenih sredstava po temama za sve podnosioce, kao i tabelu na kojoj će se nalaziti celokupni podaci koje možete pretraživati na različite načine.", align = "justify"),
               
               h4("Pretragom pojedinačnih podnosilaca pojaviće vam se informacije o ukupno dobijenim sredstvima koje je on povukao tokom ovog procesa, grafikon trenda dobijanja novca po godinama, učešća dobijenih sredstava po temama i tabela sa ostalim informacijama
                        
                        o izabranom podnosiocu projekta. Na samom dnu možete da vidite koji su mediji registrovani za  izabranog podnosioca u APR-u.", align = "justify"),
               
               #dodavalnje na levoj strani drop down opcije sa podnosiocima i tastera pretrazi
              
               selectizeInput("podnosilac", label = "Podnosilac:", choices = NULL,options = list(placeholder = "Ukucajte podnosioca ili SVI PODNOSIOCI")),
               
               actionBttn(inputId = "go", label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabelu i grafikone"),
               
               #mesto gde ce u sidebar ici koliko je ukupno dato po podnosiocu,i grafikoni
               
               tableOutput("tabelaukupnopodnosioci"),
               
               plotlyOutput("podnosiocilinebar"),
               
               plotlyOutput("chartteme"),
               
               dataTableOutput("registrovanimedijiapr")
               
             ), 
             
             #glavni panel sa velikom pretrazivom tabelom podnosilaca
             
             mainPanel ( dataTableOutput("tabelapodnosioci"))),
    
    #uredjivanje taba Ministarstva kulture
    
    tabPanel("Ministarstvo kulture i informisanja",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o konkursima", strong ("Ministarstva kulture i informisanja Republike Srbije"), "za projektno sufinansiranje medijskih sadržaja od 2015 do 2021. godine.",align= "justify"),
               
               h4("Podatke možete da pretražujete po temi projekata. Grafikoni prikazuju trend davanja novca u odnosu na određene teme. U donjem delu stranice imate i informaciju o četiri podnosioca koji su
                     
                     dobili najviše novca od Ministarstva.", align="justify"),
               
               selectizeInput("temakultura", "Izaberite temu:", options = list(placeholder = "Ukucajte temu ili Sve teme"), c("Sve teme",
                                                                  sort(unique(tabelakultura$`TEMA PROJEKTA`)))),
               
               actionBttn(inputId = "go1", label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabele i grafikone"),
               
               plotlyOutput("chartministarstvoteme"),
               
               plotlyOutput("barchartministarstvopodnosioci")
             ), 
             
             mainPanel ( dataTableOutput("tabelaministarstvo"))),
    
    #Uredjivanje taba pokrajinski sekretarijat 
    
    tabPanel("Pokrajinski sekretarijat",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o realizovanim projektima po konkursima ", strong("Pokrajinskog sekretarijata za kulturu, javno informisanje i odnose sa verskim zajednicama"),"za projektno sufinansiranje medijskih sadržaja 
                        
                        u periodu od 2015. do 2021. godine.", align = "justify"),
               
               h4 ("Grafikoni prikazuju trend davanja novca u odnosu na teme projekata. U donjem delu stranice se nalazi i informacija o četiri podnosioca projekata koji su dobili najviše novca od ovog organa vlasti.", align = "justify"),
               
               selectizeInput("temapoksek", "Izaberite temu:",options = list(placeholder = "Ukucajte temu ili Sve teme"), c("Sve teme",
                                                                 
                                                                 sort(unique(tabelapoksek$`TEMA PROJEKTA`)))),
               
               actionBttn(inputId = "go2",label = "Pretraži", icon("search"), style = "jelly", size = "sm",color = "danger"),
               
               helpText("Kliknite na Pretraži da biste videli tabele i grafikone"),
               
               plotlyOutput("chartpoksekteme"),
               
               plotlyOutput("barchartpoksekpodnosioci")
               
             ), 
             
             mainPanel ( dataTableOutput("tabelapoksek"))),
    
    #Uredjivanje taba Lokalne samouprave
    
    tabPanel("Lokalne samouprave",
             
             sidebarPanel(
               
               h4("Ovde možete da pretražite podatke o medijskim projektima koje su po Zakonu o javnom informisanju i medijima 
                        
                        podržale",strong("lokalne samouprave u Srbiji"), "od početka 2015. do 2021. godine.",align="justify"),
               
               h4( "Klikom na", strong("“Sve opštine”"), "dobijate tabelu sa informacijama koje su četiri opštine dodelile najviše sredstava,
                         
                         grafički prikaz dodele sredstava po temama (sve lokalne samouprave), četiri podnosioca projekata koji su zbirno 
                         
                         dobili najviše sredstava od svih lokalnih samouprava i tabelu sa svim informacijama vezanim za medijske projekte
                         
                         na nivou lokalnih samouprava.", align = "justify"),
               
               h4("Biranjem pojedinačnih opština dobićete podatke koliko je ukupno dala novca izabrana lokalna samouprava, grafikon
                        
                        učešća dodele sredstava po temama za taj grad ili opštinu, trend ukupno dodeljenih sredstava po godinama i veliku
                        
                        tabelu sa svim informacijama u vezi sa izabranom lokalnom samoupravom.", align = "justify"),
               
               selectizeInput("opstina", "Lokalna samouprava:", options = list(placeholder = "Ukucajte opštinu ili Sve opštine"), c("Sve opštine",
                                                                  
                                                                  sort(unique(tabelaopstinee$`ORGAN KOJI RASPISUJE KONKURS/OPŠTINA`)))),
               
               actionBttn(inputId = "go3", label = "Pretraži", icon("search"), style = "jelly", size = "sm", color = "danger"),
               
               helpText("Klikni na Pretraži da biste videli tabele i grafikone"),
               
               tableOutput("top4opstine"),
               
               plotlyOutput("barchartopstinepodnosioci"),
               
               plotlyOutput("chartopstinetemeilinechart")
               
               
             ), 
             
             mainPanel ( dataTableOutput("tabelaopstina"))),
    
    #Uredjivanje taba Podaci
    
    tabPanel("Podaci",
             
             mainPanel(
               
               h4("Ovde možete da skinete podatke prikupljene tokom trajanja ovog projekta u csv ili excel formatu.", align = "justify"),
               
               # ubacivanje tastera za skidanje podataka u csv i excel formatu
               
               downloadBttn (outputId = "downloaddatabase", label = "Preuzmi.CSV", style = "gradient", size = "sm"), 
               
               downloadBttn ("downloaddatabase1", "Preuzmi.XLSX", style = "gradient", size = "sm"),
               
               h4("Ovde možete da skinete metodologiju sa detaljima kako su prikupljeni podaci i publikaciju na srpskom i engleskom jeziku iz decembra 2021. godine." , align = "justify"),
               
               # ubacivanje tastera za skidanje metodologije i analize
               
               downloadBttn ("downloaddatadict", "Metodologija", style = "gradient", size = "sm", color = "royal"),
               
               downloadBttn ("downloadanalysis", "Publikacija 2021", style = "gradient", size = "sm", color = "royal"),
               
               downloadBttn ("downloadanalysiseng", "Publication 2021", style = "gradient", size = "sm", color = "royal"),
               
               h4(strong("Online verzija analize"), " dostupna je", a(" ovde.", href = "https://projektnosufinansiranjehtmlpublikacija.netlify.app/"), 
                        " Sva rešenja koja su dobijena od lokalnih samouprava
                        
                        možete da pogledate na sledećem ", a("linku.", href = "https://docs.google.com/spreadsheets/d/1ajgnqWStLHUQ8XUA1LU0n5_1KdqcAIef1A4e4391ORI/edit#gid=0"), align = "justify"), 
               
               h4( " Kod za kreiranje same aplikacije nalazi se na sledećoj ",
                   
                   a("veb stranici.", href = "https://github.com/Centarzaodrzivezajednice/Projektno-sufinansiranje-medija"), "Sam kod i podaci mogu da se preuzmu 
                           
                           pod licencom", a("Creative Commons Zero v1.0 Universal.", href="https://creativecommons.org/publicdomain/zero/1.0/", align = "justify"), 
                   
                   " Iako je ovo najotvorenija moguća licenca, zaista bismo voleli da, ako budete koristili podatke ili kod za veb aplikaciju da nas o tome obavestite
                        
                        kako bi mogli da pratimo njenu dalju upotrebu i razvoj i za to vam se unapred zahvaljujemo. ", align = "justify"), 
               
               h4("Zbog kompleksnosti istraživanja i prikupljanja velike količine podataka, ukoliko imate neke ispravke
                                  
                                     ili sugestije pišite na sledeću", a("email adresu.", href = "mailto:medijskikonkursi@gmail.com"), align = "justify"))
    )
  )
