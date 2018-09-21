
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(ggptt)
library(scales)
library(shinyWidgets)
library(lubridate)
library(statfitools)


set_ptt(base_size = 18)
default_options <- options()

# Functions

year_q_label <- function(x){
  gsub("\\.", " Q", lubridate::quarter(x,with_year = TRUE))
}

# Työnvälitystilaston data, siistitty scriptissä "Datan siistintä.R"
    data_kunnat <- readRDS("data/kuntadata.rds")   
    data_maakunnat <- readRDS("data/maakuntadata.rds")
    data_kokomaa <- readRDS("data/kokomaadata.rds")
   #data <- readRDS("data/työllisyysdata.rds")

# Karttapohjadata ja kuntakoodidata karttaa varten
    df_kunnat <- readRDS("data/spdf.rds")
    reg_keytable <- readRDS("data/reg_keytable.rds")
    kuntakoodit <- select(reg_keytable, Knro, Kunta) %>%
      mutate(Kunta = as.character(Kunta))

   # Hitaampi tapa ladata karttapohjadata
   #load("data/kunnat_2016_milj.RData")     # loads with name "sp"
   #  df_kunnat <- sp2df(sp)

   # Yhdistä Luvia Eurajokeen ja Juankoski Kuopioon, muuttamalla Luvian ja Juankosken kuntakoodit
      df_kunnat$NATCODE[df_kunnat$NATCODE == 174] <- 297
      df_kunnat$NATCODE[df_kunnat$NATCODE == 442] <- 151

   # hyödyllisiä vektoreita
      period <- unique(data_kunnat$time)
      maakunnat <- sort(as.character(unique(data_kunnat$Maakunta)))
      greys <- c("grey90", "grey80", "grey70", "grey60", "grey50", "grey40", "grey30", "grey20")
      viivapaksuus = 0.8

# Define UI for application that draws a histogram

ui <- fluidPage(
  
  titlePanel("Alueelliset työmarkkinat"),
  
  # Period
  p(paste0(year_q_label(min(data_kunnat$time)), " - ",
           year_q_label(max(data_kunnat$time)))),
  
  sidebarLayout(
    position = "left",
    sidebarPanel(#checkboxInput("kokomaa", "Koko maa", value = TRUE),
                 
                 strong("Valitse maakunta"),
                
                 checkboxInput("ahvenanmaa", "Ahvenanmaa", value = TRUE),
                
                 conditionalPanel(
                   condition = "input.ahvenanmaa == true",
                   checkboxGroupInput("kunta_sel_ahvenanmaa", "Valitse kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Ahvenanmaa - Åland"]))),
                 
                 checkboxInput("etelakarjala", "Etelä-Karjala"),
                 
                 conditionalPanel(
                   condition = "input.etelakarjala == true",
                   checkboxGroupInput("kunta_sel_etelakarjala", "Valitse kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Etelä-Karjala"]))),
                 
                 checkboxInput("etelapohjanmaa", "Etelä-Pohjanmaa"),
                 
                 conditionalPanel(
                   condition = "input.etelapohjanmaa == true",
                   checkboxGroupInput("kunta_sel_etelapohjanmaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Etelä-Pohjanmaa"]))),
                 
                 checkboxInput("etelasavo", "Etelä-Savo"),
                 
                 conditionalPanel(
                   condition = "input.etelasavo == true",
                   checkboxGroupInput("kunta_sel_etelasavo", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Etelä-Savo"]))),
                 
                 checkboxInput("kainuu", "Kainuu"),
                 
                 conditionalPanel(
                   condition = "input.kainuu == true",
                   checkboxGroupInput("kunta_sel_kainuu", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Kainuu"]))),
                 
                 checkboxInput("kantahame", "Kanta-Häme"),
                 
                 conditionalPanel(
                   condition = "input.kantahame == true",
                   checkboxGroupInput("kunta_sel_kantahame", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Kanta-Häme"]))),
                 
                 checkboxInput("keskipohjanmaa", "Keski-Pohjanmaa"),
                 
                 conditionalPanel(
                   condition = "input.keskipohjanmaa == true",
                   checkboxGroupInput("kunta_sel_keskipohjanmaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Keski-Pohjanmaa"]))),
                 
                 checkboxInput("keskisuomi", "Keski-Suomi"),
                 
                 conditionalPanel(
                   condition = "input.keskisuomi == true",
                   checkboxGroupInput("kunta_sel_keskisuomi", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Keski-Suomi"]))),
                 
                 checkboxInput("kymenlaakso", "Kymenlaakso"),
                 
                 conditionalPanel(
                   condition = "input.kymenlaakso == true",
                   checkboxGroupInput("kunta_sel_kymenlaakso", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Kymenlaakso"]))),
                 
                 checkboxInput("lappi", "Lappi"),
                 
                 conditionalPanel(
                   condition = "input.lappi == true",
                   checkboxGroupInput("kunta_sel_lappi", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Lappi"]))),
            
                 checkboxInput("pirkanmaa", "Pirkanmaa"),
                 
                 conditionalPanel(
                   condition = "input.pirkanmaa == true",
                   checkboxGroupInput("kunta_sel_pirkanmaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Pirkanmaa"]))),
                 
                 checkboxInput("pohjanmaa", "Pohjanmaa"),
                 
                 conditionalPanel(
                   condition = "input.pohjanmaa == true",
                   checkboxGroupInput("kunta_sel_pohjanmaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Pohjanmaa"]))),
                 
                 checkboxInput("pohjoiskarjala", "Pohjois-Karjala"),
                 
                 conditionalPanel(
                   condition = "input.pohjoiskarjala == true",
                   checkboxGroupInput("kunta_sel_pohjoiskarjala", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Pohjois-Karjala"]))),
                 
                 checkboxInput("pohjoispohjanmaa", "Pohjois-Pohjanmaa"),
                 
                 conditionalPanel(
                   condition = "input.pohjoispohjanmaa == true",
                   checkboxGroupInput("kunta_sel_pohjoispohjanmaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Pohjois-Pohjanmaa"]))),
                 
                 checkboxInput("pohjoissavo", "Pohjois-Savo"),
                 
                 conditionalPanel(
                   condition = "input.pohjoissavo == true",
                   checkboxGroupInput("kunta_sel_pohjoissavo", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Pohjois-Savo"]))),
                 
                 checkboxInput("paijathame", "Päijät-Häme"),
                 
                 conditionalPanel(
                   condition = "input.paijathame == true",
                   checkboxGroupInput("kunta_sel_paijathame", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Päijät-Häme"]))),
                 
                 checkboxInput("satakunta", "Satakunta"),
                 
                 conditionalPanel(
                   condition = "input.satakunta == true",
                   checkboxGroupInput("kunta_sel_satakunta", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Satakunta"]))),
                 
                 checkboxInput("uusimaa", "Uusimaa"),
                 
                 conditionalPanel(
                   condition = "input.uusimaa == true",
                   checkboxGroupInput("kunta_sel_uusimaa", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Uusimaa"]))),
                 
                 checkboxInput("varsinaissuomi", "Varsinais-Suomi"),
                 
                 conditionalPanel(
                   condition = "input.varsinaissuomi == true",
                   checkboxGroupInput("kunta_sel_varsinaissuomi", "Valitse Kunta",
                                      choices = unique(data_kunnat$alue[data_kunnat$Maakunta == "Varsinais-Suomi"]))),
                 

           
                 p("Kuntien työllisyys ja työttömyystiedot Työ- ja elinkeinoministeriön työnvälitystilastosta. Kausitasoitettu trendi laskettu PTT:ssä
                   (X-13-ARIMA-SEATS)")
                # p("Työvoimatutkimuksen otokset kunnissa ovat varsin pieniä,
                 #  joten varsinkin pienten kuntien tietoihin sisältyy paljon
                  # satunnaisvaihtelua")
                ),

    mainPanel(
      tabsetPanel(
        tabPanel("Taso",
                 selectInput("adjustment", "Valitse aikasarjatyyppi",
                                    choices = c("Alkuperäinen", "Kausitasoitettu", "Trenditasoitettu"),
                                    selected = "Trenditasoitettu"),
                 
                 plotOutput("unemployment_rate"),
                 checkboxInput("kokomaa", "Näytä koko maan työttömyysaste", value = FALSE),
                 plotOutput("unemployment_plot"),
                 checkboxInput("org", "Näytä alkuperäinen työttömyyssarja", value = FALSE)
                 
        ),
        tabPanel("Muutos",
                 sliderTextInput("change_period", "Valitse aikaväli",
                      choices = period,
                      selected = c(max(period) - lubridate::years(1), max(period))
                 ),
                 plotOutput("change_tyott_aste"),
                 checkboxInput("kokomaa2", "Näytä koko maan työttömyysasteen muutos", value = FALSE),
                 plotOutput("map"))
      )
    )
    )
  )

# Define server logic required to draw a histogram

server <- function(input, output, session) {
  
  # Tallenna kaikki valitut kunnat vektoriin kunta_sel, inputit input$kunta_sel_* ovat vektoreita, joissa on kuntia
  kunta_sel <- reactive({
    
    kunta_sel <-  c(input$kunta_sel_ahvenanmaa, input$kunta_sel_etelakarjala, input$kunta_sel_etelapohjanmaa, 
                    input$kunta_sel_etelasavo, input$kunta_sel_kainuu, input$kunta_sel_kantahame, 
                    input$kunta_sel_keskipohjanmaa, input$kunta_sel_keskisuomi,input$kunta_sel_kymenlaakso, 
                    input$kunta_sel_lappi, input$kunta_sel_pirkanmaa, input$kunta_sel_pohjanmaa, 
                    input$kunta_sel_pohjoiskarjala, input$kunta_sel_pohjoispohjanmaa,
                    input$kunta_sel_pohjoissavo, input$kunta_sel_paijathame, input$kunta_sel_satakunta,
                    input$kunta_sel_uusimaa, input$kunta_sel_varsinaissuomi)
    kunta_sel <- kunta_sel[!is.null(kunta_sel)]
    kunta_sel
  })
    
  # Tallenna kaikki valitut maakunnat vektoriin maakunta_sel, input values input$maakunta are truth values
  maakunta_sel <- reactive({
  
    maakunta_sel <- c(input$ahvenanmaa, input$etelakarjala, input$etelapohjanmaa,
                      input$etelasavo, input$kainuu,
                      input$kantahame, input$keskipohjanmaa, input$keskisuomi,
                      input$kymenlaakso, input$lappi, input$pirkanmaa,
                      input$pohjanmaa, input$pohjoiskarjala, input$pohjoispohjanmaa,
                      input$pohjoissavo, input$paijathame, input$satakunta,
                      input$uusimaa, input$varsinaissuomi)
    maakunta_sel <- maakunnat[maakunta_sel]
    maakunta_sel
  })  
  
  adjustment <- reactive({
    
    adjustment <- ifelse(input$adjustment == "Trenditasoitettu", "_trend", 
                         ifelse(input$adjustment == "Alkuperäinen", "", "_sa"))
    adjustment
  })
    
  # Tämä poistaa kuntavalinnan, mikäli käyttäjä poistaa kyseisen kunnan maakuntavalinnan. 
     observe({ 
       
       for(mk in maakunnat) {
         
          if(!(mk %in% maakunta_sel())) {
            if(mk == "Ahvenanmaa - Åland") {
              updateCheckboxGroupInput(session, paste("kunta_sel", 
                                                      tolower(unlist(strsplit("Ahvenanmaa - Åland", "-"))[1]), 
                                                      sep = "_"), 
                                       choices = unique(data_kunnat$alue[data_kunnat$Maakunta == mk]), 
                                       selected = NULL)
              updateCheckboxGroupInput(session, paste("kunta_sel", 
                                                      gsub("ö","o",gsub("ä","a",tolower(paste(unlist(strsplit(mk, "-")), sep = "", collapse = "")))), 
                                                      sep = "_"), 
                                       choices = unique(data_kunnat$alue[data_kunnat$Maakunta == mk]), 
                                       selected = NULL)
            } else {
          updateCheckboxGroupInput(session, paste("kunta_sel", 
                                                  gsub("ö","o",gsub("ä","a",tolower(paste(unlist(strsplit(mk, "-")), sep = "", collapse = "")))),
                                                  sep = "_"), 
                                 choices = unique(data_kunnat$alue[data_kunnat$Maakunta == mk]), 
                                 selected = NULL)
            }
         }
       }
     })
  
 ########################################################################################################
     # Taso tab
  
  output$unemployment_plot <- renderPlot({
    
    set_ptt(base_size = 18)
    
    # if(is.null(input$alue_sel)) {
    #   alue_sel <- "Koko maa"
    #   colour_values = "grey20"
    # } else {
    #   kunta_sel <-  input$kunta_sel
    #   colour_values = ptt_pal(length(alue_sel))
    # }
    
    kunta_sel <- kunta_sel()
    maakunta_sel <- maakunta_sel()
    
    if(input$org) {
      tyottomyysluku <- c("Tyottomat", paste("Tyottomat", adjustment(), sep = ""))
    } else {
      tyottomyysluku <- paste("Tyottomat", adjustment(), sep = "")
    }
    
    #kokomaa_sel <- input$kokomaa
    
    # if(input$kokomaa) {
    #   data_kokomaa %>% filter(value_type == "Työttömät_trend") %>%
    #     ggplot(aes(x = time, y = value, group = alue, col = value_type)) +
    #     geom_line(alpha = 0.5) + 
    #     ggtitle("Työttömät, koko maa") +
    #     ylab("Työttömiä, trendi") +
    #     xlab(NULL) +
    #     theme_ptt()
      
   # } else 

    # Piirrä maakuntakuvaaja jos maakunta_sel ei ole tyhjä ja kunta_sel on tyhjä
      if(!is.null(maakunta_sel) & is.null(kunta_sel)) {
      
      data_maakunnat %>% filter(alue %in% maakunta_sel,
                                value_type %in% tyottomyysluku) %>%
          mutate(org = ifelse(grepl(adjustment(),value_type), FALSE, TRUE)) %>%
        ggplot(aes(x = time, y = value/1000, alpha = org, col = alue)) +
        geom_line(size = viivapaksuus) + 
        labs(title = "Työttömät",
             y = paste("1000 henkilöä, ", tolower(input$adjustment), sep = ""),
             x = NULL,
             caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT") +
        theme_ptt() +
        scale_alpha_discrete(range = c(1,0.5), guide = "none") +
        theme(legend.title = element_blank())
      
     # Piirrä kuntakuvaaja jos kunta_sel ei ole tyhjä ja kunkin valitun kunnan maakunta on myös valittu
    } else if (all(kunta_sel %in% unique(data_kunnat$alue[data_kunnat$Maakunta %in% maakunta_sel]))){
      
      data_kunnat %>% filter(alue %in% kunta_sel,
                             value_type %in% tyottomyysluku) %>%
        mutate(org = ifelse(grepl(adjustment(),value_type), FALSE, TRUE)) %>%
        ggplot(aes(x = time, y = value/1000,alpha = org, col = alue)) +
        geom_line(size = viivapaksuus) +
        labs(title = "Työttömät",
             y = paste("1000 henkilöä, ", tolower(input$adjustment), sep = ""),
             x = NULL,
             caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT") +
        theme_ptt() +
        scale_alpha_discrete(range = c(1,0.5), guide = "none") +
        theme(legend.title = element_blank())
    }
    
  })

  output$unemployment_rate <- renderPlot({
    
    set_ptt(base_size = 18)
    
    kunta_sel <- kunta_sel()
    maakunta_sel <- maakunta_sel()
    

      tyottomyysluku <- paste("Tyottomyysaste", adjustment(), sep = "")
    
    
    #kokomaa_sel <- input$kokomaa
    
    # if(input$kokomaa) {
    #   data_kokomaa %>% filter(value_type == "Työttömyysaste_trend") %>%
    #     ggplot(aes(x = time, y = value, col = value_type)) +
    #     geom_line(alpha = 0.5) + 
    #     ggtitle("Työttömyysaste, koko maa") +
    #     ylab("%, trend") +
    #     scale_y_continuous(labels = percent_comma) + 
    #     xlab(NULL) +
    #     theme_ptt()
      
    # } else 
      


    
    # Piirrä maakuntakuvaaja jos maakunta_sel ei ole tyhjä ja kunta_sel on tyhjä
      if(!is.null(maakunta_sel) & is.null(kunta_sel)) {
        
        if(input$kokomaa) {
          maakunta_sel <- c("Koko maa", maakunta_sel)
          data_maakunnat <- rbind(data_maakunnat, data_kokomaa)
          
          colours = na.omit(c("black", ptt_pal(length(maakunta_sel[-1])), "grey90"))
        } else {
          colours = na.omit(c(ptt_pal(length(maakunta_sel)), "grey90"))
        }
      
          data_maakunnat %>% 
          mutate(alue2 = fct_other(alue, keep = maakunta_sel,
                            other_level = "muut maakunnat"),
                 alue2 = fct_relevel(alue2, maakunta_sel),
                 alue = fct_relevel(alue, maakunta_sel, after = Inf)) %>%
                         filter(value_type %in% tyottomyysluku) %>%
        ggplot(aes(x = time, y = 100*value, group = alue,  col = alue2)) +
        scale_color_manual(values = colours) + #na.omit(c(ptt_pal(length(maakunta_sel)), "grey90"))) +
        geom_line(size = viivapaksuus) + 
        labs(title = "Työttömyysaste",
             y = paste("%, ", tolower(input$adjustment), sep = ""),
             x = NULL,
             caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus,  PTT") +
        scale_y_continuous(labels = deci_comma) +
        theme_ptt() +
        theme(legend.title = element_blank())
   
        # Piirrä kuntakuvaaja jos kunta_sel ei ole tyhjä ja kunkin valitun kunnan maakunta on myös valittu         
    } else if (all(kunta_sel %in% unique(data_kunnat$alue[data_kunnat$Maakunta %in% maakunta_sel]))){
      
      if(input$kokomaa) {
        kunta_sel <- c("Koko maa", kunta_sel)
        maakunta_sel <- c("Koko maa", maakunta_sel)
        data_kunnat <- rbind(data_kunnat, mutate(data_kokomaa, Maakunta = "Koko maa"))
        colours = na.omit(c("black", ptt_pal(length(kunta_sel[-1])), greys[1:length(maakunta_sel[-1])]))
      } else {
        colours = na.omit(c(ptt_pal(length(kunta_sel)), greys[1:length(maakunta_sel[-1])]))
      }
      
     
      
        data_kunnat %>% filter(Maakunta %in% maakunta_sel) %>%            # tämän filtterin poistamlla kaikki Suomen kunnat 
        mutate(alue2 = fct_other(alue, keep = kunta_sel,               # näkyvät taustalla, set geom_line(size < 0.5)
                                 other_level = paste("muut kunnat, ", maakunta_sel[-1], sep = "")),
               alue2 = fct_relevel(alue2, kunta_sel),
               alue = fct_relevel(alue, kunta_sel, after = Inf)) %>%
                      filter(value_type %in% tyottomyysluku) %>%
        ggplot(aes(x = time, y = 100*value, group = alue, col = alue2)) +
        scale_color_manual(values = colours) + #na.omit(c(ptt_pal(length(kunta_sel)), greys[1:length(maakunta_sel)]))) + 
        geom_line(size = viivapaksuus) +
        labs(title = "Työttömyysaste",
             y = paste("%, ", tolower(input$adjustment), sep = ""),
             x = NULL,
             caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus,  PTT") +
        scale_y_continuous(labels = deci_comma) +
        theme_ptt() +
        theme(legend.title = element_blank())
    }
  })
  
  #############################################################################################
  # Muutos tab

   output$change_tyott_aste <- renderPlot({
     
     kunta_sel <- kunta_sel()
     maakunta_sel <- maakunta_sel()
     set_ptt(base_size = 18)
     
    if(!is.null(maakunta_sel) & is.null(kunta_sel)) {
      
      if(input$kokomaa2) {
        data_maakunnat <- rbind(data_maakunnat, data_kokomaa)
      }
     
       data_maakunnat %>%
       group_by(alue) %>%
          #select(-Työvoima) %>%
        filter(value_type == "Tyottomyysaste_trend") %>%
        spread(value_type, value) %>%
        summarise(t0 = Tyottomyysaste_trend[time == as.Date(input$change_period[1])],
                  t1 = Tyottomyysaste_trend[time == as.Date(input$change_period[2])]) %>%
        ungroup() %>%
        #mutate(alue2 = fct_other(alue, keep = input$maakunta_sel,
                            #    other_level = "muut")) %>%
        mutate(alue = fct_reorder(alue, t1)) %>%
        mutate(direction = factor(t1 > t0, levels = c(FALSE, TRUE))) %>%
        ggplot(aes(x = t0, xend = t1, group = alue, y = alue, colour = direction)) +
        ggalt::geom_dumbbell(colour_x ="#a3c4dc", size=2, colour_xend="#0e668b",
                             show.legend = TRUE) +
        # Hack to to add point colour guide using line colour scale
        scale_colour_manual(values = c("#46ad39", "#dd5a5e"),     # colour to line
                            labels = c(year_q_label(input$change_period[1]),
                                       year_q_label(input$change_period[2])), #  labels to points
                            guide = guide_legend(override.aes = list(colour = c("#a3c4dc", "#0e668b"))),
                            drop = FALSE) +  # colour to guide points
        scale_x_continuous(labels = percent_comma) +
        labs(title = "Työttömyysasteen muutos, trendi",
             x = NULL,  caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT") +
        the_title_blank(c("y", "l")) +
         theme(plot.title = element_text(size = 15), plot.caption = element_text(size = 10))
  
  
     } else if (all(kunta_sel %in% unique(data_kunnat$alue[data_kunnat$Maakunta %in% maakunta_sel]))){
       
       if(input$kokomaa2) {
         maakunta_sel <- c("Koko maa", maakunta_sel)
         data_kunnat <- rbind(data_kunnat, mutate(data_kokomaa, Maakunta = "Koko maa"))
       }
       
       data_kunnat %>%
         filter(Maakunta %in% maakunta_sel) %>%
         group_by(alue) %>%
         #select(-Työvoima) %>%
         filter(value_type == "Tyottomyysaste_trend") %>%
         filter(!is.na(value)) %>%
         spread(value_type, value) %>%
         summarise(t0 = Tyottomyysaste_trend[time == as.Date(input$change_period[1])],
                   t1 = Tyottomyysaste_trend[time == as.Date(input$change_period[2])]) %>%
         ungroup() %>%
         #mutate(alue2 = fct_other(alue, keep = input$maakunta_sel,
         #    other_level = "muut")) %>%
         mutate(alue = fct_reorder(alue, t1)) %>%
         mutate(direction = factor(t1 > t0, levels = c(FALSE, TRUE))) %>%
         ggplot(aes(x = t0, xend = t1, group = alue, y = alue, colour = direction)) +
         ggalt::geom_dumbbell(colour_x ="#a3c4dc", size=2, colour_xend="#0e668b",
                              show.legend = TRUE) +
         # Hack to to add point colour guide using line colour scale
         scale_colour_manual(values = c("#46ad39", "#dd5a5e"),     # colour to line
                             labels = c(year_q_label(input$change_period[1]),
                                        year_q_label(input$change_period[2])), #  labels to points
                             guide = guide_legend(override.aes = list(colour = c("#a3c4dc", "#0e668b"))),
                             drop = FALSE) +  # colour to guide points
         scale_x_continuous(labels = percent_comma) +
         labs(title = "Työttömyysasteen muutos, trendi",
              x = NULL,  caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT") +
         the_title_blank(c("y", "l")) +
         theme(plot.title = element_text(size = 15), plot.caption = element_text(size = 10))
  
  
     }
     })
   
   output$map <- renderPlot({
     
     kunta_sel <- kunta_sel()
     maakunta_sel <- maakunta_sel()

     df <- data_kunnat %>%
           group_by(alue) %>%
           filter(value_type == "Tyottomyysaste_trend") %>%
           spread(value_type, value) %>%
           summarise(t0 = Tyottomyysaste_trend[time == as.Date(input$change_period[1])], 
                     t1 = Tyottomyysaste_trend[time == as.Date(input$change_period[2])]) %>%   
           ungroup() %>%
           mutate(muutos = t1 - t0,
                  Kunta = alue) %>%
           select(Kunta, muutos)
     
     df <- right_join(df, kuntakoodit, by = "Kunta")
     names(df) <- c("Kunta", "muutos", "NATCODE")
     df <- select(df, muutos, NATCODE)
     
     df_kunnat2 <- df_kunnat %>% left_join(df, df_kunnat, by = "NATCODE") 
     
     df_muutos_posit <- filter(df_kunnat2, muutos >= 0)
     df_muutos_negat <- filter(df_kunnat2, muutos < 0)
     
     valitut_kunnat <- filter(kuntakoodit, kuntakoodit$Kunta %in% kunta_sel) %>%
                       select(Knro) 
                       
     
     # Cowplot-paketti on jännä teemojen kanssa, tämä näyttää toimivan
     
     if("package:cowplot" %in% search()) {detach("package:cowplot")}
     unset_ptt()
     library(cowplot)
     
     
     
     muutosvoitto <- ggplot() +  geom_polygon(data = df_muutos_posit,
                                             aes(x = long, y = lat,fill = muutos, group = group)) +
       scale_fill_gradientn(colours = colorRampPalette(c("red4", "lightcoral"))(100),
                            name = "Muutos, %-yksikköä") +
       coord_fixed(0.9) +
       xlim(0, 8e+05) +
       ylim(6600000,7800000) +
       theme(axis.text = element_blank(),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             axis.title = element_blank(),
             panel.grid.major = element_blank(),
             legend.position = c(0.97,0.6)) +
       labs(caption = "")
     
     muutostappio <- ggplot() + geom_polygon(data = df_muutos_negat, 
                                             aes(x = long, y = lat,fill = muutos, group = group)) +
       scale_fill_gradientn(colours = colorRampPalette(c("darkgreen", "lawngreen"))(100),
                            name = "Muutos, %-yksikköä") +
       coord_fixed(0.9) + 
       xlim(0, 8e+05) +
       ylim(6600000,7800000) +
       theme(axis.text = element_blank(),
             axis.line = element_blank(),
             axis.ticks = element_blank(),
             panel.border = element_blank(),
             panel.grid = element_blank(),
             axis.title = element_blank(),
             panel.grid.major = element_blank(),
             legend.justification = "bottom",
             legend.position = c(1.22,0.20)) +
       labs(caption = "Lähde: Työnvälitystilasto (TEM), Tilastokeskus, PTT")  # Karttalähde?
     
     # Kuntavalinta rajaa kunnan kartasta
     
     Kuntarajaus <- ggplot() + geom_polygon(data = filter(df_kunnat2, NATCODE %in% valitut_kunnat$Knro),
                                                          aes(x = long, y = lat, group = group),fill = NA, color = "black", size = 1) +
                                              coord_fixed(0.9) +
                                              xlim(0, 8e+05) +
                                              ylim(6600000,7800000) +
                                              theme(axis.text = element_blank(),
                                                    axis.line = element_blank(),
                                                    axis.ticks = element_blank(),
                                                    panel.border = element_blank(),
                                                    panel.grid = element_blank(),
                                                    axis.title = element_blank(),
                                                    panel.grid.major = element_blank(),
                                                    legend.position = c(0.97,0.6)) +
                                              labs(caption = "")
     
     koko = 1
     cowplot::ggdraw() + cowplot::draw_plot(muutostappio, x = -0.1, y = 0, scale = koko) + 
                         cowplot::draw_plot(muutosvoitto, x = -0.1, y = 0, scale = koko) + 
                         cowplot::draw_plot(Kuntarajaus, x = -0.1, y = 0, scale = koko)
      
     
     
   })

  
}

# Run the application
shinyApp(ui = ui, server = server)

