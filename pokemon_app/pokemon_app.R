library(dplyr)
library(ggplot2)
library(tidyr)
library(readr)
library(stringr)
library(tibble)
library(shiny)
library(fmsb)
library(ggdist)
library(ggfittext)
library(ggchicklet)


poke_data <- read_csv("data/pokemon.csv")


ui <- fluidPage(
  
  theme = bslib::bs_theme(
     bg = "#FBFBF7", 
     fg = c("#127B9E"), 
     base_font = bslib::font_google("VT323", local = TRUE),
     heading_font	= bslib::font_google("Unna", local = TRUE),
     font_scale = 1.2),
  
  tabsetPanel(id = "options",
              tabPanel("Pokémon stats",
                       titlePanel("Pokémon stats"),
                       selectInput("poke1", "Pokémon 1", choices = poke_data$name),
                       selectInput("poke2", "Pokémon 2", choices = c("",poke_data$name), selected = ""),
                       selectInput("poke3", "Pokémon 3", choices = c("",poke_data$name), selected = ""),
                       tableOutput("tbl"),
                       plotOutput("radar_plot")
                       ),
              tabPanel("Type vs Stats",
                       titlePanel("Stats by type"),
                       selectInput("type1", "Pokémon type", choices = c("All", poke_data$type1)),
                       selectInput("type2", "Pokémon type", choices = c("", poke_data$type1), selected = ""),
                       selectInput("type3", "Pokémon type", choices = c("", poke_data$type1), selected = ""),
                       selectInput("stat", "Stat", choices = c("Attack", "Defense", "Sp. Attack", "Sp. Defense", "Speed", "HP"), selected = "Attack"),
                       tableOutput("tbl_type_stat"),
                       fluidRow(column(6, plotOutput("type_stat_plot")))
                       ),
              tabPanel("Type effectiveness",
                       titlePanel("Type effectiveness"),
                       selectInput("pokemon", "Pokémon", choices = c("",poke_data$name), selected = ""),
                       plotOutput("effectiveness")
                       ),
              tabPanel("Pokémon by type",
                       titlePanel("Pokémon by type"),
                       radioButtons("is_legend", "Pokémon", choices = c("All", "Legendary")),
                       plotOutput("by_type")
                       ),
              tabPanel("Height & Weight",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput("value", "Show", choices = c("weight", "height")),
                           tabsetPanel(
                             id = "switch",
                             type = "hidden",
                             tabPanelBody("weight", "Pokémon weight"),
                             tabPanelBody("height", "Pokémon height")
                           )),
                         mainPanel(
                           plotOutput("hist")
                            )
                           )
                         )
                       
              )
)


server <- function(input, output) {
  
  #bslib::bs_themer()
  
  cols = c("normal"="#A19B64","fighting"="#C43131","flying"="#C09EF0","poison"="#A3239D", 
           "ground"="#E6CD71","rock"="#B39944","bug"="#AECF53","ghost"="#695487","steel"="#C2BEBE",
           "fire"="#E37E0C","water"="#4E94D9","grass"="#26AD2D","electric"="#FFE205",
           "psychic"="#D97781FC","ice"="#B0FCFB","dragon"="#2E0DB3","dark"="#423434","fairy"="#F5C4D8", 
           "none" = "#2B2929","fight"="#C43131")

    # Radar plot ----
  radar_tbl <- eventReactive(list(input$poke1, input$poke2, input$poke3), {
    req(input$poke1)
    
    df <- data.frame(rbind(rep(250, 6), rep(0, 6)))
    colnames(df) <- c("sp_attack", "sp_defense", "speed", "attack", "defense", "hp")
    rownames(df) <- c("Max", "Min")
    
    poke_data %>% 
      select(name, sp_attack, sp_defense, speed, attack, defense, hp) %>% 
      filter(name %in% c(input$poke1, input$poke2, input$poke3)) %>% 
      column_to_rownames(var="name")  %>% 
      bind_rows(df, .) %>% 
      rename("Sp. Attack" = sp_attack, "Sp. Defence" = sp_defense, Speed = speed, 
             Attack = attack, Defense = defense, HP = hp)
    
  })
  
  output$tbl <- renderTable({
    radar_tbl() %>% 
      rownames_to_column("name") %>% 
      filter(name != "Max" & name != "Min") %>% 
      rename(Pokemon = name)
  })
  
  output$radar_plot <- renderPlot({
    op <- par(mar=c(2, 1, 1, 1), xpd=TRUE)
    color = c("#00AFBB", "#E7B800", "#FC4E07")
    radarchart(radar_tbl(), axistype = 1, seg = 5, caxislabels = c(0, 50, 100, 150, 200, 250),
               pcol = color, pfcol = scales::alpha(color, 0.1), plwd = 2, plty = 1, 
               centerzero = TRUE,
               # Customize the grid
               cglcol = "grey", cglty = 1, cglwd = 0.8,
               # Customize the axis
               axislabcol = "grey")
    legend(
      x = "bottom", legend = rownames(radar_tbl()[-c(1,2),]), horiz = TRUE,
      bty = "n", pch = 20 , col = c("#00AFBB", "#E7B800", "#FC4E07"),
      text.col = "black", cex = 1, pt.cex = 1.5, inset=c(0, -0.1)
    )
    par(op)
  }, res = 96)
  
  # Type vs stat ----
  selected <- reactive({
    if (input$type1 != "All") {
      poke_data %>% 
        select(name, type1, sp_attack, sp_defense, speed, attack, defense, hp) %>% 
        filter(type1 == input$type1 | type1 == input$type2 | type1 == input$type3)  %>% 
        rename("Sp. Attack" = sp_attack, "Sp. Defense" = sp_defense, Speed = speed, 
               Attack = attack, Defense = defense, HP = hp)
    } else {
      poke_data %>% 
        select(name, type1, sp_attack, sp_defense, speed, attack, defense, hp) %>% 
        rename("Sp. Attack" = sp_attack, "Sp. Defense" = sp_defense, Speed = speed, 
               Attack = attack, Defense = defense, HP = hp)
    }
  })
  
  #output$tbl_type_stat <- renderTable(head(selected()))
  
  output$type_stat_plot <- renderPlot({
    ggplot(selected(), aes(x = type1, y = .data[[input$stat]], fill = type1, colour = type1)) + theme_bw() +
      ggdist::stat_halfeye(adjust = .5, width = .9, .width = 0, point_colour = NA, alpha = 0.6) + 
      ggdist::stat_dots(side = "left", dotsize = .6, justification = 1.05, binwidth = 1) +
      theme(legend.position = "none") + labs(x = "Pokemon type") +
      coord_flip() + scale_fill_manual(values = cols) + scale_color_manual(values = cols)
    }, res = 96, height = 500)

  
  # Type effectiveness ----
  status_tbl <- eventReactive(input$pokemon, {
    req(input$pokemon)
    status_fixed <- c("Damaged normally by:", "Resistant to:", "Weak to:", "Immune to:")
    
    poke_data %>% 
      filter(name == input$pokemon) %>% 
      select(name, starts_with("type"), starts_with("against")) %>% 
      pivot_longer(cols = starts_with("against"), values_to = "damaged", names_to = "type") %>% 
      mutate(type = gsub("against_","", type)) %>%  
      mutate(status = case_when(
        damaged == 1 ~ "Damaged normally by:",
        damaged == 2 | damaged == 4 ~ "Weak to:",
        damaged == 0 ~ "Immune to:",
        damaged == 0.5 | damaged == 0.25 ~ "Resistant to:"
      )) %>% 
      complete(name, status = status_fixed, fill = list(type = "none", damaged = 0)) %>% 
      group_by(name, status) %>% 
      summarise(damaged = paste0(type, collapse = ", ")) %>%
      ungroup() %>% 
      select(Pokemon = name, Status = status, Damaged = damaged) %>%
      separate(Damaged, into = paste0('', 1:16), sep = ", ") %>% 
      select_if(~!all(is.na(.))) %>% 
      pivot_longer(cols = -c(Pokemon, Status), values_to = "type", names_to = "cols") %>% 
      filter(!is.na(type))
  })
  
 
 output$effectiveness <- renderPlot({
   status_tbl() %>% 
     ggplot(aes(x = as.numeric(cols), y = Status, fill = type, label = type)) + 
     geom_tile(aes(height = .25), colour = "#B9D2FA", size = 2, linejoin = "round") +
     geom_fit_text(reflow = TRUE, show.legend = FALSE, color = "white") +
     labs(title = "Type effectiveness", x = "", y = "") +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#B9D2FA"),
           plot.background = element_rect(fill = "#B9D2FA", linetype = "solid", colour = "#127B9E", size = 2),
           line = element_line(lineend = "round"),
           axis.ticks = element_blank(),
           axis.text.x = element_blank(),
           axis.text.y = element_text(face = "bold", size = 12.5),
           plot.title = element_text(colour = "black", size = 20, face = "bold", 
                                     vjust = -1, hjust = 0),
           legend.position = "none") +
    ggchicklet:::geom_rrect(aes(xmax = -3, ymax = 3.5, xmin = 20, ymin = 4.5, alpha = 0.0), 
                            alpha = 0.01, fill = "#9FBBF5", r = unit(0.01, 'npc')) +
    ggchicklet:::geom_rrect(aes(xmax = -3, ymax = 1.5, xmin = 20, ymin = 2.5, alpha = 0.0), 
                            alpha = 0.01, fill = "#9FBBF5", r = unit(0.01, 'npc')) +
    coord_cartesian(xlim = c(1,18), clip = "off") +
     scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
     scale_fill_manual(values = cols)
 }, height = 600, width = 1000)
 
 # Pokemon by type ----
 
 poke_data_type <- reactive({
   if (input$is_legend != "All") {
     poke_data %>% 
       filter(is_legendary == 1)
   } else {
       poke_data
     }
   })
 
 poke_by_type1 <- reactive({
   poke_data_type() %>% 
   select(name, type1) %>% 
   group_by(type1) %>% 
   count() %>% 
   rename(n1 = n, type = type1)
   })
 
 poke_by_type2 <- reactive({
   poke_data_type() %>% 
   select(name, type2) %>% 
   group_by(type2) %>% 
   count() %>% 
   filter(!is.na(type2)) %>% 
   rename(n2 = n, type = type2)
 })
 
 poke_by_type <- reactive({
   poke_by_type1() %>% 
   full_join(., poke_by_type2()) %>% 
   select(type_name = type, first = n1, second = n2) %>% 
   pivot_longer(cols = c(first, second), values_to = "count", names_to = "type")
 })

 output$by_type <- renderPlot({
   poke_by_type() %>% 
     ggplot(aes(x = count, y = reorder(type_name, count))) + 
     geom_line(aes(group = type_name), color="grey") +
     geom_point(aes(color = type), size = 6) + theme_classic(18) + 
     labs(x = "Number of Pokémon", y = "", title = "Number of Pokémon by type") +
     theme(panel.grid = element_blank(),
           panel.background = element_rect(fill = "#B9D2FA"),
           plot.background = element_rect(fill = "#B9D2FA", linetype = "solid", colour = "#127B9E", size = 2),
           line = element_line(lineend = "round"),
           axis.text.y = element_text(face = "bold", size = 18),
           plot.title = element_text(colour = "black", size = 18, face = "bold"),
           legend.position = c(0.85,0.2),
           legend.direction = "vertical",
           legend.title = element_blank()) +
     scale_color_manual(values = c("#1985B0", "#F2961D"))
 }, height = 600, width = 1000) 
 
 # Pokemon weight and height ----
 
 observeEvent(input$value, {
   updateTabsetPanel(inputId = "switch", selected = input$value)
 })
 
 plot <- reactive({
   switch(input$value,
          weight = poke_data %>% 
            ggplot(aes(x = log(weight_kg), label = name, fill = type1, 
                       alpha = 0.1)) + 
            geom_histogram() + 
            ggrepel::geom_label_repel(data = poke_data %>% 
                                        filter(weight_kg == max(weight_kg, na.rm = TRUE)),
                                      aes(x = max(log(weight_kg), na.rm = TRUE), y = 0),
                                      min.segment.length = 0) +
            ggrepel::geom_label_repel(data = poke_data %>% 
                                        filter(weight_kg == min(weight_kg, na.rm = TRUE)),
                                      aes(x = min(log(weight_kg), na.rm = TRUE), y = 0),
                                      min.segment.length = 0) +
            labs(x = "Log weight", y = "count", title = "Pokémon by weight") +
            theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = "#B9D2FA"),
              plot.background = element_rect(fill = "#B9D2FA", linetype = "solid", colour = "#127B9E", size = 2),
              line = element_line(lineend = "round"),
              axis.ticks = element_blank(),
              axis.text.y = element_text(face = "bold", size = 12.5),
              axis.text.x = element_text(face = "bold", size = 12.5),
              plot.title = element_text(colour = "black", size = 20, face = "bold"),
              legend.position = "none") +
            scale_fill_manual(values = cols),
          height = poke_data %>% 
            ggplot(aes(x = log(height_m), label = name, fill = type1, 
                       alpha = 0.1)) + 
            geom_histogram() + 
            ggrepel::geom_label_repel(data = poke_data %>% 
                                        filter(height_m == max(height_m, na.rm = TRUE)),
                                      aes(x = max(log(height_m), na.rm = TRUE), y = 0),
                                      min.segment.length = 0) +
            ggrepel::geom_label_repel(data = poke_data %>% 
                                        filter(height_m == min(height_m, na.rm = TRUE)),
                                      aes(x = min(log(height_m), na.rm = TRUE), y = 0),
                                      min.segment.length = 0) +
            labs(x = "Log height", y = "count", title = "Pokémon by height") +
            theme(panel.grid.minor = element_blank(),
              panel.grid.major.x = element_blank(),
              panel.background = element_rect(fill = "#B9D2FA"),
              plot.background = element_rect(fill = "#B9D2FA", linetype = "solid", colour = "#127B9E", size = 2),
              line = element_line(lineend = "round"),
              axis.ticks = element_blank(),
              axis.text.y = element_text(face = "bold", size = 12.5),
              axis.text.x = element_text(face = "bold", size = 12.5),
              plot.title = element_text(colour = "black", size = 20, face = "bold"),
              legend.position = "none") +
            scale_fill_manual(values = cols))
   })
 
 output$hist <- renderPlot(plot(), height = 500, width = 700)

 
}


shinyApp(ui = ui, server = server)
