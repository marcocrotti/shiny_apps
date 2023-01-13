library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(gt)
library(rphylopic)
library(cowplot)
library(sf)
library(htmltools)


my_data <- read_csv("data/trait_data_imputed.csv")

my_data_clean <- my_data %>% 
  filter(phylacine_binomial != "Not recognised") %>% 
  mutate(species_name = paste(genus, species))

eco_simp <- read_sf("data/Ecoregions_simp.shp")


# UI ----

ui <- fluidPage(
  
  theme = bslib::bs_theme(
    bg = "#FBEEC1", 
    fg = c("#05386B"), 
    base_font = bslib::font_google("Roboto Mono", local = TRUE),
    heading_font	= bslib::font_google("Unna", local = TRUE),
    font_scale = 1.2),
  
  tabsetPanel(
    tabPanel("Introduction",
             includeMarkdown("description_md/intro.md"),
             br(),
             includeMarkdown("description_md/table.md"),
             br(),
             tableOutput("summary_species"),
             br(),br(),br(),
             includeMarkdown("description_md/shapefile.md")
             ),
    tabPanel("General Overview",
             titlePanel("Number of Taxa per Taxonomic Rank"),
             #fluidRow(
             #  div(style='width:1000px;overflow-x: scroll;',
             #  plotOutput("fam_per_order"))),
             plotOutput("fam_per_order"),
             selectInput("order", "Order", choices = c("", sort(unique(my_data_clean$order)))),
             plotOutput("gen_per_fam"),
             selectInput("family", "Family", choices = c("", sort(unique(my_data_clean$family)))),
             fluidRow(
               div(style='overflow-x: scroll;',
                   plotOutput("spec_per_gen"))),
             #plotOutput("spec_per_gen"),
             selectInput("genus", "Genus", choices = c("", sort(unique(my_data_clean$genus)))),
             dataTableOutput("spec_list")
             ),
    tabPanel("Weight & Length",
             sidebarLayout(
               sidebarPanel(
                 selectInput("order2", "Choose Order", choices = c("", sort(unique(my_data_clean$order)))),
                 #selectInput("measure", "Show", choices = c("Weight", "Height")),
                 width = 2
               ),
               mainPanel(
                 plotOutput("w_l_plot"),
                 br(),br(),
                 gt_output("w_l_tbl"),
                 br(),br(),
                 position = "left"
               )
             )),
    tabPanel("Life history",
             titlePanel("Life history traits"),
             selectInput("order3", "Order", choices = c("", sort(unique(my_data_clean$order)))),
             dataTableOutput("reproduction")
             ),
    tabPanel("Diet",
             titlePanel("Feeding ecology"),
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput("order4", "Choose Order", choices = c("", sort(unique(my_data_clean$order)))),
                 selectInput("species_name", "Species", choices = c("", sort(unique(my_data_clean$species_name))))
                 ),
               mainPanel(
                 plotOutput("diet_percentage")
                 )
               )
             ),
    tabPanel("Range & Dispersal",
             titlePanel("Distribution"),
             sidebarLayout(
               sidebarPanel(
                 width = 2,
                 selectInput("order5", "Choose Order", choices = c("", sort(unique(my_data_clean$order)))),
                 selectInput("species_name2", "Species", choices = c("", sort(unique(my_data_clean$species_name))))
               ),
               mainPanel(
                 plotOutput("alt_range"),
                 br(),br(),
                 plotOutput("bio_realm")
                 )
               )
             )
  )
  
)

server <- function(input, output, session) {
  
  # Introduction ----

  species_summary <- reactive({
    my_data_clean %>% 
      group_by(order) %>% 
      count() %>% 
      ungroup() %>% 
      rename(Order = order, "Number of species" = n)
  })
  
  output$summary_species <- renderTable({
    species_summary()
  })
  
  # General overview ----
  order <- reactive({
    my_data_clean %>% 
      filter(order == input$order)
  })
  observeEvent(order(), {
    req(input$order)
    choices <- sort(unique(order()$family))
    updateSelectInput(inputId = "family", choices = c("", choices))
  })
  
  family <- reactive({
    req(input$family)
    order() %>% 
      filter(family == input$family)
  })
  observeEvent(family(), {
    req(input$family)
    choices <- sort(unique(family()$genus))
    updateSelectInput(inputId = "genus", choices = c("", choices))
  })
  
  genus <- reactive({
    req(input$genus)
    family() %>% 
      filter(genus == input$genus)
  })
  
  output$fam_per_order <- renderPlot({
    my_data_clean %>% 
      group_by(order) %>% 
      summarise(n_taxa = n_distinct(family)) %>% 
      ungroup() %>% 
      ggplot(aes(x = order, y = n_taxa, fill = order)) + theme_bw() +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n_taxa), vjust=-0.3, size=3.5) +
      labs(x = "", y = "", title = "Number of families per order") +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, 
                                       vjust = 0.9, size = 10),
            panel.grid = element_blank())
  }, width = 1000)
  
  plot_gen_per_fam <- reactive({
    req(input$order)
    order() %>% 
      group_by(family) %>% 
      summarise(n_taxa = n_distinct(genus)) %>% 
      ungroup() %>% 
      ggplot(aes(x = family, y = n_taxa, fill = family)) + theme_bw() +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n_taxa), vjust=-0.3, size=3.5) +
      labs(x = "", y = "", title = paste("Number of genera per family in the order", input$order , sep = " ")) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, 
                                       vjust = 0.9, size = 10),
            panel.grid = element_blank())
  })
  
  output$gen_per_fam <- renderPlot(plot_gen_per_fam(), width = 1000)
  
  output$spec_per_gen <- renderPlot({
    family() %>% 
      group_by(genus) %>% 
      summarise(n_taxa = n_distinct(species)) %>% 
      ungroup() %>% 
      ggplot(aes(x = genus, y = n_taxa, fill = genus)) + theme_bw() +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n_taxa), vjust=-0.3, size=3.5) +
      labs(x = "", y = "", title = paste("Number of species per genus in the family", input$family , sep = " ")) +
      theme(legend.position = "none",
            axis.text.x = element_text(angle = 45, hjust = 1, 
                                       vjust = 0.9, size = 10),
            panel.grid = element_blank())
  }, width = function() {
    if (length(unique(family()$genus)) > 40) {
      length(unique(family()$genus)) * 25
    } else if (length(unique(family()$genus)) > 100) {
      length(unique(family()$genus)) * 17
    } else if (length(unique(family()$genus)) > 20) {
      1000
    } else {
      600
    }
  })
  
 species_list <- reactive({
   req(input$genus)
   genus() %>% 
     select(Genus = genus, Species = species)
 })
 output$spec_list <- renderDataTable(species_list(), options = list(pageLength = 5))
  
 # Weight & Height ----
 
 plot_w_l <- reactive({
   req(input$order2)
   
   my_data_clean %>% 
     ggplot(aes(x = log(adult_mass_g), y = log(adult_body_length_mm))) + theme_bw() + 
     geom_point(alpha = 0.1, size = 3) + 
     geom_point(data = my_data_clean %>% filter(order == input$order2), 
                 aes(x = log(adult_mass_g), y = log(adult_body_length_mm)), colour = "blue", alpha = 0.8, size = 3) +
     labs(x = "Log adult mass (g)", y = "Log adult body length (mm)") +
     theme(axis.text = element_text(size = 16),
           axis.title = element_text(size = 18))

 })
 
 output$w_l_plot <- renderPlot(plot_w_l())
 
 order2 <- reactive({
   my_data_clean %>% 
     filter(order == input$order2)
 })
 
 mass_length_tbl <- reactive({
   req(input$order2)
   tbl <- bind_cols(order2() %>% 
                      summarise(min = round(min(adult_mass_g / 1000, na.rm = TRUE), digits = 2),
                                avg = round(mean(adult_mass_g / 1000, na.rm = TRUE), digits = 2),
                                max = round(max(adult_mass_g / 1000, na.rm = TRUE), digits = 2)) %>% 
                      rename("Mean" = avg, "Min" = min,
                             "Max" = max) %>% 
                      t() %>% 
                      as.data.frame() %>% 
                      rename(Mass = V1),
                    order2() %>% 
                      summarise(min = round(min(adult_body_length_mm / 1000, na.rm = TRUE), digits = 2),
                                avg = round(mean(adult_body_length_mm / 1000, na.rm = TRUE), digits = 2),
                                max = round(max(adult_body_length_mm / 1000, na.rm = TRUE), digits = 2)) %>% 
                      rename("Mean" = avg, "Min" = min,
                             "Max" = max) %>% 
                      t() %>% 
                      as.data.frame() %>% 
                      rename(Length = V1))
 })
 
 species_number <- reactive({order2() %>% 
   group_by(order) %>% 
   summarise(species_count = n())
 })
 
 tbl_w_l <- reactive({
   mass_length_tbl() %>% 
     gt(rownames_to_stub = TRUE) %>% 
     tab_header(title = paste0("Order ", input$order2), 
                subtitle = "Adult body mass (kg) and body length (m)") %>% 
     tab_stubhead(label = paste0("Number of species: ", species_number()$species_count))
 })
 
 output$w_l_tbl <- render_gt(tbl_w_l())
 
 
 # Life history ----
  order3 <- reactive({
   my_data_clean %>% 
     filter(order == input$order3)
 })
 observeEvent(order3(), {
   req(input$order3)
   choices <- sort(unique(order3()$family))
   updateSelectInput(inputId = "family3", choices = c("", choices))
 })
 
 reproduction_traits_table <- reactive({
   req(input$order3)
   order3() %>% 
     select(Family = family, Genus = genus, Species = species, "Max longevity (days)" = max_longevity_d, 
            "Age of maturity (days)" = maturity_d, "Female age of maturity (days)" = female_maturity_d,
            "Male age of maturity (days)" = male_maturity_d, "Age at first reproduction (days)" = age_first_reproduction_d,
            "Gestation length (days)" = gestation_length_d, "Litter size" = litter_size_n, "Interbirth interval (days)" = interbirth_interval_d,
            "Weaning age (days)" = weaning_age_d, "Generation length (days)" = generation_length_d)
 })
 
 output$reproduction <- renderDataTable(reproduction_traits_table())
 
 # Diet ----
 
 order4 <- reactive({
   my_data_clean %>% 
     filter(order == input$order4)
 })
 observeEvent(order4(), {
   req(input$order4)
   choices4 <- sort(unique(order4()$species_name))
   updateSelectInput(inputId = "species_name", choices = c("", choices4))
 })

 
 diet <- reactive({
   req(input$species_name)
   order4() %>% 
     filter(species_name == input$species_name) %>% 
     select(species_name, starts_with("det_"), 
            starts_with("dphy_"), trophic_level, foraging_stratum) %>% 
     #pivot_longer(cols = c(c(starts_with("det_")), -det_diet_breadth_n), values_to = "percentage_diet", names_to = "type_diet") %>% 
     pivot_longer(cols = starts_with("dphy_"), values_to = "percentage_diet_phylacine", names_to = "type_diet_phylacine") %>% 
     mutate(percentage_diet_phylacine = paste(percentage_diet_phylacine, "%", sep = " "))
 })
 
 
 mouse_pic <- image_data("92989e35-4e68-4a2d-b3a2-191ba9da671a", size = 256)[[1]] # get actual icon, define size. Don't run this alone
 fly_pic <- image_data("beb0072f-bd66-46f1-8c31-17b64a9c6670", size = 256)[[1]]
 plant_pic <- image_data("3b3c19c5-51d2-46c5-886d-3607e62efc73", size = 256)[[1]]
 
 p1 <- reactive({
   diet() %>% 
   #filter(species_name == "Abditomys latidens") %>% 
   filter(type_diet_phylacine == "dphy_vertebrate") %>% 
   ggplot(aes(x = percentage_diet_phylacine, label = percentage_diet_phylacine)) +
   geom_bar(fill = "white", alpha = 0.01) +
   geom_text(aes(label = percentage_diet_phylacine, y = 0.85), size = 10) +
   add_phylopic(mouse_pic, alpha = 1, ysize = 10) +
   annotate("text", x = 1, y = 0.23, label = "Vertebrate diet", size = 7) +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         panel.background = element_rect(fill = "white")
         )
 })
   
 
 p2 <- reactive({
   diet() %>% 
   #filter(species_name == "Abditomys latidens") %>% 
   filter(type_diet_phylacine == "dphy_invertebrate") %>%
   ggplot(aes(x = percentage_diet_phylacine, label = percentage_diet_phylacine)) + 
   geom_bar(fill = "white", alpha = 0.01) +
   geom_text(aes(label = percentage_diet_phylacine, y = 0.85), size = 10) +
   add_phylopic(fly_pic, alpha = 1, ysize = 10) +
   annotate("text", x = 1, y = 0.23, label = "Invertebrate diet", size = 7) +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         panel.background = element_rect(fill = "white")
         )
 })
 
 p3 <- reactive({
   diet() %>% 
   #filter(species_name == "Abditomys latidens") %>% 
   filter(type_diet_phylacine == "dphy_plant") %>%
   ggplot(aes(x = percentage_diet_phylacine, label = percentage_diet_phylacine)) + 
   geom_bar(fill = "white", alpha = 0.01) +
   geom_text(aes(label = percentage_diet_phylacine, y = 0.85), size = 10) +
   add_phylopic(plant_pic, alpha = 1, ysize = 10) + 
   annotate("text", x = 1, y = 0.23, label = "Plant diet", size = 7) +
   theme(axis.text = element_blank(),
         axis.ticks = element_blank(),
         axis.title = element_blank(),
         panel.grid = element_blank(),
         panel.border = element_blank(),
         panel.background = element_rect(fill = "white")
         ) 
 })
 
 output$diet_percentage <- renderPlot({
   plot_grid(p1(), p2(), p3(), nrow = 1) +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 2))
 }, width = 800, height = 400)
 
 # Distribution ----
 
 order5 <- reactive({
   my_data_clean %>% 
     filter(order == input$order5)
 })
 observeEvent(order5(), {
   req(input$order5)
   choices5 <- sort(unique(order5()$species_name))
   updateSelectInput(inputId = "species_name2", choices = c("", choices5))
 })
 
 data <- reactive({
   req(input$order5)
   order5() %>% 
   filter(species_name == input$species_name2)
 })
 
 plot <- reactive({
   req(input$species_name2)
   
   if (!is.na(data()$altitude_breadth_m)) {
   ggplot() +
     geom_polygon(aes(x = c(1,2,3), y = c(1,7000,1))) +
     geom_hline(yintercept = data()$lower_elevation_m, linetype="dashed") +
     geom_hline(yintercept = data()$altitude_breadth_m + data()$lower_elevation_m, linetype="dashed") +
     labs(y = "Altitudinal range (m)") +
     annotate("text", x = 0.5 , y = data()$lower_elevation_m + 200, 
              label = paste0(data()$lower_elevation_m, " m"), size = 3) +
     annotate("text", x = 0.5 , y = data()$altitude_breadth_m + data()$lower_elevation_m + 200, 
              label = paste0(data()$altitude_breadth_m + data()$lower_elevation_m, " m"), size = 3) +
     annotate("rect", xmin = -Inf, xmax = Inf, ymin = data()$lower_elevation_m, 
              ymax = data()$altitude_breadth_m + data()$lower_elevation_m, alpha = .15, fill = "deepskyblue4") +
     theme(axis.text.x = element_blank(),
           axis.title.x = element_blank(),
           axis.ticks.x = element_blank(),
           axis.ticks.y = element_blank(),
           panel.grid = element_blank(),
           panel.grid.major.y = element_line(linetype = "solid", colour = "gray80"),
           panel.background = element_rect(fill = "white")
     )
   } else {
     ggplot() +
       geom_polygon(aes(x = c(1,2,3), y = c(1,7000,1))) +
       geom_hline(yintercept = data()$lower_elevation_m, linetype="dashed") +
       geom_hline(yintercept = data()$altitude_breadth_m + data()$lower_elevation_m, linetype="dashed") +
       labs(y = "Altitudinal range (m)") +
       annotate("text", x = 2 , y = 3000, 
                label = "No data available", size = 5, colour = "white") +
       theme(axis.text.x = element_blank(),
             axis.title.x = element_blank(),
             axis.ticks.x = element_blank(),
             axis.ticks.y = element_blank(),
             panel.grid = element_blank(),
             panel.grid.major.y = element_line(linetype = "solid", colour = "gray80"),
             panel.background = element_rect(fill = "white")
       )
   }
 })
 
 output$alt_range <- renderPlot({
   plot()
 }, width = 800, height = 400)
 
 
 realm_cols = c("Nearctic" = "#A8BA6D", "Palearctic" = "#B0503A", "Afrotropical" = "#6574AB", 
                "Indomalayan" = "#D6B778","Australasian" = "#D48F0F","Neotropical" = "#A688B3", 
                "Oceanian" = "#B5B2B1","Antarctica" = "#EDE8E8")
 
 species <- reactive({
    data() %>% 
       separate_rows(biogeographical_realm, sep = "[^[:alnum:].]+")
 })
 
 bio_realm_plot <- reactive({
    req(input$species_name2)
    ggplot() +
       geom_sf(data = eco_simp, fill = "gray60", size = .2, col = 0, alpha=.3) +
       geom_sf(data = eco_simp %>% filter(grepl(paste(species()$biogeographical_realm, collapse = "|"), REALM)), 
               aes(fill = REALM), size = .2, col = 0, alpha=.8) +
       labs(title = "Biogeographical realms") +
       theme(panel.background = element_rect(fill = "white")) +
      scale_fill_manual(values = realm_cols)
 })
 
 output$bio_realm <- renderPlot({
    bio_realm_plot()
 }, width = 800, height = 400)
 
}


shinyApp(ui, server)