library(shiny)
library(googlesheets4)
library(devtools)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)
library(forcats)
library(rsconnect)
library(geomtextpath)
library(DT) #this package allows interactive data table visualization
library(tidyr)
library(Rcpp)

#Grab data from google sheets (it will make you log into your google account and grant access)------
gs4_deauth() #This says that the sheet is public and it's chill to use it
link <- "https://docs.google.com/spreadsheets/d/10_uuMLOqu3s5fbRgF3x_Jt1VsufmJ6hLokaZ0aHvOYk/edit?usp=sharing"
diet_data <- read_sheet(link)

head(diet_data)

#Make objects that contain list of of prey species
forage_fish = c("Capelin", 
                "Pacific_herring",
                "Pacific_sand_lance",
                "Unidentified_bony_fish",
                "Unidentified_smelt",
                "Rockfish")
crab = c("Dungeness_crab",
         "Hermit_crab",
         "Pygmy_cancer_crab",
         "Graceful_decorator_crab",
         "Unidentified_crab")
flatfish = c("Rock_sole",
             "Sand_sole",
             "Unidentified_flatfish")

# Calculate relative abundance:
relabund <- diet_data %>%
  mutate(Prey_type = case_when(Species %in% forage_fish ~ 'Forage_fish',
                               Species %in% crab ~ 'Crab',
                               Species %in% flatfish ~ 'Flatfish',
                               !Species %in% c(forage_fish, crab, flatfish) ~ Species)) %>%
  filter(Contents == "Y") %>% # dplyr uses filter() instead of subset()
  group_by(Predator_spec, Prey_type) %>%
  tally() %>%
  ungroup() %>% group_by(Predator_spec) %>%
  mutate(Relabund = round((n / sum(n) * 100), 2))
head(relabund)

diet_data_fish <- diet_data %>%
  subset(Fish == "Y") %>%
  group_by(Predator_spec, Species) %>%
  tally() %>%
  mutate(Relabund=paste0(round(n/sum(n)*100,2)))
diet_data_fish$Relabund <- as.numeric(diet_data_fish$Relabund)
head(diet_data_fish)

head(diet_data)
fish_count <- diet_data %>%
  subset(Fish == "Y") %>%
  group_by(Species) %>%
  distinct(Fish_ID) %>%
  tally()
head(fish_count)

#Manipulating data so it can be used in DT package
nocolby <- diet_data %>%
  subset(select = -c(Collected_by))
dt <- datatable(nocolby,
                extensions = 'Buttons',
                options = list(
                  paging = TRUE,
                  fixedColumns = TRUE,
                  autoWidth = TRUE,
                  ordering = TRUE,
                  dom = 'tB',
                  buttons = c('csv', 'excel')
                ),
                rownames = FALSE, 
                colnames = c("Predator length (cm)" = "Length_cm",
                             "Date caught" = "Date_caught",
                             "Time caught" = "Time_caught", 
                             "Predator species" = "Predator_spec",
                             "Category = fish ?" = "Fish",
                             "Prey species" = "Species",
                             "Prey measurement" = "Measurement",
                             "Decomposition status (5 = most)" = "Decomp_rating"),
                filter = "top",
                class = "hover")
dt

#Let's try making the figures first------
#GAME FISH PREDATION
compall <- ggplot(relabund, aes(x = Predator_spec, y = Relabund, fill = Prey_type)) +
  geom_col() +
  ylab("Percent diet composition") +
  xlab("Predator species") +
  theme_classic() +
  scale_fill_npg()
compall

compfish <- ggplot(diet_data_fish, aes(x = Predator_spec, y = Relabund, fill = Species)) +
  geom_col() +
  ylab("Percent diet composition") +
  xlab("Predator species") +
  theme_classic() +
  scale_fill_brewer(palette = "Spectral")
compfish

#size density of different species by sex
head(diet_data)
predsize_sum <- diet_data %>% 
  group_by(Predator_spec, Sex) %>% 
  filter(!is.na(Length_cm)) %>%
  filter(!is.na(Sex)) %>%
  dplyr::summarise(length = Length_cm,samp = n())
head(predsize_sum)

#Chris' code
diet_data %>%
  select(Predator_spec,
         Sex,
         length = Length_cm) %>%
  drop_na(length, Sex) %>%
  distinct() %>%
  group_by(Predator_spec, Sex) %>%
  dplyr::mutate(n_pred.sex = n()) # I renamed this col to be more explicit what has been calculated

#predsize plot
head(diet_data)
predsize <- diet_data %>%
  select(Predator_spec,
         Sex,
         Length_cm) %>%
  drop_na(Length_cm, Sex) %>%
  distinct() %>%
  group_by(Predator_spec, Sex) %>%
  dplyr::mutate(n_pred.sex = n()) %>%
  filter(n_pred.sex > 2) %>%
  ggplot(aes(x = Length_cm, fill = Sex)) +
  geom_density(alpha = 0.3) +
  xlab("Length (cm)") +
  ylab("Frequency") +
  geomtextpath::geom_textdensity(aes(label = n_pred.sex),
                                 hjust = 'ymax', vjust = -0.02,
                                 straight = TRUE) +
  theme_classic() +
  facet_wrap( ~ Predator_spec)
predsize

#FORAGE FISH COMMUNITY
countfish <- fish_count %>%
  mutate(Species = fct_reorder(Species, desc(n))) %>%
  ggplot(aes(x = Species, y = n, label = n)) +
  geom_col(fill = "light blue") +
  geom_text(nudge_y = 1) + # histograms are easier to label
  ylab("Frequency") + # This is a confusing label... maybe just 'Frequency' ?
  xlab("Prey fish species") +
  theme_classic()
countfish

foragesize <- diet_data %>%
  group_by(Fish_ID) %>%
  subset(Fish == "Y") %>%
  subset(Measurement != "NULL") %>%
  ggplot(aes(x = Measurement, fill = Species)) +
  geom_density(alpha = 0.3) +
  theme_classic() +
  ylab("Density") +
  xlab("Length (cm)") +
  scale_fill_brewer(palette = "Spectral")
foragesize

#Create web app----
ui <- fluidPage(titlePanel(
  title = span(img(src = "kbnerr_logo.png", height = 50), 
               "Foraging for answers")
),
navlistPanel("A KBNERR citizen science initiative",
             #The about page is mostly text so let's minimize this-----
             tabPanel("About",
                      tags$h2("Forage fish are what's on the menu for predatory fish!"),
                      tags$h4("The forage fish community fills a trophic niche between plankton and large fish."),
                      fluidRow(tags$img(src = "trophic.png", height = "300 px"), align = "center"),
                      tags$h4("Since we eat large predatory fish, we want to make sure there's enough forage fish to maintain this food chain."),
                      tags$br(),
                      tags$h2("Alaskan scientists and fishermen are collaborating to monitor forage fish communities."),
                      tags$h4("When someone catches a fish, they remove its stomach contents and photograph these contents on a laminated “diet card."),
                      tags$h4("The fisherman records information about the fish they caught before sending a photo of the card to us."),
                      fluidRow(tags$img(src = "example.JPEG", height = "300 px"), align = "center"),
                      tags$h4("A scientist will then identify the species present in that predatory fish’s diet. In the picture above, you can see that a king salmon had eaten one Pacific sand lance."),
                      tags$br(),
                      tags$h2("See the science"),
                      tags$h4("This information can tell us:"),
                      tags$h4("-  What do different economically important species, like salmon and halibut, depend on for their food?"),
                      tags$h4("-  What species make up the forage fish population of Kachemak Bay?"),
                      tags$h4("On this website, you can see up-to-date visualizations that answer these two questions - and pose some more!"),
                      tags$br(),
                      tags$h2("Want to help out?"),
                      tags$h4("Contact syverine@alaska.edu and we will do our best to get diet card materials to you. If you already have a diet card, send in photos to this", a("link.", href = "https://tinyurl.com/fish-guts")),
                      fluidRow(tags$img(src = "sandlance.png", height = "300 px"), align = "center")
             ),
             #Now for the tabs that are working with data------
             tabPanel(title = "Game fish predation",
                      tags$h2("What are the big fish eating?"),
                      tags$h4("We would expect that groundfish, like Halibut, eat more organisms that live on the seafloor, like crabs. On the other hand, salmon - which hunt up and down the water column - tend to mostly eat forage fish."),
                      plotOutput("allHist"),
                      tags$h4("Let's visualize which forage fish different predator species prey on."),
                      plotOutput("allHistFish"),
                      tags$br(),
                      tags$h2("Diet card data can help us keep track of predator size."),
                      tags$h4("This is a density plot, which visualizes how common (the y axis - frequency) each size (the x axis - length in cm) of predator species is."),
                      tags$h4("For most fish, it is expected that female specimens are bigger. Keep in mind that fishermen often throw back too-small fish, so this data may be skewed towards larger specimens."),
                      plotOutput("predsize"),
                      tags$br(),
                      tags$h3(align = "center", "All this information helps salmon (like this King) know where their next meal will come from!"),
                      fluidRow(tags$img(src = "chinook.png", height = "200 px"), align = "center")
             ),
             tabPanel(title = "Forage fish community",
                      tags$h2("Forage fish community makeup"),
                      tags$h4("Sand lance, herring, capelin, and flatfish make up the majority of Kachemak Bay's forage fish community."),
                      plotOutput("countfish"),
                      tags$br(),
                      tags$h2("How big are the small fish?"),
                      tags$h4("The diet card program also yields valuable information about forage fishes' size and life stage."),
                      plotOutput("foragesize"),
                      fluidRow(tags$img(src = "forage.png", height = "200 px"), align = "center"),
                      tags$br(),
                      tags$h2("Which fish make up Alaska's forage fish community?"),
                      fluidRow(tags$img(src = "idpreview.png", height = "200 px"), align = "center"),
                      fluidRow(tags$a("Click to download our guide.", href = "idguide.pdf"), align = "center")
             ),
             tabPanel(title = "Resources",
                      tags$h3("Data collection process"),
                      tags$h5("1. Fishermen submit photos of a predator fish's stomach contents."),
                      tags$h5("2. Scientists identify and measure prey species, recording data into a form."),
                      tags$h5("3. The form automatically updates the figures and data table (below) on this website."),
                      tags$br(),
                      tags$h3("ID guide for forage fish species"),
                      fluidRow(tags$img(src = "guidepreview.png", height = "200 px"), align = "center"),
                      fluidRow(tags$a("Click to download", href = "guide.pdf"), align = "center"),
                      tags$br(),
                      tags$h3("Glossary"),
                      tags$ul(
                        tags$li("Forage fish: A group if fish species that are eaten by marine predators. They have an important trophic position because they transfer energy from small plankton, which they eat, to large predatory fish, which eat them."), 
                        tags$li("Trophic relationships: Think of a food web - who eats who? How is energy transferred through an ecosystem?"), 
                        tags$li("Citizen science: A “citizen,” rather than professional academic, collects or analyzes data for the benefit of scientific progress."),
                        tags$li("Juvenile: The early life stage of a species, before adulthood."),
                        tags$li("Binomial nomenclature: Every organism has a common name and a scientific name, which includes two parts: first, its genus and second, its species. For instance, humans have the scientific name Homo (genus) sapiens (species)."),
                        tags$li("Piscivore: Any organism that eats fish.")
                      ),
                      tags$br(),
                      tags$h3("Updated data (download buttons below!)"),
                      tags$table(dt),
                      ""
             )
),
wellPanel(align = "center", "Website created by Clara Benadon (NOAA Hollings scholar).
          Data collected by Miyumi Arimitsu (USGS), Clara Benadon, and volunteers.
          Scientific illustrations by Conrad Field (KBNERR).")
)

server <- function(input, output) {
  output$allHist <- renderPlot({
    compall
  })
  output$allHistFish <- renderPlot({
    compfish
  })
  output$countfish <- renderPlot({
    countfish
  })
  output$predsize <- renderPlot({
    predsize
  })
  output$foragesize <- renderPlot({
    foragesize + labs(fill = "Forage fish species")
  })
  output$coolplot <- renderPlot({
    filtered <- 
      diet_data %>%
      filter(Predator_spec == input$specInput)
    head(filtered)
    ggplot(filtered, aes(x = Predator_spec, fill = Species)) +
      geom_bar()
  })
}

shinyApp(ui = ui, server = server)
