library(shiny)
library(googlesheets4)
library(devtools)
library(ggplot2)
library(dplyr)
library(ggsci)
library(ggpubr)
library(forcats)
library(rsconnect)
library(DT) #this package allows interactive data table visualization
library(tidyr)
library(ggbiplot)
library(Rcpp)

#Grab data from google sheets (it will make you log into your google account and grant access)------
gs4_deauth() #This says that the sheet is public and it's chill to use it
link <- "https://docs.google.com/spreadsheets/d/10_uuMLOqu3s5fbRgF3x_Jt1VsufmJ6hLokaZ0aHvOYk/edit?usp=sharing"
diet_data <- read_sheet(link)

head(diet_data)
diet_data_grouped <- diet_data %>%
  mutate(Species = dplyr::recode(Species,
                                       "Capelin" = "Forage_fish",
                                       "Pacific_herring" = "Forage_fish",
                                       "Pacific_sand_lance" = "Forage_fish",
                                       "Unidentified_bony_fish" = "Forage_fish",
                                       "Unidentified_smelt" = "Forage_fish",
                                 "Rockfish" = "Forage_fish",
                                       "Dungeness_crab" = "Crab",
                                       "Hermit_crab" = "Crab",
                                       "Pygmy_cancer_crab" = "Crab",
                                 "Graceful_decorator_crab" = "Crab",
                                 "Unidentified_crab" = "Crab",
                                       "Rock_sole" = "Flatfish",
                                       "Sand_sole" = "Flatfish",
                                       "Unidentified_flatfish" = "Flatfish")) %>%
  subset(Contents == "Y") %>%
  group_by(Predator_spec, Species) %>%
  tally() %>%
  mutate(Relabund=paste0(round(n/sum(n)*100,2))) 

#PROBLEM I think the problem is in the line above, it's calculating n/sum(n) by summing ALL Predator species, not just within one Predator spec

diet_data_grouped$Relabund <- as.numeric(diet_data_grouped$Relabund)
head(diet_data_grouped)

head(diet_data)
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
          class = "hover",
          options = list(searching = FALSE, pageLength = 25))
dt

#Let's try making the figures first------
#GAME FISH PREDATION
compall <- ggplot(diet_data_grouped, aes(x = Predator_spec, y = Relabund, fill = Species)) +
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
  scale_fill_npg()
compfish

#size density of different species by sex
head(diet_data)
predsize_sum <- diet_data %>% 
  group_by(Predator_spec, Sex) %>% 
  filter(!is.na(Length_cm)) %>%
  filter(!is.na(Sex)) %>%
  dplyr::summarise(length = Length_cm,samp = n())
head(predsize_sum)
storms_sum
predsize <- diet_data %>%
  group_by(Fish_ID) %>%
  filter(!is.na(Length_cm)) %>%
  filter(!is.na(Sex)) %>%
  ggplot(aes(x = Length_cm, fill = Sex)) +
  geom_density(alpha = 0.3) +
  theme_classic() +
  facet_wrap( ~ Predator_spec)
predsize

#FORAGE FISH COMMUNITY
countfish <- fish_count %>%
  mutate(Species = fct_reorder(Species, desc(n))) %>%
  ggplot(aes(x = Species, y = n)) +
  geom_col(fill = "light blue") +
  ylab("Found in n fish stomachs") +
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
  xlab("Length (cm)")
foragesize

#----Making competition PCA----
#Calculate abundance
diet_data_abund <- diet_data %>%
  subset(Contents == "Y") %>%
  group_by(Fish_ID, Species) %>%
  tally()
head(diet_data_abund)

#Format abundance data into wide
diet_data_wide <- diet_data_abund %>%
  pivot_wider(names_from = Species,
              values_from = n,
              values_fn = list(n = sum))
diet_data_wide[is.na(diet_data_wide)] <- 0
head(diet_data_wide) #note - may need to remove fish IDs that have 0 associated species

#Splice in Predator_spec row
predspec <- diet_data %>%
  select(Fish_ID, Predator_spec)
head(predspec)
widepred <- merge(diet_data_wide, predspec, by = "Fish_ID")
head(widepred)

#Create sp and env dataframes
sp_wide <- subset(widepred, select = -c(Fish_ID, Predator_spec) )
head(sp_wide)
env_wide <- subset(widepred, select = c(Fish_ID, Predator_spec))
head(env_wide)

#Making pca
pca <- prcomp(sp_wide)
pcaplot <- ggbiplot(pca,
         ellipse = TRUE,
         groups = env_wide$Predator_spec,
         var.scale = 0.2,
         var.axes = FALSE) + #if you want species vectors present or not!
  theme_classic()
pcaplot

#Create web app----
ui <- fluidPage(titlePanel(
  title = span(img(src = "kbnerr_logo.png", height = 50), 
               "Foraging for answers")
),
                navlistPanel("A KBNERR citizen science initiative",
                             #The about page is mostly text so let's minimize this-----
                  tabPanel("About",
                           tags$h3("There’s always a bigger fish"),
                           tags$body("Forage fish are what's on the menu for predatory fish. They are a group of many species of fish - think sand lance and herrings - that fill a trophic niche between plankton and large fish. Since we (humans) eat large predatory fish, we want to make sure there's enough forage fish to maintain this food chain."),
                           tags$h3("A marine mystery"),
                           tags$body("Forage fish populations largely remain a mystery. Not many efforts exist that provide information about forage fish abundance and diversity. Such monitoring programs would alert scientists and fishermen if forage fish populations become unhealthy."),
                           tags$h3("Diet cards"),
                           tags$body("Scientists at Kachemak Bay Research Reserve (KBNERR) and USGS are collaborating with local fishermen to solve this problem. When someone catches a fish, they remove its stomach contents and photograph them on a laminated “diet card.” The fisherman also records size, sex, and (general) location data, before sending a photo of the diet card to us."),
                           fluidRow(tags$img(src = "example.JPEG", height = "300 px"), align = "center"),
                           tags$body("Identifying the species present in predatory fish’s stomachs gives important information about two things. First, what do different economically important species, like salmon and halibut, depend on for their food? And second, what species make up the forage fish population of Kachemak Bay? We can also explore how answers to these questions change across different locations and over time."),
                           tags$h3("Want to help out?"),
                           tags$body("Contact syverine@alaska.edu and we will do our best to get diet card materials to you. If you already have a diet card, send in photos to this", a("link.", href = "https://tinyurl.com/fish-guts")),
                           fluidRow(tags$img(src = "sandlance.png", height = "300 px"), align = "center")
                           ),
                  #Now for the tabs that are working with data------
                  tabPanel(title = "Game fish predation",
                           tags$h3("What are the big fish eating?"),
                           "We would expect that groundfish, like Halibut, eat more organisms that live on the seafloor, like crabs. On the other hand, salmon - which hunt up and down the water column - tend to mostly eat forage fish.",
                           plotOutput("allHist"),
                           tags$h4("Let's look at fish specifically"),
                           plotOutput("allHistFish"),
                           tags$h4("Size distribution of predator species"),
                           "For most fish, it is expected that female specimens are bigger. Keep in mind that fishermen often throw back too-small fish, so this data may be skewed towards larger specimens.",
                           plotOutput("predsize"),
                           tags$h3(align = "center", "All this information helps salmon (like this King) know where their next meal will come from!"),
                           fluidRow(tags$img(src = "chinook.png", height = "200 px"), align = "center")
                           ),
                  tabPanel(title = "Forage fish community",
                           tags$h3("Forage fish insights"),
                           "Sand lance, herring, capelin, and flatfish make up the majority of Kachemak Bay's forage fish community.",
                           plotOutput("countfish"),
                           "The diet card program also yields valuable information about forage fishes' size and life stage.",
                           plotOutput("foragesize"),
                           fluidRow(tags$img(src = "forage.png", height = "200 px"), align = "center"),
                           tags$h3("Competition plot"),
                           "The overlap between predatory species' circles in this principle components analysis plot represent predation competition.",
                           plotOutput("pcaplot"),
                           tags$h3("Identification guide"),
                           "Would you like to learn how to identify the forage fish of Kachemak Bay?",
                           tags$a("Click here to download our ID guide.", href = "idguide.pdf")
                           ),
                  tabPanel(title = "Access raw data",
                           tags$h3("Data collection process"),
                           tags$h5("1. Fishermen submit photos of a predator fish's stomach contents."),
                           tags$h5("2. Scientists identify and measure prey species, recording data into a form."),
                           tags$h5("3. The form automatically updates the figures and data table (below) on this website."),
                           tags$h3("Updated data"),
                           tags$table(dt))
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
  output$pcaplot <- renderPlot({
    pcaplot + labs(fill = "Predatory species")
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
