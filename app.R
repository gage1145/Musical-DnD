library(shiny)

# Define weighted probabilities for parameters
weighted_sample <- function(choices, weights) {
  sample(choices, size = 1, prob = weights)
}

# Parameter options
keys <- c("C", "G", "D", "A", "E", "B", "F♯", "C♯", "F", "B♭", "E♭", "A♭", "D♭", "G♭", "A♯", "D♯")
modes <- c("Major", "Minor", "Dorian", "Phrygian", "Lydian", "Mixolydian", "Locrian")
tempo_ranges <- c("60-90 BPM", "90-120 BPM", "120-150 BPM", "150-200+ BPM")
time_signatures <- c("4/4", "3/4", "6/8", "5/4", "7/8", "9/8", "12/8", "13/8")
progressions <- c("I-V-vi-IV", "I-IV-V-I", "ii-V-I", "I-vi-IV-V", "Uncommon")
moods <- c("Surprised", "Stressed", "Mad", "Loved", "Lonely", "Weird", "Uncomfortable", "Touched", "Tired", "Thankful", "Sympathetic", "Smart", "Sleepy", "Silly", "Pleased", "Pessimistic", "Sick", "Shocked", "Satisfied", "Sad", "Rushed", "Restless", "Relieved", "Relaxed", "Rejuvenated", "Rejected", "Refreshed", "Recumbent", "Quixotic", "Predatory", "Peaceful", "Optimistic", "Okay", "Numb", "Giggly", "Giddy", "Not specified", "Nerdy", "Infuriated", "Indifferent", "Indescribable", "Naughty", "Morose", "Moody", "Mischievous", "Mellow", "Melancholy", "Listless", "Lethargic", "Lazy", "Jubilant", "Jealous", "Irritated", "Irate", "Impressed", "Hyper", "Hungry", "Hot", "Hopeful", "High", "Happy", "Guilty", "Grumpy", "Groggy", "Grateful", "Dorky", "Ditzy", "Discontent", "Good", "Gloomy", "Geeky", "Full", "Frustrated", "Flirty", "Exhausted", "Excited", "Enraged", "Energetic", "Anxious", "Annoyed", "Angry", "Ecstatic", "Drunk", "Drained", "Disappointed", "Dirty", "Devious", "Determined", "Depressed", "Dark", "Cynical", "Curious", "Alone", "Aggravated", "Accomplished", "Accepted", "Crushed", "Crazy", "Crappy", "Cranky", "Content", "Confused", "Complacent", "Cold", "Chipper", "Cheerful", "Calm", "Bouncy", "Bored", "Blissful", "Blank", "Blah", "Bittersweet", "Bewildered", "Awake", "Ashamed", "Apathetic", "Amused", "Exanimate", "Envious", "Enthralled")

# Weights
mode_weights <- c(0.3, 0.25, 0.2, 0.15, 0.05, 0.03, 0.02)
tempo_weights <- c(0.25, 0.4, 0.25, 0.1)
time_weights <- c(0.7, 0.15, 0.1, 0.01, 0.01, 0.01, 0.01, 0.01)
progression_weights <- c(0.3, 0.2, 0.15, 0.15, 0.2)

# UI
ui <- fluidPage(
  titlePanel("Musical DnD"),
  sidebarLayout(
    sidebarPanel(
      actionButton("roll", "Roll for Parameters")
    ),
    mainPanel(
      h1("Your Song Parameters:"),
      verbatimTextOutput("output")
    )
  )
)

# Server
server <- function(input, output) {
  observeEvent(
    input$roll,
    {
      song_params <- list(
        Key = paste(sample(keys, 1), weighted_sample(modes, mode_weights)),
        Tempo = weighted_sample(tempo_ranges, tempo_weights),
        `Time Signature` = weighted_sample(time_signatures, time_weights),
        `Chord Progression` = weighted_sample(progressions, progression_weights),
        Mood = sample(moods, 1)
      )
      output$output <- renderText({
        paste0(names(song_params), ": ", unlist(song_params), collapse = "\n")
      })
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
