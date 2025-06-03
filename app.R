# Main application file

# Source global configuration, UI, and server files
source("global.R")
source("ui.R")
source("server.R")

# Run the application
shinyApp(ui = ui, server = server)
