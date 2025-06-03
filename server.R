# Server Definition
server <- function(input, output, session) {
  # Data validation
  if(!exists("FullData")) {
    stop("Required data not found. Please ensure data_prep.R runs successfully.")
  }
  
  # Setup app metadata (timestamps, last updated info)
  setupAppMetadata(output)
  
  # Initialize the centralized filter manager
  filter_mgr <- create_filter_manager()
  
  # Create reactive contexts for both data tables and visualizations
  tables_context <- create_filter_context(filter_mgr, input, "tables")
  viz_context <- create_filter_context(filter_mgr, input, "viz")
  
  # Initialize all server components
  initializeServerComponents(input, output, session, filter_mgr, tables_context, viz_context)
}