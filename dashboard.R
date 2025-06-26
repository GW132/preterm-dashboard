library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(plotly)

# Define variable mapping
vars = c(
"Baby Ethnicity" = "ethnicity_group",
"DHB domicile" = "BABY_DHB_GROUP",
"Singleton/Multiple birth" = "PLURALITY",
"Parity" = "PARITY_COUNT",
"Maternal age" = "Mothers_AGE",
"Deprivation index" = "deprivation_group"
)

# Define UI
ui <- fluidPage(
titlePanel("Pre-term Metrics"),
# Set left-right layout
fluidRow(
# Left panel for selection boxes
column(
width = 3,
style = "height: 90vh; overflow-y: auto;",  # Set scrolling area
wellPanel(

h3("Covariate Selection"),

# Select row facet
selectInput("row_facet", "Select Row Facet:",
choices = c("None" = "None", vars),
selected = "None"),

# Select column facet
selectInput("col_facet", "Select Column Facet:",
choices = c("None" = "None", vars),
selected = "None"),

# Dynamic filters will be created in server

# Select gestational age range
sliderInput("gest_age", "Select GESTATIONAL AGE range:",
min = 20, max = 36, value = c(20, 36), step = 1,
post = " weeks"),

# Add date slider
sliderInput("date_range", "Select Date Range:",
min = as.Date("2008-07-01"), max = as.Date("2022-12-01"),
value = c(as.Date("2008-07-01"), as.Date("2022-12-01")),
timeFormat = "%Y-%m-%d"),

# Dynamic filter controls will be placed here
uiOutput("dynamic_filters")
)
),
# Right panel for plots
column(
width = 5,  # Set width of right panel
mainPanel(
h3("Preterm Overview"),
plotlyOutput("plot1", width="65vw", height="80vh")))))

# Define server
server <- function(input, output, session) {
# Load dataset
babyinfo <- as.data.frame(readRDS("/srv/data/baby.rds"))
babyinfo$date <- as.Date(paste0(babyinfo$date, "-01"), format = "%Y-%m-%d")

# Dynamically generate filter controls
output$dynamic_filters <- renderUI({
filter_controls <- lapply(names(vars), function(label) {
var <- vars[label]
choices <- c("All", levels(babyinfo[[var]]))
selectInput(inputId = var, label = label, choices = choices, selected = "All")
})

do.call(tagList, filter_controls)
})

# Create filtered dataset
filtered_data <- reactive({
# Ensure basic inputs are valid
req(input$gest_age)

# Initialize filtered data
d_filtered <- babyinfo

# Filter by date range
d_filtered <- d_filtered %>%
dplyr::filter(date >= input$date_range[1] & date <= input$date_range[2])

# Set factor levels for ethnicity
d_filtered$ethnicity_group <- factor(d_filtered$ethnicity_group,
levels = c("Māori", "Pacific Peoples", "Indian", "Other Asian", "European/Other"))

# Apply filter for each variable
for (label in names(vars)) {
var <- vars[label]
# Check if input is defined (ensure radioButtons are rendered)
if (!is.null(input[[var]]) && input[[var]] != "All") {
d_filtered <- d_filtered[d_filtered[[var]] == input[[var]], ]
}
}
return(d_filtered)
})

# Custom colors
colors <- c("#1F77B4",  # Blue
"#FF7F0E",  # Orange
"#2CA02C",  # Green
"#D62728",  # Red
"#9467BD",  # Purple
"#8C564B")  # Brown

# Render plot
output$plot1 <- renderPlotly(tryCatch({
# Get filtered data
babyinfo_filtered <- filtered_data()

# Filter preterm data
s <- !is.na(babyinfo_filtered$GESTATIONAL_AGE) &
(babyinfo_filtered$GESTATIONAL_AGE < 37 &
babyinfo_filtered$GESTATIONAL_AGE >= input$gest_age[1] &
babyinfo_filtered$GESTATIONAL_AGE <= input$gest_age[2])

# Prepare faceting variables
row_var <- input$row_facet
col_var <- input$col_facet

# Format gestational age range label
gest_age_range_label <- paste(" >= ", input$gest_age[1], " to ", input$gest_age[2], " weeks", sep = "")

# Create appropriate plot based on facet selection
if (row_var == "None" & col_var == "None") {
# No facets - show overall plot
original <- babyinfo_filtered
mi <- xtabs(~ date, original[s, ])
original_xtabs <- xtabs(~ date, original)

df_mi <- as.data.frame(mi)
df_total <- as.data.frame(original_xtabs)
colnames(df_mi) <- c("date", "preterm_count")
colnames(df_total) <- c("date", "total_count")

# Merge and calculate statistics
f <- merge(df_mi, df_total, by = "date", all = TRUE) %>%
mutate(
proportion = preterm_count / total_count,
date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"),
mean = mean(proportion, na.rm = TRUE),
roll_preterm = rollsum(as.numeric(preterm_count), k = 12, fill = NA),
roll_total = rollsum(as.numeric(total_count), k = 12, fill = NA),
roll_prop = roll_preterm / roll_total,
running_mean = rollmean(as.numeric(proportion), k = 12, fill = NA),
se = sqrt(roll_prop * (1 - roll_prop) / roll_total),
lower = pmax(0, roll_prop - 1.96 * se),
upper = pmin(1, roll_prop + 1.96 * se)
)

# Plot without facets
p <- ggplot(f, aes(x = date, y = proportion * 100)) +
geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
geom_point(size = 1, alpha = 0.2) +
geom_line(aes(y = running_mean * 100)) +
labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = "")) +
scale_y_continuous(limits = c(0, max(f$proportion * 100, na.rm = TRUE))) +
theme_minimal(base_size = 20) +
theme(legend.position = "none")

} else if (row_var != "None" & col_var == "None") {
# Only row facet
original <- babyinfo_filtered[, c("date", row_var)]
original[[row_var]] <- factor(original[[row_var]])

# Calculate preterm rate
mi <- xtabs(as.formula(paste("~ date +", row_var)), original[s, ])
original_xtabs <- xtabs(as.formula(paste("~ date +", row_var)), original)
df_mi <- as.data.frame(mi)
df_total <- as.data.frame(original_xtabs)
names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
names(df_total)[names(df_total) == "Freq"] = "total_count"

# Merge and calculate statistics
f <- merge(df_mi, df_total, by = c("date", row_var), all = TRUE) %>%
group_by(across(row_var)) %>%
dplyr::filter(preterm_count >= 5) %>%
mutate(
proportion = as.numeric(preterm_count / total_count),
date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"),
mean = mean(proportion, na.rm = TRUE),
roll_preterm = rollsum(as.numeric(preterm_count), k = 12, fill = NA),
roll_total = rollsum(as.numeric(total_count), k = 12, fill = NA),
roll_prop = roll_preterm / roll_total,
running_mean = rollmean(proportion, k = 12, fill = NA),
se = sqrt(roll_prop * (1 - roll_prop) / roll_total),
lower = pmax(0, roll_prop - 1.96 * se),
upper = pmin(1, roll_prop + 1.96 * se)
)

# Plot with row facets
p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[row_var]])) +
geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
geom_point(size = 1, alpha = 0.2) +
geom_line(aes(y = running_mean * 100)) +
labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = row_var) +
facet_wrap(as.formula(paste("~", row_var)), nrow = 1) +
scale_color_manual(values = colors) +
scale_y_continuous(limits = c(0, max(f$proportion *100))) +
theme_minimal(base_size = 15) +
theme(legend.position = "none")

} else if (row_var == "None" & col_var != "None" | row_var == col_var) {
# Only column facet or row and column are the same
original <- babyinfo_filtered[, c("date", col_var)]
original[[col_var]] <- factor(original[[col_var]])

# Calculate preterm rate
mi <- xtabs(as.formula(paste("~ date +", col_var)), original[s, ])
original_xtabs <- xtabs(as.formula(paste("~ date +", col_var)), original)
df_mi <- as.data.frame(mi)
df_total <- as.data.frame(original_xtabs)
names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
names(df_total)[names(df_total) == "Freq"] = "total_count"

# Merge and calculate statistics
f <- merge(df_mi, df_total, by = c("date", col_var), all = TRUE) %>%
group_by(across(col_var)) %>%
dplyr::filter(preterm_count >= 5) %>%
mutate(
proportion = preterm_count / total_count,
date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"),
mean = mean(proportion, na.rm = TRUE),
roll_preterm = rollsum(as.numeric(preterm_count), k = 12, fill = NA),
roll_total = rollsum(as.numeric(total_count), k = 12, fill = NA),
roll_prop = roll_preterm / roll_total,
running_mean = rollmean(proportion, k = 12, fill = NA),
se = sqrt(roll_prop * (1 - roll_prop) / roll_total),
lower = pmax(0, roll_prop - 1.96 * se),
upper = pmin(1, roll_prop + 1.96 * se)
)

# Plot with column facets
p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[col_var]])) +
geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
geom_point(size = 1, alpha = 0.2) +
geom_line(aes(y = running_mean * 100)) +
labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = col_var) +
facet_wrap(as.formula(paste("~", col_var)), nrow = 1) +
scale_color_manual(values = colors) +
scale_y_continuous(limits = c(0, max(f$proportion *100))) +
theme_minimal(base_size = 15) +
theme(legend.position = "none")

} else if (row_var != "None" & col_var != "None") {
# Both row and column facets
original <- babyinfo_filtered[, c("date", row_var, col_var)]
original[[row_var]] <- factor(original[[row_var]])
original[[col_var]] <- factor(original[[col_var]])

# Calculate preterm rate
mi <- xtabs(as.formula(paste("~ date +", row_var, "+", col_var)), original[s, ])
original_xtabs <- xtabs(as.formula(paste("~ date +", row_var, "+", col_var)), original)
df_mi <- as.data.frame(mi)
df_total <- as.data.frame(original_xtabs)
names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
names(df_total)[names(df_total) == "Freq"] = "total_count"

# Merge and calculate statistics
f <- merge(df_mi, df_total, by = c("date", row_var, col_var), all = TRUE) %>%
group_by(across(c(row_var, col_var))) %>%
dplyr::filter(preterm_count >= 5) %>%
mutate(
proportion = preterm_count / total_count,
date = as.Date(paste0(date, "-01"), format = "%Y-%m-%d"),
mean = mean(proportion, na.rm = TRUE),
roll_preterm = rollsum(as.numeric(preterm_count), k = 12, fill = NA),
roll_total = rollsum(as.numeric(total_count), k = 12, fill = NA),
roll_prop = roll_preterm / roll_total,
running_mean = rollmean(proportion, k = 12, fill = NA),
se = sqrt(roll_prop * (1 - roll_prop) / roll_total),
lower = pmax(0, roll_prop - 1.96 * se),
upper = pmin(1, roll_prop + 1.96 * se)
)

# Plot with row and column facets
p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[col_var]])) +
geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
geom_point(size = 1, alpha = 0.2) +
geom_line(aes(y = running_mean * 100), size = 0.6) +
labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = "Interaction") +
facet_grid(as.formula(paste(row_var, "~", col_var))) +
scale_color_manual(values = colors) +
scale_y_continuous(limits = c(0, max(f$proportion *100))) +  # Set y-axis to start from 0 and auto-adjust the maximum value
theme_minimal(base_size = 15) +
theme(legend.position = "none")
} else {
return(NULL)  # If no facets are selected, return NULL
}

ggplotly(p)
}, error = function(e) add_text(plot_ly(), 0, 0, text = as.character(e))))
}

# Run Shiny app
shinyApp(ui = ui, server = server)
