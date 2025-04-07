library(readxl)
library(ggplot2)
library(dplyr)
library(janitor)
library(shiny)
library(shinythemes)

#Part 1: Import and clean data 
knitr::opts_knit$set(root.dir = '~/Downloads/Field Project/Assignment 1') 
df3 <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Data")
df2 <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values")

#extract data
df <- df3[c("q2", "q5", "q5a", "q5b", "q5c", "q5d", "q6", "q9", "q11", "q12", "q13", "q14", "q25")]
rm(df3)
summary(df)

#match data with value from sheet 2
values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B15:C19", col_names = c("q2", "q2_text"))
df=merge(df,values, by="q2", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B24:C28", col_names = c("q5", "q5_text"))
df=merge(df,values, by="q5", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B29:C33", col_names = c("q5a", "q5a_text"))
df=merge(df,values, by="q5a", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B34:C36", col_names = c("q5b", "q5b_text"))
df=merge(df,values, by="q5b", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B37:C50", col_names = c("q5c", "q5c_text"))
df=merge(df,values, by="q5c", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B51:C61", col_names = c("q5d", "q5d_text"))
df=merge(df,values, by="q5d", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B62:C66", col_names = c("q6", "q6_text"))
df=merge(df,values, by="q6", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B107:C111", col_names = c("q9", "q9_text"))
df=merge(df,values, by="q9", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B367:C369", col_names = c("q11", "q11_text"))
df=merge(df,values, by="q11", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B372:C375", col_names = c("q12", "q12_text"))
df=merge(df,values, by="q12", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B376:C386", col_names = c("q14", "q14_text"))
df=merge(df,values, by="q14", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B387:C390", col_names = c("q13", "q13_text"))
df=merge(df,values, by="q13", all.x=T)

values <- read_excel("VF_US_National_JUL19_RawData-1.xlsx", sheet = "Values", range = "B425:C434", col_names = c("q25", "q25_text"))
df=merge(df,values, by="q25", all.x=T)

rm(values)

#remove the column without the Values
df <- df[, !names(df) %in% c("q2", "q5", "q5a", "q5b", "q5c", "q5d", "q6", "q9", "q11", "q12", "q13", "q14", "q25")]

#dealing with missing value, replace NA with Inapplicable
df[is.na(df)] <- "Inapplicable"

#remove the numbers before the text i.e. (1),(2),.....
df[] <- lapply(df, function(x) gsub("^\\([0-9]+\\) ", "", x))

View(df)


#Part 2: App building

ui=fluidPage(
  
  theme=shinytheme("yeti"),
  
  titlePanel("GoDaddy Survey Dashboard"),
  
  sidebarLayout(
    sidebarPanel(width=3, height=3,
                 selectInput("purpose", "Select Website Purpose:", choices=c("Commercial", "Civic", "Community","Personal","Other")),
                 selectInput("importance", "Website Importance", choices=c("Very important", "Important", "Not very important", "Not important at all")),
                 sliderInput("recommend", "GoDaddy Recommendation Score (0-10):", min = 0, max = 10, value = 1),
                 radioButtons("duration", "Select Duration Hosted:", choices=c("Less than 1 year","1-3 years","4-5 years","6-10 years","Over 10 years"))),
  
  mainPanel(
      fluidRow(
        column(6, tableOutput("CategoryTable"))), 
      
      fluidRow(
        column(6, plotOutput("VisitorCount")), 
        column(6, plotOutput("BusinessSize"))),
      
      fluidRow(
        column(6, plotOutput("stackbar")), 
        column(6, plotOutput("barchart")))
    )
)
)


#page 
server=function(input, output, session){

  # Q1: Count and percentage table for purpose of use
  output$CategoryTable <- renderTable({
    
    # Filter for commercial purposes
    purpose_data <- df %>% filter(q5_text == input$purpose)
    
    # Summarize the counts and percentages
    summary_table <- purpose_data %>%
      group_by(q5_text) %>% 
      summarize(Count = n()) %>%
      mutate(Percentage = round((Count / 2006) * 100, 2)) %>%
      arrange(desc(Count))
  
    summary_table <- bind_rows(summary_table)
    
    # Rename columns for the output table
    colnames(summary_table) <- c("Usage purpose", "Count", "Percentage of Total (%)")
    
    summary_table
  })
  
  
  
  #Q2: Visitor count base on the purpose of use
  output$VisitorCount = renderPlot({
    filtered_df <- df %>%
      filter(q5_text == input$purpose) %>%
      filter(q6_text != "Inapplicable") %>%
      mutate(
        q6_text = factor(
          q6_text,
          levels = c("1-10", "11-100", "101-1000", "1,001-10,000", "More than 10,000")
        )
      )
    
    # Create the bar chart
    ggplot(filtered_df, aes(x = q6_text, fill = q5_text)) + 
      geom_bar(position = "dodge") +  
      labs(
        title = "Visitor Count vs. User Category",
        x = "Visitor Count",
        y = "Count",
        fill = "User Category"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "right"
      )
  })
  
  
  
  #Q3: Business size and how it is related to website importance
  output$BusinessSize=renderPlot({
    filtered_df <- df %>%
      filter(q12_text == input$importance) %>%
      filter(q5a_text != "Inapplicable" & q12_text != "Inapplicable")
    
    # Create a grouped bar chart
    ggplot(filtered_df, aes(y = q5a_text, fill = input$importance)) +
      geom_bar(position = "dodge") +  # Create grouped bars
      labs(
        title = "Business Size vs. Website Importance",
        x = "Count",
        y = "Number of Employees (Business Size)",
        fill = "Website Importance"
      ) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 0, hjust = 1))  # Rotate x-axis labels for readability
  }) 
  
  
  
  #Q4: Portion of source of income for Top 7 commercial business categories based on likelihood to recommend
  output$stackbar=renderPlot({
    df_filtered <- df %>%
      filter(q11_text != "Inapplicable" & q5c_text != "Inapplicable") %>%
      mutate(q14_text = recode(q14_text, 
                               "0 Not at all likely" = "0", 
                               "10 Extremely likely" = "10"),
             q14_text = factor(q14_text, levels = 0:10))
    data <- df_filtered %>% filter(q14_text == input$recommend) 
    
    # Count the number of responses for each category and select the top 7
    top_categories <- data %>%
      count(q5c_text) %>%
      arrange(desc(n)) %>%
      slice_head(n = 7) %>%
      pull(q5c_text)
    
    # Filter the data to only include the top 7 categories
    filtered_top_data <- data %>% filter(q5c_text %in% top_categories)
    
    # Create the stacked bar chart
    ggplot(filtered_top_data, aes(x = q5c_text, fill = q11_text)) +
      geom_bar(position = "fill") + # Stack bar chart with proportions
      labs(
        title = "Income and Top 7 Business Categories (Recommendation Score = input)",
        x = "Business Category",
        y = "Proportion",
        fill = "Income Source"
      ) +
      scale_y_continuous(labels = scales::percent_format()) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )
  })
  
  
  
  #Q5: Likelihood to recommend based on duration of use
  output$barchart=renderPlot({
   df_filtered <- df %>%
     filter(q2_text == input$duration) %>%
     mutate(q14_text = recode(q14_text, 
                             "0 Not at all likely" = "0", 
                             "10 Extremely likely" = "10"),
           q14_text = factor(q14_text, levels = 0:10))
  
  # Create the plot
  ggplot(df_filtered, aes(x = q14_text, fill = input$duration)) +
    geom_bar(position = "dodge") +
    labs(title = "Likelihood of recommendation based on duration of use",
         x = "Recommendation",
         y = "Count",
         fill = "Website Hosting Duration") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
}

#put things together
shinyApp(ui, server)
