library(shiny)
library(ggplot2)
library(dplyr)
library(zoo)
library(plotly)

# 定义变量映射
vars = c(
  "Baby Ethnicity" = "ethnicity_group",
  "DHB domicile" = "BABY_DHB_GROUP",
  "Singleton/Multiple birth" = "PLURALITY",
  "Parity" = "PARITY_COUNT",
  "Maternal age" = "Mothers_AGE",
  "Deprivation index" = "deprivation_group"
)

# 定义 UI
ui <- fluidPage(
  titlePanel("Pre-term Metrics"),
  # 设定左右布局
  fluidRow(
    # 左边的选择框区域
    column(
      width = 3,   # 设置左侧面板的宽度
      style = "height: 90vh; overflow-y: auto;",  # 设置滚动区域
      wellPanel(
        
        h3("Covariate Selection"),
        
        # 选择行分面
        selectInput("row_facet", "Select Row Facet:", 
                    choices = c("None" = "None", vars),
                    selected = "None"),
        
        # 选择列分面
        selectInput("col_facet", "Select Column Facet:", 
                    choices = c("None" = "None", vars),
                    selected = "None"),
        
        # 动态生成过滤控件将在server中创建
        
        # 选择妊娠期范围
        sliderInput("gest_age", "Select GESTATIONAL AGE range:",
                    min = 20, max = 36, value = c(20, 36), step = 1,
                    post = " weeks"),
        
        # 添加日期滑块
        sliderInput("date_range", "Select Date Range:",
                    min = as.Date("2008-07-01"), max = as.Date("2022-12-01"),
                    value = c(as.Date("2008-07-01"), as.Date("2022-12-01")),
                    timeFormat = "%Y-%m-%d"),
        
        # 这里放置动态生成的过滤控件
        uiOutput("dynamic_filters")
      )
    ),
    # 右边的图表区域
    column(
      width = 5,  # 设置右侧面板的宽度
      mainPanel(
        h3("Preterm Overview"),
        plotlyOutput("plot1", width="65vw", height="80vh")
      )
    )
  )
)

# 定义服务器
server <- function(input, output, session) {
  # 加载数据集
  babyinfo <- as.data.frame(readRDS("/srv/data/baby.rds"))
  babyinfo$date <- as.Date(paste0(babyinfo$date, "-01"), format = "%Y-%m-%d")
  
  # 动态生成过滤控件
  output$dynamic_filters <- renderUI({
    filter_controls <- lapply(names(vars), function(label) {
      var <- vars[label]
      choices <- c("All", levels(babyinfo[[var]]))
      selectInput(inputId = var, label = label, choices = choices, selected = "All")
    })
    
    do.call(tagList, filter_controls)
  })
  
  # 创建过滤后的数据集
  filtered_data <- reactive({
    # 确保基本输入有效
    req(input$gest_age)
    
    # 初始化过滤数据
    d_filtered <- babyinfo
    
    # 按日期范围过滤
    d_filtered <- d_filtered %>%
      dplyr::filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    # 设置种族因子水平
    d_filtered$ethnicity_group <- factor(d_filtered$ethnicity_group, 
                                         levels = c("Māori", "Pacific Peoples", "Indian", "Other Asian", "European/Other"))
    
    # 对每个变量应用过滤器
    for (label in names(vars)) {
      var <- vars[label]
      # 检查输入是否已定义(确保radioButtons已渲染)
      if (!is.null(input[[var]]) && input[[var]] != "All") {
        d_filtered <- d_filtered[d_filtered[[var]] == input[[var]], ]
      }
    }
    
    return(d_filtered)
  })
  
  #自定义颜色
  colors <- c("#1F77B4",  # 蓝色
              "#FF7F0E",  # 橙色
              "#2CA02C",  # 绿色
              "#D62728",  # 红色
              "#9467BD",  # 紫色
              "#8C564B")  # 棕色
  
  # 渲染图表
  output$plot1 <- renderPlotly(tryCatch({
    # 获取过滤后的数据
    babyinfo_filtered <- filtered_data()
    
    # 过滤早产数据
    s <- !is.na(babyinfo_filtered$GESTATIONAL_AGE) & 
      (babyinfo_filtered$GESTATIONAL_AGE < 37 &
         babyinfo_filtered$GESTATIONAL_AGE >= input$gest_age[1] &
         babyinfo_filtered$GESTATIONAL_AGE <= input$gest_age[2])
    
    # 准备分面变量
    row_var <- input$row_facet
    col_var <- input$col_facet
    
    # 格式化妊娠期范围标签
    gest_age_range_label <- paste(" >= ", input$gest_age[1], " to ", input$gest_age[2], " weeks", sep = "")
    
    # 根据分面选择创建适当的图表
    if (row_var == "None" & col_var == "None") {
      # 无分面 - 显示整体图
      original <- babyinfo_filtered
      mi <- xtabs(~ date, original[s, ])
      original_xtabs <- xtabs(~ date, original)
      
      df_mi <- as.data.frame(mi)
      df_total <- as.data.frame(original_xtabs)
      colnames(df_mi) <- c("date", "preterm_count")
      colnames(df_total) <- c("date", "total_count")
      
      # 合并并计算统计量
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
      
      # 绘制无分面图表
      p <- ggplot(f, aes(x = date, y = proportion * 100)) +
        geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(aes(y = running_mean * 100)) +
        labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = "")) +
        scale_y_continuous(limits = c(0, max(f$proportion * 100, na.rm = TRUE))) +
        theme_minimal(base_size = 20) +
        theme(axis.title.y = element_text(size = 16)) +

        theme(legend.position = "none")
      
    } else if (row_var != "None" & col_var == "None") {
      # 仅行分面
      original <- babyinfo_filtered[, c("date", row_var)]
      original[[row_var]] <- factor(original[[row_var]])
      
      # 计算早产率
      mi <- xtabs(as.formula(paste("~ date +", row_var)), original[s, ])
      original_xtabs <- xtabs(as.formula(paste("~ date +", row_var)), original)
      df_mi <- as.data.frame(mi)
      df_total <- as.data.frame(original_xtabs)
      names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
      names(df_total)[names(df_total) == "Freq"] = "total_count"
      
      # 合并并计算统计量
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
      
      # 绘制行分面图
      p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[row_var]])) +
        geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(aes(y = running_mean * 100)) +
        labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = row_var) +
        facet_wrap(as.formula(paste("~", row_var)), nrow = 1) +
        scale_color_manual(values = colors) +
        scale_y_continuous(limits = c(0, max(f$proportion *100))) +
        theme_minimal(base_size = 15) +
        theme(axis.title.y = element_text(size = 16)) +
        
        theme(legend.position = "none")
      
    } else if (row_var == "None" & col_var != "None" | row_var == col_var) {
      # 仅列分面或行列相同
      original <- babyinfo_filtered[, c("date", col_var)]
      original[[col_var]] <- factor(original[[col_var]])
      
      # 计算早产率
      mi <- xtabs(as.formula(paste("~ date +", col_var)), original[s, ])
      original_xtabs <- xtabs(as.formula(paste("~ date +", col_var)), original)
      df_mi <- as.data.frame(mi)
      df_total <- as.data.frame(original_xtabs)
      names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
      names(df_total)[names(df_total) == "Freq"] = "total_count"
      
      # 合并并计算统计量
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
      
      # 绘制列分面图
      p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[col_var]])) +
        geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(aes(y = running_mean * 100)) +
        labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = col_var) +
        facet_wrap(as.formula(paste("~", col_var)), nrow = 1) +
        scale_color_manual(values = colors) +
        scale_y_continuous(limits = c(0, max(f$proportion *100))) +
        theme_minimal(base_size = 15) +
        theme(axis.title.y = element_text(size = 16)) +
        
        theme(legend.position = "none")
      
    } else if (row_var != "None" & col_var != "None") {
      # 行列分面
      original <- babyinfo_filtered[, c("date", row_var, col_var)]
      original[[row_var]] <- factor(original[[row_var]])
      original[[col_var]] <- factor(original[[col_var]])
      
      # 计算早产率
      mi <- xtabs(as.formula(paste("~ date +", row_var, "+", col_var)), original[s, ])
      original_xtabs <- xtabs(as.formula(paste("~ date +", row_var, "+", col_var)), original)
      df_mi <- as.data.frame(mi)
      df_total <- as.data.frame(original_xtabs)
      names(df_mi)[names(df_mi) == "Freq"] = "preterm_count"
      names(df_total)[names(df_total) == "Freq"] = "total_count"
      
      # 合并并计算统计量
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
      
      # 绘制行列分面图
      p <- ggplot(f, aes(x = date, y = proportion * 100, color = .data[[col_var]])) +
        geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.2, color = "lightblue") +
        geom_point(size = 1, alpha = 0.2) +
        geom_line(aes(y = running_mean * 100), size = 0.6) +
        labs(x = "Date", y = paste("Proportion of live births at ", gest_age_range_label, " (%)", sep = ""), color = "Interaction") +
        facet_grid(as.formula(paste(row_var, "~", col_var))) +
        scale_color_manual(values = colors) +
        scale_y_continuous(limits = c(0, max(f$proportion *100))) +  # Set y-axis to start from 0 and auto-adjust the maximum value
        theme(axis.title.y = element_text(size = 16)) +
        theme_minimal(base_size = 15) +
        theme(legend.position = "none")
    } else {
      return(NULL)  # 如果没有选择分面，返回 NULL
    }
    
    ggplotly(p)
  }, error = function(e) add_text(plot_ly(), 0, 0, text = as.character(e))))
}

# 运行 Shiny 应用
shinyApp(ui = ui, server = server)
