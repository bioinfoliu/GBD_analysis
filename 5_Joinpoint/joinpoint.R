####joinpoint回归分析####
####第一步年龄标准化发病率与年龄标准化死亡率####


# 读取数据
TB_KOR <- read.csv('TB_KOR_GLOBAL.csv', header = TRUE) 

# 定义要循环的 location_name 和 measure_name
locations <- c("Global", "Republic of Korea")
measures <- c("Prevalence", "Incidence", "Deaths", "DALYs (Disability-Adjusted Life Years)")

# 循环遍历所有组合
for (location in locations) {
  for (measure in measures) {
    # 过滤数据
    data_subset <- subset(TB_KOR, TB_KOR$measure_name == measure & 
                            TB_KOR$age_name == "Age-standardized" & 
                            TB_KOR$location_name == location & 
                            TB_KOR$metric_name == 'Rate')
    
    # 计算标准误
    data_subset$SE <- (data_subset$upper - data_subset$lower) / (1.96 * 2)
    
    # 选择需要的列
    data_subset <- data_subset[, c("sex_name", "year", "val", "SE")]
    
    # 按 sex_name 和 year 排序
    data_subset <- data_subset[order(data_subset$sex_name, data_subset$year),]
    
    # 构建输出文件名
    output_file <- paste0(measure, "_joinpoint_", location, ".csv")
    
    # 保存为 CSV 文件
    write.csv(data_subset, output_file, row.names = FALSE)
    
    # 打印输出信息
    cat("Processed:", measure, "for", location, "\n")
  }
}






####第二步 调整发病率及可信区间的表示形式

# 定义文件名和输出文件名
files <- c('Incidence_KOR', 'Incidence_global', 'Prevalence_KOR', 'Prevalence_global', 'DALYs_KOR', 'DALYs_global', 'Deaths_KOR', 'Deaths_global')
types <- c('AAPC', 'APC')

# 定义一个函数来处理每个文件
process_file <- function(file, type) {
  # 构建输入和输出文件名
  input_file <- paste0(file, '.Export.', type, '.txt')
  output_file <- paste0(file, '_', type, '.csv')
  
  # 读取数据
  data <- read.table(input_file, header = TRUE)
  
  # 更改列名
  names(data)[6:8] <- c('val', 'lower', 'upper')
  
  # 保留两个小数点
  data$val <- sprintf("%0.2f", data$val)
  data$lower <- sprintf("%0.2f", data$lower)
  data$upper <- sprintf("%0.2f", data$upper)
  
  # 组合成一个新的列
  data[[type]] <- paste0(data$val, " (", data$lower, " - ", data$upper, ")")
  
  # 写入CSV文件
  write.csv(data, output_file, row.names = FALSE)
}

# 对每个文件和类型进行处理
for (file in files) {
  for (type in types) {
    process_file(file, type)
  }
}




