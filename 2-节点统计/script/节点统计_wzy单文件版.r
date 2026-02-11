# =============================================================================
# 系统发育树支系增长统计和可视化分析脚本（简化单文件版本）
# =============================================================================

# --- 配置区域：请在此处修改输入输出路径和参数 ---
INPUT_DIR <- "/mnt/c/Users/Administrator/Desktop/Hui_Beast"     # 输入文件夹路径
TREE_FILE <- "3亿_2.7.7_Xibei_Hui_1722_re20_final.trees"                           # 要分析的树文件名

# 分析参数详细说明：
max_distance <- 70000      # 分析的最大时间范围（年前）- 超过此时间的节点不参与统计
bin_size <- 200           # 时间分组粒度（年）- 每个时间区间的大小，值越小越精细
plot_max_distance <- 10000 # 图表显示范围（年前）- 只显示此时间范围内的数据
plot_break_interval <- 2000 # X轴刻度间隔（年）- 图表刻度线的间距

# 输出控制
EXPORT_DATA <- TRUE       # 是否输出详细的时间段节点数据到CSV文件
SHOW_SUMMARY <- TRUE      # 是否在控制台显示详细统计摘要

# =============================================================================

# --- 安装和加载R包 ---
library(ape)
library(ggplot2)
library(ggstream)
library(cols4all)
library(remotes)
library(shiny)
library(patchwork)
library(dplyr)
library(tools)

cat("所有R包已成功加载\n")

# --- 颜色选择工具（可选，用于查看可用颜色方案）---
# c4a_series()
# c4a_palettes()
# c4a_gui()
# brewer_palettes <- c4a_palettes(series = "viridis")
# print(brewer_palettes)
# palette <- c4a("cividis", n = 10)
# palette
# c4a_plot("magma", n = 100)

# --- 设置工作目录 ---
setwd(INPUT_DIR)
cat("工作目录设置为:", getwd(), "\n")

# =============================================================================
# 核心分析函数
# =============================================================================

# --- 计算节点时间分布函数（保持原有逻辑）---
# 功能说明：
# 1. 计算树的高度（从根到叶子的最大距离）
# 2. 找到所有内部节点（分支点）
# 3. 计算每个内部节点距今的时间 = 树高度 - 节点深度
# 4. 将节点按时间区间分组统计
calculate_node_distribution <- function(tree, distance_bins) {
  node_count <- Nnode(tree)
  cat("Total number of internal nodes:", node_count, "\n")
  
  # 树高度 = 从根到最远叶子的距离（代表总的进化时间）
  tree_height <- max(node.depth.edgelength(tree))  
  cat("Tree height (total evolutionary time):", tree_height, "\n")
  
  # 内部节点索引（不包括叶子节点）
  internal_nodes <- (length(tree$tip.label) + 1):(length(tree$tip.label) + Nnode(tree))
  
  # 关键计算：每个内部节点到现代的时间距离
  # 距今时间 = 树总高度 - 该节点的深度
  distances_to_present <- tree_height - node.depth.edgelength(tree)[internal_nodes]
  
  # 将连续的时间数据分组到离散的时间区间
  binned_counts <- cut(distances_to_present, breaks = distance_bins, right = FALSE, include.lowest = TRUE)
  binned_counts_table <- table(binned_counts)
  
  # 提取时间区间的起始值作为标签
  distance_labels <- as.numeric(sapply(strsplit(sub("\\[|\\(", "", names(binned_counts_table)), ","), `[`, 1))
  node_counts <- as.numeric(binned_counts_table)
  
  data <- data.frame(Distance = distance_labels, NodeCount = node_counts)
  data_filtered <- subset(data, Distance <= max_distance)
  return(data_filtered)
}

# --- 滑动窗口统计函数（可选功能）---
calculate_node_distribution_sliding <- function(
    tree,
    window_size = 200,
    step_size   = 100,
    max_distance = 70000
) {
  # 基本信息
  node_count <- Nnode(tree)
  cat("Total number of internal nodes:", node_count, "\n")
  
  # 树高与内部节点索引
  tree_height <- max(node.depth.edgelength(tree))
  internal_nodes <- (length(tree$tip.label) + 1):(length(tree$tip.label) + Nnode(tree))
  
  # 每个内部节点的"距今时间"
  distances_to_present <- tree_height - node.depth.edgelength(tree)[internal_nodes]
  
  # 过滤异常值
  distances_to_present <- distances_to_present[
    !is.na(distances_to_present) & distances_to_present >= 0 & distances_to_present <= max_distance
  ]
  
  # 滑动窗口起止
  starts <- seq(0, max_distance - window_size, by = step_size)
  ends   <- pmin(starts + window_size, max_distance)
  
  # 统计每个窗口的节点数
  node_counts <- vapply(seq_along(starts), function(i) {
    sum(distances_to_present >= starts[i] & distances_to_present < ends[i])
  }, integer(1))
  
  # 返回结果
  return(data.frame(Distance = starts, WindowEnd = ends, NodeCount = node_counts))
}

# =============================================================================
# 主要分析流程
# =============================================================================

# --- 读取树文件 ---
cat("正在读取树文件:", TREE_FILE, "\n")

# 检查文件扩展名，选择合适的读取方法
file_ext <- tools::file_ext(TREE_FILE)
if (file_ext == "trees") {
  # 对于 .trees 文件（BEAST输出），读取多树文件并选择最后一个树（通常是最优树）
  cat("检测到BEAST .trees文件，正在读取...\n")
  trees <- read.nexus(TREE_FILE)
  if (class(trees) == "multiPhylo") {
    tree1 <- trees[[length(trees)]]  # 选择最后一个树
    cat("从", length(trees), "个树中选择了最后一个树进行分析\n")
  } else {
    tree1 <- trees
  }
} else {
  # 对于其他格式文件
  tree1 <- read.tree(TREE_FILE)
}

cat("成功读取树文件，叶子节点数:", length(tree1$tip.label), "\n")

# --- 定义时间区间 ---
distance_bins <- seq(0, max_distance, by = bin_size)

# --- 计算节点分布 ---
cat("正在计算节点时间分布...\n")
data1 <- calculate_node_distribution(tree1, distance_bins)

# --- 添加树标识 ---
data1$Tree <- tools::file_path_sans_ext(TREE_FILE)

# --- 输出统计摘要和详细数据 ---
if (SHOW_SUMMARY) {
  cat("\n=== ANALYSIS SUMMARY ===\n")
  cat("File name:", TREE_FILE, "\n")
  cat("Time range analyzed:", min(data1$Distance), "-", max(data1$Distance), "years ago\n")
  cat("Total internal nodes:", sum(data1$NodeCount), "\n")
  cat("Peak node count:", max(data1$NodeCount), "\n")
  peak_time <- data1$Distance[which.max(data1$NodeCount)]
  cat("Peak time:", peak_time, "years ago\n")
  cat("Time bin size:", bin_size, "years\n")
  cat("Number of time bins:", nrow(data1), "\n")
}

# --- 输出详细的时间段节点数据 ---
if (EXPORT_DATA) {
  # 创建更详细的数据表
  detailed_data <- data1 %>%
    mutate(
      Time_Period_Start = Distance,
      Time_Period_End = Distance + bin_size,
      Time_Period_Label = paste0(Distance, "-", Distance + bin_size, " years ago"),
      Node_Count = NodeCount,
      Percentage = round(NodeCount / sum(NodeCount) * 100, 2)
    ) %>%
    select(Time_Period_Label, Time_Period_Start, Time_Period_End, Node_Count, Percentage) %>%
    arrange(Time_Period_Start)
  
  # 输出到控制台（前20行）
  cat("\n=== TIME PERIOD NODE DISTRIBUTION (Top 20 periods) ===\n")
  print(head(detailed_data, 20))
  
  # 保存到CSV文件
  output_csv <- paste0(tools::file_path_sans_ext(TREE_FILE), "_node_distribution.csv")
  write.csv(detailed_data, output_csv, row.names = FALSE)
  cat("\nDetailed data exported to:", output_csv, "\n")
  
  # 输出主要统计信息
  cat("\n=== KEY STATISTICS ===\n")
  cat("Mean nodes per time period:", round(mean(data1$NodeCount), 2), "\n")
  cat("Median nodes per time period:", median(data1$NodeCount), "\n")
  cat("Standard deviation:", round(sd(data1$NodeCount), 2), "\n")
  
  # 找出节点数最多的前5个时间段
  top_periods <- detailed_data %>%
    arrange(desc(Node_Count)) %>%
    head(5)
  
  cat("\n=== TOP 5 TIME PERIODS WITH MOST NODES ===\n")
  for(i in 1:nrow(top_periods)) {
    cat(i, ". ", top_periods$Time_Period_Label[i], ": ", 
        top_periods$Node_Count[i], " nodes (", top_periods$Percentage[i], "%)\n", sep="")
  }
}

# =============================================================================
# 可视化部分
# =============================================================================

# --- 生成支系增长图（英文版本）---
cat("\nGenerating visualization...\n")

# 计算关键统计数据用于标题
peak_time <- data1$Distance[which.max(data1$NodeCount)]
total_nodes <- sum(data1$NodeCount)
max_nodes <- max(data1$NodeCount)

# 创建主图
p <- ggplot(data1, aes(x = Distance, y = NodeCount)) +
  geom_area(fill = "#0073C2FF", alpha = 0.7, color = "#0073C2FF", linewidth = 1) +
  geom_line(color = "#0073C2FF", linewidth = 1.2) +
  geom_point(color = "#0073C2FF", size = 2) +
  labs(
    title = paste("Phylogenetic Tree Lineage Growth Analysis:", tools::file_path_sans_ext(TREE_FILE)),
    x = "Time Before Present (thousands of years ago)",
    y = "Number of Internal Nodes",
    subtitle = paste("Total nodes:", total_nodes, "| Peak:", max_nodes, "nodes at", peak_time, "years ago"),
    caption = paste("Analysis parameters: Time bins =", bin_size, "years | Max distance =", max_distance, "years")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "gray60"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray90"),
    panel.grid.minor = element_blank(),
    legend.position = "none"  # 单个文件不需要图例
  ) +
  scale_x_continuous(
    limits = c(0, plot_max_distance), 
    breaks = seq(0, plot_max_distance, by = plot_break_interval),
    labels = seq(0, plot_max_distance/1000, by = plot_break_interval/1000)
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(data1$NodeCount) * 1.1))

# 显示图形
print(p)

# --- 保存图形 ---
output_file <- paste0(tools::file_path_sans_ext(TREE_FILE), "_lineage_growth.pdf")
ggsave(output_file, plot = p, width = 12, height = 8, dpi = 300)
cat("Plot saved as:", output_file, "\n")

cat("\n=== ANALYSIS COMPLETED ===\n")
if (EXPORT_DATA) {
  cat("Data files generated:\n")
  cat("- Plot:", output_file, "\n")
  cat("- Data:", paste0(tools::file_path_sans_ext(TREE_FILE), "_node_distribution.csv"), "\n")
}

# =============================================================================
# 河流图风格可视化
# =============================================================================

# 河流图风格（使用geom_stream）
cat("\nGenerating stream plot...\n")

# 创建河流图
tree_name <- tools::file_path_sans_ext(TREE_FILE)
color_values <- "#0073C2FF"
names(color_values) <- tree_name

p_stream <- ggplot(data1, aes(x = Distance, y = NodeCount, fill = Tree)) +
  geom_stream(type = "ridge", bw = 0.55, linewidth = 0.1) +
  scale_fill_manual(values = color_values) +
  labs(
    title = paste("Internal Node Distribution:", tree_name),
    x = "Time Before Present (thousands of years ago)",
    y = "Number of Internal Nodes",
    subtitle = paste("Total nodes:", total_nodes, "| Peak:", max_nodes, "nodes at", peak_time, "years ago")
  ) +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 12),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(
    limits = c(0, plot_max_distance), 
    breaks = seq(0, plot_max_distance, by = plot_break_interval),
    labels = seq(0, plot_max_distance/1000, by = plot_break_interval/1000)
  )

# 显示和保存河流图
print(p_stream)
ggsave(paste0(tree_name, "_stream.pdf"), 
       plot = p_stream, width = 12, height = 8, dpi = 300)

cat("Stream plot saved as:", paste0(tree_name, "_stream.pdf"), "\n")

cat("\n=== 分析完成 ===\n")