# Y染色体节点统计分析 - 最终无警告版本
# 适用于CNS期刊投稿的高质量可视化

# 加载必需包
suppressPackageStartupMessages({
  library(ape)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(ggridges)
  library(forcats)
  library(cols4all)
})

# 设置工作目录
setwd("F:/回族项目/Beast/节点统计")

# 定义所有树文件 (统一使用.nwk格式)
tree_files <- c("C.nwk", "J.nwk", "N.nwk", "Q.nwk",
                "R.nwk", "O1.nwk", "O2.nwk", "Others.nwk")

# 读取所有存在的树文件
trees <- list()
tree_names <- c()

for (file in tree_files) {
  if (file.exists(file)) {
    tree <- read.tree(file)
    trees[[length(trees) + 1]] <- tree
    tree_names <- c(tree_names, gsub("\\.nwk$", "", file))
    cat("Loading file:", file, "\n")
  }
}

if (length(trees) == 0) {
  stop("No tree files found! Please check the file path.")
}

cat("Successfully loaded", length(trees), "tree files\n")

# Tree structure diagnostic function
diagnose_tree_structure <- function() {
  cat("\n=== Tree File Structure Diagnosis ===\n")
  
  for (i in seq_along(trees)) {
    tree_name <- tree_names[i]
    tree <- trees[[i]]
    
    cat("\n", tree_name, "tree structure:\n")
    cat("  - Tip nodes:", length(tree$tip.label), "\n")
    cat("  - Internal nodes:", Nnode(tree), "\n")
    cat("  - Tree height:", round(max(node.depth.edgelength(tree)), 2), "\n")
    
    if (tree_name == "Others") {
      others_tips <- tree$tip.label
      cat("  - Others contains:", length(others_tips), "samples\n")
    }
    
    if (tree_name == "C") {
      c_tips <- tree$tip.label
      cat("  - C contains:", length(c_tips), "samples\n")
    }
  }
}

# 执行诊断
diagnose_tree_structure()

# 节点分布计算函数
calculate_node_distribution <- function(tree, distance_bins) {
  node_count <- Nnode(tree)
  tree_height <- max(node.depth.edgelength(tree))
  internal_nodes <- (length(tree$tip.label) + 1):(length(tree$tip.label) + Nnode(tree))
  
  distances_to_present <- tree_height - node.depth.edgelength(tree)[internal_nodes]
  
  binned_counts <- cut(distances_to_present, breaks = distance_bins, right = FALSE, include.lowest = TRUE)
  binned_counts_table <- table(binned_counts)
  
  distance_labels <- as.numeric(sapply(strsplit(sub("\\[|\\(", "", names(binned_counts_table)), ","), `[`, 1))
  node_counts <- as.numeric(binned_counts_table)
  
  data <- data.frame(Distance = distance_labels, NodeCount = node_counts)
  return(data)
}

# 设置分析参数
max_distance <- 70000  
distance_bins <- seq(0, max_distance, by = 200)

# 计算所有树的节点分布
data_list <- list()
for (i in seq_along(trees)) {
  data <- calculate_node_distribution(trees[[i]], distance_bins)
  data$Tree <- tree_names[i]
  data_list[[i]] <- data
}

# 合并数据
data_combined <- do.call(rbind, data_list)

# 🔧 修复堆叠顺序：按总节点数排序，最大的在顶层
tree_totals <- data_combined %>%
  group_by(Tree) %>%
  summarise(total_nodes = sum(NodeCount), .groups = "drop") %>%
  arrange(desc(total_nodes))  # 从大到小排序，大的在上层

# 设置因子顺序：最大的（Others）在最顶层，最小的在最底层
data_combined$Tree <- factor(data_combined$Tree, 
                            levels = tree_totals$Tree)

cat("📊 Stream plot stacking order (bottom to top):\n")
for(i in length(tree_totals$Tree):1) {
  tree_name <- tree_totals$Tree[i]
  total <- tree_totals$total_nodes[tree_totals$Tree == tree_name]
  layer_pos <- length(tree_totals$Tree) - i + 1
  cat(sprintf("  %d. %s (total nodes: %d)\n", layer_pos, tree_name, total))
}

# 美学优化的配色方案 - Others为其他类别
cns_colors <- c(
  # 单个单倍群 - 使用柔和的单色调
  "C" = "#E24E42",     # 深红色 - 古老谱系
  "J" = "#3CB371",     # 深蓝色 - 中东欧洲
  "N" = "#F3C623",     # 中蓝色 - 北欧亚
  "Q" = "#1F9ED3",     # 浅蓝色 - 美洲中亚
  "R" = "#D65DB1",     # 橙红色 - 欧亚大陆
  "O1" = "#2E5AAC",    # 浅橙色 - O分支1
  "O2" = "#F58A07",    # 明黄色 - O分支2
  
  # Others - 其他类别，使用灰色调
  "Others" = "#a7a3a1"    # 深灰色 - 其他类别
)


actual_colors <- cns_colors[tree_names]
names(actual_colors) <- tree_names

# 方案1: 河流图 ====================================================
suppressWarnings({
  if (requireNamespace("ggstream", quietly = TRUE)) {
    p_stream <- ggplot(data_combined %>% filter(Distance <= 60000), 
                       aes(x = Distance, y = NodeCount, fill = Tree)) +
      ggstream::geom_stream(type = "ridge", bw = 0.55, linewidth = 0.15, alpha = 0.85) +
      scale_fill_manual(values = actual_colors, name = "Y-chromosome\nHaplogroups") +
      labs(title = "Temporal Stream Analysis of Y-chromosome Coalescence",
           subtitle = "Flow visualization of phylogenetic node distribution",
           x = "Time before present (years)", 
           y = "Number of internal nodes") +
      theme_classic(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey40", face = "italic"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, color = "black"),
        legend.title = element_text(size = 11, face = "bold"),
        legend.position = "right",
        panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = "black", linewidth = 0.5)
      ) +
      scale_x_continuous(
        limits = c(0, 60000), 
        breaks = seq(0, 60000, by = 3000),
        labels = function(x) paste0(x/1000, "k")
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.02, 0.05)))
  } else {
    # 备用面积图
    p_stream <- ggplot(data_combined %>% filter(Distance <= 60000), 
                       aes(x = Distance, y = NodeCount, fill = Tree)) +
      geom_area(alpha = 0.7, position = "identity") +
      scale_fill_manual(values = actual_colors, name = "Y-chromosome\nHaplogroups") +
      labs(title = "Temporal Area Analysis of Y-chromosome Coalescence",
           x = "Time before present (years)", y = "Number of internal nodes") +
      theme_classic(base_size = 12) +
      scale_x_continuous(breaks = seq(0, 60000, by = 3000),
                        labels = function(x) paste0(x/1000, "k"))
  }
})

# 显示河流图
print(p_stream)

# Save stream plot
cat("\nSaving stream plot files...\n")

# 保存PDF版本（期刊投稿用）
ggsave("Y_chromosome_stream_analysis.pdf", plot = p_stream, 
       width = 12, height = 8, dpi = 300, device = "pdf")

# 保存PNG版本（演示用）
ggsave("Y_chromosome_stream_analysis.png", plot = p_stream, 
       width = 12, height = 8, dpi = 300, device = "png")

# 方案2: 时间窗口柱状图+平滑线组合图 ===============================
cat("\nGenerating time window bar chart + smooth line combination plot...\n")

# 设置时间窗口参数（200年一个窗口）
window_size <- 200  # 200年窗口
max_time <- 12000   # 分析到12000年前
time_breaks <- seq(0, max_time, by = window_size)

# 为每个单倍群创建时间窗口统计
create_windowed_data <- function(data, breaks) {
  windowed_list <- list()
  
  for (tree_name in unique(data$Tree)) {
    tree_data <- data %>% filter(Tree == tree_name)
    
    # 为每个时间窗口统计节点数
    window_stats <- data.frame(
      TimeWindow = breaks[-length(breaks)],  # 窗口起始时间
      Tree = tree_name,
      NodeSum = NA,
      NodeMean = NA,
      NodeMax = NA
    )
    
    for (i in 1:(length(breaks)-1)) {
      start_time <- breaks[i]
      end_time <- breaks[i+1]
      
      window_data <- tree_data %>% 
        filter(Distance >= start_time & Distance < end_time)
      
      if (nrow(window_data) > 0) {
        window_stats$NodeSum[i] <- sum(window_data$NodeCount, na.rm = TRUE)
        window_stats$NodeMean[i] <- mean(window_data$NodeCount, na.rm = TRUE)
        window_stats$NodeMax[i] <- max(window_data$NodeCount, na.rm = TRUE)
      } else {
        window_stats$NodeSum[i] <- 0
        window_stats$NodeMean[i] <- 0
        window_stats$NodeMax[i] <- 0
      }
    }
    
    windowed_list[[tree_name]] <- window_stats
  }
  
  return(do.call(rbind, windowed_list))
}

# 创建窗口化数据
windowed_data <- create_windowed_data(data_combined, time_breaks)

# 保留所有单倍群（与河流图一致）
major_trees <- unique(windowed_data$Tree)

windowed_major <- windowed_data

# 精美组合图：柱状图+趋势线 - 期刊级别美化
p_combo <- ggplot(windowed_major, aes(x = TimeWindow, y = NodeSum)) +
  
  # 柱状图 - 适度透明度
  geom_col(aes(fill = Tree), alpha = 0.6, width = window_size * 0.7, 
           position = position_dodge(width = window_size * 0.75)) +
  
  # 简洁趋势线 - 细而优雅
  geom_smooth(aes(color = Tree), method = "loess", span = 0.7, se = FALSE, 
              linewidth = 0.5, alpha = 0.9) +
  
  # 高端配色方案 - 增强对比度
  scale_fill_manual(values = actual_colors[major_trees], 
                    name = "Y-chromosome Haplogroups") +
  scale_color_manual(values = actual_colors[major_trees], 
                     name = "Y-chromosome Haplogroups") +
  
  # 专业标签 - Science/Nature风格
  labs(title = "Temporal Dynamics of Y-chromosome Phylogenetic Diversity",
       subtitle = paste0("Coalescence node distribution | Time bins: ", window_size, " years | Loess smoothing with 95% span"),
       x = "Time before present (years)", 
       y = "Coalescence events per time window",
       caption = "Data: BEAST phylogenetic analysis | Visualization: Custom R pipeline") +
  # Nature/Science级别主题设计
  theme_classic(base_size = 13, base_family = "") +
  theme(
    # 高端标题设计
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                             margin = margin(b = 5), color = "grey15"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey50", 
                                margin = margin(b = 15), lineheight = 1.2),
    plot.caption = element_text(size = 9, color = "grey60", hjust = 1,
                               margin = margin(t = 10)),
    
    # 精致轴设计
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12), 
                               color = "grey20"),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 12), 
                               color = "grey20"),
    axis.text.x = element_text(size = 10, color = "grey30", margin = margin(t = 5)),
    axis.text.y = element_text(size = 10, color = "grey30", margin = margin(r = 5)),
    axis.line = element_line(color = "grey40", linewidth = 0.5),
    axis.ticks = element_line(color = "grey40", linewidth = 0.4),
    axis.ticks.length = unit(0.2, "cm"),
    
    # 现代图例设计
    legend.title = element_text(size = 11, face = "bold", color = "grey20"),
    legend.text = element_text(size = 10, color = "grey30"),
    legend.position = "bottom",
    legend.box.spacing = unit(0.3, "cm"),
    legend.margin = margin(t = 12, b = 8),
    legend.key.size = unit(0.4, "cm"),
    legend.key.width = unit(1.2, "cm"),
    
    # 精致网格系统
    panel.grid.major.x = element_line(color = "grey90", linewidth = 0.3, linetype = "solid"),
    panel.grid.major.y = element_line(color = "grey90", linewidth = 0.3, linetype = "solid"),
    panel.grid.minor.x = element_line(color = "grey95", linewidth = 0.2, linetype = "dotted"),
    panel.grid.minor.y = element_blank(),
    
    # 专业背景设计
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "grey98", color = NA),
    panel.border = element_rect(fill = NA, color = "grey70", linewidth = 0.5),
    plot.margin = margin(15, 20, 25, 15)
  ) +
  # 专业轴设计
  scale_x_continuous(
    name = "Time before present (years)",
    breaks = seq(0, max_time, by = 3000),
    minor_breaks = seq(0, max_time, by = 1000),
    labels = function(x) ifelse(x == 0, "Present", paste0(x/1000, "k")),
    limits = c(-300, max_time + 300),
    expand = c(0.01, 0)
  ) +
  scale_y_continuous(
    name = "Coalescence events per time window",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::comma_format(),
    breaks = scales::pretty_breaks(n = 6),
    minor_breaks = waiver()
  ) +
  # 现代图例设计
  guides(
    fill = guide_legend(
      title = "Y-chromosome Haplogroups",
      nrow = 3, 
      byrow = TRUE,
      title.position = "top",
      title.hjust = 0.5,
      override.aes = list(alpha = 0.8)
    ),
    color = "none"  # 隐藏线条图例，避免重复警告
  )

# 显示组合图
print(p_combo)

# 保存组合图表 - 修复设备警告和尺寸问题
ggsave("Y_chromosome_window_combo.pdf", plot = p_combo, 
       width = 16, height = 10, dpi = 300, device = "pdf")
ggsave("Y_chromosome_window_combo.png", plot = p_combo, 
       width = 16, height = 10, dpi = 300, device = "png")

# 方案3: 1000年窗口百分比分析 ===============================
cat("\nGenerating 1000-year window percentage analysis plot...\n")

# Set fine window parameters
fine_window_size <- 1000  # 1000-year window
fine_max_time <- 12000   # Analyze up to 12000 years ago
fine_time_breaks <- seq(0, fine_max_time, by = fine_window_size)

cat("Fine window analysis parameters:\n")
cat("  - Window size:", fine_window_size, "years\n")
cat("  - Analysis range: 0-", fine_max_time, "years BP\n")
cat("  - Total windows:", length(fine_time_breaks)-1, "\n")

# 创建500年窗口数据
fine_windowed_data <- create_windowed_data(data_combined, fine_time_breaks)

# 百分比计算 - 按窗口归一化到100%
cat("\n=== 500年窗口百分比计算（规范版）===\n")

fine_percentage_data <- fine_windowed_data %>%
  group_by(TimeWindow) %>%
  mutate(
    WindowTotal = sum(NodeSum, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  filter(WindowTotal > 0, NodeSum > 0, TimeWindow < fine_max_time) %>%
  mutate(
    Percentage = (NodeSum / WindowTotal) * 100,
    Tree = factor(Tree, levels = levels(data_combined$Tree)),
  WindowStart = TimeWindow,
    WindowEnd = pmin(TimeWindow + fine_window_size, fine_max_time),
  WindowLabel = sprintf("%.1f-%.1fk", WindowStart/1000, WindowEnd/1000),
    Total = WindowTotal
  ) %>%
  select(TimeWindow = WindowStart, WindowEnd, WindowLabel, Tree, NodeSum, Total, Percentage)

expected_starts <- seq(0, fine_max_time - fine_window_size, by = fine_window_size)
expected_labels <- sprintf("%.1f-%.1fk", expected_starts/1000, pmin(expected_starts + fine_window_size, fine_max_time)/1000)

fine_percentage_data <- fine_percentage_data %>%
  mutate(WindowLabel = factor(WindowLabel, levels = expected_labels)) %>%
  filter(!is.na(WindowLabel)) %>%
  arrange(WindowLabel, Tree)

window_labels <- levels(fine_percentage_data$WindowLabel)
cat("窗口标签总数:", length(window_labels), "\n")
if(length(window_labels) > 0) {
  cat("窗口范围:", window_labels[1], "→", window_labels[length(window_labels)], "\n")
} else {
  cat("⚠ 未获取到有效窗口标签，请检查数据。\n")
}

unexpected_labels <- setdiff(window_labels, expected_labels)
missing_labels <- setdiff(expected_labels, window_labels)

if(length(unexpected_labels) > 0) {
  cat("❌ 检测到超出设定范围的窗口标签:", paste(unexpected_labels, collapse = ", "), "\n")
}

if(length(missing_labels) > 0) {
  cat("⚠ 下列窗口无有效数据(不绘制柱状):", paste(missing_labels, collapse = ", "), "\n")
}

if(any(fine_percentage_data$WindowEnd > fine_max_time)) {
  cat("❌ 检测到超出", fine_max_time, "年的窗口，已自动截断。\n")
}

cat("处理结果（只包含有数据的单倍群）：\n")
cat("  - 有数据的时间窗口数量:", length(unique(fine_percentage_data$TimeWindow)), "\n")
cat("  - 总数据点数:", nrow(fine_percentage_data), "\n")
cat("  - 涉及的单倍群:", length(unique(fine_percentage_data$Tree)), "\n")
cat("  - 逻辑：每个窗口仅保留真实存在的单倍群，并归一化为100%\n")

# 数据质量验证 - 新逻辑版本
cat("\n=== 数据质量验证（可变柱高模式）===\n")

# 检查每个时间窗口的数据情况
percentage_check <- fine_percentage_data %>%
  group_by(TimeWindow) %>%
  summarise(
    haplogroup_count = n(),  # 该窗口实际有数据的单倍群数量
    percentage_sum = sum(Percentage),
    total_nodes = first(Total),
    .groups = "drop"
  )

cat("数据概览（只统计有数据的单倍群）：\n")
cat("  - 有数据的时间窗口数量:", nrow(percentage_check), "\n")
cat("  - 每窗口单倍群数量范围:", range(percentage_check$haplogroup_count), "\n")
cat("  - 百分比总和:", unique(round(percentage_check$percentage_sum, 2)), "\n")
cat("  - 节点数范围:", range(percentage_check$total_nodes), "\n")

# 显示每个窗口的详细情况
cat("\n各时间窗口详细情况：\n")
for(i in 1:min(5, nrow(percentage_check))) {  # 显示前5个窗口作为示例
  window_info <- percentage_check[i, ]
  cat(sprintf("  %d-%d年: %d个单倍群, %d个节点, 百分比和=%.1f%%\n", 
              window_info$TimeWindow, 
              window_info$TimeWindow + fine_window_size,
              window_info$haplogroup_count,
              window_info$total_nodes,
              window_info$percentage_sum))
}
if(nrow(percentage_check) > 5) {
  cat("  ... (更多窗口)\n")
}

# 检查是否有异常的百分比值
cat("\n异常值检查：\n")
abnormal_pct <- fine_percentage_data[fine_percentage_data$Percentage < 0 | fine_percentage_data$Percentage > 100, ]
if(nrow(abnormal_pct) > 0) {
  cat("❌ 发现异常百分比值：\n")
  print(abnormal_pct[, c("TimeWindow", "Tree", "NodeSum", "Total", "Percentage")])
} else {
  cat("✅ 所有百分比值都在0-100%范围内\n")
}

cat("\n✅ 新逻辑：仅保留真实单倍群，百分比严格归一化到100%\n")

# 全面数据检查
cat("\n=== 全面数据检查 ===\n")
cat("时间窗口数量:", length(levels(fine_percentage_data$WindowLabel)), " (", min(fine_percentage_data$TimeWindow), "-", max(fine_percentage_data$WindowEnd), "年)\n")
cat("百分比范围:", range(round(fine_percentage_data$Percentage, 2)), "\n")
cat("总节点数范围:", range(fine_percentage_data$Total), "\n")

if(any(fine_percentage_data$Percentage < 0 | fine_percentage_data$Percentage > 100)) {
  cat("❌ 存在异常百分比，需检查数据处理！\n")
} else {
  cat("✅ 所有百分比均在0-100%之间\n")
}

cat("✅ 超简化逻辑：500年窗口按实际节点计算占比，默认归一化到100%\n")

# 专门调试8000年窗口
cat("\n=== 8000年窗口专项调试 ===\n")
debug_8k <- fine_percentage_data %>% filter(TimeWindow == 8000)
if(nrow(debug_8k) > 0) {
  cat("8000年窗口数据详情：\n")
  print(debug_8k[, c("WindowLabel", "Tree", "NodeSum", "Total", "Percentage")])
  cat("\n验证：\n")
  cat("- 百分比总和:", sum(debug_8k$Percentage), "%\n")
  cat("- 节点总数:", unique(debug_8k$Total), "\n")
  cat("- 单倍群数量:", nrow(debug_8k), "\n")
  
  # 检查数据完整性
  if(any(is.na(debug_8k$Percentage))) {
    cat("❌ 发现NA值！\n")
  } else {
    cat("✅ 数据完整，无NA值\n")
  }
} else {
  cat("❌ 8000年窗口数据不存在！\n")
  
  # 检查原始窗口数据
  cat("\n检查原始窗口数据中的8000年：\n")
  original_8k <- fine_windowed_data %>% filter(TimeWindow == 8000)
  if(nrow(original_8k) > 0) {
    cat("原始8000年窗口数据：\n")
    print(original_8k[, c("TimeWindow", "Tree", "NodeSum")])
  } else {
    cat("原始数据中也没有8000年窗口\n")
  }
}

# 绘图前最终数据检查
cat("\n=== 绘图前数据检查 ===\n")
cat("总数据点数:", nrow(fine_percentage_data), "\n")
cat("8000年窗口数据点数:", nrow(fine_percentage_data[fine_percentage_data$TimeWindow == 8000, ]), "\n")

# 创建8000年窗口的简单测试图
test_8k_data <- fine_percentage_data[fine_percentage_data$TimeWindow == 8000, ]
if(nrow(test_8k_data) > 0) {
  cat("8000年窗口测试数据:\n")
  print(test_8k_data[, c("WindowLabel", "Tree", "Percentage")])
  
  # 简单测试图
  test_plot <- ggplot(test_8k_data, aes(x = WindowLabel, y = Percentage, fill = Tree)) +
    geom_col(position = "stack") +
    labs(title = "8000年窗口测试") +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 100))
  
  cat("8000年窗口测试图生成...\n")
  ggsave("test_8k_window.png", test_plot, width = 8, height = 6)
  cat("测试图保存为: test_8k_window.png\n")
}

# 准备显示每个窗口的总节点数
window_total_labels <- fine_percentage_data %>%
  distinct(WindowLabel, Total) %>%
  mutate(label = paste0("n=", Total)) %>%
  mutate(WindowLabel = factor(WindowLabel, levels = expected_labels))

# 创建500年窗口百分比堆叠柱状图
p_fine_percentage <- ggplot(fine_percentage_data, 
            aes(x = WindowLabel, y = Percentage, fill = Tree)) +

  # 百分比堆叠柱状图 - 每个窗口独立归一化
  geom_col(position = "stack", alpha = 0.9, color = "white", linewidth = 0.15, 
     width = 0.85, na.rm = TRUE) +
  geom_text(data = window_total_labels, aes(x = WindowLabel, y = 102, label = label),
      inherit.aes = FALSE, size = 3, color = "grey15", fontface = "bold", vjust = 0) +
  
  # 统一配色方案
  scale_fill_manual(values = actual_colors, name = "Y-chromosome\nHaplogroups") +
  
  # 专业标题设计
  labs(title = "Y-chromosome Haplogroup Composition Analysis (500-year Windows)",
    subtitle = paste0("Stacked bar chart | Windows: ", fine_window_size, 
            " years | 0-", fine_max_time/1000, "k years BP | 100% normalized per window"),
    x = "Time window (k years before present)", 
    y = "Relative composition (%)",
    caption = paste0("Data: BEAST coalescence analysis | ", fine_window_size, "-year windows | Each bar sums to 100%")) +
  
  # 顶级期刊美学设计
  theme_minimal(base_size = 13) +
  theme(
    # 专业标题设计
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, 
                             margin = margin(b = 8), color = "grey10"),
    plot.subtitle = element_text(size = 11, hjust = 0.5, color = "grey45", 
                                margin = margin(b = 16), lineheight = 1.3),
    plot.caption = element_text(size = 9, color = "grey55", hjust = 1,
                               margin = margin(t = 12)),
    
    # 精致轴设计
  axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 12), 
                 color = "grey15"),
  axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 12), 
                               color = "grey15"),
  axis.text.x = element_text(size = 9, color = "grey25", angle = 45, hjust = 1),
    axis.text.y = element_text(size = 10, color = "grey25"),
    
    # 现代图例设计
    legend.title = element_text(size = 11, face = "bold", color = "grey15"),
    legend.text = element_text(size = 10, color = "grey25"),
    legend.position = "right",
    legend.margin = margin(l = 15),
    legend.key.size = unit(0.5, "cm"),
    
    # 精致网格系统
    panel.grid.major.x = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "grey88", linewidth = 0.3),
    panel.grid.minor.x = element_line(color = "grey94", linewidth = 0.2),
    panel.grid.minor.y = element_line(color = "grey94", linewidth = 0.2),
    
    # 专业背景设计
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(15, 20, 15, 15)
  ) +
  
  # 轴刻度设置
  scale_x_discrete(limits = expected_labels, drop = FALSE) +
  scale_y_continuous(
    name = "Relative composition (%)",
    breaks = seq(0, 100, by = 20),
    minor_breaks = seq(0, 100, by = 10),
    limits = c(0, 105),
    labels = function(x) paste0(x, "%"),
    expand = c(0, 0)
  ) +
  
  # 专业图例设计
  guides(
    fill = guide_legend(
      title = "Y-chromosome\nHaplogroups",
      title.position = "top",
      title.hjust = 0.5,
      ncol = 1,
      override.aes = list(alpha = 0.9, color = "white", linewidth = 0.2)
    )
  )

# 显示500年窗口百分比堆叠柱状图
print(p_fine_percentage)

# 保存500年窗口百分比分析图
cat("\nSaving 500-year window percentage stacked bar chart files...\n")

# 保存PDF版本（期刊投稿用）
ggsave("Y_chromosome_500yr_percentage_stacked.pdf", plot = p_fine_percentage, 
       width = 16, height = 9, dpi = 300, device = "pdf")

# 保存PNG版本（演示用）
ggsave("Y_chromosome_500yr_percentage_stacked.png", plot = p_fine_percentage, 
       width = 16, height = 9, dpi = 300, device = "png")

# 方案4: 500年分辨率 LOESS 平滑百分比堆积面积图 ===============================
cat("\nGenerating 500-year resolution LOESS-smoothed percentage stacked area plot...\n")

# 1. 500年窗口聚合
area_window_size <- 500
area_time_breaks <- seq(0, fine_max_time, by = area_window_size)
area_windowed <- create_windowed_data(data_combined, area_time_breaks)

## 2. 自适应 LOESS 拟合（避免在 mutate 中存模型对象）
loess_span <- 0.5

# 诊断：各单倍群节点总数
hap_totals <- area_windowed %>% group_by(Tree) %>% summarise(Total = sum(NodeSum), NonZeroWindows = sum(NodeSum>0), .groups="drop")
cat("  • Haplogroup aggregation summary:\n")
for(i in seq_len(nrow(hap_totals))) {
  cat(sprintf("    - %s: total=%d | non-zero windows=%d\n", hap_totals$Tree[i], hap_totals$Total[i], hap_totals$NonZeroWindows[i]))
}

fit_list <- lapply(split(area_windowed, area_windowed$Tree), function(df){
  df <- df[order(df$TimeWindow), ]
  if(all(df$NodeSum == 0)) {
    df$LoessFit <- 0
    return(df)
  }
  model <- loess(NodeSum ~ TimeWindow, data = df, span = loess_span, degree = 2, family = "gaussian")
  df$LoessFit <- pmax(predict(model, df$TimeWindow), 0)
  df
})
loess_smoothed <- do.call(rbind, fit_list) %>% filter(TimeWindow < fine_max_time)

## 3. 插值到更细时间步长
interp_step <- 100
interp_points <- seq(0, fine_max_time, by = interp_step)
interp_list <- lapply(split(area_windowed, area_windowed$Tree), function(df){
  df <- df[order(df$TimeWindow), ]
  if(all(df$NodeSum == 0)) {
    return(tibble(Tree = df$Tree[1], TimeWindow = interp_points, LoessFit = 0))
  }
  model <- loess(NodeSum ~ TimeWindow, data = df, span = loess_span, degree = 2, family = "gaussian")
  tibble(Tree = df$Tree[1], TimeWindow = interp_points, LoessFit = pmax(predict(model, interp_points), 0))
})
loess_interpolated <- do.call(rbind, interp_list) %>% filter(TimeWindow < fine_max_time)

## 4. 构建用于绘图的归一化数据
loess_interpolated_valid <- loess_interpolated %>%
  group_by(TimeWindow) %>%
  mutate(WindowTotal = sum(LoessFit, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(WindowTotal > 0) %>%
  mutate(WindowMid = TimeWindow, Tree = factor(Tree, levels = levels(data_combined$Tree)))

cat("  • Interpolated steps with data:", length(unique(loess_interpolated_valid$TimeWindow)),
    " (every ", interp_step, " years) | LOESS span=", loess_span, "\n", sep="")

# 4. 绘图：position='fill' 保证窗口 100% 堆叠
p_percentage_area <- ggplot(loess_interpolated_valid,
                            aes(x = WindowMid, y = LoessFit, fill = Tree)) +
  geom_area(position = "fill", alpha = 0.9, colour = "white", linewidth = 0.18) +
  scale_fill_manual(values = actual_colors, name = "Y-chromosome\nHaplogroups") +
  labs(title = "LOESS-smoothed Y-chromosome Haplogroup Composition",
  subtitle = paste0("500-year aggregation + 100-year interpolation | LOESS span=", loess_span, " | 100% stacked"),
       x = "Time before present (years)",
       y = "Relative composition (%)",
       caption = "LOESS smoothing per haplogroup on 500-year aggregated counts; normalized per window via stacking fill") +
  scale_x_continuous(
    limits = c(0, fine_max_time),
    breaks = seq(0, fine_max_time, by = 1000),
    minor_breaks = seq(0, fine_max_time, by = 500),
    labels = function(x) ifelse(x == 0, "Present", paste0(x/1000, "k"))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, by = 0.2),
    labels = function(x) paste0(x*100, "%"),
    expand = c(0, 0)
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 11, hjust = 0.5, colour = "grey45"),
    plot.caption = element_text(size = 9, colour = "grey55", hjust = 1, margin = margin(t = 8)),
    axis.title.x = element_text(size = 12, face = "bold", margin = margin(t = 10)),
    axis.title.y = element_text(size = 12, face = "bold", margin = margin(r = 10)),
    axis.text = element_text(size = 10, colour = "grey25"),
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_percentage_area)

ggsave("Y_chromosome_500yr_percentage_area.pdf", plot = p_percentage_area,
       width = 16, height = 9, dpi = 300, device = "pdf")
ggsave("Y_chromosome_500yr_percentage_area.png", plot = p_percentage_area,
       width = 16, height = 9, dpi = 300, device = "png")

# 5. 导出 LOESS 平滑百分比数据
percentage_area_export <- loess_interpolated_valid %>%
  group_by(TimeWindow) %>%
  mutate(WindowTotal = sum(LoessFit), Percentage = LoessFit / WindowTotal * 100) %>%
  ungroup() %>%
  select(TimeWindow, WindowMid, Tree, LoessFit, Percentage)
write.csv(percentage_area_export, "Y_chromosome_500yr_percentage_area_data.csv", row.names = FALSE)

# 保存数据
write.csv(data_combined, "Y_chromosome_coalescence_data.csv", row.names = FALSE)
write.csv(windowed_major, "Y_chromosome_window_data.csv", row.names = FALSE)
write.csv(fine_percentage_data, "Y_chromosome_500yr_percentage_data.csv", row.names = FALSE)

# 生成分析报告
cat("\n" , rep("=", 75), "\n")
cat("🎨 Y-chromosome Phylogenetic Analysis Complete!\n")
cat(rep("=", 75), "\n")
cat("📊 生成的可视化文件：\n")
cat("\n🌊 1. Stream plot (absolute values): Y_chromosome_stream_analysis.pdf/png\n")
cat("    • Features: Continuous temporal flow, stacked areas showing overall trends\n")
cat("    • Colors: CNS journal standard color scheme\n")
cat("    • Values: Display absolute node counts for each haplogroup\n")
cat("\n📊 2. Combination plot (Bar + Trend lines): Y_chromosome_window_combo.pdf/png\n")
cat("    • Features:", window_size, "-year window bars + smooth trend lines\n")
cat("    • Colors: Consistent color scheme with stream plot\n")
cat("    • Design: CNS journal-level aesthetics, professional layout\n")
cat("    • Function: Precise quantity comparison and evolutionary pattern analysis\n")
cat("\n📊 3. 500-year window percentage stacked bars: Y_chromosome_500yr_percentage_stacked.pdf/png ⭐⭐⭐⭐\n")
cat("    • Features: 500-year window stacked bar chart (", fine_window_size, " years), normalized to 100% per window\n")
cat("    • Time range: 0-", fine_max_time/1000, "k years BP, 24 windows\n")
cat("    • Data processing: Only haplogroups with actual nodes, proportions within each window\n")
cat("    • Function: Clear visualization of relative haplogroup dominance patterns over time\n")
cat("    • Advantages: Clean 100% stacks without artificial padding; retains only observed haplogroups\n")
cat("    • Annotation: 'n=' labels show total internal nodes per 500-year window\n")
cat("\n📈 4. LOESS-smoothed percentage area chart: Y_chromosome_500yr_percentage_area.pdf/png\n")
cat("    • Resolution: 500-year bins (aligned with bars)\n")
cat("    • Smoothing: LOESS span=0.5 per haplogroup (adaptive, preserves signal)\n")
cat("    • Function: Continuous proportional dynamics complementing discrete 500-year composition bars\n")
cat("    • Advantages: Reduces jitter without imposing artificial window averaging; robust to sparse segments\n")
cat("\n� Data files:\n")
cat("    • Y_chromosome_coalescence_data.csv (complete time series - absolute values)\n")
cat("    • Y_chromosome_window_data.csv (", window_size, "-year window statistics - absolute values)\n")
cat("    • Y_chromosome_500yr_percentage_data.csv (", fine_window_size, "-year window - balanced resolution percentage data)\n")
cat("\n🎯 Recommended usage:\n")
cat("    • Stream plot: Overall overview and evolutionary trends\n") 
cat("    • Bar + trend combination: Precise quantity comparison\n")
cat("    • 500-year percentage bars: Discrete composition analysis ⭐⭐⭐⭐\n")
cat("    • 500-year LOESS area: Continuous proportional trends (adaptive smoothing)\n")
cat(rep("=", 75), "\n")

# Display data overview
cat("\nData overview:\n")
cat("Analyzed haplogroups:", paste(tree_names, collapse = ", "), "\n")
cat("Time range: 0-70,000 years BP\n")
cat("Visualization time range: 0-12,000 years BP\n")
cat("Total data points:", nrow(data_combined), "\n")

# 显示各单倍群节点统计
haplogroup_summary <- data_combined %>%
  group_by(Tree) %>%
  summarise(
    total_nodes = sum(NodeCount),
    max_nodes = max(NodeCount),
    peak_time = Distance[which.max(NodeCount)],
    .groups = "drop"
  ) %>%
  arrange(desc(total_nodes))

cat("\nHaplogroup statistics summary:\n")
print(haplogroup_summary)

# Data overview check
cat("\n=== Data Overview ===\n")
others_total <- haplogroup_summary$total_nodes[haplogroup_summary$Tree == "Others"]
c_total <- haplogroup_summary$total_nodes[haplogroup_summary$Tree == "C"]

if (length(others_total) > 0) {
  cat("Others total nodes:", others_total, "\n")
}
if (length(c_total) > 0) {
  cat("C total nodes:", c_total, "\n")
}

# Haplogroup node overview
cat("✅ Each haplogroup analyzed as independent component\n")

# Haplogroup node distribution overview
cat("\n=== Haplogroup Node Distribution Overview ===\n")
if (nrow(data_combined) > 0) {
  # 显示各单倍群在不同时间段的节点分布
  time_periods <- c(
    "0-2000 years BP" = c(0, 2000),
    "2000-5000 years BP" = c(2000, 5000), 
    "5000-10000 years BP" = c(5000, 10000),
    "Above 10000 years BP" = c(10000, Inf)
  )
  
  for (period_name in names(time_periods)) {
    period_range <- time_periods[[period_name]]
    period_data <- data_combined %>%
      filter(Distance >= period_range[1] & Distance < period_range[2]) %>%
      group_by(Tree) %>%
      summarise(total_nodes = sum(NodeCount), .groups = "drop") %>%
      arrange(desc(total_nodes))
    
    if (nrow(period_data) > 0) {
      cat(sprintf("\n%s node distribution:\n", period_name))
      for (i in 1:nrow(period_data)) {
        cat(sprintf("  %s: %d nodes\n", period_data$Tree[i], period_data$total_nodes[i]))
      }
    }
  }
}

cat("\nAnalysis complete! All files saved to current working directory.\n")