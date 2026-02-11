# ================================
# 系统发育树节点与分支统计分析脚本
# ================================

# 加载所需R包
library(ape)        # 用于读取NEXUS树文件
library(ggplot2)    # 用于绘图
library(dplyr)      # 用于数据处理
library(tibble)     # 用于构建数据框

# ========== 配置参数 ==========
tree_file <- "/mnt/c/Users/Administrator/Desktop/Hui_Beast/3亿_2.7.7_Xibei_Hui_1722_re20_final.trees"  # 输入系统发育树文件
output_csv <- "/mnt/c/Users/Administrator/Desktop/Hui_Beast/原始_Xibei_Hui_1722_node_summary.csv"  # 输出csv表格
bin_resolution <- 1000  # 年代分箱粒度（单位：年）
time_range <- c(20000,0) # 绘图x轴时间范围（从古至今）
# ==============================

# Step 1: 读取树并校验
if (!file.exists(tree_file)) stop("❌ 树文件不存在，请检查路径。")
tree <- read.nexus(tree_file)

# Step 2: 计算每个节点到根的距离（树的高度）
node_heights <- node.depth.edgelength(tree)
max_height <- max(node_heights)
divergence_times <- max_height - node_heights  # 将时间从根（古）到叶子（今）转为 BP

# Step 3: 获取内部节点的分化时间
n_tips <- length(tree$tip.label)
internal_nodes <- (n_tips + 1):(n_tips + tree$Nnode)
branch_times <- divergence_times[internal_nodes]

# Step 4: 按设定粒度对分化时间分箱
branch_df <- tibble(
  Divergence_Time = round(branch_times / bin_resolution) * bin_resolution
)

# Step 5: 汇总每个时间段新分支数，并计算累计值
summary_df <- branch_df %>%
  count(Divergence_Time, name = "New_Branches") %>%
  arrange(desc(Divergence_Time)) %>%
  mutate(
    Cumulative_Branches = cumsum(New_Branches),
    New_Nodes = New_Branches,
    Cumulative_Nodes = cumsum(New_Nodes)
  ) %>%
  arrange(Divergence_Time)

# Step 6: 保存结果为CSV
write.csv(summary_df, output_csv, row.names = FALSE)
cat("✅ 汇总数据已保存：", output_csv, "\n")

# Step 7: 通用绘图函数（可绘制柱状图或线图）
plot_metric <- function(data, x, y, title, ylab, color, geom_type = "line") {
  p <- ggplot(data, aes_string(x = x, y = y))
  if (geom_type == "hist") {
    p <- p + geom_col(fill = color, width = bin_resolution * 0.9)
  } else {
    p <- p + geom_line(color = color, size = 1.2)
  }
  p +
    scale_x_reverse(limits = time_range, expand = c(0, 0)) +
    labs(title = title,
         x = "分化时间（年 BP）",
         y = ylab) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12)
    )
}

# Step 8: 生成基础图（柱状或折线）
p1 <- plot_metric(summary_df, "Divergence_Time", "New_Branches",
                  "每时间段新增分支数", "新分支数", "forestgreen", "hist")
p2 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Branches",
                  "累计分支数", "累计分支", "darkgreen")
p3 <- plot_metric(summary_df, "Divergence_Time", "New_Nodes",
                  "每时间段新增节点数", "新节点数", "royalblue", "hist")
p4 <- plot_metric(summary_df, "Divergence_Time", "Cumulative_Nodes",
                  "累计节点数", "累计节点", "navy")

# Step 9: 打印图像
print(p1); print(p2); print(p3); print(p4)

# Step 10: 绘制归一化柱状图+折线图
normalize <- function(x) (x - min(x)) / (max(x) - min(x))

summary_df <- summary_df %>%
  mutate(
    norm_New_Branches = normalize(New_Branches),
    norm_Cumulative_Branches = normalize(Cumulative_Branches),
    norm_New_Nodes = normalize(New_Nodes),
    norm_Cumulative_Nodes = normalize(Cumulative_Nodes)
  )

p_combined_branch <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = norm_New_Branches), fill = "forestgreen", width = bin_resolution * 0.9) +
  geom_line(aes(y = norm_Cumulative_Branches), color = "darkgreen", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "新增分支数（归一化）",
    sec.axis = sec_axis(~., name = "累计分支数（归一化）")
  ) +
  labs(title = "新增 vs 累计分支（归一化）", x = "分化时间（年 BP）") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "forestgreen"),
    axis.title.y.right = element_text(color = "darkgreen")
  )

p_combined_node <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = norm_New_Nodes), fill = "royalblue", width = bin_resolution * 0.9) +
  geom_line(aes(y = norm_Cumulative_Nodes), color = "navy", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "新增节点数（归一化）",
    sec.axis = sec_axis(~., name = "累计节点数（归一化）")
  ) +
  labs(title = "新增 vs 累计节点（归一化）", x = "分化时间（年 BP）") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "royalblue"),
    axis.title.y.right = element_text(color = "navy")
  )

print(p_combined_branch); print(p_combined_node)

# Step 11: 非归一化双轴图（更真实的比例）
branch_scale_factor <- max(summary_df$Cumulative_Branches) / max(summary_df$New_Branches)
node_scale_factor <- max(summary_df$Cumulative_Nodes) / max(summary_df$New_Nodes)

p_combined_branch <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = New_Branches), fill = "forestgreen", width = bin_resolution * 0.9) +
  geom_line(aes(y = Cumulative_Branches / branch_scale_factor), color = "darkgreen", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "新增分支数",
    sec.axis = sec_axis(~ . * branch_scale_factor, name = "累计分支数")
  ) +
  labs(title = "新增 vs 累计分支", x = "分化时间（年 BP）") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "forestgreen"),
    axis.title.y.right = element_text(color = "darkgreen")
  )

p_combined_node <- ggplot(summary_df, aes(x = Divergence_Time)) +
  geom_col(aes(y = New_Nodes), fill = "royalblue", width = bin_resolution * 0.9) +
  geom_line(aes(y = Cumulative_Nodes / node_scale_factor), color = "navy", size = 1.2) +
  scale_x_reverse(limits = time_range, expand = c(0, 0)) +
  scale_y_continuous(
    name = "新增节点数",
    sec.axis = sec_axis(~ . * node_scale_factor, name = "累计节点数")
  ) +
  labs(title = "新增 vs 累计节点", x = "分化时间（年 BP）") +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 15, face = "bold"),
    axis.title.y.left = element_text(color = "royalblue"),
    axis.title.y.right = element_text(color = "navy")
  )

print(p_combined_branch); print(p_combined_node)
