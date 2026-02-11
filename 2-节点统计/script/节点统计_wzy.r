# install.packages("ggtree")
# remotes::install_github("DavidSchott/ggstream")
# install.packages("treeio")
# install.packages("ggnewscale")
library(ape)
library(ggplot2)
library(ggstream)
library(cols4all)
library(remotes)
library(shiny)
library(patchwork)
#颜色选择
c4a_series()
c4a_palettes()
c4a_gui()
c4a_gui
brewer_palettes <- c4a_palettes(series = "viridis")
print(brewer_palettes)

palette <- c4a("cividis", n = 10)
palette

c4a_plot("magma", n = 100)


# 设置工作目录
setwd("D:/F支系/投稿文件20241121-Science Advance/修稿1/节点饱和度")
setwd("C:/Users/王智勇/Desktop/feng")
setwd("/mnt/c/Users/Administrator/Desktop/Hui_Beast")
tree1 <- read.nexus("3亿_2.7.7_Xibei_Hui_1722_re20_final.trees")
tree2 <- read.tree("O1-1.txt")
tree3 <- read.tree("N-1.txt")
tree4 <- read.tree("C2-1.txt")


tree1 <- read.tree("o2.nwk")
tree2 <- read.tree("o1.nwk")
tree3 <- read.tree("n.nwk")
tree4 <- read.tree("c.nwk")
tree5 <- read.tree("O2a1a.txt")
tree6 <- read.tree("O2a2a.txt")
tree7 <- read.tree("O2a2b2.txt")
tree8 <- read.tree("O2b.txt")
tree9 <- read.tree("O1a.txt")
tree10 <- read.tree("O1b.txt")
tree11 <- read.tree("C.txt")
tree12 <- read.tree("Q.txt")
tree13 <- read.tree("R.txt")
tree14 <- read.tree("N.txt")
tree15 <- read.tree("D.txt")

#规定区间统计
max_distance <- 70000  # 只考虑25000年内的变化
distance_bins <- seq(0, max_distance, by = 1000)
calculate_node_distribution <- function(tree, distance_bins) {
  node_count <- Nnode(tree)
  cat("Total number of internal nodes:", node_count, "\n")
  
  tree_height <- max(node.depth.edgelength(tree))  # 树的总高度
  internal_nodes <- (length(tree$tip.label) + 1):(length(tree$tip.label) + Nnode(tree))
  
  # 计算每个内部节点到现代tip的距离
  distances_to_present <- tree_height - node.depth.edgelength(tree)[internal_nodes]
  
  binned_counts <- cut(distances_to_present, breaks = distance_bins, right = FALSE, include.lowest = TRUE)
  binned_counts_table <- table(binned_counts)
  
  distance_labels <- as.numeric(sapply(strsplit(sub("\\[|\\(", "", names(binned_counts_table)), ","), `[`, 1))
  node_counts <- as.numeric(binned_counts_table)
  
  data <- data.frame(Distance = distance_labels, NodeCount = node_counts)
  data_filtered <- subset(data, Distance <= max_distance)
  return(data_filtered)
}


#winidow统计
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
  
  # 每个内部节点的“距今时间”
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
  

# 分别计算两棵树的节点分布
data1 <- calculate_node_distribution(tree1, distance_bins)
data2 <- calculate_node_distribution(tree2, distance_bins)
data3 <- calculate_node_distribution(tree3, distance_bins)
data4 <- calculate_node_distribution(tree4, distance_bins)
data5 <- calculate_node_distribution(tree5, distance_bins)
data6 <- calculate_node_distribution(tree6, distance_bins)
data7 <- calculate_node_distribution(tree7, distance_bins)
data8 <- calculate_node_distribution(tree8, distance_bins)
data9 <- calculate_node_distribution(tree9, distance_bins)
data10 <- calculate_node_distribution(tree10, distance_bins)
data11 <- calculate_node_distribution(tree11, distance_bins)
data12 <- calculate_node_distribution(tree12, distance_bins)
data13 <- calculate_node_distribution(tree13, distance_bins)
data14 <- calculate_node_distribution(tree14, distance_bins)
data15 <- calculate_node_distribution(tree15, distance_bins)

data1$Tree <- "Tree 1"
data2$Tree <- "Tree 2"
data3$Tree <- "Tree 3"
data4$Tree <- "Tree 4"
data5$Tree <- "Tree 5"
data6$Tree <- "Tree 6"
data7$Tree <- "Tree 7"
data8$Tree <- "Tree 8"
data9$Tree <- "Tree 9"
data10$Tree <- "Tree 10"
data11$Tree <- "Tree 11"
data12$Tree <- "Tree 12"
data13$Tree <- "Tree 13"
data14$Tree <- "Tree 14"
data15$Tree <- "Tree 15"

#
library(dplyr)
data0 <- bind_rows(
  data1, data2, data3, data4,
  data5, data6, data7, data8,
  data9, data10, data11, data12,
  data13, data14
)
data0 <- aggregate(NodeCount ~ Distance, data = data0, FUN = sum)
data0 <- data0 %>% 
  mutate(Tree = "Tree 0") %>%          # 新增列
  select(Distance, NodeCount, Tree)    # 确保列顺序




data_combined1 <- rbind(data1, data2,data3, data4, data5,data6,data7,data8,data9,data10,data11,data12,data13,data14,data15)

data_combined1 <- rbind(data0,data9,data10,data11,data12,data13,data14,data15)
 
data11 <- rbind(data1, data2,data3, data4, data5,data6,data7,data8)
data_combined1 <- rbind(data0,data9,data10,data11,data12,data13,data14,data15)

data_combined1 <- rbind(data1,data2,data3, data4)
data_combined2 <- rbind(data3, data4, data5)
data_combined3 <- rbind(data6, data7, data8)

data_combined1 <- rbind(data1, data2)
#都在上面
ggplot(data1, aes(x = Distance, y = NodeCount, fill = Tree)) +
  geom_stream(type = "ridge", bw = 0.55,  linewidth = 0.1) +  # 使用 geom_stream 绘制河流图
 #ggalt::geom_xspline(aes(x = Distance, y = NodeCount, group = Tree), color = "#0073C2FF", size = 1, spline_shape = -0.4) +  # 在中间添加原始数据的样条曲线
  #geom_text(aes(label = NodeCount), position = position_stack(vjust = 0.5), size = 3, color = "black") +  # 标记节点数量和位置
  scale_fill_manual(values = c("Tree 1" = "#0073C2FF", "Tree 2" = "#FC4E07", 
                               "Tree 3" = "#FC0E07", "Tree 4" = "#FC9E07",
                               "Tree 5" = "#007C32FF","Tree 6" = "#00224E",
                               "Tree 7" = "#78a236","Tree 8" = "#65c8cc","Tree 9" = "#f3878d","Tree 10" = "#c87323",
                               "Tree 11" = "#611912","Tree 12" = "#c6519f","Tree 13" = "#716db2","Tree 14" = "#30499c","Tree 15" = "#fad031")) +
  labs(title = "Internal Node Distribution Across Two Trees",
       x = "Distance from Present (in units of 1000 years)",
       y = "Number of Internal Nodes") +
  theme_minimal(base_size = 15) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(color = "black"),
    panel.grid.major = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(limits = c(0, 10000), breaks = seq(0, 10000, by = 1000))
  
  #xlim(0, 25000)