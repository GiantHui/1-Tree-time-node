#####################################################
# 1.加载所需 R 包
#####################################################

library(dplyr)       # 数据处理与管道操作
library(data.table)  # 高效的数据读写
library(itol.toolkit)# 生成 iTOL 对应的数据单元
library(tidyr)       # 数据整理与表格旋转等操作
library(RColorBrewer)

#####################################################
# 2.设置工作目录、读入树和元数据
#####################################################
setwd("/mnt/c/Users/Administrator/Desktop")

# 树文件 (Newick)
tree_1 <- "cqHan_combine_20k.final.93K_R使用.trees"

# 创建一个 hub，用来管理该树的所有数据单元
hub_1  <- create_hub(tree_1)

# 元数据文件
data_file_1 <- "cqHan_Beast_metadata.txt"
data_1      <- data.table::fread(data_file_1)

#####################################################
# 3.为节点添加标签：按 Label 显示
#####################################################
unit_label <- create_unit(
  data = data_1 %>% select(ID, Label),
  key  = "itol_labels",
  type = "LABELS",
  tree = tree_1
)
write_unit(unit_label, paste0(getwd(), "/cqHan_labels.txt"))

#####################################################
# 4.按 Range 分类，为节点添加颜色（TREE_COLORS, subtype=range）
#####################################################
unit_range <- create_unit(
  data    = data_1 %>% select(ID, Range),
  key     = "itol_range",
  type    = "TREE_COLORS",
  subtype = "range",
  tree    = tree_1
)
write_unit(unit_range, paste0(getwd(), "/cqHan_range.txt"))

#####################################################
# 5.按 Branch 分类，为分支添加随机颜色
#####################################################
Branch_unique <- unique(data_1$Branch)   # 所有唯一的 Branch 值
set.seed(123)                            # 随机种子，保证可复现
colors <- sample(colors(), length(Branch_unique), replace = FALSE)
Branch_colors <- setNames(colors, Branch_unique)

# 将颜色与原数据绑定
data_1$Color <- Branch_colors[data_1$Branch]

# 明确使用 dplyr::select，避免函数冲突
data_selected <- dplyr::select(data_1, ID, Color)

unit_branch <- create_unit(
  data    = data_selected,
  key     = "itol_random_Branch_colors",
  type    = "TREE_COLORS",
  subtype = "branch",
  tree    = tree_1
)
write_unit(unit_branch, paste0(getwd(), "/cqHan_Branch.txt"))

#####################################################
# 6. 按Strip 分类，为节点添加颜色条带
#####################################################
unit_strip <- create_unit(
  data  = data_1 %>% select(ID, Strip),
  key   = "itol_strip",
  type  = "DATASET_COLORSTRIP",
  color = "wesanderson",   # 使用Wes Anderson调色板
  tree  = tree_1
)
# 设置条带的边缘空白
unit_strip@common_themes$basic_theme$margin <- 50
write_unit(unit_strip, paste0(getwd(), "/cqHan_strip.txt"))

#####################################################
# 7.按 Symbol 分类，为枝上添加形状
#####################################################
unit_symbol <- create_unit(
  data     = data_1 %>% select(ID, Symbol),
  key      = "Sample_Symbols",
  type     = "DATASET_SYMBOL",
  position = 1,    # 形状位置(数值越大离节点越远)
  size     = 2,    # 形状大小
  subtype  = "Symbol",
  tree     = tree_1,
  fill     = 1     # 填充方式
)
write_unit(unit_symbol, paste0(getwd(), "/Nanjing_Symbol.txt"))

#####################################################
# 8.按 Domain 分类，为节点添加“结构域”(DATASET_DOMAINS)示例
#####################################################
# 在元数据 data_1 中，假设每个 ID 仅有一个 Domain 值
# 若一个 ID 需要展示多个 Domain，可根据实际情况展开/拆分

# 8.1 为每种 Domain 分配一种形状
# iTOL 支持的 shape 如 “HH”, “HV”, “EL”, “DI”, “TR”, “TL”, “PL”, “R*”等
# 这里示例中仅做简单映射，如果 Domain 种类很多，可以自行拓展
domain_unique <- unique(data_1$Domain)
# 准备一批可用形状（数量必须 >= domain_unique 的长度）
all_shapes <- c("HH", "HV", "EL", "DI", "TR", "TL", "PL", 
                "SR","R2","R3","R4","R5","R6","R7","R8","R9")
# 建立 Domain -> shape 的对应表
domain_map <- data.frame(
  Domain = domain_unique,
  shape  = all_shapes[seq_along(domain_unique)],
  stringsAsFactors = FALSE
)

# 8.2 整理出符合 create_unit(...) 的数据格式
# 在 iTOL 的 DOMAINS 数据集中，关键列有:
# - variable(或相当于 ID)：用于匹配树叶名称
# - group(或相当于“域”名)：此处用数字或字符串都行
# - start、end、length 等表示结构域在“蛋白”或序列中的位置（本例做简单处理 0~1）
# - shape：决定可视化时的域形状

domain_data <- data_1 %>%
  select(ID, Domain) %>%
  left_join(domain_map, by = "Domain") %>%
  mutate(
    variable = ID,          # 用于匹配树节点的名称
    group    = as.character(Domain),
    start    = 0,           # 简单示例：都从 0 开始
    end      = 1,           # 都在 1 结束
    length   = 1,            # 整个长度(可视化用)
  )

# create_unit 时传入 domain_data，每行都代表一个结构域片段
unit_domain <- create_unit(
  data = domain_data %>% select(variable, length, start, end, group, shape),
  key  = "modern_Domain",
  type = "DATASET_DOMAINS",
  tree = tree_1
)
write_unit(unit_domain, paste0(getwd(), "/Nanjing_1439_Domain.txt"))

#####################################################
# 9.按 Text 分类，为节点添加文字
#####################################################
unit_text <- create_unit(
  data        = data_1 %>% select(ID, Text),
  key         = "rename.itol",
  type        = "DATASET_TEXT",
  size_factor = 2,
  rotation    = 0,
  position    = -1,
  color       = "#000000",
  tree        = tree_1
)
write_unit(unit_text, paste0(getwd(), "/5803_text.txt"))

#####################################################
# 10.简单柱状图 (DATASET_SIMPLEBAR)
#####################################################
# 在元数据中需要存在名为 “SimpleBar” 的数值列
unit_simplebar <- create_unit(
  data = data_1 %>% select(ID, SimpleBar),
  key  = "itol_simplebar",
  type = "DATASET_SIMPLEBAR",
  tree = tree_1
)
# 可选：设置柱状图的最大宽度
unit_simplebar@specific_themes$basic_plot$size_max <- 100
write_unit(unit_simplebar, paste0(getwd(), "/itol_simplebar.txt"))

#####################################################
# 11.多数据柱状图 (DATASET_MULTIBAR)
#####################################################
# 在元数据中需要存在列 MultiBar1、MultiBar2 等多个数值列
unit_multibar <- create_unit(
  data = data_1 %>% select(ID, MultiBar1, MultiBar2),
  key  = "itol_multibar",
  type = "DATASET_MULTIBAR",
  tree = tree_1
)
# 可选：设置柱状图的最大宽度
unit_multibar@specific_themes$basic_plot$size_max <- 100
write_unit(unit_multibar, paste0(getwd(), "/itol_multibar.txt"))

#####################################################
# 12.梯度色柱状图 (DATASET_GRADIENT)
#####################################################
# 在元数据中需要存在列 “Gradient” 或其他数值指标
unit_gradient <- create_unit(
  data = data_1 %>% select(ID, Gradient),
  key  = "itol_gradient",
  type = "DATASET_GRADIENT",
  tree = tree_1
)
# 可选：设定渐变色的最小值和最大值对应的颜色
# unit_gradient@specific_themes$heatmap$color$min <- "#0000ff"
# unit_gradient@specific_themes$heatmap$color$max <- "#ff0000"
write_unit(unit_gradient, paste0(getwd(), "/itol_gradient.txt"))

#####################################################
# 13.热图 (DATASET_HEATMAP)
#####################################################
# 在元数据中需要存在多个数值列，如 Heatmap1、Heatmap2 等
unit_heatmap <- create_unit(
  data = data_1 %>% select(ID, Heatmap1, Heatmap2),
  key  = "itol_heatmap",
  type = "DATASET_HEATMAP",
  tree = tree_1
)
write_unit(unit_heatmap, paste0(getwd(), "/itol_heatmap.txt"))
