install.packages("itol.toolkit")
install.packages("dplyr")
install.packages("data.table")
install.packages("ape")
install.packages("stringr")
install.packages("tidyr")
library(itol.toolkit) # main package
library(dplyr) # data manipulation
library(data.table) # file read
library(ape) # tree operation
library(stringr) # string operation
library(tidyr) # data manipulation




#建立工作路径
setwd("C:/Users/LuzHu/Desktop/")
#导入树文件和data文件
tree <- read.tree("Hui_192_aDNA.nwk")
# data <- read.table("Hui_symbol.txt",header=T)
data1 <- read.table("Hui_rename.txt",header=T, sep="\t")
# data2 <- read.table("branch.txt",header=T)
#判断分类
data <- data.frame(id = unique(data$line), 
                      data = unique(data$line))

#create_unit(
#  data,
#  key,
#  type,
#  style = "default",
#  subtype = NULL,
#  color = NULL,
#  line_type = NULL,
# font_type = NULL,
# size_factor = NULL,
# position = NULL,
#  background_color = NULL,
# rotation = NULL,
# method = NULL,
# shape = NULL,
# fill = NULL,
# tree
#)
#data 表示输入数据
#type 表示
#
#显示配色方案 aaas—NCS wesanderson-艺术级别颜色供后期选择
get_color(n = 100, set = "wesanderson")
get_color()

#文本
unit_20 <- create_unit(data = data1,
                       key = "rename.itol",
                       type = "DATASET_TEXT",
                       size_factor = 2,
                       rotation = 0,
                       position = -1,
                       color = "#000000",
                       tree = tree)
write_unit(unit_20)


#外面圈圈颜色
unit_8 <- create_unit(data = data, 
                      key = "haplogroup.itol", 
                      type = "TREE_COLORS", 
                      subtype = "range",
                      color = "wesanderson",
                      tree = tree)
write_unit(unit_8)

#区域
# 读取txt文件
file_path <- "Hui_symbol.txt"
data <- read.table(file_path, header = FALSE, sep = "\t", stringsAsFactors = FALSE)
# 为列命名
colnames(data) <- c("sample", "group")
# 添加符号类型（1为圆形，2为三角形）
data$shape <- ifelse(data$group == 1, 2, 4)
# 创建可视化单元
unit <- create_unit(
  data = data, 
  key = "Sample_Symbols", 
  type = "DATASET_SYMBOL", 
  shape = data$shape, 
  position = 1,
  tree = tree
)
# 保存注释文件
write_unit(unit, file = "iTOL_Symbol.txt")





unit_27 <- create_unit(data =data,
                       key = "province_SYMBOL",
                       type = "DATASET_SYMBOL",
                       position = 0.5,
                       fill = 1,
                       shape = 1,
                       tree = tree)
write_unit(unit_27)


#枝长
unit_2 <- create_unit(data = data, 
                      key = "haologroup_clade", 
                      type = "TREE_COLORS", 
                      subtype = "clade", 
                      color = "wesanderson",
                      tree = tree)
write_unit(unit_2)


#枝长
unit_2 <- create_unit(data = data, 
                      key = "haologroup_branch.itol.txt", 
                      type = "TREE_COLORS", 
                      subtype = "branch", 
                      color = "wesanderson",
                      tree = tree)
write_unit(unit_2)


unit_28 <- create_unit(data = data1,
                       key = "language_shape",
                       type = "DATASET_DOMAINS",
                       tree = tree)


write_unit(unit_28)

#柱状图# 读取你的数据文件
data_file <- "branch.txt"  # 请替换为你的数据文件路径
data <- fread(data_file, header=TRUE, sep="/t")

# 检查数据框的列名和结构
print(data)
str(data)

# 根据实际列名进行重命名
colnames(data) <- c("Sample", "Value")

# 确保数据框格式正确
data <- data %>%
  rename(Sample = 1, Value = 2) %>%    # 重命名列
  mutate(Sample = as.character(Sample), 
         Value = as.numeric(Value))    # 确保列的类型正确

# 检查并处理 NA 值
if(any(is.na(data$Value))) {
  cat("存在NA值，已移除/n")
  data <- data %>% filter(!is.na(Value))
}

# 打印数据框以检查格式
print(data, max = 20)  # 限制输出的行数
str(data)

# 创建柱状图数据集
unit <- create_unit(data = data,
                    key = "branch.itol",
                    type = "DATASET_SIMPLEBAR",
                    size_factor = 2,
                    rotation = 0,
                    position = -1,
                    tree = tree)

# 检查生成的unit对象
print(unit)
