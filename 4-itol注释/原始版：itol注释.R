# 加载必要的R包
library(dplyr)  # 加载dplyr包，用于数据处理和管道操作
library(itol.toolkit)  # 加载itol.toolkit包，用于制作和管理iTOL树图的数据单元
library(data.table)  # 加载data.table包，提供高效的数据读取和操作功能

# 设置工作目录
setwd("C:/Users/LuzHu/Desktop/")  # 将当前R会话的工作目录设置为指定路径

# 读取和处理数据
tree_1 <- "North_13607.nwk"  # 指定新克文件的路径，这个文件包含树的信息
hub_1 <- create_hub(tree_1)  # 创建一个以此树为中心的hub，用于添加不同的数据单元
data_file_1 <- "13607_metadata.txt"  # 指定元数据文件的路径
data_1 <- data.table::fread(data_file_1)  # 使用data.table的fread函数读取元数据文件

############################Label################################################
# 为树的节点添加标签按Label分类
unit_1 <- create_unit(data = data_1 %>% select(ID, Label),  # 从data_1中选取ID和Label列
                      key = "itol_labels",  # 为这个单元设置一个键名
                      type = "LABELS",  # 设置数据单元的类型为标签
                      tree = tree_1)  # 指定这个单元关联的树文件
write_unit(unit_1, paste0(getwd(), "/Nanjing_labels.txt"))  # 将单元写入文件

#############################Range################################################
# 为树的节点添加颜色，按Range分类
unit_2 <- create_unit(data = data_1 %>% select(ID, Range),
                      key = "itol_range",
                      type = "TREE_COLORS",  # 设置类型为树颜色
                      subtype = "range",  # 子类型为范围，表示颜色将根据指定的范围变化
                      tree = tree_1)
write_unit(unit_2, paste0(getwd(), "/aDNA_range.txt"))

#############################Branch################################################
# 为树的分支添加颜色和形状，按Branch分类
# 生成每个唯一Branch的随机颜色
Branch_unique <- unique(data_1$Branch)  # 提取所有唯一的Branch值
set.seed(123)  # 设置随机种子以保证颜色可以复现
colors <- sample(colors(), length(Branch_unique), replace = FALSE)  # 为每个Branch随机分配颜色
Branch_colors <- setNames(colors, Branch_unique)  # 创建一个以Branch为名称、颜色为值的向量
data_1$Color <- Branch_colors[data_1$Branch]  # 将颜色值分配给相应的Branch
unit_colors <- create_unit(data = select(data_1, ID, Color),
                           key = "itol_random_Branch_colors",
                           type = "TREE_COLORS",
                           subtype = "branch",  # 指定subtype为branch，可以改为range或clade
                           tree = tree_1)  # 请替换为您的树文件名称
write_unit(unit_colors, paste0(getwd(), "/Nanjing_Branch.txt"))


#############################Strip################################################
# 为树添加颜色条带，按Strip分类
set.seed(123)  # 设置随机数种子，确保颜色选择的可重复性
unit_3 <- create_unit(data = data_1 %>% select(ID, Strip),
                      key = "itol_strip",
                      type = "DATASET_COLORSTRIP",  # 设置类型为颜色条带
                      color = "wesanderson",  # 使用Wes Anderson调色板
                      tree = tree_1)
unit_3@common_themes$basic_theme$margin <- 50  # 设置条带的边缘空白
write_unit(unit_3, paste0(getwd(), "/Nanjing_strip_language.txt"))

#############################Symbol################################################
# 为树添加枝上的形状，按Symbol分类
unit_9 <- create_unit(data = data_1 %>%  select(ID, Symbol),
                      key = "Sample_Symbols", 
                      type = "DATASET_SYMBOL",
                      position = 1,
                      size = 2,
                      subtype = "Symbol",
                      tree = tree_1,
                      fill = 1)
write_unit(unit_9,paste0(getwd(),"/Nanjing_Symbol.txt"))

#############################Domain################################################
# 为末端节点添加区域形状，按Domain分类
tab_id_group <- tab_tmp[,c(1,2)]
tab_tmp <- tab_tmp[,-c(1,2)]
tab_tmp_01 <- convert_01(object = tab_tmp)
tab_tmp_01 <- cbind(tab_id_group,tab_tmp_01)
para_order <- c("type",
                "separator",
                "profile",
                "field",
                "common themes",
                "specific themes",
                "data")
template_with_start <- tab_tmp_01 %>%
                   pivot_longer(-c('parameter', 'group'), 
                                names_to = 'variable',
                                values_to = 'value') %>%
                   group_by(group,variable) %>%
                   summarise(sublen = sum(value)) %>%
                   spread(key=variable,
                          value=sublen) %>%
                   mutate(group=factor(group,levels = para_order)) %>%
                   arrange(group)
group_start <- data.frame(group = template_with_start$group, 
                          Freq = apply(template_with_start[,-1], 1, max)) %>%
               mutate(start=lag(cumsum(Freq))) %>%
               mutate(start=replace_na(start, 0))
template_with_start_and_end <- sapply(as.list(template_with_start[,-1]), 
                                      function(x)x + group_start$start) %>% 
  as.data.frame() %>%
  mutate(group=template_with_start$group) %>%
  relocate(group) %>%
  pivot_longer(-c("group"),
               names_to = 'variable',
               values_to = "end") %>%
  left_join(group_start[c('group','start')]) %>%
  mutate(length=sum(group_start$Freq),
         group =factor(.$group, levels=para_order)) %>%
  relocate(variable, length, start, end, group)

group2shape <- tribble(
  ~group,               ~shape,
  "type",               "HH",
  "separator",          "HV",
  "profile",            "EL",
  "field",              "DI",
  "common themes",      "TR",
  "specific themes",    "TL",
  "data",               "PL"
)
template_end_group_long <- template_with_start_and_end  %>% 
  left_join(group2shape)
  
unit_34 <- create_unit(data = template_end_group_long,
                       key = "E034_domains_1",
                       type = "DATASET_DOMAINS",
                       tree = tree)
write_unit(unit_34,paste0(getwd(),"/aDNA_Domain.txt"))

#############################Text################################################
# 为树添加额外的文字标签，按Text分类
unit_10 <- create_unit(data = data_1 %>%  select(ID, Text),
                       key = "rename.itol",
                       type = "DATASET_TEXT",
                       size_factor = 2,
                       rotation = 0,
                       position = -1,
                       color = "#000000",
                       tree = tree_1)
write_unit(unit_10,paste0(getwd(),"/itol_text.txt"))






#############################功能4################################################
# 添加柱状图，表示某个数值特征
# unit_4 <- create_unit(data = data_1 %>% select(ID, NS),
#                       key = "itol_3al_4_simplebar",
#                       type = "DATASET_SIMPLEBAR",  # 类型为简单柱状图
#                       tree = tree_1)
# unit_4@specific_themes$basic_plot$size_max <- 100  # 设置柱状图的最大宽度
# write_unit(unit_4, paste0(getwd(), "/itol_3al_4_simplebar.txt"))

#############################功能5################################################
# 添加多数据柱状图，同时表示多个数值特征
# unit_5 <- create_unit(data = data_1 %>% select(ID, NS, OS),
#                       key = "itol_3al_5_multibar",
#                       type = "DATASET_MULTIBAR",  # 类型为多数据柱状图
#                       tree = tree_1)
# unit_5@specific_themes$basic_plot$size_max <- 100  # 设置柱状图的最大宽度
# write_unit(unit_5, paste0(getwd(), "/itol_3al_5_multibar.txt"))

#############################功能6################################################
# 添加梯度色柱状图，用于展示数据的变化
# unit_6 <- create_unit(data = data_1 %>% select(ID, Dissimilarity),
#                       key = "itol_3al_6_gradient",
#                       type = "DATASET_GRADIENT",  # 类型为渐变数据集
#                       tree = tree_1)
# unit_6@specific_themes$heatmap$color$min <- "#0000ff"  # 设置渐变的最小颜色
# unit_6@specific_themes$heatmap$color$max <- "#ff0000"  # 设置渐变的最大颜色
# write_unit(unit_6, paste0(getwd(), "/itol_3al_6_gradient.txt"))

#############################功能7################################################
# 绘制热图，用于展示多个变量的组合数据
# unit_7 <- create_unit(data = data_1 %>% select(ID, NS, OS),
#                       key = "itol_7_heatmap",
#                       type = "DATASET_HEATMAP",  # 类型为热图
#                       tree = tree_1)
# write_unit(unit_7, paste0(getwd(), "/itol_7_heatmap.txt"))