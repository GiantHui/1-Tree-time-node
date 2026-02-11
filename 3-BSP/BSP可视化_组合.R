
library(ggplot2)
setwd("E:/OneDrive/Y-chromosome/壮侗全测/BSP/东亚-东南亚壮侗人群")
# ========== 配置：输入文件和标签 ==========
files <- c("2M_405TK_skyline.log", "2M_744TK_skyline.log")  # ← 替换成你的文件名
labels <- c("Group1", "Group2")


# ========== 函数：读取并清理 ==========
read_bsp <- function(file, label){
  raw <- read.delim(file, header = TRUE, skip = 1, check.names = FALSE)
  names(raw) <- tolower(names(raw))
  need <- c("time","mean","median","upper","lower")
  df <- raw[, need]
  for (v in need) df[[v]] <- suppressWarnings(as.numeric(df[[v]]))
  df <- df[is.finite(df$time) & is.finite(df$median) & is.finite(df$lower) & is.finite(df$upper), ]
  df <- df[order(df$time), ]
  df$group <- label
  df
}

# 读入并合并
dfs <- mapply(read_bsp, files, labels, SIMPLIFY = FALSE)
all_df <- do.call(rbind, dfs)

# ========== 作图 ==========
p <- ggplot(all_df, aes(x = time, group = group)) +
  #geom_ribbon(aes(ymin = lower, ymax = upper, fill = group), alpha = 0.2, show.legend = FALSE) +
  geom_line(aes(y = median, color = group), linewidth = 1) +
  #geom_line(aes(y = mean, color = group), linetype = "22", size = 0.6) +
  scale_x_continuous(
    limits = c(0, 70000),        # 设置X轴范围
    breaks = seq(0, 70000, 5000) # 每隔5000年一个刻度
  )+

  labs(x = "Time (years before present)", y = "Effective population size", color = "Group") +
  theme_classic(base_size = 12) +
  theme(legend.position = "top")
p

