# -*- coding: utf-8 -*-
"""
Y染色体时间统计绘图脚本（自动识别时间列，输出正确的累计折线图）
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib

# ========== 设置字体支持 ==========  
matplotlib.rcParams['font.sans-serif'] = ['Arial']  # 设置字体为 Arial，可替换为支持中文的字体如 SimHei
plt.rcParams['pdf.fonttype'] = 42  # 确保导出 PDF 中字体可编辑
plt.rcParams['ps.fonttype'] = 42   # 确保导出 PS 文件中字体可编辑

# ========== 配置参数 ==========
input_file = "C:/Users/LuzHu/Desktop/1.csv"     # 输入文件路径
output_csv = "C:/Users/LuzHu/Desktop/Nanjing_ml_time.csv"   # 输出统计CSV路径
time_range = [50000, 0]                         # 时间范围（从古至今）
bin_resolution = 1000                           # 时间分箱粒度（单位：年）

# ========== 读取数据并检测时间列 ==========
df = pd.read_csv(input_file, header=None)

# 自动识别“时间列”：选择数值最多的一列作为时间列
numeric_cols = df.apply(pd.to_numeric, errors='coerce')
time_col_index = numeric_cols.count().idxmax()

# 建立新DataFrame，第一列是ID，第二列是Time
time_col = pd.to_numeric(df.iloc[:, time_col_index], errors='coerce')
id_col = df.iloc[:, 1 - time_col_index] if df.shape[1] >= 2 else df.index
df_clean = pd.DataFrame({'ID': id_col, 'Time': time_col}).dropna(subset=['Time'])

# ========== 构建分箱 ==========
bin_edges = list(range(time_range[1], time_range[0] + bin_resolution, bin_resolution))  # [0,1000,...,50000]
bin_labels = [f"[{bin_edges[i]},{bin_edges[i+1]})" for i in range(len(bin_edges)-1)]
bin_centers = [(bin_edges[i] + bin_edges[i+1]) // 2 for i in range(len(bin_edges)-1)]

# 统计每个时间段的新样本数
new_counts = []
for i in range(len(bin_edges)-1):  # 正序：从今到古
    left = bin_edges[i]
    right = bin_edges[i+1]
    count = ((df_clean['Time'] >= left) & (df_clean['Time'] < right)).sum()
    new_counts.append(count)

# 从古至今计算累计值：反转new_counts → 累加 → 再反转回来
cumulative_counts = list(pd.Series(new_counts[::-1]).cumsum()[::-1])

# 构建结果表格
bin_df = pd.DataFrame({
    'Time_Bin': bin_labels,
    'Bin_Center': bin_centers,
    'New_Count': new_counts,
    'Cumulative_Count': cumulative_counts
})

# ========== 输出统计结果 ==========
bin_df.to_csv(output_csv, index=False)

# ========== 绘图 ==========
fig, ax1 = plt.subplots(figsize=(12, 6))

# 柱状图：每个时间段的新样本数
ax1.bar(bin_df['Bin_Center'], bin_df['New_Count'],
        width=bin_resolution * 0.8, color='skyblue', label='New samples per interval')
ax1.set_xlabel("Years before Present")
ax1.set_ylabel("New nodes per time interval", color='skyblue')
ax1.tick_params(axis='y', labelcolor='skyblue')
ax1.invert_xaxis()  # 从古至今（大→小）

# 折线图：累计样本数
ax2 = ax1.twinx()
ax2.plot(bin_df['Bin_Center'], bin_df['Cumulative_Count'],
         color='orange', marker='o', label='Cumulative count')
ax2.set_ylabel("Cumulative number of nodes", color='orange')
ax2.tick_params(axis='y', labelcolor='orange')

# 标题与布局
plt.title("Node Accumulation Over Time")
fig.tight_layout()
plt.show()


