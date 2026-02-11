# -*- coding: utf-8 -*-
"""
Y染色体时间统计绘图脚本（自动识别时间列，输出累计折线图，支持平滑曲线）
"""

import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
import numpy as np
from scipy.interpolate import PchipInterpolator  # 替代 make_interp_spline，防止曲线末端回落

# ========== 设置字体支持 ==========
matplotlib.rcParams['font.sans-serif'] = ['Arial']
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42

# ========== 配置参数 ==========
input_file = "C:/Users/LuzHu/Desktop/1.csv"
output_csv = "C:/Users/LuzHu/Desktop/test.csv"
time_range = [50000, 0]
bin_resolution = 1000

# ========== 读取数据并检测时间列 ==========
df = pd.read_csv(input_file, header=None)
numeric_cols = df.apply(pd.to_numeric, errors='coerce')
time_col_index = numeric_cols.count().idxmax()
time_col = pd.to_numeric(df.iloc[:, time_col_index], errors='coerce')
id_col = df.iloc[:, 1 - time_col_index] if df.shape[1] >= 2 else df.index
df_clean = pd.DataFrame({'ID': id_col, 'Time': time_col}).dropna(subset=['Time'])

# ========== 构建分箱 ==========
bin_edges = list(range(time_range[1], time_range[0] + bin_resolution, bin_resolution))
bin_labels = [f"[{bin_edges[i]},{bin_edges[i+1]})" for i in range(len(bin_edges) - 1)]
bin_centers = [(bin_edges[i] + bin_edges[i+1]) // 2 for i in range(len(bin_edges) - 1)]

new_counts = []
for i in range(len(bin_edges) - 1):
    left = bin_edges[i]
    right = bin_edges[i+1]
    count = ((df_clean['Time'] >= left) & (df_clean['Time'] < right)).sum()
    new_counts.append(count)

cumulative_counts = list(pd.Series(new_counts[::-1]).cumsum()[::-1])

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
ax1.bar(bin_df['Bin_Center'], bin_df['New_Count'],
        width=bin_resolution * 0.8, color='skyblue')
ax1.set_xlabel("Years before Present")
ax1.set_ylabel("New nodes per time interval", color='skyblue')
ax1.tick_params(axis='y', labelcolor='skyblue')
ax1.invert_xaxis()

# 平滑折线图（防止末端回落）
x = np.array(bin_df['Bin_Center'])
y = np.array(bin_df['Cumulative_Count'])

if len(x) > 3:
    x_smooth = np.linspace(x.min(), x.max(), 300)
    interpolator = PchipInterpolator(x, y)  # 保持单调的插值器
    y_smooth = interpolator(x_smooth)

    ax2 = ax1.twinx()
    ax2.plot(x_smooth, y_smooth, color='orange', label='Smoothed cumulative count')
else:
    ax2 = ax1.twinx()
    ax2.plot(x, y, color='orange', label='Cumulative count')

ax2.set_ylabel("Cumulative number of nodes", color='orange')
ax2.tick_params(axis='y', labelcolor='orange')

plt.title("Node Accumulation Over Time (Smoothed)")
fig.tight_layout()
plt.show()
