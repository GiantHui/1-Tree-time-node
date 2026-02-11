import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from matplotlib.ticker import MultipleLocator, AutoMinorLocator  # ✅ 新增

# 读取文件并保留整数
input_file_path = '/mnt/c/Users/Administrator/Desktop/cq_98.snp_2nd_bsp.txt'

# 读取文件
with open(input_file_path, 'r', encoding='utf-8') as file:
    lines = file.readlines()

# 提取标题和表头
title = lines[0].strip()
header = lines[1].strip().split('\t')

# 读取数据并保留整数
data_lines = lines[2:]

def is_numeric_list(line):
    try:
        [float(x) for x in line.strip().split('\t')]
        return True
    except ValueError:
        return False

while data_lines and not is_numeric_list(data_lines[-1]):
    data_lines.pop()

data = [list(map(lambda x: int(float(x)), line.strip().split('\t'))) for line in data_lines]

df = pd.DataFrame(data, columns=header)
df = df.astype(float)
df.set_index('time', inplace=True)
df_rounded = df.round()

time_lower_limit = int(input("请输入时间尺度的下限 (e.g., 0): "))
time_upper_limit = int(input("请输入时间尺度的上限 (e.g., 60000): "))

data_filtered = df_rounded[(df_rounded.index >= time_lower_limit) & (df_rounded.index <= time_upper_limit)]

time_values_int = data_filtered.index.to_numpy().astype(int)
upper_values_int = data_filtered['upper'].to_numpy().astype(int)
lower_values_int = data_filtered['lower'].to_numpy().astype(int)

y_min = 10**np.floor(np.log10(lower_values_int.min()))
y_max = 10**np.ceil(np.log10(upper_values_int.max()))

plt.rcParams['font.family'] = 'Arial'
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42

fig, ax = plt.subplots(figsize=(10, 6))

ax.plot(time_values_int, data_filtered['mean'].round().astype(int), label='Mean', color='#C36B5F', linewidth=1, linestyle='--')
ax.plot(time_values_int, data_filtered['median'].round().astype(int), label='Median', color='#72B6AB', linewidth=1)
ax.fill_between(time_values_int, upper_values_int, lower_values_int, color='#B9DEDD', alpha=0.3)

ax.grid(False)
ax.set_title(title)
ax.set_xlabel('Time (years before present)')
ax.set_ylabel('Values')
ax.set_yscale('log')
ax.set_ylim(y_min, y_max)

ax.set_yticks([10**i for i in range(int(np.log10(y_min)), int(np.log10(y_max)) + 1)])
ax.set_yticklabels([f'{10**i:.0f}' for i in range(int(np.log10(y_min)), int(np.log10(y_max)) + 1)])

ax.spines['left'].set_position(('data', 0))
ax.legend()

# ✅ 设置 X 轴主刻度（每500年）和次刻度（每100年）
ax.xaxis.set_major_locator(MultipleLocator(5000))
ax.xaxis.set_minor_locator(MultipleLocator(500))
0
# ✅ 显示次刻度线
ax.tick_params(axis='x', which='major', length=7)
ax.tick_params(axis='x', which='minor', length=4, color='gray')

# ✅ 设置横轴范围与上面输入一致
ax.set_xlim(time_lower_limit, time_upper_limit)

plt.show()
