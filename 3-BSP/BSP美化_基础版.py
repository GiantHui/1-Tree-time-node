import numpy as np
import matplotlib.pyplot as plt
import pandas as pd

# 读取文件并保留整数
input_file_path = 'C:/Users/LuzHu/Desktop/Nanjing_38_BSP_all.txt'

# 读取文件
with open(input_file_path, 'r', encoding='utf-8') as file:
    lines = file.readlines()

# 提取标题和表头
title = lines[0].strip()
header = lines[1].strip().split('\t')

# 读取数据并保留整数
data_lines = lines[2:]

# 检查最后一行是否为非数字类型，如果是则删除
def is_numeric_list(line):
    try:
        [float(x) for x in line.strip().split('\t')]
        return True
    except ValueError:
        return False

while data_lines and not is_numeric_list(data_lines[-1]):
    data_lines.pop()

data = [list(map(lambda x: int(float(x)), line.strip().split('\t'))) for line in data_lines]

# 创建DataFrame
df = pd.DataFrame(data, columns=header)

# Ensuring all data are float64
df = df.astype(float)

# Set 'time' as the index
df.set_index('time', inplace=True)

# Rounding the data to remove all decimals
df_rounded = df.round()

# Asking user for time scale limits
time_lower_limit = int(input("请输入时间尺度的下限 (e.g., 0): "))
time_upper_limit = int(input("请输入时间尺度的上限 (e.g., 60000): "))

# Filtering the data based on the user input
data_filtered = df_rounded[(df_rounded.index >= time_lower_limit) & (df_rounded.index <= time_upper_limit)]

# Converting columns to integer numpy arrays
time_values_int = data_filtered.index.to_numpy().astype(int)
upper_values_int = data_filtered['upper'].to_numpy().astype(int)
lower_values_int = data_filtered['lower'].to_numpy().astype(int)

# Determine the Y-axis range
y_min = 10**np.floor(np.log10(lower_values_int.min()))
y_max = 10**np.ceil(np.log10(upper_values_int.max()))

# 设置字体为Arial，并确保生成的图形中的文本是矢量可编辑的
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42

# Plotting the optimized data with specified adjustments
fig, ax = plt.subplots(figsize=(10, 6))

# Plot each line with different colors and increased linewidth for mean and median
ax.plot(time_values_int, data_filtered['mean'].round().astype(int), label='Mean', color='#C36B5F', linewidth=1, linestyle='--')
ax.plot(time_values_int, data_filtered['median'].round().astype(int), label='Median', color='#72B6AB', linewidth=1)
#ax.plot(time_values_int, upper_values_int, label='Upper', color='white')
#ax.plot(time_values_int, lower_values_int, label='Lower', color='white')

# Fill the area between upper and lower using the specified color
ax.fill_between(time_values_int, upper_values_int, lower_values_int, color='#B9DEDD', alpha=0.3)

# Removing background grid
ax.grid(False)

# Adding title and labels
ax.set_title(title)
ax.set_xlabel('Time')
ax.set_ylabel('Values')
ax.set_yscale('log')
ax.set_ylim(y_min, y_max)

# Adjusting y-axis ticks to standard notation
ax.set_yticks([10**i for i in range(int(np.log10(y_min)), int(np.log10(y_max)) + 1)])
ax.set_yticklabels([f'{10**i:.0f}' for i in range(int(np.log10(y_min)), int(np.log10(y_max)) + 1)])

# Set Y-axis crossing X-axis at 0
ax.spines['left'].set_position(('data', 0))

# Adding legend
ax.legend()

# Display the plot
plt.show()

