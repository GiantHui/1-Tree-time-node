import os
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import matplotlib.cm as cm

# 读取文件的函数
def read_file(input_file_path):
    with open(input_file_path, 'r', encoding='utf-8') as file:
        lines = file.readlines()

    # 提取标题和表头
    title = lines[0].strip()  # 第一行作为标题
    header = lines[1].strip().split('\t')  # 第二行作为表头

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

    # 处理数据：使用NaN值替换无效数据
    data = []
    for line in data_lines:
        row = line.strip().split('\t')
        row_float = [float(val) for val in row]  # 直接转换为float，不处理NaN
        data.append(row_float)

    # 创建DataFrame
    df = pd.DataFrame(data, columns=header)

    # Ensuring all data are float64
    df = df.astype(float)

    # Set 'time' as the index
    df.set_index('time', inplace=True)

    return title, df  # 返回文件的标题和DataFrame

# 获取文件夹内所有文件路径
input_folder = "E:/桌面文件/GSRD/BSP/mtDNA/群体mtBSP可视化"  # 这里替换为你的文件夹路径
file_paths = [os.path.join(input_folder, f) for f in os.listdir(input_folder) if os.path.isfile(os.path.join(input_folder, f))]  # 获取文件夹内所有文件

# 获取颜色映射
colormap = cm.get_cmap('tab10')  # 使用matplotlib的' tab10'颜色映射

# 创建一个图形
fig, ax = plt.subplots(figsize=(10, 6))

# 设置时间尺度的下限和上限
time_lower_limit = int(input("请输入时间尺度的下限 (e.g., 0): "))
time_upper_limit = int(input("请输入时间尺度的上限 (e.g., 60000): "))

# 遍历文件夹中的所有文件
for idx, file_path in enumerate(file_paths):
    # 读取数据和标题
    title, df = read_file(file_path)

    # 将数据四舍五入
    df_rounded = df.round()

    # 筛选数据
    data_filtered = df_rounded[(df_rounded.index >= time_lower_limit) & (df_rounded.index <= time_upper_limit)]

    # 获取时间和值
    time_values_int = data_filtered.index.to_numpy().astype(int)
    upper_values_int = data_filtered['upper'].to_numpy().astype(int)
    lower_values_int = data_filtered['lower'].to_numpy().astype(int)

    # 获取当前文件的颜色
    color = colormap(idx % 10)  # 使用tab10颜色库，并避免索引越界

    # 使用文件标题作为图例标签
    ax.plot(time_values_int, data_filtered['mean'].round().astype(int), label=f'Mean ({title})', color=color, linewidth=2, linestyle='--')
    ax.plot(time_values_int, data_filtered['median'].round().astype(int), label=f'Median ({title})', color=color, linewidth=2)
    ax.fill_between(time_values_int, upper_values_int, lower_values_int, color=color, alpha=0.3)

# 设置字体为Arial，并确保生成的图形中的文本是矢量可编辑的
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['pdf.fonttype'] = 42
plt.rcParams['ps.fonttype'] = 42

# 设置Y轴范围，y_min设置为1000，y_max保持为自动计算的最大值
y_min = 1000
y_max = 10**np.ceil(np.log10(max(upper_values_int.max() for f in file_paths)))

# 设置图形标题和标签
ax.set_title("BSP Analysis Comparison")
ax.set_xlabel('Time')
ax.set_ylabel('Effective population size')
ax.set_yscale('log')
ax.set_ylim(y_min, y_max)

# 设置Y轴的刻度标签，使用更合适的间隔
y_ticks = [10**i for i in range(int(np.log10(y_min)), int(np.log10(y_max)) + 1)]
ax.set_yticks(y_ticks)
ax.set_yticklabels([f'{tick:.0f}' for tick in y_ticks])

# 去掉背景网格
ax.grid(False)

# 设置Y轴在X轴为0时交叉
ax.spines['left'].set_position(('data', 0))

# 添加图例
ax.legend()

# 显示图形
plt.show()
