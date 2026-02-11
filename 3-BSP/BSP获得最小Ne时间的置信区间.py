import os
import pandas as pd

# 替换为你的BSP结果文件所在的目录路径
directory_path = 'C:/Users/LuzHu/Desktop/1' # 请替换为你的BSP结果文件所在的目录路径
output_file = 'C:/Users/LuzHu/Desktop/output_summary.txt'  # 请替换为你的输出结果所在的文件路径

def calculate_time_interval(years_since_expansion, mutation_rate, mutation_rate_confidence): 
    """
    根据遗传距离和突变率计算时间估计的置信区间。

    参数:
    years_since_expansion (int): 自种群扩张以来的年数。
    mutation_rate (float): 每年每碱基的平均突变率。
    mutation_rate_confidence (tuple): 突变率置信区间的上下界。

    返回:
    tuple: 时间估计的置信区间的下限和上限。
    """
    # 使用平均突变率计算遗传距离
    genetic_distance = years_since_expansion * mutation_rate

    # 使用突变率置信区间计算时间的上限和下限
    lower_bound_time = genetic_distance / mutation_rate_confidence[1]  #高突变率给出较近期的时间
    upper_bound_time = genetic_distance / mutation_rate_confidence[0]  #低突变率给出较古老的时间

    return (lower_bound_time, upper_bound_time)

# 遍历目录下所有文件
with open(output_file, 'w', encoding='utf-8') as output:
    # 写入文件头
    output.write(f"文件名\t最小median对应的year值\t置信区间(下限,上限)\n")
    
    for file_name in os.listdir(directory_path):
        # 拼接文件路径
        file_path = os.path.join(directory_path, file_name)
        
        # 检查是否为文件
        if os.path.isfile(file_path):
            try:
                # 读取txt文件并将其转为DataFrame
                df = pd.read_csv(file_path, sep='\t', skiprows=1)
                df.columns = df.columns.str.strip()  # 移除列名中的多余空格

                # 查找median列的最小值及对应的time值
                min_median_row = df.loc[df['median'].idxmin()]
                years_since_expansion = int(round(min_median_row['time']))  # 保留为整数

                # 预设参数
                mutation_rate = 0.76e-9  # 突变率
                mutation_rate_confidence = (0.67e-9, 0.86e-9)  # 突变率置信区间

                # 计算置信区间
                time_interval = calculate_time_interval(years_since_expansion, mutation_rate, mutation_rate_confidence)

                # 将置信区间结果四舍五入为整数
                time_interval = (int(round(time_interval[0])), int(round(time_interval[1])))

                # 将结果保存到输出文件中
                output.write(f"{file_name}\t{years_since_expansion}\t{time_interval}\n")
            except Exception as e:
                # 如果文件处理过程中出现错误，输出错误信息
                output.write(f"{file_name}\t错误: {str(e)}\n")

print(f"结果已保存到 {output_file}")
