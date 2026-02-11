import pandas as pd

# 读取原文件的第一行（跳过的那行）
with open('C:/Users/LuzHu/Desktop/Nanjing_10K.trees.txt', 'r') as f:
    skipped_line = f.readline()

# 读取跳过第一行后的数据部分
df_bsp = pd.read_csv('C:/Users/LuzHu/Desktop/Nanjing_10K.trees.txt', sep='\t', skiprows=1)

# 修改time列：先转为整数，再乘以1.8
df_bsp['time'] = df_bsp['time'].astype(int) * 1.8

# 将原跳过的行与修改后的数据一起写入新的文件
output_file = 'C:/Users/LuzHu/Desktop/Nanjing_10K_NO.trees.txt'
with open(output_file, 'w') as f:
    # 写入原先跳过的行
    f.write(skipped_line)
    # 写入DataFrame数据，注意此处to_csv直接写入打开的文件流
    df_bsp.to_csv(f, index=False, sep='\t')