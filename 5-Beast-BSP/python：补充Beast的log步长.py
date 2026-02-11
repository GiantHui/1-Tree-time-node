# #!/usr/bin/env python3
## 这个脚本只补充第一列
# # ===============================
# # 自定义参数
# # ===============================
# INPUT_FILE = "/mnt/c/Users/Administrator/Desktop/1.txt"   # 输入文件路径
# OUTPUT_FILE = "/mnt/c/Users/Administrator/Desktop/output.txt" # 输出文件路径
# END_NUMBER = 39660000  # 填充到的最大数字
# # ===============================


# def fill_numbers(input_file, output_file, end_number):
#     # 读取所有数字
#     with open(input_file, "r") as f:
#         numbers = [int(line.strip().split()[0]) for line in f if line.strip()]

#     if len(numbers) < 2:
#         raise ValueError("文件中至少需要两行来推断步长！")

#     # 自动推断步长
#     step = numbers[1] - numbers[0]
#     print(f"推断步长: {step}")

#     filled = []
#     current = numbers[0]

#     while current <= end_number:
#         filled.append(current)
#         current += step

#     # 写出结果
#     with open(output_file, "w") as f:
#         for num in filled:
#             f.write(f"{num}\n")

#     print(f"已写入 {len(filled)} 行到 {output_file}")


# if __name__ == "__main__":
#     fill_numbers(INPUT_FILE, OUTPUT_FILE, END_NUMBER)


#!/usr/bin/env python3

# ===============================
# 自定义参数
# ===============================
INPUT_FILE = "/mnt/c/Users/Administrator/Desktop/1.txt"        # 输入文件路径
OUTPUT_FILE = "/mnt/c/Users/Administrator/Desktop/output.txt"  # 输出文件路径
END_NUMBER = 39660000  # 填充到的最大数字（包含）
# ===============================


def fill_numbers(input_file, output_file, end_number):
    # 读取所有行，按空白分列
    with open(input_file, "r", encoding="utf-8") as f:
        rows = [line.rstrip("\n").split() for line in f if line.strip()]

    if len(rows) < 2:
        raise ValueError("文件中至少需要两行来推断步长！")

    # 第一列转数字
    numbers = [int(row[0]) for row in rows]

    # 推断步长并做基本校验
    step = numbers[1] - numbers[0]
    if step == 0:
        raise ValueError("步长推断为 0，请检查输入。")
    if step < 0:
        raise ValueError("当前脚本仅支持递增步长。")

    print(f"推断步长: {step}")

    # 以第一列为键保存原始整行
    row_dict = {int(row[0]): row for row in rows}

    # 生成完整序列并向前填充其他列
    filled = []
    current = numbers[0]
    last_row = None  # 记录上一行（用于复制其余列）

    while current <= end_number:
        if current in row_dict:
            row = row_dict[current]
            last_row = row  # 更新“上一行”
        else:
            # 新行：复制上一行的第2列及之后的内容
            if last_row is None:
                # 理论上不会发生，因为第一步就是已有行
                new_tail = []
            else:
                new_tail = last_row[1:] if len(last_row) > 1 else []
            row = [str(current)] + new_tail
            # 不更新 last_row（保持上一条“真实或填充后的行”都可，这里更新能实现逐行延用）
            last_row = row
        filled.append(row)
        current += step

    # 写出结果（用制表符分隔）
    with open(output_file, "w", encoding="utf-8") as f:
        for row in filled:
            f.write("\t".join(map(str, row)) + "\n")

    print(f"已写入 {len(filled)} 行到 {output_file}")


if __name__ == "__main__":
    fill_numbers(INPUT_FILE, OUTPUT_FILE, END_NUMBER)
