

import re

def extract_sample_ids_from_nwk(nwk_file, output_file):
    # 读取 .nwk 文件内容
    with open(nwk_file, 'r', encoding='utf-8') as file:
        nwk_data = file.read()

    # 使用正则表达式提取符合条件的样本ID
    # (?<=\(|,) 匹配左侧为左括号 '(' 或英文逗号 ',' 的内容
    # [\w\.\-]+__?[\w\.\-]* 匹配字母、数字、下划线、短横线、双下划线 '__'、英文句号 '.'
    # (?=[:\[]) 确保右侧是冒号 ':' 或 '['
    sample_ids = re.findall(r'(?<=\(|,)[^,]*?([\w\.\-]+__?[\w\.\-]*)(?=[:\[])', nwk_data)

    # 将样本ID写入到输出文件
    with open(output_file, 'w', encoding='utf-8') as output:
        for sample_id in sample_ids:
            output.write(sample_id + '\n')

    print(f"提取的样本ID已保存到 {output_file}")

# 使用示例
if __name__ == "__main__":
    nwk_file = '/mnt/c/Users/Administrator/Desktop/topology/data/Figure1_validation.nwk'  # 请将此处替换为你的 .nwk 文件路径
    output_file = '/mnt/c/Users/Administrator/Desktop/topology/data/Figure1_validation_ID.txt'  # 输出的样本ID文件
    extract_sample_ids_from_nwk(nwk_file, output_file)

    