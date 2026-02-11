import os

def read_id_file(id_file_path):
    """读取ID文件，返回ID替换对的字典"""
    id_map = {}
    with open(id_file_path, 'r', encoding='utf-8') as id_file:
        for line in id_file:
            original_id, new_id = line.strip().split()  # 假设ID用空格或Tab分隔
            id_map[original_id] = new_id
    return id_map

def replace_ids_in_tree(tree_file_path, id_map, output_file_path):
    """遍历树文件内容，替换指定的ID"""
    with open(tree_file_path, 'r', encoding='utf-8') as tree_file:
        tree_content = tree_file.read()

    for original_id, new_id in id_map.items():
        tree_content = tree_content.replace(original_id, new_id)

    # 输出到新的文件
    with open(output_file_path, 'w', encoding='utf-8') as output_file:
        output_file.write(tree_content)

    print(f"替换后的文件已保存到: {output_file_path}")

if __name__ == "__main__":
    # 设置文件路径
    id_file_path = 'C:/Users/LuzHu/Desktop/Hui_all.txt'  # ID替换文件路径，没有表头，第一列是原始ID，第二列是替换ID
    tree_file_path = 'C:/Users/LuzHu/Desktop/Hui_all_Raxml.raxml.bestTree'  # 树文件路径
    output_file_path = 'C:/Users/LuzHu/Desktop/Hui_all_Raxml.raxml.rename.bestTree'  # 输出的树文件路径

    # 读取ID替换文件
    id_map = read_id_file(id_file_path)

    # 替换树文件中的ID
    replace_ids_in_tree(tree_file_path, id_map, output_file_path)
