#!/usr/bin/env python3
"""Scale the time column in a BSP results file by a constant factor."""
from __future__ import annotations

from decimal import Decimal, getcontext
from pathlib import Path


# Use high precision to avoid rounding issues on long decimal inputs.
getcontext().prec = 40

# 输入/输出路径与缩放因子写死，避免每次调用手动输入。
INPUT_PATH = Path("/mnt/d/捕获体系/3-Beast/data/cqHan_combine_400_bsp.txt")
OUTPUT_PATH = Path("/mnt/d/捕获体系/3-Beast/data/cqHan_combine_400_bsp_scaled.txt")
SCALE_FACTOR = Decimal("1.269485926451719")


def scale_bsp_times(input_path: Path, output_path: Path, factor: Decimal) -> None:
    if not input_path.exists():
        raise FileNotFoundError(f"Input file not found: {input_path}")

    # Preserve the same decimal precision as the source file for the time column.
    quantize_cache: dict[int, Decimal] = {}

    with input_path.open("r", encoding="utf-8") as source, output_path.open("w", encoding="utf-8") as target:
        for line in source:
            stripped = line.strip()
            if not stripped:
                target.write(line)
                continue

            parts = line.rstrip("\n").split("\t")
            if not parts:
                target.write(line)
                continue

            first_field = parts[0]
            if not first_field:
                target.write(line)
                continue

            try:
                original_value = Decimal(first_field)
            except Exception:
                target.write(line)
                continue

            decimal_places = 0
            if "." in first_field:
                decimal_places = len(first_field.split(".", 1)[1])

            if decimal_places not in quantize_cache:
                if decimal_places == 0:
                    quantize_cache[decimal_places] = Decimal("1")
                else:
                    quantize_cache[decimal_places] = Decimal(f"1e-{decimal_places}")

            # 按固定倍数缩放time列，并保持原始的小数精度和格式。
            scaled_value = (original_value * factor).quantize(quantize_cache[decimal_places])
            parts[0] = format(scaled_value, "f") if decimal_places else str(int(scaled_value))
            target.write("\t".join(parts) + "\n")


def main() -> None:
    # 读取原始BSP结果文件，将time列按固定倍数缩放后写入新文件。
    scale_bsp_times(INPUT_PATH, OUTPUT_PATH, SCALE_FACTOR)


if __name__ == "__main__":
    main()
