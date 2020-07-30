import importlib.util
import sys

magic_number_int = int.from_bytes(importlib.util.MAGIC_NUMBER, sys.byteorder)

print(magic_number_int)
