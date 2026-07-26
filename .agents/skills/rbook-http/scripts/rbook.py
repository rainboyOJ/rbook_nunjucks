#!/usr/bin/env python3
import runpy
from pathlib import Path


CLIENT_PATH = Path(__file__).resolve().parents[4] / "scripts" / "rbook.py"


if __name__ == "__main__":
    runpy.run_path(str(CLIENT_PATH), run_name="__main__")
