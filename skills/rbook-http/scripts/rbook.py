#!/usr/bin/env python3
"""Forward the bundled skill entrypoint to the repository's canonical client."""

import runpy
from pathlib import Path


THIS_FILE = Path(__file__).resolve()


def find_client():
    for ancestor in THIS_FILE.parents:
        candidate = ancestor / "scripts" / "rbook.py"
        if candidate != THIS_FILE and candidate.is_file():
            return candidate
    raise FileNotFoundError(
        "could not find the repository scripts/rbook.py; run this skill inside an rbook checkout"
    )


if __name__ == "__main__":
    runpy.run_path(str(find_client()), run_name="__main__")
