#!/usr/bin/env -S uv run --script

# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///

"""
Uses the secret-tool to lookup a Title (Label) in the Freedesktop Secrets manager.
"""

import argparse
import subprocess


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Lookup a field with the help of secret-tool."
    )
    parser.add_argument(
        "-f", "--field", dest="field", help="The title to lookup.", required=True
    )
    args = parser.parse_args()

    result = subprocess.run(["/usr/bin/secret-tool", "lookup", "Title", args.field])


if __name__ == "__main__":
    main()
