#!/usr/bin/env -S uv run --script

# /// script
# requires-python = ">=3.12"
# dependencies = []
# ///

"""
Uses the secret-tool to lookup a Title (Label) in the Freedesktop Secrets manager.
"""

import argparse
import os


def main() -> None:
    parser = argparse.ArgumentParser(
        description="Lookup a field with the help of secret-tool."
    )
    parser.add_argument(
        "-f", "--field", dest="field", help="The title to lookup.", required=True
    )
    args = parser.parse_args()

    passwd = f"secret-tool lookup Title {args.field}"
    os.system(passwd)


if __name__ == "__main__":
    main()
