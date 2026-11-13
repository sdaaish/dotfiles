#!/usr/bin/env bash
# -*- mode: sh; -*-

# Template for direnv and .envrc files

# Include parent .envrc if it exist
source_up_if_exists

# Include local config
source_env_if_exists .config/envrc.local

# Include virtual environment
source_env_if_exists .venv/bin/activate
