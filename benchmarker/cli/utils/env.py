"""Utility constants and functions for environment management"""
import json
import os
from contextlib import contextmanager
from enum import Enum
from typing import (
    Dict,
    Generator,
    Optional,
)

__all__ = ["is_verbose_logging", "process_env"]


def is_verbose_logging() -> bool:
    """Return if verbosity is set using the benchmarker environment variable"""
    return bool(_get_env_var(EnvironmentVariable.VERBOSE))


def get_config_file_path() -> str:
    """Get the path to the benchmarker config file"""
    env_config_file = _get_env_var(EnvironmentVariable.CONFIG)
    default_config_file = os.path.realpath(DEFAULT_CONFIG_FILE)
    return str(env_config_file or default_config_file)


def get_gh_token() -> Optional[str]:
    """Get the github token from environment variable"""
    gh_token = _get_env_var(EnvironmentVariable.GH_TOKEN)
    return str(gh_token) if gh_token else None


def get_gh_user_repo() -> Optional[str]:
    """Get the github user/repo from environment variable"""
    gh_user_repo = _get_env_var(EnvironmentVariable.GH_USER_REPO)
    return str(gh_user_repo) if gh_user_repo else None


def _get_env_var(
    env_var: "EnvironmentVariable", raise_on_missing: bool = False
) -> object:
    try:
        return json.loads(os.environ[env_var.value])
    except KeyError as exc:
        if raise_on_missing:
            raise MissingEnvironmentVariableError(env_var) from exc
        return None
    except json.decoder.JSONDecodeError:
        return os.environ.get(env_var.value)


DEFAULT_CONFIG_FILE = "benchmarker.ini"


@contextmanager
def process_env(env_vars: "Dict[str, str]") -> "Generator[None, None, None]":
    """Run code with specific enviroment variables that are reset afterwards"""
    prev_env_vars = {**os.environ}
    _set_envs(env_vars)
    yield
    _set_envs(prev_env_vars)


def _set_envs(values: "Dict[str, str]") -> None:
    os.environ.clear()
    os.environ.update(**values)


class EnvironmentVariable(Enum):
    """Environment variables benchmarker cares about"""

    CONFIG = "BENCHMARKER_CONFIG"
    VERBOSE = "BENCHMARKER_VERBOSE"
    GH_TOKEN = "GITHUB_TOKEN"
    GH_USER_REPO = "GITHUB_REPO"


class MissingEnvironmentVariableError(Exception):
    """Error for missing environment variable"""

    def __init__(self, env_var: "EnvironmentVariable"):
        super().__init__()
        self.env_var = env_var
