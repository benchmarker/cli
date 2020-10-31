import os
from contextlib import contextmanager
from typing import (
    Dict,
    Iterator,
    List,
    Union,
)

from invoke import Context

ENV_FILE = ".env"
ENV_PATH_DELIMITER = ":"


def ctx_run(ctx: "Context", *args: str, **kwargs: str):
    """Wraps context run as posix systems"""
    kwargs["pty"] = os.name == "posix"
    poetry_bin_path = get_poetry_bin_path(load_env_file())
    path = ENV_PATH_DELIMITER.join(filter(bool, [poetry_bin_path, *get_current_path()]))
    with env_context(PATH=path):
        return ctx.run(*args, **kwargs)


def get_poetry_bin_path(env_file: "Dict[str, str]") -> "Union[str, None]":
    """Get poetry bin path from environment variables"""
    poetry_home = env_file.get("POETRY_HOME")
    return poetry_home and f"{poetry_home}/bin"


def load_env_file() -> "Dict[str, str]":
    """Load environment variables from .env file"""
    env_dict = {}

    if os.path.exists(ENV_FILE):
        with open(".env", "r") as env_file:
            for line in env_file.readlines():
                [env_var, env_value] = line.split("=")
                env_dict[env_var.strip()] = env_value.strip()

    return env_dict


def get_current_path() -> "List[str]":
    """Get the current PATH environment as list"""
    return os.environ.get("PATH", "").split(ENV_PATH_DELIMITER)


@contextmanager
def env_context(**kwargs: str) -> "Iterator[None]":
    """Set temporary environment arguments reverted after context is exited"""
    prev_env = {**os.environ}
    try:
        yield os.environ.update(kwargs)
    finally:
        os.environ.clear()
        os.environ.update(prev_env)
