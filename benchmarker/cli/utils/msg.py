"""Messages printed by benchmarker"""
from enum import (
    Enum,
    IntEnum,
)
from subprocess import CalledProcessError

from colored import (
    fg,
    stylize,
)

from .env import (
    EnvironmentVariable,
    is_verbose_logging,
)

__all__ = [
    "print_env_var_missing",
    "print_running_version",
    "print_no_config_file",
    "print_no_result_branch",
    "print_process_interrupted",
    "print_unable_to_run",
]


def print_env_var_missing(env_var: "EnvironmentVariable") -> None:
    """Print message for missing environment variable"""
    _print_error(f"Environment variable '{env_var.value}' missing")


def print_running_version(version_number: str) -> None:
    """Print which version of current benchmarker"""
    _print(f"Executing benchmarker version {version_number}", level=MessageLevel.QUIET)


def print_no_config_file(config_file: str) -> None:
    _print(f"No benchmarker config loaded from '{config_file}'")


def print_no_result_branch() -> None:
    _print("No result branch could be loaded from config")


def print_no_initial_branch() -> None:
    _print("No branch found. Please confirm this is running from a git repo")


def print_orphan_initial_branch(branch: str) -> None:
    _print(f"Create orphan from initial branch: {branch}", level=MessageLevel.QUIET)


def print_create_orphan_branch(branch: str) -> None:
    _print(f"Create orphan branch with name: {branch}", level=MessageLevel.QUIET)


def print_process_interrupted(exc: "KeyboardInterrupt") -> None:
    """Print error for interrupt handler"""
    _print(f"\nInterrupted. {exc}")


def print_unable_to_run(exc: "CalledProcessError") -> None:
    """Print error for failed sub process run"""
    _print(str(exc), level=MessageLevel.QUIET)


class Color(Enum):
    """Message colors"""

    ERROR = fg("red")
    NOTICE = fg("yellow")


class MessageLevel(IntEnum):
    """Message levels"""

    LOUD = 2
    NORMAL = 1
    QUIET = 0


def _print_error(text: str) -> None:
    _print_stylized(text, Color.ERROR, MessageLevel.LOUD)


def _print_stylized(
    text: str, color: "Color", level: "MessageLevel" = MessageLevel.NORMAL
) -> None:
    _print(_stylize(text, color), level=level)


def _stylize(text: str, color: "Color") -> str:
    return stylize(text, color.value)


def _print(*args: object, level: "MessageLevel" = MessageLevel.NORMAL) -> None:
    if level < _level():
        return

    print(*args)


def _level() -> MessageLevel:
    """Minium threshold level for logging to occur"""

    return MessageLevel.QUIET if is_verbose_logging() else MessageLevel.NORMAL
