"""Run process utility functions"""
import re
import subprocess
import sys
from typing import Any

from .err import (
    ErrorCode,
    exit_with_code,
)
from .msg import (
    print_process_interrupted,
    print_unable_to_run,
)

__all__ = ["clean_output", "run"]


def clean_output(output: str) -> str:
    """Removes ansi color codes from string"""
    return re.sub(r"\x1B[@-_][0-?]*[ -/]*[@-~]", "", str(output).strip())


def run(*args: "Any", **kwargs: "Any") -> subprocess.CompletedProcess[str]:
    """
    Wraps subprocess.run passing varargs as the first parameter and kwargs as is.
    Handles keyboard interrupt and called process error with sys exit error code.
    """
    return _run_with_error_handler(*args, **kwargs)


def _run_with_error_handler(
    *args: "Any", **kwargs: "Any"
) -> "subprocess.CompletedProcess[str]":
    try:
        return subprocess.run(args, encoding="UTF-8", **kwargs, check=True)
    except KeyboardInterrupt as interrupt_e:
        print_process_interrupted(interrupt_e)
        exit_with_code(ErrorCode.KEYBOARD_INTERRUPT)
    except subprocess.CalledProcessError as process_e:
        print_unable_to_run(process_e)
        sys.exit(process_e.returncode)
