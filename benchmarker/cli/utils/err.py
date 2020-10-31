import sys
from enum import Enum
from typing import NoReturn


class ErrorCode(Enum):
    """Error codes"""

    NO_RESULT_BRANCH = 1
    NO_INITIAL_BRANCH = 2
    KEYBOARD_INTERRUPT = 3


def exit_with_code(*error_codes: "ErrorCode") -> "NoReturn":
    """Exit with the error codes provided"""
    err_code = 0
    for code in error_codes:
        err_code |= pow(2, code.value)
    sys.exit(err_code)
