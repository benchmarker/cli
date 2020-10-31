import configparser
from enum import Enum
from typing import (
    Sequence,
    Union,
)

from benchmarker.cli.commands import init
from benchmarker.cli.utils import msg
from benchmarker.cli.utils.env import get_config_file_path

__all__ = ["main"]


def main(command: "Command", args: "Sequence[str]") -> None:
    config = parse_config()
    if command == Command.INIT:
        init.main(args, config)


class Command(Enum):
    INIT = "init"


def parse_config() -> "Union[configparser.ConfigParser, None]":
    config_file_path = get_config_file_path()
    config = configparser.ConfigParser()
    files_read = config.read(config_file_path)
    if files_read:
        return config
    msg.print_no_config_file(config_file_path)
    return None
