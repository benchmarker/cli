import configparser
from enum import Enum
from typing import (
    Optional,
    Sequence,
)

from benchmarker.cli.commands import (
    init,
    run,
)
from benchmarker.cli.utils import msg
from benchmarker.cli.utils.env import get_config_file_path

__all__ = ["main"]


def main(command: "Command", args: "Sequence[str]") -> None:
    config = parse_config()
    if command == Command.INIT:
        init.main(args, config)
    elif command == Command.RUN:
        run.main(args, config)


class Command(Enum):
    INIT = "init"
    RUN = "run"


def parse_config() -> "Optional[configparser.ConfigParser]":
    config_file_path = get_config_file_path()
    config = configparser.ConfigParser()
    files_read = config.read(config_file_path)
    if files_read:
        return config
    msg.print_no_config_file(config_file_path)
    return None
