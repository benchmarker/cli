from enum import Enum
from typing import Sequence

from benchmarker.cli.commands import (
    init,
    run,
)
from benchmarker.cli.utils import conf

__all__ = ["main"]


def main(command: "Command", args: "Sequence[str]") -> None:
    config = conf.load()
    if command == Command.INIT:
        init.main(args, config)
    elif command == Command.RUN:
        run.main(args, config)


class Command(Enum):
    INIT = "init"
    RUN = "run"
