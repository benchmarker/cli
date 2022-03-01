import argparse
import sys
from typing import (
    List,
    Sequence,
    Tuple,
)

from benchmarker import __version__
from benchmarker.cli import commands
from benchmarker.cli.utils import msg

__all__ = ["main"]


def main(version_number: str = __version__) -> None:
    """Run main benchmarker logic"""
    msg.print_running_version(version_number)
    parsed_args, unknown_args = parse_args(sys.argv[1:])
    try:
        command = commands.Command(parsed_args.cmd)
    except ValueError as e:
        raise SystemExit(e) from e
    commands.main(command, unknown_args)


def parse_args(args: "Sequence[str]") -> "Tuple[argparse.Namespace, List[str]]":
    """
    Parse the arguments used to execute the benchmarker
    :return: parsed and unknown arguments
    """
    parser = argparse.ArgumentParser(
        description="Run benchmarker",
        add_help=False,
    )
    parser.add_argument(
        "cmd",
        metavar="command",
        type=str,
        help="The command to be run e.g. init",
    )

    return parser.parse_known_args(args)
