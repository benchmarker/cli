import argparse
from configparser import ConfigParser
from typing import (
    List,
    Optional,
    Sequence,
    Tuple,
)

__all__ = ["main"]


def main(args: Sequence[str], config: "Optional[ConfigParser]") -> None:
    """Run benchmark for given commit hash"""
    known_args, _ = parse_args(args)
    print(known_args.__dict__)
    command = config["commands"] if config else {}
    print("command", command)


def parse_args(args: "Sequence[str]") -> "Tuple[argparse.Namespace, List[str]]":
    """
    Parse the arguments used to execute the run command
    :return: parsed and unknown arguments
    """
    parser = argparse.ArgumentParser(
        description="Run benchmarker",
        add_help=True,
    )
    parser.add_argument(
        "-c",
        "--commit",
        dest="commit",
        type=str,
        help="The commit to run the benchmark commands on.",
        required=False,
    )
    parser.add_argument(
        "-r",
        "--report",
        dest="report",
        action="store_true",
        help="If the benchmark results should be shown as a GitHub status.",
        required=False,
    )
    parser.add_argument(
        "-a",
        "--command",
        dest="command",
        type=str,
        help="When provided the benchmarker runs the single command.",
        required=False,
    )

    return parser.parse_known_args(args)
