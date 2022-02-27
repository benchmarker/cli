import argparse
import os
from asyncio import subprocess
from configparser import ConfigParser
from typing import (
    List,
    Sequence,
    Tuple,
    Union,
)

from benchmarker.cli.utils import (
    err,
    msg,
    process,
)

__all__ = ["main"]


def main(args: Sequence[str], config: "Union[ConfigParser, None]") -> None:
    """Initialise the branch in the repo the benchmark results are pushed to"""
    print("args", args)
    parsed_args, _ = parse_args(args)
    print(parsed_args, _)
    result_branch = parsed_args.branch or get_result_branch(config)
    if result_branch:
        create_orphan_branch(result_branch)
    else:
        msg.print_no_result_branch()
        err.exit_with_code(err.ErrorCode.NO_RESULT_BRANCH)


def parse_args(args: "Sequence[str]") -> "Tuple[argparse.Namespace, List[str]]":
    """
    Parse the arguments used to execute the init command
    :return: parsed and unknown arguments
    """
    parser = argparse.ArgumentParser(
        description="Init benchmarker branch",
        add_help=True,
    )
    parser.add_argument(
        "-b",
        dest="branch",
        type=str,
        help="The branch create. Overrides config file",
        required=False
    )

    return parser.parse_known_args(args)


def get_result_branch(config: "Union[ConfigParser, None]") -> "Union[str, None]":
    return config.get("options", "result_branch") if config else None


def create_orphan_branch(branch: str) -> None:
    initial_branch = get_current_branch()
    msg.print_orphan_initial_branch(initial_branch)
    if not initial_branch:
        msg.print_no_initial_branch()
        err.exit_with_code(err.ErrorCode.NO_INITIAL_BRANCH)
    try:
        msg.print_create_orphan_branch(branch)
        process.run("git", "checkout", "--orphan", branch)
        git_clean_current()
        if os.path.exists(GITIGNORE_FILE):
            process.run("git", "rm", GITIGNORE_FILE)
        create_result_branch_readme()
        process.run("git", "add", README_FILE)
        process.run("git", "commit", "-n", "-a", "-m", "'Initial Commit'")
        process.run("git", "push", "origin", branch)
    finally:
        process.run("git", "reset", "--hard")
        process.run("git", "checkout", "-f", initial_branch)


def get_current_branch() -> str:
    args = ("git", "symbolic-ref", "--short", "HEAD")
    run_result = process.run(*args, stdout=subprocess.PIPE)
    return run_result.stdout.strip()


def git_clean_current() -> None:
    process.run("git", "rm", "-rf", ".")


README_FILE = "README.md"
GITIGNORE_FILE = ".gitignore"


def create_result_branch_readme() -> None:
    with open(README_FILE, "w") as readme_file:
        readme_file.write("# Benchmark Results\n")
