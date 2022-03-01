import argparse
from configparser import ConfigParser
from typing import (
    List,
    Optional,
    Sequence,
    Tuple,
)

from github import (
    Github,
    GithubException,
)

from benchmarker.cli.utils import (
    env,
    err,
    gh,
    msg,
)

__all__ = ["main"]


def main(args: Sequence[str], config: "Optional[ConfigParser]") -> None:
    """Initialise the branch in the repo the benchmark results are pushed to"""
    known_args, _ = parse_args(args)
    gh_token = known_args.gh_token or env.get_gh_token()
    user_repo = known_args.user_repo or env.get_gh_user_repo()
    result_branch = known_args.branch or get_result_branch(config)
    if not gh_token:
        msg.print_no_github_token()
        err.exit_with_code(err.ErrorCode.NO_GITHUB_TOKEN)
    elif not user_repo:
        msg.print_no_github_user_repo()
        err.exit_with_code(err.ErrorCode.NO_GITHUB_USER_REPO)
    elif not result_branch:
        msg.print_no_result_branch()
        err.exit_with_code(err.ErrorCode.NO_RESULT_BRANCH)

    try:
        gh.create_orphan_branch(
            Github(str(gh_token)),
            str(user_repo),
            str(result_branch),
            known_args.fail_existing,
        )
    except GithubException as e:
        raise SystemExit(e) from e


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
        "--branch",
        dest="branch",
        type=str,
        help="The branch to be created.\nOverrides config file.",
        required=False,
    )
    parser.add_argument(
        "-r",
        "--repo",
        dest="user_repo",
        type=str,
        help="""The github {user}/{repo} to be used.
Overrides environment variable $GITHUB_REPO.""",
        required=False,
    )
    parser.add_argument(
        "-t",
        "--token",
        dest="gh_token",
        type=str,
        help="""The github {token} to be used.
Overrides environment variable $GITHUB_TOKEN.""",
        required=False,
    )
    parser.add_argument(
        "-f",
        "--fail-existing",
        dest="fail_existing",
        action="store_true",
        help="When given, fail the initialisation if the branch already exists.",
        required=False,
    )

    return parser.parse_known_args(args)


def get_result_branch(config: "Optional[ConfigParser]") -> "Optional[str]":
    return config.get("options", "result_branch") if config else None
