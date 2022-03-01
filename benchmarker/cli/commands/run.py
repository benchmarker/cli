import argparse
from configparser import ConfigParser
from pathlib import Path
from typing import (
    List,
    Sequence,
    Tuple,
)

import github

from benchmarker.cli.utils import (
    env,
    gh,
    git,
    msg,
    proc,
)
from benchmarker.cli.utils.conf import (
    get_commit_msg,
    get_folder_path,
    get_result_branch,
    get_result_name,
)

__all__ = ["main"]


def main(args: Sequence[str], config: "ConfigParser") -> None:
    """Run benchmark for given commit hash"""
    known_args, _ = parse_args(args)
    config_commands = config["commands"] or dict[str, str]()
    if known_args.command in config_commands:
        command = config_commands[known_args.command]
        command_name = known_args.command
    else:
        msg.print_command_not_in_config(known_args.command)
        command = str(known_args.command)
        command_name = DEFAULT_COMMAND_RESULT

    try:
        commit_sha = (
            known_args.commit or env.get_gh_commit_sha() or git.get_current_commit()
        )
        run_id = known_args.run_id or commit_sha or DEFAULT_RUN_ID
        result_folder_path = Path(get_result_folder_path(run_id, config))
        result_folder_path.mkdir(parents=True, exist_ok=True)

        result_file_name = get_result_file_name(command_name, config)
        result_file_path = result_folder_path.joinpath(result_file_name)
        run_args = ("python", "-m", "pyperf")
        proc.run(*run_args, "command", "-o", result_file_path, "--", command)
        proc.run(*run_args, "check", result_file_path)
    except Exception as e:
        raise SystemExit(e) from e

    if not known_args.report:
        return

    with open(result_file_path, "r", encoding="UTF-8") as result_file:
        try:
            gh.get_repo().create_file(
                path=str(result_file_path),
                message=get_result_commit_msg(
                    config, run_id=run_id, commit_sha=commit_sha
                ),
                content=result_file.read(),
                branch=get_result_branch(config),
            )
        except github.GithubException as e:
            raise SystemExit(e) from e


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
        "command",
        type=str,
        help="Name of command in config to run.",
    )
    parser.add_argument(
        "-c",
        "--commit",
        dest="commit",
        type=str,
        help="The commit SHA to run the benchmark commands on.",
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
        "-u",
        "--run-id",
        dest="run_id",
        type=str,
        help="The id for this particular run. Defaults to commit when not given.",
        required=False,
    )

    return parser.parse_known_args(args)


def get_result_folder_path(run_id: str, config: "ConfigParser") -> str:
    return get_folder_path(config).format(run_id=run_id)


def get_result_file_name(command_name: str, config: "ConfigParser") -> str:
    return get_result_name(config).format(command_name=command_name)


def get_result_commit_msg(
    config: "ConfigParser", *, run_id: str, commit_sha: str
) -> str:
    return get_commit_msg(config).format(
        run_id=run_id,
        commit_sha=commit_sha,
        commit_sha_short=commit_sha[:7],
    )


DEFAULT_COMMAND_RESULT = "result"
DEFAULT_RUN_ID = "-"
