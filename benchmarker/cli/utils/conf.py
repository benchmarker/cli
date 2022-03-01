import configparser

from benchmarker.cli.utils import (
    env,
    msg,
)


def load() -> "configparser.ConfigParser":
    config_file_path = env.get_config_file_path()
    config = configparser.ConfigParser(default_section=DEFAULT_SECTION)
    config.read_dict(DEFAULT_CONFIG)
    if not config.read(config_file_path):
        msg.print_no_config_file(config_file_path)
    return config


def get_folder_path(config: "configparser.ConfigParser") -> str:
    return config.get(DEFAULT_SECTION, OPTION_FOLDER_PATH)


def get_result_branch(config: "configparser.ConfigParser") -> str:
    return config.get(DEFAULT_SECTION, OPTION_RESULT_BRANCH)


def set_result_branch(config: "configparser.ConfigParser", result_branch: str) -> None:
    return config.set(DEFAULT_SECTION, OPTION_RESULT_BRANCH, result_branch)


def get_result_name(config: "configparser.ConfigParser") -> str:
    return config.get(DEFAULT_SECTION, OPTION_RESULT_NAME)


def get_commit_msg(config: "configparser.ConfigParser") -> str:
    return config.get(DEFAULT_SECTION, OPTION_COMMIT_MSG)


DEFAULT_SECTION = "options"
OPTION_COMMIT_MSG = "commit_msg"
OPTION_FAILURE_HEAD = "failure_head"
OPTION_FAILURE_MAIN = "failure_main"
OPTION_FOLDER_PATH = "folder_path"
OPTION_RESULT_BRANCH = "result_branch"
OPTION_RESULT_NAME = "result_name"
OPTION_VERBOSITY = "verbosity"
DEFAULT_CONFIG = {
    DEFAULT_SECTION: {
        # Commit message used when saving benchmark run results
        OPTION_COMMIT_MSG: "build: benchmark run {commit_sha_short}",
        # Failure condition when comparing the current head against target
        OPTION_FAILURE_HEAD: 1,
        # Failure condition when comparing the current branch against main
        OPTION_FAILURE_MAIN: 1,
        # path to benchmark results on the branch
        OPTION_FOLDER_PATH: "./artifacts/benchmarker/{run_id}",
        # branch that will be used to commit results
        OPTION_RESULT_BRANCH: "benchmarks",
        # name of the result file
        OPTION_RESULT_NAME: "{command_name}.json",
        # 0 (quiet), 1 (normal), 2 (verbose) | -v (1)
        OPTION_VERBOSITY: 1,
    }
}
