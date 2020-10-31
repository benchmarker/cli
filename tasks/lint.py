from invoke import (
    exceptions,
    task,
)

from .utils import ctx_run

LINT_PATHS = "benchmarker tests"
LINT_COMMANDS = {
    "isort": lambda fix: f"isort {'' if fix else '--check'} .",
    "black": lambda fix: f"black {'' if fix else '--check'} .",
    "flake8": lambda _: f"pflake8 {LINT_PATHS} tasks stubs *.py",
    "mypy": lambda _: f"mypy --strict {LINT_PATHS}",
}


def run_lint(ctx, section, fix):
    """Run single lint command"""
    print(f"\033[1m[{section}]\033[0m")
    ctx_run(ctx, f"poetry run python -m {LINT_COMMANDS[section](fix)}")


@task(default=True)
def all(ctx, fix=False):
    """
    Check and fix syntax using various linters
    """
    last_error = None
    for section in LINT_COMMANDS:
        try:
            run_lint(ctx, section, fix)
            print()
        except exceptions.Failure as ex:
            last_error = ex
    if last_error:
        raise last_error


def add_individual_task(name: str):
    """Add invoke task programatically"""

    @task(name=name)
    def lint(ctx, fix=False):
        run_lint(ctx, name, fix)

    globals()[name] = lint


for section in LINT_COMMANDS:
    add_individual_task(section)
