from invoke import task

from .utils import ctx_run


@task(
    default=True,
    help={
        "cmd": "The top level command to run",
        "args": "The args passed to the command wrapped in single quotes e.g. '--help'",
    },
)
def cli(ctx, cmd, args=""):
    """Run in development version command with args"""
    ctx_run(ctx, f"python benchmarker/cli {cmd} {args}")
