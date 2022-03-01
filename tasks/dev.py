from invoke import task

from .utils import ctx_run


@task(
    default=True,
    help={
        "cmd": "The top level command to run",
    },
)
def cli(ctx, cmd):
    """Run in development version command with args"""
    ctx_run(ctx, f"python benchmarker/cli {cmd}")
