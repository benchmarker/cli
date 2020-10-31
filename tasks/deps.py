from invoke import task

from .utils import ctx_run


@task(default=True)
def install(ctx):
    """Install dev dependencies"""
    ctx_run(ctx, "poetry install")


@task(
    help={
        "packages": "The list of packages to install",
        "dev": "If the dependency is dev only, defaults to False",
    },
)
def add(ctx, packages, dev=False):
    """Add new dependency"""
    dev_flag = "-D" if dev else ""
    ctx_run(ctx, f"poetry add {packages} {dev_flag}")
