from contextlib import contextmanager
from subprocess import CalledProcessError
from typing import (
    Generator,
    Optional,
)

from benchmarker.cli.utils import proc


@contextmanager
def git_checkout(commit_sha: str) -> "Generator[None, None, None]":
    current_ref = get_current_branch() or get_current_commit()
    did_stash = False
    did_checkout = False

    try:
        proc.run("git", "stash", "push", "-a", f"'{GIT_STASH_ID}'")
        did_stash = True
        proc.run("git", "checkout", commit_sha)
        did_checkout = True
        yield
    finally:
        if did_checkout:
            proc.run("git", "checkout", current_ref)
        if did_stash:
            proc.run("git", "stash", "pop")


def get_current_commit() -> "Optional[str]":
    try:
        return proc.run("git", "rev-parse", "HEAD").stdout
    except CalledProcessError:
        return None


def get_current_branch() -> "Optional[str]":
    try:
        result = proc.run("git", "rev-parse", "--abbrev-ref", "HEAD").stdout
        return None if "(no branch)" in result else result
    except CalledProcessError:
        return None


GIT_STASH_ID = "temp-stash-for-benchmarker"
