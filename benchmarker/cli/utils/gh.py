from typing import Optional

from github import (
    Branch,
    Github,
    GithubException,
    InputGitTreeElement,
    Repository,
)

from benchmarker.cli.utils import (
    err,
    msg,
)


def create_orphan_branch(
    gh: "Github", repo: str, branch: str, fail_existing: bool = False
) -> None:
    if _get_branch(gh, repo, branch):
        return (
            err.exit_with_code(err.ErrorCode.RESULT_BRANCH_EXISTS)
            if fail_existing
            else None
        )

    gh_repo = _get_repo(gh, repo)
    gh_tree = gh_repo.create_git_tree([INITIAL_TREE_ELEMENT])
    gh_commit = gh_repo.create_git_commit(INITIAL_COMMIT_MSG, gh_tree, [])
    msg.print_create_orphan_branch(branch, repo)
    gh_repo.create_git_ref(f"refs/heads/{branch}", gh_commit.sha)


INITIAL_COMMIT_MSG = "Initial commit"
INITIAL_TREE_ELEMENT = InputGitTreeElement(".gitignore", "100644", "blob", "")


def _get_branch(gh: "Github", repo: str, branch: str) -> "Optional[Branch.Branch]":
    gh_branch = None
    try:
        gh_branch = _get_repo(gh, repo).get_branch(branch)
        msg.print_result_branch_exists(branch, repo)
    except GithubException:
        msg.print_result_branch_not_found(branch, repo)
    return gh_branch


def _get_repo(gh: "Github", repo: str) -> "Repository.Repository":
    try:
        return gh.get_repo(repo)
    except GithubException as e:
        raise SystemExit(e) from e
