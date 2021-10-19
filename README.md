# Benchmarker CLI

**NOTE:** This package is still a work in progress.

[![Haskell CI](https://github.com/benchmarker/cli/workflows/Haskell%20CI/badge.svg)](https://github.com/benchmarker/cli/actions?query=workflow%3A%22Haskell+CI%22)
[![Hackage CI](https://matrix.hackage.haskell.org/api/v2/packages/benchmarker-cli/badge)](https://hackage.haskell.org/package/benchmarker-cli)

Command line tool for running and publishing benchmarks to a git repository.

It uses [criterion][criterion] under the hood for performance analysis heavy lifting.

## Prerequisites

| Name         | Description                                                                                          |
| ------------ | ---------------------------------------------------------------------------------------------------- |
| [`git`][git] | Fast, scalable, distributed revision control system. Benchmarker assumes `git` as the underlying vcs |

## Install

### [Homebrew][homebrew]

```sh
brew install benchmarker/s/cli
```

### [Hackage][hackage]

```sh
stack install benchmarker-cli
```

## Usage

```sh
$ benchmarker --help
# TODO: show output
```

### `init`

Ready the repository by creating the orphaned branch to store all benchmark results.

Assumes this is running from an initialised git repository.

```sh
$ benchmarker init --help
Usage: benchmarker init [-v|--verbosity] [-d|--folderPath FOLDER_PATH]
                        [-b|--resultBranch REPO_BRANCH]
  Initialise repository for benchmarking

Available options:
  -v,--verbosity           Level of verbosity used for the command output e.g.
                           -vv
  -d,--folderPath FOLDER_PATH
                           Folder path to the benchmark result
                           file (default: "run/{run_id}")
  -b,--resultBranch REPO_BRANCH
                           The repository branch that the results will be stored
                           on (default: "benchmarks")
  -h,--help                Show this help text
```

### `run`

Runs the commands specified in the config file.

If the repository is clean commits the results using the current commit as the run id.

```sh
$ benchmaker run --help
# TODO: show output
```

### `compare`

Compares the results of multiple benchmarks

```sh
$ benchmarker compare --help
# TODO: show output
# -e: When this is enabled the exit code returned depends on the failure conditions specified in your config file.
```

### `commit`

Commit benchmark results to repo branch

```sh
$ benchmarker commit --help
# TODO: show output
# -p: When this is enabled the branch is pushed to the remote tracking target.
```

## Integration

CI Recipes for using this tool.

### [GitHub][github-action]

Easily add this to your GitHub CI worflow.
See the actions repository for [documentation][github-action].

## Contributing

Feel free to open a PR or GitHub issue. Contributions welcome!

To develop locally, clone this repository and run . script/bootstrap to setup dependencies.

See contributing [guide][contributing].

## Contributors

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore-start -->
<!-- markdownlint-disable -->
<table>
  <tr>
    <td align="center"><a href="http://emmanuel.ogbizi.com"><img src="https://avatars0.githubusercontent.com/u/2528959?v=4" width="100px;" alt=""/><br /><sub><b>Emmanuel Ogbizi</b></sub></a><br /><a href="#ideas-iamogbz" title="Ideas, Planning, & Feedback">🤔</a> <a href="#design-iamogbz" title="Design">🎨</a> <a href="#infra-iamogbz" title="Infrastructure (Hosting, Build-Tools, etc)">🚇</a> <a href="https://github.com/tophat/syrupy/commits?author=iamogbz" title="Code">💻</a> <a href="https://github.com/tophat/syrupy/commits?author=iamogbz" title="Documentation">📖</a> <a href="https://github.com/tophat/syrupy/commits?author=iamogbz" title="Tests">⚠️</a></td>
  </tr>
</table>

<!-- markdownlint-enable -->
<!-- prettier-ignore-end -->

<!-- ALL-CONTRIBUTORS-LIST:END -->

This section is automatically generated via tagging the all-contributors bot in a PR:

```text
@all-contributors please add <username> for <contribution type>
```

<!-- LINKS SORTED ALPHABETICALLY -->

[contributing]: ./CONTRIBUTING.md
[criterion]: https://github.com/bos/criterion
[git]: https://github.com/git/git
[github-action]: https://github.com/benchmarker/github-action
[hackage]: http://hackage.haskell.org/
[homebrew]: https://brew.sh/
