# Contributing

:tada: Thanks for taking the time to contribute! :tada:

The following is a set of guidelines for contributing to this [repo](https://github.com/benchmarker/cli).
These are mostly guidelines, not rules. Use your best judgment, and feel free to propose changes to this document.

## Code of Conduct

This project and everyone participating in it is governed by our [Code of Conduct](CODE_OF_CONDUCT.md). By participating, you are expected to uphold this code.

## Get Started

Install dependencies and run tests.

```sh
./script/bootstrap
inv test
```

## Development

Add more dependencies.

```sh
inv deps.add [packages] [--dev]
```

Run development version commands.

```sh
inv run 'init --help'
```

## Resources

- [Python](https://docs.python.org/3/)
- [Pytest](https://docs.pytest.org/en/7.0.x/)
