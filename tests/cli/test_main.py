import pathlib
from os import PathLike

import pytest
from syrupy.assertion import SnapshotAssertion


@pytest.fixture
def main_file() -> "PathLike[str]":
    path = pathlib.Path(__file__.replace("tests", "benchmarker"), "..", "__main__.py")
    return path.resolve()


def test_main_runs_main(
    main_file: PathLike[str],
    pytester: "pytest.Pytester",
    snapshot: "SnapshotAssertion",
) -> None:
    result = pytester.runpython(main_file)
    assert snapshot == "\n".join(result.errlines)
