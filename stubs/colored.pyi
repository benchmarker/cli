from typing import Union

def attr(color: Union[str, int]) -> str: ...
def bg(color: Union[str, int]) -> str: ...
def fg(color: Union[str, int]) -> str: ...
def stylize(text: Union[str, int], style: str, reset: bool = True) -> str: ...
