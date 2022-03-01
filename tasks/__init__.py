from invoke import Collection

from . import (
    deps,
    dev,
    lint,
    test,
)

ns = Collection()
# Module collection tasks
ns.add_collection(deps)
ns.add_collection(lint)
ns.add_collection(test)
ns.add_collection(dev)
