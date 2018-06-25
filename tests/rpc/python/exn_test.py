"""
Tests that the exceptions are correctly generated.
"""

import bindings

if __name__ == "__main__":
    try:
        raise bindings.Error1("test")
    except bindings.Error1 as exn:
        pass
    try:
        raise bindings.Error2((4, True))
    except bindings.Error2 as exn:
        pass
    try:
        raise bindings.Error3((4, True, "error"))
    except bindings.Error3 as exn:
        pass
    try:
        raise bindings.Error2(("4", "True"))
    except bindings.TypeError:
        pass
    except _:
        raise Exception("Should have raised TypeError")
