"""
Test for the generated Python commadline code.
Currently it just calls the only function in the interface called "add".
"""

import bindings


class CalcImplementation(object):
    """
    Implementation of the test interface "Calc" in test_pythongen.ml
    """

    def __init__(self):
        pass

    def add(self, int1, int2):
        """Add two numbers"""
        return int1 + int2


def _call_calc_command():
    """Parse the arguments and call the required command"""
    cmd = bindings.Calc_commandline(CalcImplementation())
    cmd.add()


if __name__ == "__main__":
    _call_calc_command()
