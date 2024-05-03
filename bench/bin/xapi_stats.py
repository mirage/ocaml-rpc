#!/usr/bin/python3
"""Create a TAR file of XAPI pool and VM object XMLRPC replies.

This TAR file can be unpacked, and given as input to bin/rpc_stats.exe.
"""

import io
import sys
import tarfile
import time
import xmlrpc
import xmlrpc.client

# ruff: noqa: FA100
# Deprecated, but needed for Python3.6
from typing import Dict

import XenAPI


def _add_xml(
    tar: tarfile.TarFile,
    kind: str,
    objs: Dict[str, "xmlrpc.client._Marshallable"],
) -> None:
    for ref, obj in objs.items():
        data = xmlrpc.client.dumps((obj,), methodresponse=True)
        tarinfo = tarfile.TarInfo(f"{kind}/{ref}.xml")
        tarinfo.size = len(data)
        tarinfo.mtime = time.time()
        tar.addfile(tarinfo, io.BytesIO(data.encode("utf-8")))


if __name__ == "__main__":
    import logging

    logging.basicConfig(level=logging.INFO)
    logger = logging.getLogger(__name__)
    logger.info("Connecting to local XAPI socket")
    session = XenAPI.xapi_local()
    logger.info("Logging in")
    session.login_with_password("", "", "stats.py", "0.5")
    try:
        logger.info("Retrieving pool objects")
        pools = session.xenapi.pool.get_all_records()
        logger.info("Retrieving VM objects")
        vms = session.xenapi.VM.get_all_records()
    finally:
        logger.info("Logging out")
        session.logout()
    logger.info("Dumping XMLRPC responses")
    with tarfile.open(fileobj=sys.stdout.buffer, mode="w|") as tar:
        _add_xml(tar, "pool", pools)
        _add_xml(tar, "vm", vms)
    logger.info("Dumped response to TAR file on stdout")
