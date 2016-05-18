bc-tools
========

This package contains command-line tools for working with blockchains.

## Install

First install [stack](https://www.stackage.org/), then:

```
git clone https://github.com/shmookey/bc-tools
cd bc-tools
make
sudo make install
```

Stack will download an appropriate Haskell distribution on first build.

## jrc

**Usage**

```
jrc -- JSON-RPC request client

Usage: jrc SOCKET METHOD
  Query a JSON-RPC server.

Available options:
  -h,--help                Show this help text
  SOCKET                   Path to socket file.
  METHOD                   Method name to call.
```

**Example**

```
user@host:~$ jrc /tmp/geth.ipc eth_blockNumber
0x5
```

## arc

**Usage**

```
arc -- asynchronous request client

Usage: arc SOCKET DATA
  Query an asynchronously-responding server.

Available options:
  -h,--help                Show this help text
  SOCKET                   Path to socket file.
  DATA                     Data to send.
```

**Example**

```
user@host:~$ arc /tmp/geth.ipc '{"jsonrpc":"2.0","method":"eth_blockNumber","params":[],"id":83}'
{"jsonrpc":"2.0","id":83,"result":"0x5"}
```

