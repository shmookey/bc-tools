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

## arc

```
arc -- asynchronous request client

Usage: arc SOCKET DATA
  Query an asynchronously-responding server.

Available options:
  -h,--help                Show this help text
  SOCKET                   Path to socket file.
  DATA                     Data to send.
```
