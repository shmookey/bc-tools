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

## munt

Munt is a compact language for manipulating streams of binary data, suited to
use on the command line and in scripts. Munt programs are *pipelines* composed
by chaining functions together in a similar way to programs piped together by
the shell's own pipe operator. Munt extends this concept with a more flexible
piping mechanism and a large library of built-in functions, supporting a range
of stream processing scenarios including cryptography*, signal processing and
working with binary protocols.

Compared to other programming languages, munt has no types or data structures,
no variables or named values, no side-effects, and flow control is "downstream-
only" - there are no for-loops, no recursion and no "backwards jump". In spite
of these omissions it's quite powerful, and very easy to pick up because of
them. It's also fast and memory-efficient. Programs only read as much from the
stream as they need to generate the next chunk of output, then forget about it.
Your old laptop from 5 years ago can probably stream gigabits of data through
a munt program without ever runing out of memory.

\* Cryptography does not mean security! Use munt for experimenting with
cryptographic primitives and implementing your own. Don't use it to sign tokens
in your web service. Munt is not security software.

**Usage**

```
munt - cryptographic function composition

Usage: munt EXPR
  Transform input with cryptographic functions.

Available options:
  -h,--help                Show this help text
  EXPR                     Function expression to use for transformation.

Available functions:
  Encode   b64e b64d
  Format   bin dec hex unbin undec unhex
  Math     + - * / % ^
  Bitwise  and or xor not rsh lsh
  Util     id trace
  List     append drop head init last len prepend reverse tail take
  Stream   after before bytes concat consume count dup filter flip lines repeat
           unlines unwords words
  Cipher   aes128d aes128e aes192d aes192e aes256d aes256e bfe bfd bf64e bf64d
           bf128e bf128d bf256e bf256d bf448e bf448d dese desd deseee3e
           deseee3d desede3e desede3d deseee2e deseee2d desede2e desede2d
           cam128e cam128d
  Hash     blake2s256 blake2s224 blake2sp256 blake2sp224 blake2b512 blake2bp512
           md2 md4 md5 sha1 sha224 sha256 sha384 sha512 sha512t256 sha512t224
           sha3512 sha3384 sha3256 sha3224 keccak512 keccak384 keccak256
           keccak224 ripemd160 skein256256 skein256224 skein512512 skein512384
           skein512256 skein512224 whirlpool
```

**Examples**

```
user@host:~$ echo -n "hello, world" | munt b64e
aGVsbG8sIHdvcmxkCg== 
user@host:~$ echo -n "hello, world" | munt 'b64e -> b64d'
hello, world
user@host:~$ echo -n " | munt 'b64e -> b64d'
hello, world
user@host:~$ echo -n "hello, world" | munt 'md5 -> hex'
E4D7F1B4ED2E42D15898F4B27B019DA4
user@host:~$ echo -n "AB" | munt '+ 1 -> hex'
4242
```

**More advanced examples**

Generate an ethereum node private/public key pair:

```
user@host:~$ openssl ecparam -name secp256k1 -genkey -outform DER | munt 'repeat 2->1/2{drop 7->take 32}->2/2{drop 53->take 65}->hex->unlines'
```

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

