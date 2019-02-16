# blockheaders-dl

Connects to Electrum servers and downloads the blockchain headers.

## Usage

```
$ blockheaders-dl -h
Help Options:
  -h, --help
    Show option summary.
  --help-all
    Show all help options.

Application Options:
  --chain :: chain
    default: BitcoinCore
  --net :: net
    default: Mainnet
```

For example, to download the Bitcoin Cash testnet chain headers:

```
$ blockheaders-dl --chain=BitcoinCash --net=Testnet
```
