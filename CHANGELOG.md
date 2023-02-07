# Revision history for du

## 0.1.0.0 -- 2023-02-06

a `du` linux command implemented with `Haskell`

an exercise for << Haskell in Depth >> with lots of comments.
## Build
```shell
cabal build
```
## Usage
```shell
cabal -v0 run du -- . -e ".hs" -d1
```
`-- .` means the base directory is `'.'`

`-e ".hs" ` means the file extension is `'.hs'`. This is a optional argument.

`-d1` means the max dir depth is 1.
