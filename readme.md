# 液体「ekitai」

ekitai.hs is a terminal fluid simulator written in haskell. it is a submission to RUHacks 2021. the simulation algorithm is quite simple, it's a similar particle based one that games like 'the sandbox' and 'terraria' use.

## Usage
ekitai reads a text file with the desired simulation. samples can be found in the samples/ directory of this repo. valid characters for the simulation are as follows
- `#`: wall block
- `~`: water
- `@`: pump, spawns water below it
- `O`: drain, removes water around it

## Installation
### Deb based
### Arch based
### Build from Source
as this project does not use stack or cabal or anything, haskell dependencies will have to be installed separately. if you are on arch, make sure you have the following (find equivalent depending on distro):
- ghc
- ghc-static
- ghc-libs
- haskell-brick

and then simply run `make install`.

## FAQ
**why ekitai?**

ekitai is fluid in japanese. since naming your projects in a different language is apparently cool.

**why not in c?**

c would have probably been one of the best choices to do this project in, but i wanted to add a bit of challenge by using a language that i didn't know.

