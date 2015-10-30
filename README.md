# raskell-git-download
[![Build Status](https://travis-ci.org/jonathankochems/raskell-git-download.svg)](https://travis-ci.org/jonathankochems/raskell-git-download) [![codecov.io](http://codecov.io/github/jonathankochems/raskell-git-download/coverage.svg?branch=master)](http://codecov.io/github/jonathankochems/raskell-git-download?branch=master) [![BSD3 License](http://img.shields.io/badge/license-BSD3-brightgreen.svg)](https://tldrlegal.com/license/bsd-3-clause-license-%28revised%29) [![Documentation](https://img.shields.io/badge/Documentation-0.1.0.0-brightgreen.svg)](http://jonathankochems.github.io/raskell-git-download-doc/) 


Raskell-git-download is a simple module to download Haskell modules onto a iOS Raskell installation. 

## Installation

Download the following two files into Raskell 

  https://raw.githubusercontent.com/jonathankochems/raskell-git-download/master/src/RaskellDownload.hs 

  https://raw.githubusercontent.com/jonathankochems/raskell-git-download/master/src/RaskellDownload/Internal.hs

as `RaskellDownload.hs` and `RaskellDownload/Internal.hs`. Then load RaskellDownload.hs into the interpreter and run `installRaskellGitDownload`.

## Usage

You can define a package as follows:

```haskell
 raskellGitDownload = 
 	Package{
      packageRepository = Repository{ repository="jonathankochems/raskell-git-download",
                                      prefix="src/",
                                      branch="master"
                                    },
      rootDir = "../",
      modules = [["RaskellDownload", "Internal"],
                 ["RaskellDownload"]
                ]
    }
```

The package can then be installed using

```haskell
downloadPackage raskellGitDownload
```

## How to run tests

```
cabal configure --enable-tests && cabal build && cabal test
```

## Contributing

1. fork this repository
2. create a feature branch
3. commit and push your code to your feature branch
4. make sure all tests pass, test coverage is above 90%, and there are no hlint warnings
5. create a pull request to this repository

