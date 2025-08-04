# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is `bangumi-hs`, a Haskell project that appears to be in early development stages. The project uses Cabal as its build system and is structured as a standard Haskell package with library, executable, and test suite components.

## Development Commands

### Build System
- `cabal build` - Build the entire project
- `cabal build bangumi-hs` - Build just the executable
- `cabal build bangumi-hs:lib` - Build just the library
- `cabal run bangumi-hs` - Build and run the executable
- `cabal test` - Run the test suite (currently not implemented)
- `cabal clean` - Clean build artifacts

### Code Formatting
- `fourmolu --mode inplace src/ app/ test/` - Format all Haskell source files using fourmolu
- `fourmolu --mode check src/ app/ test/` - Check formatting without modifying files

The project uses fourmolu with specific configuration in `fourmolu.yaml`:
- 4-space indentation
- Trailing function arrows
- Leading comma style
- Diff-friendly import/export style

### Dependencies
The project currently depends on:
- aeson (JSON handling)
- servant/servant-client/servant-server (web API framework)
- wai/warp (web application interface and server)
- pretty-simple (pretty printing)

## Architecture

The project follows standard Haskell package structure:

- **src/Bangumi.hs**: Main library module with `someFunc` placeholder
- **app/Main.hs**: Executable entry point (currently has import issue - imports `MyLib` instead of `Bangumi`)
- **test/Main.hs**: Test suite entry point (not yet implemented)

The project uses:
- GHC2024 language standard
- OverloadedStrings extension by default
- Wall warning flags for all components

## Notes

- The executable currently has an incorrect import (`MyLib` instead of `Bangumi`)
- Test suite is not yet implemented
- The project appears to be targeting web API development based on servant dependencies
- MIT licensed project by birdgg
