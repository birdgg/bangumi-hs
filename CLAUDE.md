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
- `cabal test` - Run the test suite
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
- megaparsec (parser combinator library)
- tasty/tasty-hunit (testing framework)

## Architecture

The project follows standard Haskell package structure:

### Library Structure
- **src/Moe/App.hs**: Main application module
- **src/Moe/Parser/Internal/Util.hs**: Parser utilities for Chinese text processing
  - `parseChineseSeason`: Parses Chinese season indicators (第一季, 第二季, etc.)
  - `parseChineseNumber`: Parses Chinese numerals (一, 二, 三, etc.)
  - `parseNumber`: Parses Arabic numerals
- **src/Moe/Parser/BgmParser.hs**: Bangumi title parsing functionality

### Executable
- **app/Main.hs**: Executable entry point

### Test Structure
- **test/Main.hs**: Test suite entry point
- **test/Moe/Parser/Internal/UtilSpec.hs**: Comprehensive tests for parser utilities
  - Tests for `parseChineseSeason` (7 test cases)
  - Tests for `parseChineseNumber` (5 test cases)  
  - Tests for `parseNumber` (4 test cases)

The project uses:
- GHC2024 language standard
- OverloadedStrings extension by default
- Wall warning flags for all components

## Testing

The project uses a modular test structure with Tasty framework:
- Run all tests: `cabal test`
- Tests are organized by module in separate spec files
- Currently 16 tests covering Chinese text parsing functionality

## Notes

- The project focuses on parsing Chinese anime/bangumi titles and metadata
- Uses megaparsec for robust text parsing with proper error handling
- Modular architecture allows easy extension of parsing capabilities
- MIT licensed project by birdgg
