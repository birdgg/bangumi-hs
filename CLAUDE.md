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
- persistent/persistent-sqlite/persistent-template (database ORM)
- tasty/tasty-hunit (testing framework)

## Architecture

The project follows standard Haskell package structure:

### Backend Structure (Haskell)
- **src/Moe/App.hs**: Main application module
- **src/Moe/Database/Model.hs**: Persistent database models
  - `Bangumi` model with fields: titleZh, titleJp, season, cover, group, totalEps, currentEp, tags, rss
  - Automatic migration support with `migrateAll`
- **src/Moe/Parser/Internal/Util.hs**: Parser utilities for Chinese text processing
  - `parseChineseSeason`: Parses Chinese season indicators (第一季, 第二季, etc.)
  - `parseChineseNumber`: Parses Chinese numerals (一, 二, 三, etc.)
  - `parseNumber`: Parses Arabic numerals
- **src/Moe/Parser/BgmParser.hs**: Bangumi title parsing functionality
  - `BgmBangumi` data type with title and season fields
  - `parseBangumiTitle`: Parses text like "怪兽8号第二季" into structured data

### Frontend Structure (TypeScript/React)
- **web/**: Modern React frontend with TypeScript
  - **Framework**: Vite + React + TypeScript
  - **UI Components**: shadcn/ui component library
  - **Routing**: TanStack Router for type-safe routing
  - **State Management**: Zustand stores
  - **Styling**: Tailwind CSS with custom theming
- **Key Features**:
  - Authentication system (sign-in/sign-up/forgot-password)
  - Dashboard with overview and analytics
  - Settings pages (account, appearance, notifications)
  - Search functionality with dialog interface
  - Error handling pages (401, 403, 404, 500, 503)
  - Responsive layout with sidebar navigation
  - Dark/light theme support

### Executable
- **app/Main.hs**: Haskell backend executable entry point

### Test Structure
- **test/Main.hs**: Test suite entry point
- **test/Moe/Parser/Internal/UtilSpec.hs**: Comprehensive tests for parser utilities
  - Tests for `parseChineseSeason` (7 test cases)
  - Tests for `parseChineseNumber` (5 test cases)  
  - Tests for `parseNumber` (4 test cases)
- **test/Moe/Parser/BgmParserSpec.hs**: Tests for bangumi title parsing
  - Tests for `parseBangumiTitle` (7 test cases)

The project uses:
- GHC2024 language standard
- OverloadedStrings extension by default
- Wall warning flags for all components

## Testing

### Backend Testing
The project uses a modular test structure with Tasty framework:
- Run all tests: `cabal test`
- Tests are organized by module in separate spec files
- Currently 23 tests covering Chinese text parsing and bangumi title parsing

### Frontend Development
Frontend development commands (from `web/` directory):
- `npm install` or `bun install` - Install dependencies
- `npm run dev` or `bun dev` - Start development server
- `npm run build` or `bun run build` - Build for production
- `npm run preview` or `bun preview` - Preview production build

## Project Structure

This is a full-stack bangumi (anime) management application with:
- **Backend**: Haskell with Servant API, Persistent database, and Chinese text parsing
- **Frontend**: Modern React TypeScript SPA with shadcn/ui components
- **Database**: SQLite with Persistent ORM for bangumi metadata storage
- **Architecture**: Clean separation between API server and frontend client

## Notes

- **Full-stack Application**: Combines Haskell backend API with React frontend
- **Chinese Text Processing**: Specialized parsers for Chinese anime titles and season indicators
- **Database-driven**: Persistent models for storing bangumi metadata
- **Modern Frontend**: Uses latest React ecosystem (Vite, TanStack Router, shadcn/ui)
- **Type Safety**: Both backend (Haskell) and frontend (TypeScript) are fully typed
- **Modular Architecture**: Clean separation of concerns with extensible design
- **MIT Licensed**: Open source project by birdgg
