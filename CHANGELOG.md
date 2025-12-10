# Changelog

All notable changes to the Loreal compiler will be documented in this file.

## [0.1.0] - 2025-01-03

### Added
- Initial Loreal compiler implementation
- Lexer with full token support
- Recursive descent parser
- Complete AST definitions
- Type checker with concrete types
- Mid-Level IR (MIR) with CFG
- ANF (A-Normal Form) transformation
- Liveness analysis
- Reference counting insertion
- Borrow inference
- Reuse analysis (FBIP optimization)
- CLI with build and run commands
- Runtime library with RC support

### Architecture
- Workspace structure with 8 crates
- Separation of concerns across pipeline
- Modular design for extensibility
