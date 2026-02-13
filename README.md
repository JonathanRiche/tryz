# Attribution
This project is a port of tobi's original CLI: https://github.com/tobi/try-cli

# tryz
Zig implementation of the `try` workspace manager.

The binary command is still `try` for shell compatibility.

## Install
1. Clone this repo:
```bash
git clone <your-repo-url> try-zig
cd try-zig
```
2. Build:
```bash
zig build
```
3. Optional global command install:
```bash
mkdir -p ~/.local/bin
ln -sfn "$(pwd)/zig-out/bin/try" ~/.local/bin/try
hash -r
```
4. Verify:
```bash
try -h
```

## Shell Integration
`try` prints shell commands that your shell function evaluates.

### zsh / bash
Current session:
```bash
eval "$(try init ~/src/tries)"
```
Persist:
```bash
echo 'eval "$(try init ~/src/tries)"' >> ~/.zshrc
```
For bash, write to `~/.bashrc` instead.

### fish
Current session:
```fish
try init ~/src/tries | source
```
Persist:
```fish
echo 'try init ~/src/tries | source' >> ~/.config/fish/config.fish
```

## Usage
```bash
try
try <query>
try exec <query>
try clone <url> [name]
try worktree <name>
try . <name>
try init [path]
```

URL shorthand is supported:
```bash
try https://github.com/owner/repo.git
try git@github.com:owner/repo.git
```

## CLI Flags
Global flags:
- `-h`, `--help` show help
- `-v`, `--version` show version
- `--path <dir>` or `--path=<dir>` set tries root path
- `--no-colors` compatibility flag
- `--and-exit` compatibility flag
- `--and-keys <keys>` or `--and-keys=<keys>` compatibility flag

## Selector Keys
- `Up/Down` move selection
- `j/k` move selection
- `Enter` select workspace
- `q` quit selector
- `Ctrl-C` cancel
