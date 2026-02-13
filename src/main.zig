//! Zig port of `try` (workspace manager).
const std = @import("std");
const builtin = @import("builtin");

const SCRIPT_HEADER =
    "# if you can read this, you didn't launch try from an alias. run try --help.\n";
const DEFAULT_TRIES_PATH_SUFFIX = "src/tries";
const APP_NAME = "tryz";
const VERSION = "dev";
const ANSI_RESET = "\x1b[0m";
const ANSI_BOLD = "\x1b[1m";
const ANSI_DIM = "\x1b[2m";
const ANSI_CYAN = "\x1b[36m";
const ANSI_GREEN = "\x1b[32m";
const ANSI_YELLOW = "\x1b[33m";
const ANSI_RED = "\x1b[31m";

const ActionType = enum {
    none,
    cd,
    mkdir,
};

const SelectionResult = struct {
    action: ActionType,
    path: ?[]u8,
};
const TryEntry = struct {
    name: []u8,
    path: []u8,
    mtime_ns: i128,
    score: f64,
};

const SelectorKey = enum {
    up,
    down,
    enter,
    ctrl_c,
    quit,
    other,
};

fn scoreColor(score: f64) []const u8 {
    if (score >= 1.0) return ANSI_GREEN;
    if (score >= 0.2) return ANSI_YELLOW;
    return ANSI_DIM;
}

fn isHelpOrVersionFlag(arg: []const u8) bool {
    return std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h") or
        std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v");
}

fn parseOptionValue(args: []const [:0]u8, index: usize, flag: []const u8) ?struct { value: []const u8, skip_next: bool } {
    const arg = args[index];
    if (std.mem.startsWith(u8, arg, flag) and arg.len > flag.len and arg[flag.len] == '=') {
        return .{ .value = arg[flag.len + 1 ..], .skip_next = false };
    }
    if (std.mem.eql(u8, arg, flag)) {
        if (index + 1 >= args.len) {
            return null;
        }
        return .{ .value = args[index + 1], .skip_next = true };
    }
    return null;
}

fn isCloneUrl(value: []const u8) bool {
    return std.mem.startsWith(u8, value, "https://") or
        std.mem.startsWith(u8, value, "http://") or
        std.mem.startsWith(u8, value, "git@");
}

fn getEnvVar(allocator: std.mem.Allocator, name: []const u8) ?[]u8 {
    return std.process.getEnvVarOwned(allocator, name) catch |err| switch (err) {
        error.EnvironmentVariableNotFound => null,
        else => null,
    };
}

fn getHomeDir(allocator: std.mem.Allocator) ![]u8 {
    if (getEnvVar(allocator, "HOME")) |home| {
        return home;
    }

    return error.HomeUnavailable;
}

fn expandPath(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    if (path.len >= 2 and path[0] == '~' and path[1] == '/') {
        const home = try getHomeDir(allocator);
        defer allocator.free(home);
        return std.fs.path.join(allocator, &.{ home, path[2..] });
    }

    return allocator.dupe(u8, path);
}

fn getTriesPath(allocator: std.mem.Allocator, explicit_path: ?[]const u8) ![]u8 {
    if (explicit_path) |path| {
        return expandPath(allocator, path);
    }

    if (getEnvVar(allocator, "TRY_PATH")) |env_path| {
        defer allocator.free(env_path);
        return expandPath(allocator, env_path);
    }

    const home = try getHomeDir(allocator);
    defer allocator.free(home);
    return std.fs.path.join(allocator, &.{ home, DEFAULT_TRIES_PATH_SUFFIX });
}

fn ensureTriesDirectory(allocator: std.mem.Allocator, tries_path: []const u8) !void {
    _ = allocator;
    try std.fs.cwd().makePath(tries_path);
}

fn shellEscape(allocator: std.mem.Allocator, value: []const u8) ![]u8 {
    var result: std.ArrayList(u8) = .empty;
    try result.append(allocator, '\'');

    for (value) |c| {
        if (c == '\'') {
            try result.appendSlice(allocator, "'\"'\"'");
        } else {
            try result.append(allocator, c);
        }
    }

    try result.append(allocator, '\'');
    return result.toOwnedSlice(allocator);
}

fn hasDatePrefix(name: []const u8) bool {
    if (name.len < 11) return false;
    if (name[0] < '0' or name[0] > '9') return false;
    if (name[1] < '0' or name[1] > '9') return false;
    if (name[2] < '0' or name[2] > '9') return false;
    if (name[3] < '0' or name[3] > '9') return false;
    if (name[4] != '-') return false;
    if (name[5] < '0' or name[5] > '9') return false;
    if (name[6] < '0' or name[6] > '9') return false;
    if (name[7] != '-') return false;
    if (name[8] < '0' or name[8] > '9') return false;
    if (name[9] < '0' or name[9] > '9') return false;
    if (name[10] != '-') return false;
    return true;
}

fn scoreEntry(allocator: std.mem.Allocator, name: []const u8, query: ?[]const u8, mtime_ns: i128) f64 {
    _ = allocator;

    const now_seconds = std.time.timestamp();
    const now_i128 = @as(i128, now_seconds) * std.time.ns_per_s;
    const age_ns = @max(0, now_i128 - mtime_ns);
    const age_hours = @as(f64, @floatFromInt(@divTrunc(age_ns, std.time.ns_per_ms * std.time.ms_per_s))) / 3600.0;

    if (query == null or query.?.len == 0) {
        return 3.0 / std.math.sqrt(age_hours + 1.0);
    }

    const q = query.?;
    var query_idx: usize = 0;
    var last_pos: isize = -1;
    var match_score: f64 = 0.0;

    for (name, 0..) |c, i| {
        const cl = std.ascii.toLower(c);
        const ql = std.ascii.toLower(q[query_idx]);
        if (query_idx < q.len and cl == ql) {
            match_score += 1.0;

            if (i == 0 or !std.ascii.isAlphanumeric(name[i - 1])) {
                match_score += 1.0;
            }

            if (last_pos >= 0) {
                const gap = @as(f64, @floatFromInt(@as(i64, @intCast(i)) - last_pos - 1));
                match_score += 2.0 / std.math.sqrt(gap + 1.0);
            }

            last_pos = @intCast(i);
            query_idx += 1;
            if (query_idx == q.len) break;
        }
    }

    if (query_idx < q.len) {
        return 0.0;
    }

    const density = (@as(f64, @floatFromInt(q.len)) / @as(f64, @floatFromInt(last_pos + 1))) *
        (10.0 / (@as(f64, @floatFromInt(name.len)) + 10.0));
    var result = match_score * density;

    if (hasDatePrefix(name)) {
        result += 2.0;
    }

    result += 3.0 / std.math.sqrt(age_hours + 1.0);
    return result;
}

fn formatRelativeTime(allocator: std.mem.Allocator, mtime_ns: i128) ![]u8 {
    const now_seconds = std.time.timestamp();
    const now_ns: i128 = @as(i128, now_seconds) * std.time.ns_per_s;
    const age_ns = @max(0, now_ns - mtime_ns);
    const age_seconds = @as(f64, @floatFromInt(age_ns)) / @as(f64, @floatFromInt(std.time.ns_per_s));

    if (age_seconds < 60.0) {
        return try allocator.dupe(u8, "just now");
    }

    if (age_seconds < 3600.0) {
        return std.fmt.allocPrint(allocator, "{d}m ago", .{@as(u64, @intFromFloat(@floor(age_seconds / 60.0)))});
    }

    if (age_seconds < 86400.0) {
        return std.fmt.allocPrint(allocator, "{d}h ago", .{@as(u64, @intFromFloat(@floor(age_seconds / 3600.0)))});
    }

    return std.fmt.allocPrint(allocator, "{d}d ago", .{@as(u64, @intFromFloat(@floor(age_seconds / 86400.0)))});
}

fn todayPrefix(allocator: std.mem.Allocator) ![]u8 {
    const now_seconds = std.time.timestamp();
    const secs = if (now_seconds < 0) @as(u64, 0) else @as(u64, @intCast(now_seconds));
    const epoch = std.time.epoch.EpochSeconds{ .secs = secs };
    const year_day = epoch.getEpochDay().calculateYearDay();
    const month_day = year_day.calculateMonthDay();
    return std.fmt.allocPrint(
        allocator,
        "{d:0>4}-{d:0>2}-{d:0>2}",
        .{ year_day.year, @intFromEnum(month_day.month), month_day.day_index + 1 },
    );
}

fn normalizeDirName(allocator: std.mem.Allocator, input: []const u8) ![]u8 {
    var output: std.ArrayList(u8) = .empty;
    var last_hyphen = true;

    for (input) |c| {
        if (!std.ascii.isAlphanumeric(c) and c != '_' and c != '-' and c != '.' and !std.ascii.isWhitespace(c)) {
            return allocator.dupe(u8, "");
        }

        if (std.ascii.isWhitespace(c) or c == '-') {
            if (!last_hyphen) {
                try output.append(allocator, '-');
                last_hyphen = true;
            }
            continue;
        }

        try output.append(allocator, c);
        last_hyphen = false;
    }

    while (output.getLastOrNull()) |last| {
        if (last == '-') {
            _ = output.pop();
        } else {
            break;
        }
    }

    if (output.items.len == 0) {
        return allocator.dupe(u8, "");
    }

    return output.toOwnedSlice(allocator);
}

fn buildDatePrefixedName(allocator: std.mem.Allocator, base_name: []const u8) ![]u8 {
    const normalized = try normalizeDirName(allocator, base_name);
    defer allocator.free(normalized);

    if (normalized.len == 0) {
        return allocator.dupe(u8, "");
    }

    const prefix = try todayPrefix(allocator);
    defer allocator.free(prefix);

    return std.fs.path.join(allocator, &.{ prefix, normalized });
}

fn parseCloneName(allocator: std.mem.Allocator, url: []const u8, name: ?[]const u8) ![]u8 {
    const prefix = try todayPrefix(allocator);
    defer allocator.free(prefix);

    if (name) |forced_name| {
        const normalized = try normalizeDirName(allocator, forced_name);
        defer allocator.free(normalized);
        if (normalized.len == 0) {
            return allocator.dupe(u8, "");
        }
        return std.fs.path.join(allocator, &.{ prefix, normalized });
    }

    const last_slash = std.mem.lastIndexOfScalar(u8, url, '/');
    const last_colon = std.mem.lastIndexOfScalar(u8, url, ':');

    const repo_start = if (last_slash) |idx| idx + 1 else if (last_colon) |idx| idx + 1 else 0;
    const repo_name = if (std.mem.endsWith(u8, url[repo_start..], ".git"))
        url[repo_start .. url.len - 4]
    else
        url[repo_start..];

    var user_part: ?[]const u8 = null;
    if (last_slash) |ls| {
        if (ls > 0) {
            var p: usize = ls;
            while (p > 0) : (p -= 1) {
                const prev = p - 1;
                if (url[prev] == '/' or url[prev] == ':') {
                    user_part = url[prev + 1 .. ls];
                    break;
                }
            }
        }
    } else if (last_colon) |lc| {
        if (std.mem.indexOfScalar(u8, url[lc + 1 ..], '/')) |slash| {
            user_part = url[lc + 1 .. lc + 1 + slash];
        }
    }

    if (user_part) |u| {
        const base = try std.fs.path.join(allocator, &.{ prefix, u, repo_name });
        return base;
    }

    const combined = try std.fs.path.join(allocator, &.{ prefix, repo_name });
    return combined;
}

fn scanEntries(allocator: std.mem.Allocator, base_path: []const u8, query: ?[]const u8) !std.ArrayList(TryEntry) {
    var entries: std.ArrayList(TryEntry) = .empty;

    var base_dir = try std.fs.cwd().openDir(base_path, .{ .iterate = true });
    defer base_dir.close();

    var iter = base_dir.iterate();
    while (try iter.next()) |item| {
        if (item.name.len == 0 or item.name[0] == '.') {
            continue;
        }

        if (item.kind != .directory) {
            continue;
        }

        const full_path = try std.fs.path.join(allocator, &.{ base_path, item.name });
        const st = base_dir.statFile(item.name) catch continue;

        const score = scoreEntry(allocator, item.name, query, st.mtime);
        if (query != null and query.?.len > 0 and score <= 0.0) {
            allocator.free(full_path);
            continue;
        }

        try entries.append(allocator, .{
            .name = try allocator.dupe(u8, item.name),
            .path = full_path,
            .mtime_ns = st.mtime,
            .score = score,
        });
    }

    std.mem.sort(TryEntry, entries.items, {}, struct {
        fn less(_: void, a: TryEntry, b: TryEntry) bool {
            if (a.score == b.score) {
                return a.name.len < b.name.len;
            }
            return a.score > b.score;
        }
    }.less);

    return entries;
}

fn formatSelectorLine(allocator: std.mem.Allocator, index: usize, entry: TryEntry, selected: bool, styled: bool) !void {
    const rel = try formatRelativeTime(allocator, entry.mtime_ns);
    defer allocator.free(rel);
    const name_color = if (hasDatePrefix(entry.name)) ANSI_GREEN else ANSI_RESET;
    const score_color = scoreColor(entry.score);

    if (!styled) {
        const marker = if (selected) ">" else " ";
        return printStderr("{s} {d}) {s}  ({s})  [{d:.1}]\n", .{ marker, index + 1, entry.name, rel, entry.score });
    }

    if (selected) {
        try printStderr(
            "{s}{s}> {d}) {s}{s}{s}  {s}({s}){s}  {s}[{d:.1}]{s}\n",
            .{
                ANSI_BOLD, ANSI_CYAN, index + 1,
                name_color, entry.name, ANSI_RESET,
                ANSI_DIM, rel, ANSI_RESET,
                score_color, entry.score, ANSI_RESET,
            },
        );
    } else {
        try printStderr(
            "  {d}) {s}{s}{s}  {s}({s}){s}  {s}[{d:.1}]{s}\n",
            .{
                index + 1, name_color, entry.name, ANSI_RESET,
                ANSI_DIM, rel, ANSI_RESET,
                score_color, entry.score, ANSI_RESET,
            },
        );
    }
}

fn buildClonePathName(allocator: std.mem.Allocator, tries_path: []const u8, name: []const u8) ![]u8 {
    const date_name = try normalizeDirName(allocator, name);
    defer allocator.free(date_name);
    const prefix = try todayPrefix(allocator);
    defer allocator.free(prefix);

    const full = try std.fs.path.join(allocator, &.{ tries_path, prefix, date_name });
    return full;
}

fn buildCdScript(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const escaped = try shellEscape(allocator, path);
    defer allocator.free(escaped);
    return std.fmt.allocPrint(
        allocator,
        "touch {s} && \\\n  cd {s} && \\\n  printf '%s\\n' {s}\n",
        .{ escaped, escaped, escaped },
    );
}

fn buildMkdirScript(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const escaped = try shellEscape(allocator, path);
    defer allocator.free(escaped);
    return std.fmt.allocPrint(
        allocator,
        "mkdir -p {s} && \\\n  cd {s} && \\\n  printf '%s\\n' {s}\n",
        .{ escaped, escaped, escaped },
    );
}

fn buildCloneScript(allocator: std.mem.Allocator, url: []const u8, path: []const u8) ![]u8 {
    const escaped_url = try shellEscape(allocator, url);
    defer allocator.free(escaped_url);
    const escaped_path = try shellEscape(allocator, path);
    defer allocator.free(escaped_path);

    return std.fmt.allocPrint(
        allocator,
        "git clone {s} {s} && \\\n  cd {s} && \\\n  printf '%s\\n' {s}\n",
        .{ escaped_url, escaped_path, escaped_path, escaped_path },
    );
}

fn buildWorktreeScript(allocator: std.mem.Allocator, path: []const u8) ![]u8 {
    const escaped_path = try shellEscape(allocator, path);
    defer allocator.free(escaped_path);

    return std.fmt.allocPrint(
        allocator,
        "git worktree add {s} && \\\n  cd {s} && \\\n  printf '%s\\n' {s}\n",
        .{ escaped_path, escaped_path, escaped_path },
    );
}

fn emitScript(script: []const u8) !void {
    try printStdout("{s}{s}", .{ SCRIPT_HEADER, script });
}

fn isInteractiveTerminal() bool {
    if (builtin.os.tag == .windows) return false;
    return std.posix.isatty(std.posix.STDIN_FILENO) and std.posix.isatty(std.posix.STDERR_FILENO);
}

fn enterRawMode() !std.posix.termios {
    const original = try std.posix.tcgetattr(std.posix.STDIN_FILENO);
    var raw = original;
    raw.lflag.ICANON = false;
    raw.lflag.ECHO = false;
    raw.cc[@intFromEnum(std.posix.V.MIN)] = 1;
    raw.cc[@intFromEnum(std.posix.V.TIME)] = 0;
    try std.posix.tcsetattr(std.posix.STDIN_FILENO, .NOW, raw);
    return original;
}

fn restoreTerminalMode(original: std.posix.termios) void {
    std.posix.tcsetattr(std.posix.STDIN_FILENO, .NOW, original) catch {};
}

fn readSelectorKey() !SelectorKey {
    var key: [1]u8 = undefined;
    const n = try std.posix.read(std.posix.STDIN_FILENO, key[0..]);
    if (n == 0) return .ctrl_c;

    return switch (key[0]) {
        '\n', '\r' => .enter,
        3 => .ctrl_c,
        'q', 'Q' => .quit,
        'k', 'K' => .up,
        'j', 'J' => .down,
        27 => blk: {
            var seq: [2]u8 = undefined;
            const first = try std.posix.read(std.posix.STDIN_FILENO, seq[0..1]);
            if (first == 0 or seq[0] != '[') break :blk .other;
            const second = try std.posix.read(std.posix.STDIN_FILENO, seq[1..2]);
            if (second == 0) break :blk .other;
            break :blk switch (seq[1]) {
                'A' => .up,
                'B' => .down,
                else => .other,
            };
        },
        else => .other,
    };
}

fn renderSelector(allocator: std.mem.Allocator, entries: []const TryEntry, selected_index: usize, create_preview: ?[]const u8) !void {
    const max_visible_rows: usize = 12;
    try printStderr("\x1b[2J\x1b[H", .{});
    try printStderr("{s}{s}try selector{s}\n", .{ ANSI_BOLD, ANSI_CYAN, ANSI_RESET });
    try printStderr(
        "{s}{d} workspaces{s}  {s}[{d}/{d}]{s}\n\n",
        .{ ANSI_DIM, entries.len, ANSI_RESET, ANSI_DIM, selected_index + 1, entries.len, ANSI_RESET },
    );

    var window_start: usize = 0;
    if (entries.len > max_visible_rows) {
        const half = max_visible_rows / 2;
        if (selected_index > half) {
            window_start = selected_index - half;
        }
        if (window_start + max_visible_rows > entries.len) {
            window_start = entries.len - max_visible_rows;
        }
    }
    const window_end = @min(entries.len, window_start + max_visible_rows);

    if (window_start > 0) {
        try printStderr("{s}... {d} more above ...{s}\n", .{ ANSI_YELLOW, window_start, ANSI_RESET });
    }

    var idx = window_start;
    while (idx < window_end) : (idx += 1) {
        const entry = entries[idx];
        try formatSelectorLine(allocator, idx, entry, idx == selected_index, true);
    }

    if (window_end < entries.len) {
        try printStderr("{s}... {d} more below ...{s}\n", .{ ANSI_YELLOW, entries.len - window_end, ANSI_RESET });
    }

    if (create_preview) |preview| {
        try printStderr("\n{s}new:{s} {s}\n", .{ ANSI_GREEN, ANSI_RESET, preview });
    }

    try printStderr(
        "\n{s}Up/Down{s} move  {s}j/k{s} move  {s}Enter{s} select  {s}q{s} quit  {s}Ctrl-C{s} cancel\n",
        .{ ANSI_CYAN, ANSI_RESET, ANSI_CYAN, ANSI_RESET, ANSI_CYAN, ANSI_RESET, ANSI_RED, ANSI_RESET, ANSI_CYAN, ANSI_RESET },
    );
}

fn cmdInit(allocator: std.mem.Allocator, args: [][]const u8, tries_path: []const u8) !void {
    const shell_name = getEnvVar(allocator, "SHELL");
    defer if (shell_name) |value| allocator.free(value);
    const is_fish = if (shell_name) |value|
        std.mem.indexOf(u8, value, "fish") != null
    else
        false;

    var exe_path_buffer: [std.fs.max_path_bytes]u8 = undefined;
    const exe_path = std.fs.selfExePath(&exe_path_buffer) catch "command try";

    const escaped_self = try shellEscape(allocator, exe_path);
    defer allocator.free(escaped_self);

    const shell_path = if (args.len > 0) args[0] else tries_path;
    const escaped_tries = try shellEscape(allocator, shell_path);
    defer allocator.free(escaped_tries);

    if (is_fish) {
        try printStdout(
            \\function try
            \\  if test (count $argv) -gt 0
            \\    switch $argv[1]
            \\      case -h --help -v --version
            \\        {s} $argv
            \\        return $status
            \\    end
            \\  end
            \\  set -l out ({s} exec --path {s} $argv 2>/dev/tty | string collect)
            \\  or begin; echo $out; return $status; end
            \\  eval $out
            \\end
            \\
        , .{ escaped_self, escaped_self, escaped_tries });
    } else {
        try printStdout(
            \\try() {{
            \\  case "$1" in
            \\    -h|--help|-v|--version)
            \\      {s} "$@"
            \\      return $?
            \\      ;;
            \\  esac
            \\  local out
            \\  out=$({s} exec --path {s} "$@" 2>/dev/tty) || {{
            \\    echo "$out"
            \\    return $?
            \\  }}
            \\  eval "$out"
            \\}}
            \\
        , .{ escaped_self, escaped_self, escaped_tries });
    }
}

fn isInGitRepo(allocator: std.mem.Allocator) !bool {
    const cwd = try std.process.getCwdAlloc(allocator);
    defer allocator.free(cwd);

    var current: []const u8 = cwd;
    while (true) {
        const git_dir = try std.fs.path.join(allocator, &.{ current, ".git" });
        defer allocator.free(git_dir);

        if (std.fs.cwd().statFile(git_dir)) |st| {
            if (st.kind == .directory) return true;
        } else |_| {}

        const parent = std.fs.path.dirname(current) orelse break;
        if (std.mem.eql(u8, parent, current)) {
            break;
        }
        current = parent;
    }

    return false;
}

fn cmdClone(allocator: std.mem.Allocator, args: [][]const u8, tries_path: []const u8) !void {
    if (args.len == 0) {
        try printStderr("Usage: try clone <url> [name]\n", .{});
        return;
    }

    const name = try parseCloneName(allocator, args[0], if (args.len > 1) args[1] else null);
    defer allocator.free(name);
    const full = try std.fs.path.join(allocator, &.{ tries_path, name });
    defer allocator.free(full);

    const script = try buildCloneScript(allocator, args[0], full);
    defer allocator.free(script);
    try emitScript(script);
}

fn cmdWorktree(allocator: std.mem.Allocator, args: [][]const u8, tries_path: []const u8) !void {
    if (args.len == 0) {
        try printStderr("Usage: try worktree <name>\n", .{});
        return;
    }

    const full_name = try buildClonePathName(allocator, tries_path, args[0]);
    defer allocator.free(full_name);

    const script = if (try isInGitRepo(allocator))
        try buildWorktreeScript(allocator, full_name)
    else
        try buildMkdirScript(allocator, full_name);
    defer allocator.free(script);

    try emitScript(script);
}

fn cmdSelector(allocator: std.mem.Allocator, args: [][]const u8, tries_path: []const u8) !void {
    const query = if (args.len == 0) null else try std.mem.join(allocator, " ", args);
    defer if (query) |q| allocator.free(q);

    var entries = try scanEntries(allocator, tries_path, query);
    defer {
        for (entries.items) |entry| {
            allocator.free(entry.name);
            allocator.free(entry.path);
        }
        entries.deinit(allocator);
    }

    const has_query = query != null and query.?.len > 0;
    if (has_query and entries.items.len == 1) {
        const script = try buildCdScript(allocator, entries.items[0].path);
        defer allocator.free(script);
        return emitScript(script);
    }

    if (has_query and entries.items.len == 0) {
        const candidate = try buildClonePathName(allocator, tries_path, query.?);
        defer allocator.free(candidate);
        const script = try buildMkdirScript(allocator, candidate);
        defer allocator.free(script);
        return emitScript(script);
    }

    if (entries.items.len == 0) {
        try printStderr("No matches.\n", .{});
        return;
    }

    const create_preview = if (has_query) blk: {
        const preview_path = try buildClonePathName(allocator, tries_path, query.?);
        defer allocator.free(preview_path);
        break :blk try allocator.dupe(u8, std.fs.path.basename(preview_path));
    } else null;
    defer if (create_preview) |p| allocator.free(p);

    if (!isInteractiveTerminal()) {
        for (entries.items, 0..) |entry, idx| {
            try formatSelectorLine(allocator, idx, entry, false, false);
        }

        try printStdout("Enter selection number (1-{d}): ", .{entries.items.len});
        if (create_preview) |preview| {
            try printStdout("or press Enter to create {s}: ", .{preview});
        }

        const raw_line = readLineFromStdin() catch |err| switch (err) {
            error.EndOfStream => "",
            else => return err,
        };
        const trimmed = std.mem.trim(u8, raw_line, " \r\n\t");

        if (trimmed.len == 0) {
            if (has_query) {
                const created = try buildClonePathName(allocator, tries_path, query.?);
                defer allocator.free(created);
                const script = try buildMkdirScript(allocator, created);
                defer allocator.free(script);
                return emitScript(script);
            }
            return;
        }

        const selected = std.fmt.parseInt(usize, trimmed, 10) catch 0;
        if (selected == 0 or selected > entries.items.len) {
            try printStderr("Invalid selection\n", .{});
            return;
        }

        const chosen = entries.items[selected - 1].path;
        const script = try buildCdScript(allocator, chosen);
        defer allocator.free(script);
        return emitScript(script);
    }

    const original_term = try enterRawMode();
    defer restoreTerminalMode(original_term);

    try printStderr("\x1b[?25l", .{});
    defer printStderr("\x1b[?25h", .{}) catch {};

    var selected_index: usize = 0;
    while (true) {
        try renderSelector(allocator, entries.items, selected_index, create_preview);

        switch (try readSelectorKey()) {
            .up => {
                if (selected_index == 0) {
                    selected_index = entries.items.len - 1;
                } else {
                    selected_index -= 1;
                }
            },
            .down => {
                selected_index = (selected_index + 1) % entries.items.len;
            },
            .enter => break,
            .quit => return,
            .ctrl_c => return,
            .other => {},
        }
    }

    const chosen = entries.items[selected_index].path;
    const script = try buildCdScript(allocator, chosen);
    defer allocator.free(script);
    return emitScript(script);
}

fn cmdRoute(allocator: std.mem.Allocator, args: [][]const u8, tries_path: []const u8) !void {
    if (args.len == 0) {
        return cmdSelector(allocator, args, tries_path);
    }

    const subcmd = args[0];

    if (std.mem.eql(u8, subcmd, "--help") or std.mem.eql(u8, subcmd, "-h")) {
        return printHelp(allocator, tries_path);
    }

    if (std.mem.eql(u8, subcmd, "--version") or std.mem.eql(u8, subcmd, "-v")) {
        return printStdout("{s} {s}\n", .{ APP_NAME, VERSION });
    }

    if (std.mem.eql(u8, subcmd, "init")) {
        return cmdInit(allocator, args[1..], tries_path);
    }

    if (std.mem.eql(u8, subcmd, "cd")) {
        if (args.len > 1 and isCloneUrl(args[1])) {
            return cmdClone(allocator, args[1..], tries_path);
        }
        return cmdSelector(allocator, args[1..], tries_path);
    }

    if (std.mem.eql(u8, subcmd, "clone")) {
        return cmdClone(allocator, args[1..], tries_path);
    }

    if (std.mem.eql(u8, subcmd, "worktree")) {
        return cmdWorktree(allocator, args[1..], tries_path);
    }

    if (isCloneUrl(subcmd)) {
        return cmdClone(allocator, args, tries_path);
    }

    if (std.mem.eql(u8, subcmd, ".")) {
        return cmdWorktree(allocator, args[1..], tries_path);
    }

    return cmdSelector(allocator, args, tries_path);
}

fn printHelp(allocator: std.mem.Allocator, tries_path: []const u8) !void {
    _ = allocator;
    try printStdout("{s} v{s} - ephemeral workspace manager (Zig)\n", .{ APP_NAME, VERSION });
    try printStdout("\n", .{});
    try printStdout("Shell integration:\n", .{});
    try printStdout("  echo 'eval \"$(try init ~/src/tries)\"' >> ~/.zshrc\n", .{});
    try printStdout("\n", .{});
    try printStdout("Usage:\n", .{});
    try printStdout("  try\n", .{});
    try printStdout("  try <query>\n", .{});
    try printStdout("  try clone <url> [name]\n", .{});
    try printStdout("  try ./repo <name>\n", .{});
    try printStdout("  try init [path]\n", .{});
    try printStdout("  try exec <query>\n", .{});
    try printStdout("  try --help\n", .{});
    try printStdout("\nCurrent path: {s}\n", .{tries_path});
}

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len == 1) {
        const default_path = try getTriesPath(allocator, null);
        defer allocator.free(default_path);
        try ensureTriesDirectory(allocator, default_path);
        var empty_args: [0][]const u8 = .{};
        return cmdRoute(allocator, empty_args[0..], default_path);
    }

    var tries_path: ?[]u8 = null;
    defer if (tries_path) |p| allocator.free(p);
    var positional: std.ArrayList([]const u8) = .empty;
    defer positional.deinit(allocator);

    var i: usize = 1;
    while (i < args.len) {
        const arg = args[i];

        if (std.mem.eql(u8, arg, "--help") or std.mem.eql(u8, arg, "-h")) {
            const current = try getTriesPath(allocator, tries_path);
            defer allocator.free(current);
            try ensureTriesDirectory(allocator, current);
            return printHelp(allocator, current);
        }

        if (std.mem.eql(u8, arg, "--version") or std.mem.eql(u8, arg, "-v")) {
            try printStdout("{s} {s}\n", .{ APP_NAME, VERSION });
            return;
        }

        if (std.mem.eql(u8, arg, "--no-colors")) {
            i += 1;
            continue;
        }

        if (std.mem.eql(u8, arg, "--and-exit")) {
            i += 1;
            continue;
        }

        if (parseOptionValue(args, i, "--path")) |v| {
            if (tries_path) |old| allocator.free(old);
            tries_path = try expandPath(allocator, v.value);
            i += if (v.skip_next) 2 else 1;
            continue;
        }

        if (parseOptionValue(args, i, "--and-keys")) |v| {
            _ = v.value;
            i += if (v.skip_next) 2 else 1;
            continue;
        }

        if (isHelpOrVersionFlag(arg)) {
            try positional.append(allocator, arg);
            i += 1;
            continue;
        }

        try positional.append(allocator, arg);
        i += 1;
    }

    const resolved_tries = if (tries_path) |path| path else try getTriesPath(allocator, null);
    defer if (tries_path == null) allocator.free(resolved_tries);

    try ensureTriesDirectory(allocator, resolved_tries);

    if (positional.items.len == 0) {
        return printHelp(allocator, resolved_tries);
    }

    const command = positional.items[0];
    const command_args = positional.items[1..];

    if (std.mem.eql(u8, command, "init")) {
        try cmdInit(allocator, command_args, resolved_tries);
        return;
    }

    if (std.mem.eql(u8, command, "exec")) {
        return cmdRoute(allocator, command_args, resolved_tries);
    }

    if (std.mem.eql(u8, command, "cd")) {
        return cmdSelector(allocator, command_args, resolved_tries);
    }

    if (std.mem.eql(u8, command, "clone")) {
        return cmdClone(allocator, command_args, resolved_tries);
    }

    if (std.mem.eql(u8, command, "worktree")) {
        return cmdWorktree(allocator, command_args, resolved_tries);
    }

    if (isCloneUrl(command) or (command.len > 0 and std.mem.indexOfScalar(u8, command, ':') != null and std.mem.indexOfScalar(u8, command, '/') != null)) {
        return cmdClone(allocator, positional.items, resolved_tries);
    }

    if (std.mem.eql(u8, command, ".")) {
        return cmdWorktree(allocator, command_args, resolved_tries);
    }

    return cmdSelector(allocator, positional.items, resolved_tries);
}

fn printStdout(comptime fmt: []const u8, args: anytype) !void {
    var buf: [4096]u8 = undefined;
    var writer = std.fs.File.stdout().writer(&buf);
    try writer.interface.print(fmt, args);
    try writer.interface.flush();
}

fn printStderr(comptime fmt: []const u8, args: anytype) !void {
    var buf: [4096]u8 = undefined;
    var writer = std.fs.File.stderr().writer(&buf);
    try writer.interface.print(fmt, args);
    try writer.interface.flush();
}

fn readLineFromStdin() ![]const u8 {
    var buf: [1024]u8 = undefined;
    var reader = std.fs.File.stdin().reader(&buf);
    return reader.interface.takeDelimiterExclusive('\n');
}
