const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;
const assert = std.debug.assert;
const expect = std.testing.expect;

pub const BuiltinFunction = *const fn (a: i64, b: i64) i64;

pub fn main() !void {
    const bfs = std.ComptimeStringMap(BuiltinFunction, .{
        .{ "add", builtinFunctionAdd },
    });

    const func = bfs.kvs[0];
    _ = func;

    print("Hi\n", .{});

    for (bfs.kvs) |kv| {
        print("{s}\n", .{kv.key});
        print("{?}\n", .{kv.value});
    }
}

fn builtinFunctionAdd(a: i64, b: i64) i64 {
    return a + b;
}
