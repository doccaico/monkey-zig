const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;
const assert = std.debug.assert;
const expect = std.testing.expect;

pub fn main() !void {
    const Types1 = union(enum) {
        // null: ?null,
        // null: @TypeOf(null),
        //     null: void,
        null: anyopaque,
        integer: i64,
    };
    const Test = struct {
        []const u8,
        Types1,
    };
    const tests = [_]Test{
        .{ "if (true) { 10 }", 10 },
        .{ "if (false) { 10 }", null },
    };

    // const result: ?i64 = 99;
    const result: ?i64 = null;

    if (result) |_| {
        print("{?}\n", .{result});
    } else {
        if (tests[1][1] == null) {
            print("{s}\n", .{"null"});
        }
    }

    // switch (result) {
    //
    //     .integer => |y| ,
    //     .null => |y| {
    //         if (y == @TypeOf(null)) {
    //             print("{?}\n", .{y});
    //         }
    //         print("{?}\n", .{y});
    //     },
    //     // else => {},
    // }

    // switch (got) {
    //     .integer => |y| print("{?}\n", .{y}),
    //     .null => |y| {
    //         if (y == @TypeOf(null)) {
    //             print("{?}\n", .{y});
    //         }
    //         print("{?}\n", .{y});
    //     },
    //     // else => {},
    // }
    // }
}
