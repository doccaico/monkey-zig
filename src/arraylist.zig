const std = @import("std");
const fmt = std.fmt;
const mem = std.mem;
const print = std.debug.print;
const assert = std.debug.assert;
const expect = std.testing.expect;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    const msg = "Hello, World";

    var input = std.ArrayList(u8).init(gpa);
    defer input.deinit();

    // stdin.readAllArrayList();
    // const stdin = std.io.getStdIn();
    try input.appendSlice(msg);

    print("{s}\n", .{input.items});
}
// zig test filename.zig
// test "if" {
//     try expect(1 == 1);
// }
//  const stdout = std.io.getStdOut().writer();
//  const message: []const u8 = "Hello, World!";
//  try stdout.print("{s}\n", .{message});
