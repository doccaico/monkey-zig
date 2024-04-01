const std = @import("std");
const fmt = std.fmt;
const print = std.debug.print;
// const mem = std.mem;
// const assert = std.debug.assert;
// const expect = std.testing.expect;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var buffer = std.ArrayList(u8).init(gpa);
    defer buffer.deinit();

    try buffer.writer().writeAll("abcde");
    print("{d}\n", .{buffer.items.len});
    print("{s}\n", .{buffer.items});
    buffer.clearRetainingCapacity();

    try buffer.writer().writeAll("ab");
    print("{d}\n", .{buffer.items.len});
    print("{s}\n", .{buffer.items});
    buffer.clearRetainingCapacity();
}
