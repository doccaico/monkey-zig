const std = @import("std");
const print = std.debug.print;
// const fmt = std.fmt;
// const mem = std.mem;
// const assert = std.debug.assert;
// const expect = std.testing.expect;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var map = std.AutoHashMap(*const [1:0]u8, i64).init(gpa);
    defer map.deinit();

    try map.put("a", 10);
    try map.put("b", 100);
    try map.put("c", 1000);

    var iterator = map.iterator();
    while (iterator.next()) |entry| {
        print("{s} : {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
    print("count: {d}\n", .{map.count()});
}
