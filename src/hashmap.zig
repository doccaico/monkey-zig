const std = @import("std");
const print = std.debug.print;
// const fmt = std.fmt;
// const mem = std.mem;
// const assert = std.debug.assert;
// const expect = std.testing.expect;

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var map = std.StringHashMap(i64).init(gpa);
    defer map.deinit();

    {
        var input = std.ArrayList(u8).init(gpa);
        defer input.deinit();
        try input.append('a');
        try input.append('a');

        try map.put(input.items, 1);

        input.clearRetainingCapacity();
    }
    // try map.put("a", 1);
    try map.put("b", 1);
    try map.put("a", 10);
    try map.put("a", 100);

    var iterator = map.iterator();
    while (iterator.next()) |entry| {
        print("{s} : {d}\n", .{ entry.key_ptr.*, entry.value_ptr.* });
    }
    print("count: {d}\n", .{map.count()});
}
