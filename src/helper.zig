const std = @import("std");

const Object = @import("Object.zig");

const Evaluator = @import("Evaluator.zig");

// pub fn initInteger(allocator: std.mem.Allocator, value: i64) anyerror!*Evaluator.Types1 {
pub fn initInteger(allocator: std.mem.Allocator, value: i64) anyerror!*Object.Integer {
    // pub fn initInteger(allocator: std.mem.Allocator, value: i64) anyerror!*Object.Object {
    // const obj = try allocator.create(Evaluator.Types1);
    // const obj = try allocator.create(Object.Object);
    const obj = try allocator.create(Object.Integer);
    // const obj = try allocator.create(Object.Object);
    // if (obj == null) {
    //     std.debug.print("NULLL        ---", .{});
    // }
    // std.debug.print("NULLL        ---{}", .{obj});
    // obj.value = value;
    // std.debug.print("{}\n", .{obj.integer});
    // _ = value;
    // obj.* = Object.Integer{ .value = value };
    // obj.* = Evaluator.Types1{ .value = value };
    // obj.* = Evaluator.Types1{ .integer = Object.Integer{ .value = value } };
    // obj.integer = Object.Integer{ .value = value };
    obj.value = value;

    // if (obj) |*o| {
    //     o.integer.value = value;
    // } else |_| {}

    return obj;
}

pub fn initNull(allocator: std.mem.Allocator) anyerror!*Object.Object {
    const obj = try allocator.create(Object.Object);
    return obj;
}
