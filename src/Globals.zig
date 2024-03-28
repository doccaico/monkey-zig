const std = @import("std");

const Ast = @import("Ast.zig");
const Object = @import("Object.zig");

var allocator: std.mem.Allocator = undefined;
pub var object_list: std.ArrayList(*Object.Object) = undefined;
pub var node_list: std.ArrayList(*Ast.Node) = undefined;

pub fn init(global_allocator: std.mem.Allocator) void {
    allocator = global_allocator;
    object_list = std.ArrayList(*Object.Object).init(allocator);
    node_list = std.ArrayList(*Ast.Node).init(allocator);
}

pub fn deinit() void {
    for (object_list.items) |item| {
        switch (item.*) {
            .integer => |x| allocator.destroy(x),
            .boolean => |x| allocator.destroy(x),
            .null => |x| allocator.destroy(x),
            .return_value => |x| allocator.destroy(x),
            .@"error" => |x| {
                allocator.free(x.message);
                allocator.destroy(x);
            },
        }
        allocator.destroy(item);
    }
    object_list.deinit();

    for (node_list.items) |item| {
        allocator.destroy(item);
    }
    node_list.deinit();
}

pub fn nodeAppend(node: *Ast.Node) void {
    node_list.append(node) catch @panic("OOM");
}

pub fn objectAppend(obj: *Object.Object) void {
    object_list.append(obj) catch @panic("OOM");
}
