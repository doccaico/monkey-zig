const std = @import("std");

const Ast = @import("Ast.zig");
const Object = @import("Object.zig");

var allocator: std.mem.Allocator = undefined;
pub var object_list: std.ArrayList(*Object.Object) = undefined;
pub var node_list: std.ArrayList(*Ast.Node) = undefined;
pub var input_list: std.ArrayList(std.ArrayList(u8)) = undefined;

pub var TRUE: *Object.Object = undefined;
pub var FALSE: *Object.Object = undefined;
pub var NULL: *Object.Object = undefined;

pub fn init(global_allocator: std.mem.Allocator) void {
    allocator = global_allocator;
    object_list = std.ArrayList(*Object.Object).init(allocator);
    node_list = std.ArrayList(*Ast.Node).init(allocator);
    input_list = std.ArrayList(std.ArrayList(u8)).init(allocator);

    TRUE = createObjectBoolean(true);
    FALSE = createObjectBoolean(false);
    NULL = createObjectNull();
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

    for (input_list.items) |item| {
        item.deinit();
    }

    allocator.destroy(TRUE.boolean);
    allocator.destroy(TRUE);
    allocator.destroy(FALSE.boolean);
    allocator.destroy(FALSE);
    allocator.destroy(NULL.null);
    allocator.destroy(NULL);
}

pub fn nodeAppend(node: *Ast.Node) void {
    node_list.append(node) catch @panic("OOM");
}

pub fn objectAppend(obj: *Object.Object) void {
    object_list.append(obj) catch @panic("OOM");
}

pub fn inputAppend(input: std.ArrayList(u8)) void {
    input_list.append(input) catch @panic("OOM");
}

fn createObjectBoolean(value: bool) *Object.Object {
    const new_boolean_obj = allocator.create(Object.Boolean) catch @panic("OOM");
    new_boolean_obj.value = value;

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .boolean = new_boolean_obj };
    return new_obj;
}

fn createObjectNull() *Object.Object {
    const new_null_obj = allocator.create(Object.Null) catch @panic("OOM");

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    new_obj.* = Object.Object{ .null = new_null_obj };
    return new_obj;
}
