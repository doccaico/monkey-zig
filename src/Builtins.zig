const std = @import("std");

const Globals = @import("Globals.zig");
const Object = @import("Object.zig");

const Builtins = @This();

const bfs = std.ComptimeStringMap(Object.BuiltinFunction, .{
    .{ "len", builtinFunctionLen },
});

var allocator: std.mem.Allocator = undefined;

pub fn init(builtin_allocator: std.mem.Allocator) std.StringHashMap(*Object.Object) {
    allocator = builtin_allocator;

    var result = std.StringHashMap(*Object.Object).init(allocator);
    for (bfs.kvs) |kv| {
        const new_builtin_obj = allocator.create(Object.Builtin) catch @panic("OOM");
        new_builtin_obj.function = bfs.get(kv.key).?;

        const new_obj = allocator.create(Object.Object) catch @panic("OOM");
        Globals.objectAppend(new_obj);
        new_obj.* = Object.Object{ .builtin = new_builtin_obj };

        result.put(kv.key, new_obj) catch @panic("OOM");
    }
    return result;
}

pub fn createError(comptime format: []const u8, args: anytype) *Object.Object {
    const message = std.fmt.allocPrint(allocator, format, args) catch @panic("OOM");

    const new_error_obj = allocator.create(Object.Error) catch @panic("OOM");
    new_error_obj.message = message;

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    Globals.objectAppend(new_obj);
    new_obj.* = Object.Object{ .@"error" = new_error_obj };

    return new_obj;
}

// builtin functions

fn builtinFunctionLen(args: std.ArrayList(*Object.Object)) *Object.Object {
    if (args.items.len != 1) {
        return createError("wrong number of arguments. got={d}, want=1", .{args.items.len});
    }
    switch (args.items[0].*) {
        .string => |x| {
            const new_integer_obj = allocator.create(Object.Integer) catch @panic("OOM");
            new_integer_obj.value = @intCast(x.value.len);

            const new_obj = allocator.create(Object.Object) catch @panic("OOM");
            Globals.objectAppend(new_obj);
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
        else => return createError("argument to `len` not supported, got {s}", .{args.items[0].getType()}),
    }
}
