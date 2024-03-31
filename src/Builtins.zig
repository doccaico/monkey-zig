const std = @import("std");

const Environment = @import("Environment.zig");
const Globals = @import("Globals.zig");
const Object = @import("Object.zig");

const Builtins = @This();

const bfs = std.ComptimeStringMap(Object.BuiltinFunction, .{
    .{ "len", builtinFunctionLen },
    .{ "first", builtinFunctionFirst },
    .{ "last", builtinFunctionLast },
    .{ "rest", builtinFunctionRest },
    .{ "push", builtinFunctionPush },
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
        .array => |x| {
            const new_integer_obj = allocator.create(Object.Integer) catch @panic("OOM");
            new_integer_obj.value = @intCast(x.elements.items.len);

            const new_obj = allocator.create(Object.Object) catch @panic("OOM");
            Globals.objectAppend(new_obj);
            new_obj.* = Object.Object{ .integer = new_integer_obj };

            return new_obj;
        },
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

fn builtinFunctionFirst(args: std.ArrayList(*Object.Object)) *Object.Object {
    if (args.items.len != 1) {
        return createError("wrong number of arguments. got={d}, want=1", .{args.items.len});
    }
    if (!std.mem.eql(u8, args.items[0].getType(), Object.ARRAY_OBJ)) {
        return createError("argument to `first` must be ARRAY, got {s}", .{args.items[0].getType()});
    }

    const arr = args.items[0].array;
    if (arr.elements.items.len > 0) {
        return arr.elements.items[0];
    }
    return Environment.NULL;
}

fn builtinFunctionLast(args: std.ArrayList(*Object.Object)) *Object.Object {
    if (args.items.len != 1) {
        return createError("wrong number of arguments. got={d}, want=1", .{args.items.len});
    }
    if (!std.mem.eql(u8, args.items[0].getType(), Object.ARRAY_OBJ)) {
        return createError("argument to `last` must be ARRAY, got {s}", .{args.items[0].getType()});
    }

    const arr = args.items[0].array;
    const length = arr.elements.items.len;
    if (length > 0) {
        return arr.elements.items[length - 1];
    }
    return Environment.NULL;
}

fn builtinFunctionRest(args: std.ArrayList(*Object.Object)) *Object.Object {
    if (args.items.len != 1) {
        return createError("wrong number of arguments. got={d}, want=1", .{args.items.len});
    }
    if (!std.mem.eql(u8, args.items[0].getType(), Object.ARRAY_OBJ)) {
        return createError("argument to `rest` must be ARRAY, got {s}", .{args.items[0].getType()});
    }

    const arr = args.items[0].array;
    const length = arr.elements.items.len;
    if (length > 0) {
        var new_elements = std.ArrayList(*Object.Object).initCapacity(allocator, length - 1) catch @panic("OOM");
        new_elements.appendSliceAssumeCapacity(arr.elements.items[1..length]);

        const new_array_obj = allocator.create(Object.Array) catch @panic("OOM");
        new_array_obj.elements = new_elements;

        const new_obj = allocator.create(Object.Object) catch @panic("OOM");
        Globals.objectAppend(new_obj);
        new_obj.* = Object.Object{ .array = new_array_obj };

        return new_obj;
    }

    return Environment.NULL;
}

fn builtinFunctionPush(args: std.ArrayList(*Object.Object)) *Object.Object {
    if (args.items.len != 2) {
        return createError("wrong number of arguments. got={d}, want=2", .{args.items.len});
    }
    if (!std.mem.eql(u8, args.items[0].getType(), Object.ARRAY_OBJ)) {
        return createError("argument to `push` must be ARRAY, got {s}", .{args.items[0].getType()});
    }

    const arr = args.items[0].array;
    const length = arr.elements.items.len;

    var new_elements = std.ArrayList(*Object.Object).initCapacity(allocator, length + 1) catch @panic("OOM");
    new_elements.appendSliceAssumeCapacity(arr.elements.items);
    new_elements.items.len = length + 1;
    new_elements.items[length] = args.items[1];

    const new_array_obj = allocator.create(Object.Array) catch @panic("OOM");
    new_array_obj.elements = new_elements;

    const new_obj = allocator.create(Object.Object) catch @panic("OOM");
    Globals.objectAppend(new_obj);
    new_obj.* = Object.Object{ .array = new_array_obj };

    return new_obj;
}
