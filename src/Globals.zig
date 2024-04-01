const std = @import("std");

const Ast = @import("Ast.zig");
const Object = @import("Object.zig");
const Environment = @import("Environment.zig");

var allocator: std.mem.Allocator = undefined;
pub var node_list: std.ArrayList(*Ast.Node) = undefined;
pub var object_list: std.ArrayList(*Object.Object) = undefined;
pub var env_list: std.ArrayList(*Environment) = undefined;
pub var args_list: std.ArrayList(std.ArrayList(*Object.Object)) = undefined;
pub var node_program_list: std.ArrayList(*Ast.Node) = undefined;
pub var line_list: std.ArrayList([]const u8) = undefined;
pub var string_list: std.ArrayList([]const u8) = undefined;

pub fn init(global_allocator: std.mem.Allocator) void {
    allocator = global_allocator;
    node_list = std.ArrayList(*Ast.Node).init(allocator);
    object_list = std.ArrayList(*Object.Object).init(allocator);
    env_list = std.ArrayList(*Environment).init(allocator);
    args_list = std.ArrayList(std.ArrayList(*Object.Object)).init(allocator);
    node_program_list = std.ArrayList(*Ast.Node).init(allocator);
    line_list = std.ArrayList([]const u8).init(allocator);
    string_list = std.ArrayList([]const u8).init(allocator);
}

pub fn deinit() void {
    // object_list
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
            .function => |x| allocator.destroy(x),
            .string => |x| allocator.destroy(x),
            .builtin => |x| allocator.destroy(x),
            .array => |x| {
                x.elements.deinit();
                allocator.destroy(x);
            },
            .hash => |x| {
                x.pairs.deinit();
                allocator.destroy(x);
            },
        }
        allocator.destroy(item);
    }
    object_list.deinit();

    // node_list
    for (node_list.items) |item| {
        allocator.destroy(item);
    }
    node_list.deinit();

    // env_list
    for (env_list.items) |item| {
        item.store.deinit();
        allocator.destroy(item);
    }
    env_list.deinit();

    // args_list
    for (args_list.items) |item| {
        item.deinit();
    }
    args_list.deinit();

    // node_program_list
    for (node_program_list.items) |item| {
        item.deinit();
    }
    node_program_list.deinit();

    // line_list
    line_list.deinit();

    // string_list
    for (string_list.items) |item| {
        allocator.free(item);
    }
    string_list.deinit();
}

pub fn nodeAppend(node: *Ast.Node) void {
    node_list.append(node) catch @panic("OOM");
}

pub fn objectAppend(obj: *Object.Object) void {
    object_list.append(obj) catch @panic("OOM");
}

pub fn envAppend(env: *Environment) void {
    env_list.append(env) catch @panic("OOM");
}

pub fn argsAppend(args: std.ArrayList(*Object.Object)) void {
    args_list.append(args) catch @panic("OOM");
}

pub fn nodeProgramAppend(node_program: *Ast.Node) void {
    node_program_list.append(node_program) catch @panic("OOM");
}

pub fn lineAppend(line: []const u8) void {
    line_list.append(line) catch @panic("OOM");
}

pub fn stringAppend(string: []const u8) void {
    string_list.append(string) catch @panic("OOM");
}
