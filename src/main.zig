const std = @import("std");

const repl = @import("Repl.zig");

pub fn main() !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    try stdout.writeAll("Hello! This is the Monkey programming language!\n");
    try stdout.writeAll("Feel free to type in commands\n");

    try repl.start(gpa, stdin, stdout);
}

test {
    _ = @import("Ast.zig");
    _ = @import("Environment.zig");
    _ = @import("Evaluator.zig");
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("Repl.zig");
    _ = @import("Token.zig");
}
