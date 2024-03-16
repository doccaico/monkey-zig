const std = @import("std");

// const token = @import("token.zig");
// const Token = token.Token;
// const lookupIdent = token.lookupIdent;

const repl = @import("Repl.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    // std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // {
    //     const t: Token = .{ .type = .ident, .literal = "var" };
    //
    //     std.debug.print("{?}\n", .{t});
    //     std.debug.print("{?}\n", .{t.type});
    //     std.debug.print("{?}\n", .{lookupIdent(t.literal)});
    // }
    // {
    //     const t: Token = .{ .type = .ident, .literal = "fn" };
    //
    //     std.debug.print("{?}\n", .{t});
    //     std.debug.print("{?}\n", .{t.type});
    //     std.debug.print("{?}\n", .{lookupIdent(t.literal)});
    // }

    const in = std.io.getStdIn();
    const out = std.io.getStdOut();

    try out.writer().writeAll("Hello! This is the Monkey programming language!\n");
    try out.writer().writeAll("Feel free to type in commands\n");

    try repl.start(in.reader(), out.writer());
    // try repl.start(in, out);

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    // const stdout_file = std.io.getStdOut().writer();
    // var bw = std.io.bufferedWriter(stdout_file);
    // const stdout = bw.writer();
    //
    // try stdout.print("Run `zig build test` to run the tests.\n", .{});
    //
    // try bw.flush(); // don't forget to flush!
}

test {
    _ = @import("Token.zig");
    _ = @import("Lexer.zig");
    _ = @import("Parser.zig");
    _ = @import("Ast.zig");
    _ = @import("Statement.zig");
    _ = @import("Expression.zig");
    _ = @import("Repl.zig");
    // _ = @import("Evaluator.zig");
}
