const builtin = @import("builtin");
const std = @import("std");

const Token = @import("Token.zig");
const Lexer = @import("Lexer.zig");
const Parser = @import("Parser.zig");
const Evaluator = @import("Evaluator.zig");

const PROMPT = ">> ";
const DELIMITER = if (builtin.os.tag == .windows) '\r' else '\n';
const MONKEY_FACE =
    \\            __,__
    \\   .--.  .-"     "-.  .--.
    \\  / .. \/  .-. .-.  \/ .. \
    \\ | |  '|  /   Y   \  |'  | |
    \\ | \   \  \ 0 | 0 /  /   / |
    \\  \ '- ,\.-"""""""-./, -' /
    \\   ''-' /_   ^ ^   _\ '-''
    \\       |  \._   _./  |
    \\       \   \ '~' /   /
    \\        '._ '-=-' _.'
    \\           '-----'
;

pub fn start(stdin: anytype, stdout: anytype) !void {
    var general_purpose_allocator = std.heap.GeneralPurposeAllocator(.{}){};
    const gpa = general_purpose_allocator.allocator();

    var input = std.ArrayList(u8).init(gpa);
    defer input.deinit();

    while (true) {
        try stdout.writeAll(PROMPT);

        stdin.streamUntilDelimiter(input.writer(), DELIMITER, null) catch |err| switch (err) {
            error.EndOfStream => {
                try stdout.writeAll("\nKeyboardInterrupt");
                input.clearRetainingCapacity();
                break;
            },
            else => |e| return e,
        };

        const space = if (builtin.os.tag == .windows) " \n" else " ";
        const line = std.mem.trim(u8, input.items, space);
        if (line.len == 0) {
            input.clearRetainingCapacity();
            continue;
        }

        const lexer = Lexer.init(line);
        var parser = try Parser.init(gpa, lexer);
        defer parser.deinit();
        var program = try parser.parseProgram();
        defer program.deinit();

        if (parser.errors.items.len != 0) {
            try printParserErrors(stdout, parser.errors);
            input.clearRetainingCapacity();
            continue;
        }

        // try program.string(stdout);
        // _ = try stdout.write("\n");

        const evaluated = Evaluator.eval(program);
        try evaluated.inspect(stdout);
        _ = try stdout.write("\n");

        // switch (evaluated) {
        //     .null
        // };

        // try stdout.writeAll(program.string(stdout));

        // var lexer = Lexer.init(line);
        // var tok: Token = lexer.nextToken();
        //
        // while (tok.type != .eof) {
        //     try stdout.print("{?}\n", .{tok});
        //     tok = lexer.nextToken();
        // }

        input.clearRetainingCapacity();
    }
}

fn printParserErrors(writer: anytype, errors: std.ArrayList([]const u8)) !void {
    _ = try writer.print("{s}\n", .{MONKEY_FACE});
    _ = try writer.write("Woops! We ran into some monkey business here!\n");
    for (errors.items) |msg| {
        try writer.print("\t{s}\n", .{msg});
    }
}
