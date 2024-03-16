const std = @import("std");

const Token = @import("Token.zig");

const Lexer = @This();

input: []const u8,
position: usize = 0,
read_position: usize = 0,
ch: u8 = 0,

pub fn init(input: []const u8) Lexer {
    var lexer = Lexer{
        .input = input,
    };
    lexer.readChar();
    return lexer;
}

pub fn readChar(self: *Lexer) void {
    if (self.read_position >= self.input.len) {
        self.ch = 0;
    } else {
        self.ch = self.input[self.read_position];
    }
    self.position = self.read_position;
    self.read_position += 1;
}

fn isLetter(ch: u8) bool {
    return 'a' <= ch and ch <= 'z' or 'A' <= ch and ch <= 'Z' or ch == '_';
}

fn readIdentifier(self: *Lexer) []const u8 {
    const position = self.position;
    while (isLetter(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn skipWhitespace(self: *Lexer) void {
    while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
        self.readChar();
    }
}

fn isDigit(ch: u8) bool {
    return '0' <= ch and ch <= '9';
}

fn readNumber(self: *Lexer) []const u8 {
    const position = self.position;
    while (isDigit(self.ch)) {
        self.readChar();
    }
    return self.input[position..self.position];
}

fn peekChar(self: *const Lexer) ?u8 {
    if (self.read_position >= self.input.len) {
        return null;
    } else {
        return self.input[self.read_position];
    }
}

pub fn nextToken(self: *Lexer) Token {
    self.skipWhitespace();

    const tok: Token = switch (self.ch) {
        '=' => blk: {
            if (self.peekChar()) |c| {
                if (c == '=') {
                    self.readChar();
                    break :blk .{ .type = .eq, .literal = "==" };
                } else {
                    break :blk .{ .type = .assign, .literal = "=" };
                }
            }
        },
        '+' => .{ .type = .plus, .literal = "+" },
        '-' => .{ .type = .minus, .literal = "-" },
        '!' => blk: {
            if (self.peekChar()) |c| {
                if (c == '=') {
                    self.readChar();
                    break :blk .{ .type = .noteq, .literal = "!=" };
                } else {
                    break :blk .{ .type = .bang, .literal = "!" };
                }
            }
        },
        '/' => .{ .type = .slash, .literal = "/" },
        '*' => .{ .type = .asterisk, .literal = "*" },
        '<' => .{ .type = .lt, .literal = "<" },
        '>' => .{ .type = .gt, .literal = ">" },
        ';' => .{ .type = .semicolon, .literal = ";" },
        ',' => .{ .type = .comma, .literal = "," },
        '(' => .{ .type = .lparen, .literal = "(" },
        ')' => .{ .type = .rparen, .literal = ")" },
        '{' => .{ .type = .lbrace, .literal = "{" },
        '}' => .{ .type = .rbrace, .literal = "}" },
        0 => .{ .type = .eof, .literal = "" },
        else => blk: {
            if (isLetter(self.ch)) {
                const ident_literal = self.readIdentifier();
                return .{ .type = Token.lookupIdent(ident_literal), .literal = ident_literal };
            } else if (isDigit(self.ch)) {
                return .{ .type = .int, .literal = self.readNumber() };
            } else {
                const illegal_literal = self.input[self.position .. self.position + 1];
                break :blk .{ .type = .illegal, .literal = illegal_literal };
            }
        },
    };

    self.readChar();

    return tok;
}

test "nextToken() - 1" {
    const input = "=+(){},;";

    const tests = [_]Token{
        .{ .type = .assign, .literal = "=" },
        .{ .type = .plus, .literal = "+" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .lbrace, .literal = "{" },
        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected| {
        const got: Token = lexer.nextToken();
        std.testing.expectEqualDeep(expected, got) catch |e| {
            std.debug.print("expected {?}, got {?}\n", .{ expected, got });
            return e;
        };
    }
}

test "nextToken() - 2" {
    const input =
        \\let five = 5;
        \\let ten = 10;
        \\
        \\let add = fn(x, y) {
        \\  x + y;
        \\};
        \\
        \\let result = add(five, ten);
    ;

    const tests = [_]Token{
        .{ .type = .let, .literal = "let" },
        .{ .type = .ident, .literal = "five" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .let, .literal = "let" },
        .{ .type = .ident, .literal = "ten" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .let, .literal = "let" },
        .{ .type = .ident, .literal = "add" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .function, .literal = "fn" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .ident, .literal = "x" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .ident, .literal = "y" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .lbrace, .literal = "{" },
        .{ .type = .ident, .literal = "x" },
        .{ .type = .plus, .literal = "+" },
        .{ .type = .ident, .literal = "y" },
        .{ .type = .semicolon, .literal = ";" },
        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .let, .literal = "let" },
        .{ .type = .ident, .literal = "result" },
        .{ .type = .assign, .literal = "=" },
        .{ .type = .ident, .literal = "add" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .ident, .literal = "five" },
        .{ .type = .comma, .literal = "," },
        .{ .type = .ident, .literal = "ten" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected| {
        const got: Token = lexer.nextToken();
        std.testing.expectEqualDeep(expected, got) catch |e| {
            std.debug.print("expected {?}, got {?}\n", .{ expected, got });
            return e;
        };
    }
}

test "nextToken() - 3" {
    const input =
        \\!-/*5;
        \\5 < 10 > 5;
    ;

    const tests = [_]Token{
        .{ .type = .bang, .literal = "!" },
        .{ .type = .minus, .literal = "-" },
        .{ .type = .slash, .literal = "/" },
        .{ .type = .asterisk, .literal = "*" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .int, .literal = "5" },
        .{ .type = .lt, .literal = "<" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .gt, .literal = ">" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected| {
        const got: Token = lexer.nextToken();
        std.testing.expectEqualDeep(expected, got) catch |e| {
            std.debug.print("expected {?}, got {?}\n", .{ expected, got });
            return e;
        };
    }
}

test "nextToken() - 4" {
    const input =
        \\if (5 < 10) {
        \\    return true;
        \\} else {
        \\    return false;
        \\}
    ;

    const tests = [_]Token{
        .{ .type = .@"if", .literal = "if" },
        .{ .type = .lparen, .literal = "(" },
        .{ .type = .int, .literal = "5" },
        .{ .type = .lt, .literal = "<" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .rparen, .literal = ")" },
        .{ .type = .lbrace, .literal = "{" },

        .{ .type = .@"return", .literal = "return" },
        .{ .type = .true, .literal = "true" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .rbrace, .literal = "}" },
        .{ .type = .@"else", .literal = "else" },
        .{ .type = .lbrace, .literal = "{" },

        .{ .type = .@"return", .literal = "return" },
        .{ .type = .false, .literal = "false" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .rbrace, .literal = "}" },

        .{ .type = .eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected| {
        const got: Token = lexer.nextToken();
        std.testing.expectEqualDeep(expected, got) catch |e| {
            std.debug.print("expected {?}, got {?}\n", .{ expected, got });
            return e;
        };
    }
}

test "nextToken() - 5" {
    const input =
        \\10 == 10;
        \\10 != 9;
    ;

    const tests = [_]Token{
        .{ .type = .int, .literal = "10" },
        .{ .type = .eq, .literal = "==" },
        .{ .type = .int, .literal = "10" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .int, .literal = "10" },
        .{ .type = .noteq, .literal = "!=" },
        .{ .type = .int, .literal = "9" },
        .{ .type = .semicolon, .literal = ";" },

        .{ .type = .eof, .literal = "" },
    };

    var lexer = Lexer.init(input);

    for (tests) |expected| {
        const got: Token = lexer.nextToken();
        std.testing.expectEqualDeep(expected, got) catch |e| {
            std.debug.print("expected {?}, got {?}\n", .{ expected, got });
            return e;
        };
    }
}
