const std = @import("std");
// const Self = @This();
// const Token = @This();

// type: TokenType = .illegal,
// literal: []const u8 = "",

type: TokenType,
literal: []const u8,

pub const TokenType = enum(u8) {
    illegal, // $, ^, ...
    eof, // end of file

    // Identifiers + literals
    ident, // add, foobar, x, y, ...
    int, // 1234

    // Operators
    assign, // =
    plus, // +
    minus, // -
    bang, // !
    asterisk, // *
    slash, // /

    lt, // <
    gt, // >

    eq, // ==
    noteq, // !=

    // Delimiters
    comma, // ,
    semicolon, // ;

    lparen, // (
    rparen, // )
    lbrace, // {
    rbrace, // }

    // Keywords
    function, // fn
    let, // let
    true, // true
    false, // false
    @"if", // if
    @"else", // else
    @"return", // return
};

pub const keywords = std.ComptimeStringMap(TokenType, .{
    .{ "fn", .function },
    .{ "let", .let },
    .{ "true", .true },
    .{ "false", .false },
    .{ "if", .@"if" },
    .{ "else", .@"else" },
    .{ "return", .@"return" },
});

pub fn lookupIdent(literal: []const u8) TokenType {
    return keywords.get(literal) orelse .ident;
}

// pub const Token = struct {
// };

// pub fn init(@"type": TokenType, literal: []const u8) Self {
//     return Self{
//         .type = @"type",
//         .literal = literal,
//     };
// }

// pub fn newToken(tokenType: TokenType, ch: []const u8) Self {
//     return switch(tokenType) {
//         .illegal => Self{.Type: tokenType, Literal: }
//     };
// }
// pub const Token = struct {
// };
