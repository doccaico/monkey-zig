const std = @import("std");

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
    lbracket, // [
    rbracket, // ]

    // Keywords
    function, // fn
    let, // let
    true, // true
    false, // false
    @"if", // if
    @"else", // else
    @"return", // return

    string, // "foobar"
    colon, // :
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
