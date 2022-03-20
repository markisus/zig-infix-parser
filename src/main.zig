const std = @import("std");
const testing = std.testing;

const Token = union(enum) {
    ref: []const u8,
    literal: f64,
    @"+": void,
    @"*": void,
    @"-": void,
    @"(": void,
    @")": void,
};

fn tokenPrecedence(token: Token) u8 {
    return switch (token) {
        .ref => 0,
        .literal => 0,
        .@"(" => 0,
        .@")" => 255,
        .@"*" => 9,
        .@"-" => 8,
        .@"+" => 1,
    };
}

pub fn Stack(comptime T: type) type {
    return struct {
        const Self = @This();

        _buf: [128]T = undefined,
        len: u8 = 0,

        pub fn push(comptime self: *Self, token: T) void {
            self._buf[self.len] = token;
            self.len += 1;
        }

        pub fn peek(comptime self: *const Self) T {
            return self._buf[self.len - 1];
        }

        pub fn pop(comptime self: *Self) void {
            self.len -= 1;
        }

        pub fn peekPop(comptime self: *Self) T {
            const token = self.peek();
            self.pop();
            return token;
        }

        pub fn empty(comptime self: *const Self) bool {
            return self.len == 0;
        }
    };
}

fn pushAstNode(comptime operator_stack: *Stack(Token), comptime ast: *Stack(type)) void {
    // helper function for CompileExpression
    // assemble an AST node from the top of the operator stack
    // and the top few AST nodes of the ast stack

    switch (operator_stack.peekPop()) {
        .@"+" => {
            const rhs = ast.peekPop();
            const lhs = ast.peekPop();
            ast.push(PlusOp(lhs, rhs));
        },
        .@"-" => {
            const rhs = ast.peekPop();
            const lhs = ast.peekPop();
            ast.push(MinusOp(lhs, rhs));
        },
        .@"*" => {
            const rhs = ast.peekPop();
            const lhs = ast.peekPop();
            ast.push(MulOp(lhs, rhs));
        },
        else => unreachable,
    }
}

fn CompileExpression(comptime expression: []const u8) type {
    comptime var work_buf: [100]Token = undefined;
    const tokens = comptime try tokenize(expression, work_buf[0..]);

    var operator_stack: Stack(Token) = .{};
    var ast_stack: Stack(type) = .{};

    for (tokens) |token| {
        switch (token) {
            .literal => {
                ast_stack.push(LiteralOp(token.literal));
            },
            .ref => {
                ast_stack.push(RefOp(token.ref));
            },
            .@"(" => {
                operator_stack.push(token);
            },
            .@")" => {
                while (!operator_stack.empty() and operator_stack.peek() != .@"(") {
                    pushAstNode(&operator_stack, &ast_stack);
                }
                std.debug.assert(!operator_stack.empty() and operator_stack.peek() == .@"(");
                operator_stack.pop();
            },
            else => {
                while (!operator_stack.empty() and (tokenPrecedence(operator_stack.peek()) >= tokenPrecedence(token))) {
                    pushAstNode(&operator_stack, &ast_stack);
                }
                operator_stack.push(token);
            },
        }
    }
    while (!operator_stack.empty()) {
        pushAstNode(&operator_stack, &ast_stack);
    }

    if (ast_stack.len != 1) {
        @compileLog("malformed expression");
    }

    return ast_stack.peek();
}

fn LiteralOp(comptime f: f64) type {
    return struct {
        pub fn eval(vars: anytype) f64 {
            _ = vars;
            return f;
        }
    };
}

fn PlusOp(comptime a: type, comptime b: type) type {
    return struct {
        pub fn eval(vars: anytype) f64 {
            return a.eval(vars) + b.eval(vars);
        }
    };
}

fn MulOp(comptime a: type, comptime b: type) type {
    return struct {
        pub fn eval(vars: anytype) f64 {
            return a.eval(vars) * b.eval(vars);
        }
    };
}

fn MinusOp(comptime a: type, comptime b: type) type {
    return struct {
        pub fn eval(vars: anytype) f64 {
            return a.eval(vars) - b.eval(vars);
        }
    };
}

fn RefOp(comptime name: []const u8) type {
    return struct {
        pub fn eval(vars: anytype) f64 {
            // @compileLog(vars);
            return @field(vars, name);
        }
    };
}

pub fn dumpTokens(tokens: []const Token) void {
    for (tokens) |token| {
        switch (token) {
            .ref => {
                std.debug.print("{s}", .{token.ref});
            },
            .literal => {
                std.debug.print("{d}", .{token.literal});
            },
            .@"(" => {
                std.debug.print("(", .{});
            },
            .@")" => {
                std.debug.print(")", .{});
            },
            .@"+" => {
                std.debug.print("+", .{});
            },
            .@"-" => {
                std.debug.print("-", .{});
            },
            .@"*" => {
                std.debug.print("*", .{});
            },
        }
        std.debug.print(" ", .{});
    }
    std.debug.print("\n", .{});
}

pub fn isTokenEnd(comptime char: u8) bool {
    return switch (char) {
        ' ' => true,
        '(' => true,
        ')' => true,
        '*' => true,
        '+' => true,
        '-' => true,
        else => false,
    };
}

const CharSegment = struct { start: comptime_int, end: comptime_int };

fn nextSegment(comptime str: []const u8, search_start: comptime_int) ?CharSegment {
    // find the first continguous sequence of characters that is not space
    comptime var start_idx = search_start;
    inline while (start_idx < str.len and str[start_idx] == ' ') {
        start_idx += 1;
    }
    comptime var end_idx = start_idx;
    inline while (end_idx < str.len and comptime !isTokenEnd(str[end_idx])) {
        end_idx += 1;
    }
    if (end_idx == start_idx) end_idx += 1;
    if (start_idx < str.len and end_idx <= str.len) {
        return CharSegment{ .start = start_idx, .end = end_idx };
    }
    return null;
}

fn isDigit(d: u8) bool {
    return ('0' <= d and d <= '9');
}

fn isAlpha(d: u8) bool {
    return ('a' <= d and d <= 'z') or ('A' <= d and d <= 'Z');
}

fn tokenize(comptime str: []const u8, comptime output_buf: []Token) ![]Token {
    _ = output_buf;
    comptime var output_idx = 0;
    comptime var start_idx = 0;
    inline while (comptime nextSegment(str, start_idx)) |segment| : (start_idx = segment.end) {
        const token = comptime str[segment.start..segment.end];

        if (isDigit(token[0])) {
            // parse as literal
            output_buf[output_idx] = Token{ .literal = try std.fmt.parseFloat(f64, token) };
            output_idx += 1;
        } else if (isAlpha(token[0])) {
            // parse as a reference
            output_buf[output_idx] = Token{ .ref = token };
            output_idx += 1;
        } else {
            // parse as operator or parens
            if (token.len != 1) {
                @compileError("unexpected token");
            }
            const char = token[0];
            output_buf[output_idx] = switch (char) {
                '*' => .@"*",
                '+' => .@"+",
                '-' => .@"-",
                '(' => .@"(",
                ')' => .@")",
                else => unreachable,
            };
            output_idx += 1;
        }
    }
    return output_buf[0..output_idx];
}

test "parse1" {
    const E = CompileExpression("x*(x+1)*y-3*y");
    const x: f64 = 1.234;
    const y: f64 = -3.456;
    try std.testing.expect(E.eval(.{ .x = x, .y = y }) == (x * (x + 1) * y - 3 * y));
}

test "parse2" {
    const E = CompileExpression("2*3");
    try std.testing.expect(E.eval(.{}) == (2 * 3));
}

// Malformed expression should trigger compile error
// test "parse3" {
//     const E = CompileExpression("2*3  8");
//     try std.testing.expect(E.eval(.{}) == (2 * 3));
// }
