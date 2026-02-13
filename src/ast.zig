const std = @import("std");
const Token = @import("lexer.zig").Token;

pub const Node = union(enum) {
    Statement: *Statement,
    Expression: *Expression,
    Program: *Program,
};

pub const Program = struct {
    statements: std.ArrayListUnmanaged(*Statement),

    pub fn init(allocator: std.mem.Allocator) *Program {
        const p = allocator.create(Program) catch unreachable;
        p.statements = .empty;
        return p;
    }
    
    pub fn deinit(self: *Program, allocator: std.mem.Allocator) void {
        self.statements.deinit(allocator);
        allocator.destroy(self);
    }
};

pub const Statement = union(enum) {
    Let: *LetStatement,
    Return: *ReturnStatement,
    Expression: *ExpressionStatement,
    Block: *BlockStatement,
    Class: *ClassStatement,
    While: *WhileStatement,
    For: *ForStatement,
};

pub const Expression = union(enum) {
    Identifier: *Identifier,
    IntegerLiteral: *IntegerLiteral,
    StringLiteral: *StringLiteral,
    Boolean: *Boolean,
    Prefix: *PrefixExpression,
    Infix: *InfixExpression,
    If: *IfExpression,
    Function: *FunctionLiteral,
    Call: *CallExpression,
    New: *NewExpression,
    Dot: *DotExpression,
    This: *ThisExpression,

    pub fn token(self: Expression) Token {
        switch (self) {
            inline else => |case| return case.token,
        }
    }
};

// Statements

pub const LetStatement = struct {
    token: Token,
    name: *Identifier,
    value: *Expression,
};

pub const ReturnStatement = struct {
    token: Token,
    return_value: *Expression,
};

pub const ExpressionStatement = struct {
    token: Token,
    expression: *Expression,
};

pub const BlockStatement = struct {
    token: Token,
    statements: std.ArrayListUnmanaged(*Statement),
};

// Expressions

pub const Identifier = struct {
    token: Token,
    value: []const u8,
};

pub const IntegerLiteral = struct {
    token: Token,
    value: i64,
};

pub const StringLiteral = struct {
    token: Token,
    value: []const u8,
};

pub const Boolean = struct {
    token: Token,
    value: bool,
};

pub const PrefixExpression = struct {
    token: Token,
    operator: []const u8,
    right: *Expression,
};

pub const InfixExpression = struct {
    token: Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,
};

pub const IfExpression = struct {
    token: Token,
    condition: *Expression,
    consequence: *BlockStatement,
    alternative: ?*BlockStatement,
};

pub const FunctionLiteral = struct {
    token: Token,
    parameters: std.ArrayListUnmanaged(*Identifier),
    body: *BlockStatement,
    name: []const u8,
};

pub const CallExpression = struct {
    token: Token,
    function: *Expression,
    arguments: std.ArrayListUnmanaged(*Expression),
};

pub const ClassStatement = struct {
    token: Token,
    name: *Identifier,
    methods: std.ArrayListUnmanaged(*FunctionLiteral),
};

pub const NewExpression = struct {
    token: Token,
    class_name: *Identifier,
    arguments: std.ArrayListUnmanaged(*Expression),
};

pub const DotExpression = struct {
    token: Token,
    left: *Expression,
    member: *Identifier,
};

pub const ThisExpression = struct {
    token: Token,
};

pub const WhileStatement = struct {
    token: Token,
    condition: *Expression,
    body: *BlockStatement,
};

pub const ForStatement = struct {
    token: Token,
    init: ?*Statement,
    condition: ?*Expression,
    post: ?*Statement,
    body: *BlockStatement,
};
