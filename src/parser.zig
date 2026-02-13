const std = @import("std");
const Lexer = @import("lexer.zig").Lexer; // Import Struct
const Token = @import("lexer.zig").Token; // Import Struct
const TokenType = @import("lexer.zig").TokenType; // Import Enum
const ast = @import("ast.zig");

const Precedence = enum(u8) {
    Lowest,
    Assign,      // =
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
    Dot,         // object.member
};

pub const Parser = struct {
    lexer: *Lexer,
    cur_token: Token,
    peek_token: Token,
    errors: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, lexer: *Lexer) Parser {
        var p = Parser{
            .lexer = lexer,
            .cur_token = undefined,
            .peek_token = undefined,
            .errors = .empty,
            .allocator = allocator,
        };
        // Read two tokens to clear specific undefined state
        p.nextToken();
        p.nextToken();
        return p;
    }

    pub fn deinit(self: *Parser) void {
        for (self.errors.items) |err| {
            self.allocator.free(err);
        }
        self.errors.deinit(self.allocator);
    }

    pub fn nextToken(self: *Parser) void {
        self.cur_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    pub fn parseProgram(self: *Parser) *ast.Program {
        var program = ast.Program.init(self.allocator);

        while (self.cur_token.type != .EOF) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                program.statements.append(self.allocator, s) catch unreachable;
            }
            if (self.cur_token.type != .EOF) {
                self.nextToken();
            }
        }
        return program;
    }

    fn parseStatement(self: *Parser) ?*ast.Statement {
        switch (self.cur_token.type) {
            .Var => return self.parseVarStatement(),
            .Class => return self.parseClassStatement(),
            .While => return self.parseWhileStatement(),
            .For => return self.parseForStatement(),
            .Return => return null, // TODO
            else => return self.parseExpressionStatement(),
        }
    }

    fn parseVarStatement(self: *Parser) ?*ast.Statement {
        // var x = 5;
        const stmt = self.allocator.create(ast.LetStatement) catch unreachable;
        stmt.token = self.cur_token; // .Var

        if (!self.expectPeek(.Identifier)) {
            return null;
        }

        const name = self.allocator.create(ast.Identifier) catch unreachable;
        name.token = self.cur_token;
        name.value = self.cur_token.literal;
        stmt.name = name;

        if (!self.expectPeek(.Equals)) {
            return null;
        }

        self.nextToken(); // skip =

        stmt.value = self.parseExpression(.Lowest) orelse return null;

        if (self.peekTokenIs(.Semicolon)) {
            self.nextToken();
        }

        const wrapper = self.allocator.create(ast.Statement) catch unreachable;
        wrapper.* = ast.Statement{ .Let = stmt };
        return wrapper;
    }

    fn parseExpressionStatement(self: *Parser) ?*ast.Statement {
        const stmt = self.allocator.create(ast.ExpressionStatement) catch unreachable;
        stmt.token = self.cur_token;

        const expr = self.parseExpression(.Lowest);
        if (expr) |e| {
            stmt.expression = e;
        } else {
            return null; // Should not happen for valid expressions
        }

        if (self.peekTokenIs(.Semicolon)) {
            self.nextToken();
        }

        const wrapper = self.allocator.create(ast.Statement) catch unreachable;
        wrapper.* = ast.Statement{ .Expression = stmt };
        return wrapper;
    }

    fn parseExpression(self: *Parser, precedence: Precedence) ?*ast.Expression {
        const prefix = self.prefixParseFn(self.cur_token.type);
        if (prefix == null) {
            const msg = std.fmt.allocPrint(self.allocator, "No prefix parse function for {any} found", .{self.cur_token.type}) catch return null;
            self.addError(self.cur_token, msg);
            return null;
        }
        var left_exp = prefix.?(self);

        while (!self.peekTokenIs(.Semicolon) and @intFromEnum(precedence) < @intFromEnum(self.peekPrecedence())) {
            const infix = self.infixParseFn(self.peek_token.type);
            if (infix == null) {
                return left_exp;
            }
            
            self.nextToken();
            left_exp = infix.?(self, left_exp.?); // Recursively parse infix
        }

        return left_exp;
    }

    // Parsing Strategy functions
    
    fn prefixParseFn(self: *Parser, token_type: TokenType) ?*const fn(*Parser) ?*ast.Expression {
        _ = self;
        switch (token_type) {
            .Identifier => return parseIdentifier,
            .NumberLiteral => return parseIntegerLiteral,
            .StringLiteral => return parseStringLiteral,
            .Echo => return parseCallExpression, // Treat echo as a built-in call for now
            .New => return parseNewExpression,
            .This => return parseThisExpression,
            .Function => return parseFunctionLiteral,
            .If => return parseIfExpression,
            else => return null,
        }
    }

    fn infixParseFn(self: *Parser, token_type: TokenType) ?*const fn(*Parser, *ast.Expression) ?*ast.Expression {
        _ = self;
        switch (token_type) {
            .Dot => return parseDotExpression,
            .Plus, .Minus, .Asterisk, .Slash => return parseInfixExpression,
            .Equals, .LT, .GT => return parseInfixExpression,
            .OpenParen => return parseCallExpressionInfix, // Handle obj.method() or func()
            else => return null,
        }
    }

    fn parseIdentifier(self: *Parser) ?*ast.Expression {
        const ident = self.allocator.create(ast.Identifier) catch unreachable;
        ident.token = self.cur_token;
        ident.value = self.cur_token.literal;
        
        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Identifier = ident };
        return wrapper;
    }

    fn parseIntegerLiteral(self: *Parser) ?*ast.Expression {
        const lit = self.allocator.create(ast.IntegerLiteral) catch unreachable;
        lit.token = self.cur_token;
        
        // Parse int
        const value = std.fmt.parseInt(i64, self.cur_token.literal, 10) catch {
            return null;
        };
        lit.value = value;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .IntegerLiteral = lit };
        return wrapper;
    }

    fn parseStringLiteral(self: *Parser) ?*ast.Expression {
        const lit = self.allocator.create(ast.StringLiteral) catch unreachable;
        lit.token = self.cur_token;
        lit.value = self.cur_token.literal;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .StringLiteral = lit };
        return wrapper;
    }
    
    fn parseCallExpression(self: *Parser) ?*ast.Expression {
        // echo("hello")
        // Token is .Echo
        const call = self.allocator.create(ast.CallExpression) catch unreachable;
        call.token = self.cur_token; // echo
        
        // Treat 'echo' as the function identifier
        const func_ident = self.allocator.create(ast.Identifier) catch unreachable;
        func_ident.token = self.cur_token;
        func_ident.value = self.cur_token.literal;
        
        const func_wrapper = self.allocator.create(ast.Expression) catch unreachable;
        func_wrapper.* = ast.Expression{ .Identifier = func_ident };
        
        call.function = func_wrapper;
        call.arguments = .empty;
        
        if (!self.expectPeek(.OpenParen)) {
            return null;
        }
        
        // Parse arguments
        if (!self.peekTokenIs(.CloseParen)) {
             self.nextToken(); // move to first arg
             while (true) {
                 const arg = self.parseExpression(.Lowest);
                 if (arg) |a| {
                     call.arguments.append(self.allocator, a) catch |err| {
                         std.debug.print("Create argument failed: {any}\n", .{err});
                         return null;
                     };
                 }
                 
                 if (self.peekTokenIs(.Comma)) {
                     self.nextToken();
                     self.nextToken(); // move to next arg
                 } else {
                     break;
                 }
             }
        }
        
        if (!self.expectPeek(.CloseParen)) {
            return null;
        }

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Call = call };
        return wrapper;
    }

    fn parseClassStatement(self: *Parser) ?*ast.Statement {
        const stmt = self.allocator.create(ast.ClassStatement) catch unreachable;
        stmt.token = self.cur_token; // .Class

        if (!self.expectPeek(.Identifier)) return null;

        const name = self.allocator.create(ast.Identifier) catch unreachable;
        name.token = self.cur_token;
        name.value = self.cur_token.literal;
        stmt.name = name;

        if (!self.expectPeek(.OpenBrace)) return null;

        stmt.methods = .empty;

        self.nextToken(); // consume {

        while (!self.curTokenIs(.CloseBrace) and !self.curTokenIs(.EOF)) {
            if (self.curTokenIs(.Function)) {
                 const method_expr = self.parseFunctionLiteral() orelse return null;
                 stmt.methods.append(self.allocator, method_expr.Function) catch unreachable;
                 self.nextToken();
            } else {
                 self.nextToken();
            }
        }
        
        if (!self.curTokenIs(.CloseBrace)) {
            const msg = std.fmt.allocPrint(self.allocator, "Expected }} to close class, got {any}", .{self.cur_token.type}) catch return null;
            self.addError(self.cur_token, msg);
            return null;
        }

        const wrapper = self.allocator.create(ast.Statement) catch unreachable;
        wrapper.* = ast.Statement{ .Class = stmt };
        return wrapper;
    }

    fn parseNewExpression(self: *Parser) ?*ast.Expression {
        const expr = self.allocator.create(ast.NewExpression) catch unreachable;
        expr.token = self.cur_token; // .New

        if (!self.expectPeek(.Identifier)) return null;

        const class_name = self.allocator.create(ast.Identifier) catch unreachable;
        class_name.token = self.cur_token;
        class_name.value = self.cur_token.literal;
        expr.class_name = class_name;

        if (!self.expectPeek(.OpenParen)) return null;

        expr.arguments = .empty;
        // ... parse arguments (similar to call expression)
        if (!self.peekTokenIs(.CloseParen)) {
             self.nextToken();
             while (true) {
                 const arg = self.parseExpression(.Lowest);
                 if (arg) |a| {
                     expr.arguments.append(self.allocator, a) catch unreachable;
                 }
                 if (self.peekTokenIs(.Comma)) {
                     self.nextToken();
                     self.nextToken();
                 } else {
                     break;
                 }
             }
        }
        if (!self.expectPeek(.CloseParen)) return null;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .New = expr };
        return wrapper;
    }

    fn parseDotExpression(self: *Parser, left: *ast.Expression) ?*ast.Expression {
        const expr = self.allocator.create(ast.DotExpression) catch unreachable;
        expr.token = self.cur_token; // .Dot
        expr.left = left;

        if (!self.expectPeek(.Identifier)) return null;

        const member = self.allocator.create(ast.Identifier) catch unreachable;
        member.token = self.cur_token;
        member.value = self.cur_token.literal;
        expr.member = member;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Dot = expr };
        return wrapper;
    }

    fn parseThisExpression(self: *Parser) ?*ast.Expression {
        const expr = self.allocator.create(ast.ThisExpression) catch unreachable;
        expr.token = self.cur_token;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .This = expr };
        return wrapper;
    }

    fn parseFunctionLiteral(self: *Parser) ?*ast.Expression {
        const lit = self.allocator.create(ast.FunctionLiteral) catch unreachable;
        lit.token = self.cur_token; // .Function
        
        // Optional name (for methods)
        if (self.peekTokenIs(.Identifier)) {
            self.nextToken();
            lit.name = self.cur_token.literal;
        } else {
            lit.name = "";
        }

        if (!self.expectPeek(.OpenParen)) return null;

        lit.parameters = .empty;
        if (!self.peekTokenIs(.CloseParen)) {
            self.nextToken();
            while (true) {
                const ident = self.allocator.create(ast.Identifier) catch unreachable;
                ident.token = self.cur_token;
                ident.value = self.cur_token.literal;
                lit.parameters.append(self.allocator, ident) catch unreachable;

                if (self.peekTokenIs(.Comma)) {
                    self.nextToken();
                    self.nextToken();
                } else {
                    break;
                }
            }
        }
        if (!self.expectPeek(.CloseParen)) return null;

        if (!self.expectPeek(.OpenBrace)) return null;
        lit.body = self.parseBlockStatement() orelse return null;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Function = lit };
        return wrapper;
    }

    fn parseBlockStatement(self: *Parser) ?*ast.BlockStatement {
        const block = self.allocator.create(ast.BlockStatement) catch unreachable;
        block.token = self.cur_token;
        block.statements = .empty;

        self.nextToken();

        while (!self.curTokenIs(.CloseBrace) and !self.curTokenIs(.EOF)) {
            const stmt = self.parseStatement();
            if (stmt) |s| {
                block.statements.append(self.allocator, s) catch unreachable;
            }
            self.nextToken();
        }
        
        return block;
    }

    fn parseInfixExpression(self: *Parser, left: *ast.Expression) ?*ast.Expression {
        const expr = self.allocator.create(ast.InfixExpression) catch unreachable;
        expr.token = self.cur_token;
        expr.operator = self.cur_token.literal;
        expr.left = left;

        const precedence = self.curPrecedence();
        self.nextToken();
        expr.right = self.parseExpression(precedence) orelse return null;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Infix = expr };
        return wrapper;
    }

    fn curPrecedence(self: *Parser) Precedence {
        return switch (self.cur_token.type) {
            .Equals => .Equals,
            .LT, .GT => .LessGreater,
            .Plus, .Minus => .Sum,
            .Asterisk, .Slash => .Product,
            .Dot => .Dot,
            .OpenParen => .Call,
            else => .Lowest,
        };
    }

    fn parseCallExpressionInfix(self: *Parser, left: *ast.Expression) ?*ast.Expression {
        const call = self.allocator.create(ast.CallExpression) catch unreachable;
        call.token = self.cur_token; // (
        call.function = left;
        call.arguments = .empty;

        // self.nextToken(); // consume ( is already done by parseExpression loop? 
        // No, parseExpression loop calls self.nextToken() BEFORE calling infix.?
        // So cur_token IS (. Correct.
        
        if (!self.peekTokenIs(.CloseParen)) {
            self.nextToken(); // move to first arg
            while (true) {
                const arg = self.parseExpression(.Lowest);
                if (arg) |a| {
                    call.arguments.append(self.allocator, a) catch unreachable;
                }
                if (self.peekTokenIs(.Comma)) {
                    self.nextToken();
                    self.nextToken();
                } else {
                    break;
                }
            }
        }

        if (!self.expectPeek(.CloseParen)) return null;

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .Call = call };
        return wrapper;
    }

    // Helpers

    fn curTokenIs(self: *Parser, t: TokenType) bool {
        return self.cur_token.type == t;
    }

    fn peekTokenIs(self: *Parser, t: TokenType) bool {
        return self.peek_token.type == t;
    }

    fn expectPeek(self: *Parser, t: TokenType) bool {
        if (self.peekTokenIs(t)) {
            self.nextToken();
            return true;
        }
        self.peekError(t);
        return false;
    }

    fn peekPrecedence(self: *Parser) Precedence {
        return switch (self.peek_token.type) {
            .Equals => .Equals,
            .LT, .GT => .LessGreater,
            .Plus, .Minus => .Sum,
            .Asterisk, .Slash => .Product,
            .Dot => .Dot,
            .OpenParen => .Call,
            else => .Lowest,
        };
    }
    
    fn peekError(self: *Parser, t: TokenType) void {
        const msg = std.fmt.allocPrint(self.allocator, "Expected next token to be {any}, got {any} instead", .{ t, self.peek_token.type }) catch return;
        self.addError(self.peek_token, msg);
    }

    fn addError(self: *Parser, token: Token, msg: []const u8) void {
        const full_msg = std.fmt.allocPrint(self.allocator, "[Parser Error] Line {d}, Col {d}: {s}", .{ token.line, token.column, msg }) catch {
            self.allocator.free(msg);
            return;
        };
        self.allocator.free(msg);
        self.errors.append(self.allocator, full_msg) catch {
            self.allocator.free(full_msg);
        };
    }

    fn parseIfExpression(self: *Parser) ?*ast.Expression {
        const expr = self.allocator.create(ast.IfExpression) catch unreachable;
        expr.token = self.cur_token; // if or jodi

        if (!self.expectPeek(.OpenParen)) return null;
        self.nextToken();
        expr.condition = self.parseExpression(.Lowest) orelse return null;
        if (!self.expectPeek(.CloseParen)) return null;

        if (!self.expectPeek(.OpenBrace)) return null;
        expr.consequence = self.parseBlockStatement() orelse return null;
        // parseBlockStatement ends at }

        if (self.peekTokenIs(.Else)) {
            self.nextToken(); // move to else/na
            if (!self.expectPeek(.OpenBrace)) return null;
            expr.alternative = self.parseBlockStatement() orelse return null;
        } else {
            expr.alternative = null;
        }

        const wrapper = self.allocator.create(ast.Expression) catch unreachable;
        wrapper.* = ast.Expression{ .If = expr };
        return wrapper;
    }

    fn parseWhileStatement(self: *Parser) ?*ast.Statement {
        const stmt = self.allocator.create(ast.WhileStatement) catch unreachable;
        stmt.token = self.cur_token; // while or ghur

        if (!self.expectPeek(.OpenParen)) return null;
        self.nextToken();
        stmt.condition = self.parseExpression(.Lowest) orelse return null;
        if (!self.expectPeek(.CloseParen)) return null;

        if (!self.expectPeek(.OpenBrace)) return null;
        stmt.body = self.parseBlockStatement() orelse return null;

        const wrapper = self.allocator.create(ast.Statement) catch unreachable;
        wrapper.* = ast.Statement{ .While = stmt };
        return wrapper;
    }

    fn parseForStatement(self: *Parser) ?*ast.Statement {
        const stmt = self.allocator.create(ast.ForStatement) catch unreachable;
        stmt.token = self.cur_token; // .For

        if (!self.expectPeek(.OpenParen)) return null;
        self.nextToken(); // Move to init

        // Init
        if (self.curTokenIs(.Semicolon)) {
            stmt.init = null;
            self.nextToken();
        } else {
            stmt.init = self.parseStatement();
            if (self.curTokenIs(.Semicolon)) {
                 self.nextToken();
            }
        }

        // Condition
        if (self.curTokenIs(.Semicolon)) {
             stmt.condition = null;
        } else {
             stmt.condition = self.parseExpression(.Lowest);
        }
        
        if (!self.expectPeek(.Semicolon)) return null;
        self.nextToken();

        // Post
        if (self.curTokenIs(.CloseParen)) {
             stmt.post = null;
        } else {
             const post_expr = self.parseExpression(.Lowest);
             if (post_expr) |p| {
                  const p_stmt = self.allocator.create(ast.ExpressionStatement) catch unreachable;
                  p_stmt.token = p.*.token();
                  p_stmt.expression = p;
                  const wrapper = self.allocator.create(ast.Statement) catch unreachable;
                  wrapper.* = ast.Statement{ .Expression = p_stmt };
                  stmt.post = wrapper;
             } else {
                  stmt.post = null;
             }
             if (!self.expectPeek(.CloseParen)) return null;
        }
        
        if (!self.expectPeek(.OpenBrace)) return null;
        stmt.body = self.parseBlockStatement() orelse return null;

        const wrapper = self.allocator.create(ast.Statement) catch unreachable;
        wrapper.* = ast.Statement{ .For = stmt };
        return wrapper;
    }
};
