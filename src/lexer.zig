const std = @import("std");

pub const TokenType = enum {
    // Keywords
    Function,  // function
    Var,       // var
    Echo,      // echo/print
    If,        // if
    Else,      // else
    While,     // while
    Return,    // return
    Class,     // class/shreni
    New,       // new/naya
    This,      // this/iswa
    For,       // for
    
    // Types
    Int,       // int
    String,    // string
    
    // Literals
    Identifier,
    StringLiteral,
    NumberLiteral,

    // Symbols
    OpenParen,
    CloseParen,
    OpenBrace,
    CloseBrace,
    Comma,
    Semicolon,
    Equals,
    Plus,
    Minus,
    Asterisk,
    Slash,
    Dot,
    LT,        // <
    GT,        // >
    
    EOF,
    Illegal,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    line: usize,
    column: usize,
};

pub const Lexer = struct {
    input: []const u8,
    position: usize = 0,
    read_position: usize = 0,
    ch: u32 = 0, // Current char (unicode code point)
    line: usize = 1,
    column: usize = 0,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{ .input = input };
        l.readChar();
        return l;
    }

    fn peekChar(self: *Lexer) u32 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }

    fn skipComment(self: *Lexer) void {
        while (self.ch != '\n' and self.ch != 0) {
            self.readChar();
        }
    }

    pub fn nextToken(self: *Lexer) Token {
        self.skipWhitespace();

        const start_line = self.line;
        const start_column = self.column;

        const token = switch (self.ch) {
            '{' => self.newTokenAt(.OpenBrace, "{", start_line, start_column),
            '}' => self.newTokenAt(.CloseBrace, "}", start_line, start_column),
            '(' => self.newTokenAt(.OpenParen, "(", start_line, start_column),
            ')' => self.newTokenAt(.CloseParen, ")", start_line, start_column),
            ',' => self.newTokenAt(.Comma, ",", start_line, start_column),
            ';' => self.newTokenAt(.Semicolon, ";", start_line, start_column),
            '=' => self.newTokenAt(.Equals, "=", start_line, start_column),
            '+' => self.newTokenAt(.Plus, "+", start_line, start_column),
            '-' => self.newTokenAt(.Minus, "-", start_line, start_column),
            '*' => self.newTokenAt(.Asterisk, "*", start_line, start_column),
            '/' => {
                if (self.peekChar() == '/') {
                    self.skipComment();
                    return self.nextToken();
                }
                const token = self.newTokenAt(.Slash, "/", start_line, start_column);
                self.readChar();
                return token;
            },
            '.' => {
                const token = self.newTokenAt(.Dot, ".", start_line, start_column);
                self.readChar();
                return token;
            },
            '<' => self.newTokenAt(.LT, "<", start_line, start_column),
            '>' => self.newTokenAt(.GT, ">", start_line, start_column),
            0 => self.newTokenAt(.EOF, "", start_line, start_column),
            '"' => return self.readString(),
            else => {
                if (isLetter(self.ch)) {
                    return self.readIdentifier();
                } else if (isDigit(self.ch)) {
                    return self.readNumber();
                } else {
                    const token = self.newTokenAt(.Illegal, "INVALID", start_line, start_column);
                    self.readChar();
                    return token;
                }
            },
        };

        self.readChar();
        return token;
    }

    fn newTokenAt(self: *Lexer, token_type: TokenType, literal: []const u8, line: usize, col: usize) Token {
        _ = self;
        return Token{
            .type = token_type,
            .literal = literal,
            .line = line,
            .column = col,
        };
    }

    fn newToken(self: *Lexer, token_type: TokenType, literal: []const u8) Token {
        return Token{
            .type = token_type,
            .literal = literal,
            .line = self.line,
            .column = self.column,
        };
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            // Basic ASCII handling for now, need UTF-8 decoding for proper Hindi/Assamese support
            // For MVP, simplistic byte reading (will fix for unicode later)
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.column += 1;
    }
    
    fn skipWhitespace(self: *Lexer) void {
        while (self.ch == ' ' or self.ch == '\t' or self.ch == '\n' or self.ch == '\r') {
            if (self.ch == '\n') {
                self.line += 1;
                self.column = 0;
            }
            self.readChar();
        }
    }

    fn readIdentifier(self: *Lexer) Token {
        const line = self.line;
        const col = self.column;
        const position = self.position;
        while (isLetter(self.ch) or isDigit(self.ch)) {
            self.readChar();
        }
        
        const literal = self.input[position..self.position];
        return Token{
            .type = lookupIdent(literal),
            .literal = literal,
            .line = line,
            .column = col,
        };
    }

    fn readNumber(self: *Lexer) Token {
        const line = self.line;
        const col = self.column;
        const position = self.position;
        while (isDigit(self.ch)) {
            self.readChar();
        }
        const literal = self.input[position..self.position];
        return Token{
            .type = .NumberLiteral,
            .literal = literal,
            .line = line,
            .column = col,
        };
    }
    
    fn readString(self: *Lexer) Token {
        const line = self.line;
        const col = self.column;
        const position = self.position + 1; // skip opening quote
        self.readChar(); // consume opening quote
        
        while (self.ch != '"' and self.ch != 0) {
            self.readChar();
        }
        
        const literal = self.input[position..self.position];
        // Consume closing quote if present
        if (self.ch == '"') {
            self.readChar(); 
        } 
        
        return Token{
            .type = .StringLiteral,
            .literal = literal,
            .line = line,
            .column = col,
        };
    }

    fn isLetter(ch: u32) bool {
        return ('a' <= ch and ch <= 'z') or ('A' <= ch and ch <= 'Z') or ch == '_';
        // TODO: crucial - add Hindi/Assamese Unicode ranges here
    }

    fn isDigit(ch: u32) bool {
        return '0' <= ch and ch <= '9';
    }

    fn lookupIdent(ident: []const u8) TokenType {
        if (std.mem.eql(u8, ident, "function") or std.mem.eql(u8, ident, "kaam")) return .Function;
        if (std.mem.eql(u8, ident, "var") or std.mem.eql(u8, ident, "dhor")) return .Var;
        if (std.mem.eql(u8, ident, "echo") or std.mem.eql(u8, ident, "bol") or std.mem.eql(u8, ident, "print")) return .Echo;
        if (std.mem.eql(u8, ident, "if") or std.mem.eql(u8, ident, "jodi")) return .If;
        if (std.mem.eql(u8, ident, "else") or std.mem.eql(u8, ident, "na")) return .Else;
        if (std.mem.eql(u8, ident, "while") or std.mem.eql(u8, ident, "ghur")) return .While;
        if (std.mem.eql(u8, ident, "return")) return .Return;
        if (std.mem.eql(u8, ident, "int") or std.mem.eql(u8, ident, "ank")) return .Int;
        if (std.mem.eql(u8, ident, "string") or std.mem.eql(u8, ident, "txt")) return .String;
        if (std.mem.eql(u8, ident, "class") or std.mem.eql(u8, ident, "shreni")) return .Class;
        if (std.mem.eql(u8, ident, "new") or std.mem.eql(u8, ident, "naya") or std.mem.eql(u8, ident, "noya")) return .New;
        if (std.mem.eql(u8, ident, "this") or std.mem.eql(u8, ident, "iswa")) return .This;
        if (std.mem.eql(u8, ident, "for")) return .For;
        return .Identifier;
    }
};
