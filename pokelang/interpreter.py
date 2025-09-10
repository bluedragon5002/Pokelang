"""
PokéLang interpreter (initial implementation)

- Java-like syntax but Pokémon-themed types/keywords:
    Pikachu   -> int
    Charizard -> char
    Bulbasaur -> boolean
    Squirtle  -> float
    Mew       -> String

- Print with:  Pokedex.println(expr);
- Supports:
    - variable declarations: Pikachu a = 5;
    - assignments: a = a + 2;
    - arithmetic and boolean ops
    - string and char literals
    - function call for Pokedex.println(...)

This is intentionally small and easy to extend.
"""

import re
import sys

# Token specification (order matters)
TOKEN_SPEC = [
    ('NEWLINE',  r'\n'),
    ('SKIP',     r'[ \t\r]+'),
    ('STRING',   r'"([^"\\]|\\.)*"'),
    ('CHAR',     r"'([^'\\]|\\.)'"),
    ('NUMBER',   r'\d+(\.\d+)?'),
    ('ID',       r'[A-Za-z_][A-Za-z0-9_]*'),
    ('OP',       r'\+\+|--|==|!=|<=|>=|&&|\|\||[+\-*/%!=<>]'),
    ('SEMIC',    r';'),
    ('LPAREN',   r'\('),
    ('RPAREN',   r'\)'),
    ('LBRACE',   r'\{'),
    ('RBRACE',   r'\}'),
    ('COMMA',    r','),
    ('DOT',      r'\.'),
    ('MISMATCH', r'.'),
]

TOK_REGEX = '|'.join('(?P<%s>%s)' % pair for pair in TOKEN_SPEC)
TOK_PATTERN = re.compile(TOK_REGEX)

POKE_TYPES = {
    'Pikachu': 'int',
    'Charizard': 'char',
    'Bulbasaur': 'bool',
    'Squirtle': 'float',
    'Mew': 'string',
}

KEYWORDS = set(list(POKE_TYPES.keys()) + ['true', 'false', 'Pokedex'])

class Token:
    def __init__(self, kind, value, pos):
        self.kind = kind
        self.value = value
        self.pos = pos
    def __repr__(self):
        return f'Token({self.kind}, {self.value!r}, {self.pos})'

def tokenize(code):
    tokens = []
    for mo in TOK_PATTERN.finditer(code):
        kind = mo.lastgroup
        value = mo.group()
        pos = mo.start()
        if kind == 'NUMBER':
            if '.' in value:
                value = float(value)
            else:
                value = int(value)
        elif kind == 'STRING':
            value = bytes(value[1:-1], "utf-8").decode("unicode_escape")
        elif kind == 'CHAR':
            inner = value[1:-1]
            value = bytes(inner, "utf-8").decode("unicode_escape")
        elif kind == 'ID':
            if value in KEYWORDS:
                kind = 'KEYWORD'
        elif kind == 'NEWLINE' or kind == 'SKIP':
            continue
        elif kind == 'MISMATCH':
            raise SyntaxError(f'Unexpected character {value!r} at position {pos}')
        tokens.append(Token(kind, value, pos))
    tokens.append(Token('EOF', '', len(code)))
    return tokens

# ---- Parser (simple Pratt-style expression parser) ----

class Parser:
    def __init__(self, tokens):
        self.tokens = tokens
        self.pos = 0

    def peek(self):
        return self.tokens[self.pos]

    def next(self):
        tok = self.peek()
        self.pos += 1
        return tok

    def expect(self, kind, value=None):
        tok = self.next()
        if tok.kind != kind or (value is not None and tok.value != value):
            raise SyntaxError(f'Expected {kind} {value}, got {tok.kind} {tok.value!r} at {tok.pos}')
        return tok

    def parse(self):
        stmts = []
        while self.peek().kind != 'EOF':
            stmts.append(self.parse_statement())
        return stmts

    def parse_statement(self):
        tok = self.peek()
        if tok.kind == 'KEYWORD' and tok.value in POKE_TYPES:
            return self.parse_var_decl()
        else:
            return self.parse_expr_statement()

    def parse_var_decl(self):
        t = self.next()
        typename = t.value
        id_tok = self.expect('ID')
        name = id_tok.value
        init = None
        if self.peek().kind == 'OP' and self.peek().value == '=':
            self.expect('OP', '=')
            init = self.parse_expression()
        self.expect('SEMIC')
        return ('decl', typename, name, init)

    def parse_expr_statement(self):
        expr = self.parse_expression()
        # allow assignment as top-level expression (returns assign tuple)
        if expr[0] == 'assign':
            self.expect('SEMIC')
            return ('assign', expr[1], expr[2])
        else:
            self.expect('SEMIC')
            return ('expr', expr)

    # Pratt parser helpers
    def parse_expression(self, rbp=0):
        tok = self.next()
        left = self.nud(tok)
        while rbp < self.lbp(self.peek()):
            tok = self.next()
            left = self.led(tok, left)
        return left

    def nud(self, tok):
        if tok.kind == 'NUMBER':
            return ('num', tok.value)
        if tok.kind == 'STRING':
            return ('str', tok.value)
        if tok.kind == 'CHAR':
            return ('char', tok.value)
        if tok.kind == 'KEYWORD' and tok.value in ('true', 'false'):
            return ('bool', tok.value == 'true')
        if tok.kind in ('ID', 'KEYWORD'):
            name = tok.value
            # dotted names
            while self.peek().kind == 'DOT':
                self.expect('DOT')
                attr = self.expect('ID').value
                name = f'{name}.{attr}'
            # function call?
            if self.peek().kind == 'LPAREN':
                self.expect('LPAREN')
                args = []
                if self.peek().kind != 'RPAREN':
                    args.append(self.parse_expression())
                    while self.peek().kind == 'COMMA':
                        self.expect('COMMA')
                        args.append(self.parse_expression())
                self.expect('RPAREN')
                return ('call', name, args)
            return ('var', name)
        if tok.kind == 'LPAREN':
            expr = self.parse_expression()
            self.expect('RPAREN')
            return expr
        if tok.kind == 'OP' and tok.value == '-':
            expr = self.parse_expression(70)
            return ('neg', expr)
        raise SyntaxError(f'Unexpected token in expression: {tok}')

    def lbp(self, tok):
        if tok.kind == 'OP':
            v = tok.value
            if v in ('*','/','%'): return 60
            if v in ('+','-'): return 50
            if v in ('==','!=','<','>','<=','>='): return 40
            if v in ('&&','||'): return 30
            if v == '=': return 10
        return 0

    def led(self, tok, left):
        if tok.kind == 'OP':
            v = tok.value
            if v in ('+','-','*','/','%','==','!=','<','>','<=','>=','&&','||'):
                right = self.parse_expression(self.lbp(tok))
                return ('binop', v, left, right)
            if v == '=':
                if left[0] != 'var':
                    raise SyntaxError('Left-hand side of assignment must be a variable')
                right = self.parse_expression(self.lbp(tok)-1)
                return ('assign', left[1], right)
        raise SyntaxError(f'Unexpected operator {tok} in led')

# ---- Interpreter ----

class Environment:
    def __init__(self):
        self.vars = {}
        self.types = {}

class Interpreter:
    def __init__(self, ast, output_func=print):
        self.ast = ast
        self.env = Environment()
        self.output = output_func

    def run(self):
        for stmt in self.ast:
            self.exec_stmt(stmt)

    def exec_stmt(self, stmt):
        kind = stmt[0]
        if kind == 'decl':
            _, tname, name, init = stmt
            if tname not in POKE_TYPES:
                raise RuntimeError(f'Unknown type {tname}')
            self.env.types[name] = POKE_TYPES[tname]
            if init is not None:
                val = self.eval_expr(init)
                self.env.vars[name] = self.cast(val, self.env.types[name])
            else:
                self.env.vars[name] = self.default_value(self.env.types[name])
        elif kind == 'assign':
            _, name, expr = stmt
            if name not in self.env.types:
                raise RuntimeError(f'Undeclared variable {name}')
            val = self.eval_expr(expr)
            self.env.vars[name] = self.cast(val, self.env.types[name])
        elif kind == 'expr':
            self.eval_expr(stmt[1])
        else:
            raise RuntimeError(f'Unknown statement {stmt}')

    def default_value(self, t):
        return {
            'int': 0,
            'float': 0.0,
            'char': '\0',
            'bool': False,
            'string': '',
        }[t]

    def cast(self, val, t):
        if t == 'int':
            if isinstance(val, bool): return int(val)
            if isinstance(val, (int, float)): return int(val)
            raise RuntimeError(f'Cannot cast {val!r} to int')
        if t == 'float':
            if isinstance(val, (int, float)): return float(val)
            raise RuntimeError(f'Cannot cast {val!r} to float')
        if t == 'char':
            if isinstance(val, str) and len(val) == 1: return val
            if isinstance(val, int): return chr(val)
            raise RuntimeError(f'Cannot cast {val!r} to char')
        if t == 'bool':
            if isinstance(val, bool): return val
            if isinstance(val, (int, float)): return val != 0
            raise RuntimeError(f'Cannot cast {val!r} to bool')
        if t == 'string':
            return str(val)
        return val

    def eval_expr(self, expr):
        etype = expr[0]
        if etype == 'num': return expr[1]
        if etype == 'str': return expr[1]
        if etype == 'char': return expr[1]
        if etype == 'bool': return expr[1]
        if etype == 'var':
            name = expr[1]
            if name in self.env.vars:
                return self.env.vars[name]
            raise RuntimeError(f'Undefined variable {name}')
        if etype == 'neg':
            return -self.eval_expr(expr[1])
        if etype == 'binop':
            op = expr[1]
            a = self.eval_expr(expr[2])
            b = self.eval_expr(expr[3])
            try:
                if op == '+': return a + b
                if op == '-': return a - b
                if op == '*': return a * b
                if op == '/': return a / b
                if op == '%': return a % b
                if op == '==': return a == b
                if op == '!=': return a != b
                if op == '<': return a < b
                if op == '>': return a > b
                if op == '<=': return a <= b
                if op == '>=': return a >= b
                if op == '&&': return bool(a) and bool(b)
                if op == '||': return bool(a) or bool(b)
            except Exception as e:
                raise RuntimeError(f'Error evaluating binop {op}: {e}')
            raise RuntimeError(f'Unknown operator {op}')
        if etype == 'assign':
            name = expr[1]
            val = self.eval_expr(expr[2])
            if name not in self.env.types:
                raise RuntimeError(f'Undeclared variable {name}')
            self.env.vars[name] = self.cast(val, self.env.types[name])
            return self.env.vars[name]
        if etype == 'call':
            fname = expr[1]
            args = [self.eval_expr(a) for a in expr[2]]
            return self.call_function(fname, args)
        raise RuntimeError(f'Unknown expr type {etype}')

    def call_function(self, name, args):
        if name == 'Pokedex.println':
            # simple printing
            self.output(*args)
            return None
        raise RuntimeError(f'Unknown function {name}')

# ---- CLI ----

def run_file(path):
    with open(path, 'r', encoding='utf-8') as f:
        code = f.read()
    tokens = tokenize(code)
    parser = Parser(tokens)
    ast = parser.parse()
    interp = Interpreter(ast)
    interp.run()

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print('Usage: python interpreter.py path/to/file.poke')
        sys.exit(1)
    run_file(sys.argv[1])