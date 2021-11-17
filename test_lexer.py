import ply.lex as lex
import ply.yacc as yacc
import sys
from ply.lex import TOKEN
import tokenize

tokens = ['INT', 'FLOAT', 'NAME', 'PLUS', 'MINUS', 'DIVIDE', 'MULTIPLY', 'EQUALS',
          'EQEQUAL', 'NOTEQUAL', 'LESSEQUAL', 'LEFTSHIFT', 'GREATEREQUAL',
          'RIGHTSHIFT', 'PLUSEQUAL', 'MINEQUAL', 'STAREQUAL', 'SLASHEQUAL']

t_PLUS = r'\+'
t_MINUS = r'\-'
t_MULTIPLY = r'\*'
t_DIVIDE = r'\/'
t_EQUALS = r'\='

t_EQEQUAL = r'=='
t_NOTEQUAL = r'!='
t_LESSEQUAL = r'<='
t_LEFTSHIFT = r'<<'
t_GREATEREQUAL = r'>='
t_RIGHTSHIFT = r'>>'
t_PLUSEQUAL = r'\+='
t_MINEQUAL = r'-='
t_STAREQUAL = r'\*='
t_SLASHEQUAL = r'/='
t_PERCENTEQUAL = r'%='
t_STARSTAR = r'\*\*'
t_SLASHSLASH = r'//'
t_STARSTAREQUAL = r'\*\*='
t_SLASHSLASHEQUAL = r'//='

t_COLON = r':'
t_COMMA = r','
t_SEMI  = r';'
t_PLUS  = r'\+'
t_MINUS = r'-'
t_STAR  = r'\*'
t_SLASH = r'/'
t_VBAR  = r'\|'
t_AMPER = r'&'
t_LESS  = r'<'
t_GREATER = r'>'
t_EQUAL = r'='
t_DOT  = r'\.'
t_PERCENT = r'%'
t_BACKQUOTE  = r'`'
t_CIRCUMFLEX = r'\^'
t_TILDE = r'~'
t_AT = r'@'
t_ignore = r' '


def t_FLOAT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INT(t):
    r'\d+'
    t.value = int(t.value)
    return t


def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = 'NAME'
    return t


def t_error(t):
    print("Illegal characters!")
    t.lexer.skip(1)


lexer = lex.lex()

precedence = (
    ('left', 'PLUS', 'MINUS'),
    ('left', 'MULTIPLY', 'DIVIDE')
)


# lexer.input("Javier")
#
# while True:
#     tok = lexer.token()
#     if not tok:
#         break
#     print(tok)

def p_calc(p):
    '''

    calc : expression
    | var_assign
          | empty

    '''

    print(run(p[1]))


def p_var_assign(p):
    '''
    var_assign : NAME EQUALS expression


    '''

    p[0] = ('=', p[1], p[3])


def p_expression(p):
    '''

    expression : expression MULTIPLY expression
    | expression DIVIDE expression
    | expression PLUS expression
    | expression MINUS expression
    '''

    p[0] = (p[2], p[1], p[3])


def p_expression_int_float(p):
    '''
    expression : INT
              | FLOAT


    '''

    p[0] = p[1]


def p_expression_var(p):
    '''
    expression : NAME


    '''

    p[0] = ('var', p[1])


def p_error(p):
    print("Syntax error found!")


def p_empty(p):
    '''

    empty :

    '''

    p[0] = None


parser = yacc.yacc()

env = {}


def run(p):
    global env
    if type(p) == tuple:
        if p[0] == '+':
            return run(p[1]) + run(p[2])
        elif p[0] == '-':
            return run(p[1]) - run(p[2])
        elif p[0] == '*':
            return run(p[1]) * run(p[2])
        elif p[0] == '/':
            return run(p[1]) / run(p[2])
        elif p[0] == '=':
            env[p[1]] = run(p[2])
            print(env)
        elif p[0] == 'var':
            if p[1] not in env:
                return 'Undeclared variable found!'
            else:
                return env[p[1]]
    else:
        return p


while True:
    try:
        s = input('>>')
    except EOFError:
        break
    parser.parse(s)
