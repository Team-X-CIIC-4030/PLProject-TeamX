import ply.yacc as yacc
import lexer  # our lexer

tokens = lexer.tokens
import sys
from tac import *
from symbolTable import *

programLineOffset = 0
haltExecution = False


# file_input: (NEWLINE | stmt)* ENDMARKER
def p_file_input(p):
    """file_input :	single_stmt ENDMARKER
    """
    p[0] = p[1]
    emit(getCurrentScope(), '', '', -1, 'HALT')
    addAttributeToCurrentScope('numParam', 0)
    removeCurrentScope()
    printCode()
    printSymbolTableHistory()


# Our temporary symbol
def p_single_stmt(p):
    """single_stmt	:	single_stmt NEWLINE
                    |
    """
    if len(p) == 3:
        p[0] = p[1]
    else:
        p[0] = []


def p_single_stmt1(p):
    """single_stmt	:	single_stmt stmt
    """
    p[0] = p[1] + [p[2]]


# funcdef: [decorators] 'def' NAME parameters ':' suite
def p_funcdef(p):
    """funcdef : DEF NAME MarkerScope parameters MarkerArg COLON suite
    """
    noop(getCurrentScope(), p[7]['beginlist'])
    noop(getCurrentScope(), p[7]['endlist'])
    emit(getCurrentScope(), '', '', '', 'JUMP_RETURN')
    removeCurrentScope()
    p[0] = dict()
    p[0]['type'] = 'FUNCTION'
    p[0]['name'] = p[3]['name']


def p_MarkerScope(p):
    """MarkerScope 	:
    """
    p[0] = dict()
    p[0]['name'] = p[-1]
    if existsInCurrentScope(p[0]['name']):
        error('Redefinition', p[0]['name'])
    else:
        addIdentifier(p[0]['name'], 'FUNCTION')
        place = getNewTempVar()
        addAttribute(p[0]['name'], getCurrentScope(), place)
        addAttribute(p[0]['name'], 'name', p[0]['name'])
        # emit(getCurrentScope(), place, p[0]['name'], '', 'REF')
        addScope(p[0]['name'])
        createNewFucntionCode(p[0]['name'])


def p_MarkerArg(p):
    """MarkerArg 	:
    """
    for arg in p[-1]:
        if existsInCurrentScope(arg):
            error('Redefinition', arg)
        else:
            addIdentifier(arg, 'UNDEFINED')
            place = getNewTempVar()
            addAttribute(arg, getCurrentScope(), place)
    addAttributeToCurrentScope('numParam', len(p[-1]))


# parameters: '(' [varargslist] ')'
def p_parameters(p):
    """parameters 	: LPAREN RPAREN
                    | LPAREN varargslist RPAREN"""
    if len(p) == 3:
        p[0] = []
    else:
        p[0] = p[2]


def p_function_call(p):
    """function_call 	: NAME LPAREN RPAREN
                        | NAME LPAREN testlist RPAREN
    """
    p[0] = dict()
    place = ''
    if not exists(p[1]):
        error('Referencei', p[1])
    else:
        identifierType = getAttribute(p[1], 'type')
        if identifierType == 'FUNCTION':
            if len(p) == 4:
                pass
            else:
                for param in p[3]:
                    emit(getCurrentScope(), param['place'], '', '', 'PARAM')

            emit(getCurrentScope(), '', '', p[1], 'JUMPLABEL')
            # fname = getAttribute(p[1], 'name')
            fname = p[1]
            # print fname
            p[0]['type'] = getAttributeFromFunctionList(fname, 'returnType')
            returnPlace = getNewTempVar()
            emit(getCurrentScope(), returnPlace, '', '', 'FUNCTION_RETURN')
            p[0]['place'] = returnPlace
        else:
            error('Referencej', p[1])


# p[0]['type'] = 'UNDEFINED'
# varargslist: fpdef ['=' test] (',' fpdef ['=' test])*
def p_varargslist(p):
    """varargslist 	: fpdef
                    | fpdef EQUAL test
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        pass


def p_varargslistext(p):
    """varargslist 	: fpdef COMMA varargslist
                    | fpdef EQUAL test COMMA varargslist
    """
    if len(p) == 4:
        p[0] = [p[1]] + p[3]
    else:
        pass


# fpdef: NAME | '(' fplist ')'
def p_fpdef(p):
    """fpdef 	: NAME
                | LPAREN fplist RPAREN
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        pass


# fplist: fpdef (',' fpdef)* [',']
def p_fplist(p):
    """fplist 	: fpdef
                | fpdef COMMA fplist
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        pass


# stmt: simple_stmt | compound_stmt
def p_stmt(p):
    """stmt 	: simple_stmt
                | compound_stmt
    """
    p[0] = p[1]



def p_simple_stmt(p):
    """simple_stmt 	: small_stmts NEWLINE
    """
    p[0] = p[1]


def p_small_stmts(p):
    """small_stmts 	: small_stmt
    """
    p[0] = p[1]



def p_small_stmt(p):
    """small_stmt 	: flow_stmt Marker
                    | expr_stmt Marker
                    | print_stmt Marker
    """
    p[0] = p[1]
    backpatch(getCurrentScope(), p[1].get('nextlist', []), p[2]['quad'])


def p_expr_stmt(p):
    """expr_stmt 	: test EQUAL test
                    | test EQUAL function_call
    """
    p[0] = dict()
    place = ''
    try:
        p[1]['isArray']
        try:
            # a[i] = b[j]
            p[3]['isArray']
            if p[1]['type'] != p[3]['type']:
                error('Type', p)
            absAddrLeft = p[1]['absAddr']
            value = getNewTempVar()
            emit(getCurrentScope(), value, p[3]['place'], '', '=')
            emit(getCurrentScope(), absAddrLeft, value, '', 'SW')
        except:
            # a[i] = x
            if p[1]['type'] != p[3]['type']:
                error('Type', p)
            absAddr = p[1]['absAddr']
            try:
                emit(getCurrentScope(), absAddr, p[3]['place'], '', 'SW')
            except:
                error('Referencek', p)

    except:
        if haltExecution:
            sys.exit()
        try:
            # x = a[i]
            p[3]['isArray']
            value = getNewTempVar()
            emit(getCurrentScope(), value, p[3]['absAddr'], '', 'LW')

            if exists(p[1]['name']):
                addAttribute(p[1]['name'], 'type', p[3]['type'])
                if existsInCurrentScope(p[1]['name']):
                    place = getAttribute(p[1]['name'], getCurrentScope())
                else:
                    place = getNewTempVar()
                    addAttribute(p[1]['name'], getCurrentScope(), place)
            else:
                addIdentifier(p[1]['name'], p[3]['type'])
                place = getNewTempVar()
                addAttribute(p[1]['name'], getCurrentScope(), place)
            p[0]['nextlist'] = []
            emit(getCurrentScope(), place, value, '', '=')


        except:
            # x = y
            isList = False
            try:
                p[3]['isList']
                p[1]['isList'] = True
                isList = True
            except:
                pass

            if exists(p[1]['name']):
                addAttribute(p[1]['name'], 'type', p[3]['type'])
                if existsInCurrentScope(p[1]['name']):
                    place = getAttribute(p[1]['name'], getCurrentScope())
                else:
                    place = getNewTempVar()
                    addAttribute(p[1]['name'], getCurrentScope(), place)
            else:
                addIdentifier(p[1]['name'], p[3]['type'])
                place = getNewTempVar()
                addAttribute(p[1]['name'], getCurrentScope(), place)

            p[0]['nextlist'] = []
            try:
                if isList:
                    emit(getCurrentScope(), place, p[3]['name'], 'ARRAY', '=')
                else:
                    emit(getCurrentScope(), place, p[3]['place'], '', '=')
            except:
                error('Referencel', p)


def p_print_stmt(p):
    """print_stmt 	:	PRINT
                    |	PRINT testlist
    """
    p[0] = dict()
    if len(p) == 2:
        emit(getCurrentScope(), '"\n"', '', 'STRING', 'PRINT')
    else:
        for item in p[2]:
            itemType = item.get('type')
            if itemType not in ['STRING', 'NUMBER', 'BOOLEAN', 'UNDEFINED']:
                error('Print', p)
            emit(getCurrentScope(), item['place'], '', itemType, 'PRINT')
        emit(getCurrentScope(), '"\n"', '', 'STRING', 'PRINT')


def p_flow_stmt(p):
    """flow_stmt 	: break_stmt Marker
                    | return_stmt Marker
    """
    p[0] = p[1]
    backpatch(getCurrentScope(), p[1].get('nextlist', []), p[2]['quad'])


def p_flow_stmt2(p):
    """flow_stmt 	: continue_stmt Marker
    """
    p[0] = p[1]
    backpatch(getCurrentScope(), p[1].get('beginlist', []), p[2]['quad'])


# break_stmt: 'break'
def p_break_stmt(p):
    """break_stmt 	: BREAK
    """
    p[0] = dict()
    p[0]['endlist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), '', '', -1, 'GOTO')


# continue_stmt: 'continue'
def p_continue_stmt(p):
    """continue_stmt 	: CONTINUE
    """
    p[0] = dict()
    p[0]['beginlist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), '', '', -1, 'GOTO')


# return_stmt: 'return' [testlist]
def p_return_stmt(p):
    """return_stmt 	:	RETURN
                    |	RETURN test
    """
    p[0] = dict()
    if len(p) == 2:
        addAttributeToCurrentScope('returnType', 'UNDEFINED')
        emit(getCurrentScope(), '', '', '', 'RETURN')
    else:
        returnType = getAttributeFromCurrentScope('returnType')
        if returnType == 'UNDEFINED':
            if p[2]['type'] == 'FUNCTION':
                addAttributeToCurrentScope('returnType', 'UNDEFINED')
            else:
                addAttributeToCurrentScope('returnType', p[2]['type'])
        elif p[2]['type'] != returnType:
            error('Type', p)
        else:
            pass
        emit(getCurrentScope(), p[2]['place'], '', '', 'RETURN')


def p_compound_stmt(p):
    """compound_stmt 	: if_stmt Marker
                        | for_stmt Marker
                        | while_stmt Marker
                        | funcdef Marker
                        | function_call Marker
    """
    p[0] = p[1]
    nextlist = p[1].get('nextlist', [])
    backpatch(getCurrentScope(), nextlist, p[2]['quad'])


# if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]

def p_if_stmt(p):
    """if_stmt 	:	IF test COLON MarkerIf suite
                |	IF test COLON MarkerIf suite ELSE COLON MarkerElse suite
    """
    p[0] = dict()

    if p[2]['type'] != 'BOOLEAN':
        # print 1
        error('Type', p)

    if len(p) == 6:
        p[0]['nextlist'] = merge(p[4].get('falselist', []), p[5].get('nextlist', []))
        p[0]['beginlist'] = p[5].get('beginlist', [])
        p[0]['endlist'] = p[5].get('endlist', [])
    else:
        backpatch(getCurrentScope(), p[4]['falselist'], p[8]['quad'])
        p[0]['nextlist'] = p[8]['nextlist']
        p[0]['beginlist'] = merge(p[9].get('beginlist', []), p[5].get('beginlist', []))
        p[0]['endlist'] = merge(p[9].get('endlist', []), p[5].get('endlist', []))


# while_stmt: 'while' test ':' suite
def p_while_stmt(p):
    """while_stmt 	:	WHILE Marker test COLON MarkerWhile suite
    """
    if p[3]['type'] != 'BOOLEAN':
        error('Type', p)

    p[0] = dict()
    p[0]['type'] = 'VOID'
    p[0]['nextlist'] = []
    backpatch(getCurrentScope(), p[6]['beginlist'], p[2]['quad'])
    p[0]['nextlist'] = merge(p[6].get('endlist', []), p[6].get('nextlist', []))
    p[0]['nextlist'] = merge(p[5].get('falselist', []), p[0].get('nextlist', []))
    emit(getCurrentScope(), '', '', p[2]['quad'], 'GOTO')


def p_Marker(p):
    """Marker 		:
    """
    p[0] = dict()
    p[0]['quad'] = getNextQuad(getCurrentScope())


def p_MarkerWhile(p):
    """MarkerWhile 	:
    """
    p[0] = dict()
    p[0]['falselist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), p[-2]['place'], 0, -1, 'COND_GOTO')


def p_MarkerIf(p):
    """MarkerIf 	:
    """
    p[0] = dict()
    p[0]['falselist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), p[-2]['place'], 0, -1, 'COND_GOTO')


def p_MarkerElse(p):
    """MarkerElse 	:
    """
    p[0] = dict()
    p[0]['nextlist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), '', '', -1, 'GOTO')
    p[0]['quad'] = getNextQuad(getCurrentScope())


# for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]

def p_for_stmt(p):
    """for_stmt 	:	FOR atom IN test COLON MarkerFor suite
    """
    p[0] = dict()
    p[0]['nextlist'] = merge(p[7].get('endlist', []), p[7].get('nextlist', []))
    p[0]['nextlist'] = merge(p[6].get('falselist', []), p[0].get('nextlist', []))
    emit(getCurrentScope(), p[6]['index'], p[6]['index'], 1, '+')
    emit(getCurrentScope(), '', '', p[6]['quad'], 'GOTO')


def p_MarkerFor(p):
    """MarkerFor :
    """
    p[0] = dict()
    place = ''
    # print p[-2]
    try:
        p[-2]['isList']
    except:
        error('Not an array', p[-2])
    if exists(p[-4]['name']):
        place = p[-4]['place']
        p[-4]['type'] = p[-2]['type']
    else:
        addIdentifier(p[-4]['name'], p[-2]['type'])
        place = getNewTempVar()
        addAttribute(p[-4]['name'], getCurrentScope(), place)
    index = getNewTempVar()
    emit(getCurrentScope(), index, 0, '', '=')
    array = getNewTempVar()
    emit(getCurrentScope(), array, p[-2]['place'], 'ARRAY', '=')
    size = len(p[-2]['place'])
    length = getNewTempVar()
    emit(getCurrentScope(), length, size, '', '=')
    condition = getNewTempVar()
    p[0]['quad'] = getNextQuad(getCurrentScope())
    emit(getCurrentScope(), condition, index, length, '==')
    p[0]['falselist'] = [getNextQuad(getCurrentScope())]
    emit(getCurrentScope(), condition, 1, -1, 'COND_GOTO')

    width = getWidthFromType(p[-2]['type'])
    baseAddr = getBaseAddress(getCurrentScope(), p[-2]['name'])
    relativeAddr = getNewTempVar()
    emit(getCurrentScope(), relativeAddr, index, width, '*')
    absAddr = getNewTempVar()
    emit(getCurrentScope(), absAddr, baseAddr, relativeAddr, '+')
    emit(getCurrentScope(), place, absAddr, '', 'LW')

    # emit(getCurrentScope(), place, array+'['+index+']', '', '=')
    p[0]['index'] = index

# expr: xor_expr ('|' xor_expr)*
def p_expr(p):
    """expr 	: xor_expr
                | xor_expr VBAR expr
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[1]['type'] == p[3]['type'] == 'NUMBER':
            p[0] = dict()
            p[0]['place'] = getNewTempVar()
            p[0]['type'] = 'NUMBER'
            emit(getCurrentScope(), p[0]['place'], p[1]['place'], p[3]['place'], p[2])
        else:
            if (p[1]['type'] == 'REFERENCE_ERROR' or p[3]['type'] == 'REFERENCE_ERROR'):
                error('Reference7', p)
            error('Type', p)


# xor_expr: and_expr ('^' and_expr)*
def p_xor_expr(p):
    """xor_expr 	: and_expr
                    | and_expr CIRCUMFLEX xor_expr
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[1]['type'] == p[3]['type'] == 'NUMBER':
            p[0] = dict()
            p[0]['place'] = getNewTempVar()
            p[0]['type'] = 'NUMBER'
            emit(getCurrentScope(), p[0]['place'], p[1]['place'], p[3]['place'], p[2])
        else:
            if (p[1]['type'] == 'REFERENCE_ERROR' or p[3]['type'] == 'REFERENCE_ERROR'):
                error('Reference8', p)
            error('Type', p)


# and_expr: shift_expr ('&' shift_expr)*
def p_and_expr(p):
    """and_expr 	: shift_expr
                    | shift_expr AMPER and_expr
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[1]['type'] == p[3]['type'] == 'NUMBER':
            p[0] = dict()
            p[0]['place'] = getNewTempVar()
            p[0]['type'] = 'NUMBER'
            emit(getCurrentScope(), p[0]['place'], p[1]['place'], p[3]['place'], p[2])
        else:
            if (p[1]['type'] == 'REFERENCE_ERROR' or p[3]['type'] == 'REFERENCE_ERROR'):
                error('Reference9', p)
            error('Type', p)

# arith_expr: term (('+'|'-') term)*
def p_arith_expr(p):
    """arith_expr 	:	term
                    |	term PLUS arith_expr
                    |	term MINUS arith_expr
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[1]['type'] in ['NUMBER', 'BOOLEAN', 'UNDEFINED'] and p[3]['type'] in ['NUMBER', 'BOOLEAN', 'UNDEFINED']:
            p[0] = dict()
            p[0]['place'] = getNewTempVar()
            p[0]['type'] = 'NUMBER'
            emit(getCurrentScope(), p[0]['place'], p[1]['place'], p[3]['place'], p[2])
        else:
            if (p[1]['type'] == 'REFERENCE_ERROR' or p[3]['type'] == 'REFERENCE_ERROR'):
                error('Referenceb', p)
            error('Type', p)

# factor: ('+'|'-') factor | power
def p_factor(p):
    """factor 	: power
                | PLUS factor
                | MINUS factor
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        if p[2]['type'] != 'NUMBER':
            error('Type', p)
        p[0] = dict()
        p[0]['place'] = getNewTempVar()
        p[0]['type'] = 'NUMBER'
        emit(getCurrentScope(), p[0]['place'], 0, p[2]['place'], '-')



# listmaker: test (',' test)* [',']
def p_listmaker(p):
    """listmaker 	: test
                    | test COMMA listmaker
    """
    p[0] = dict()
    if len(p) == 2:
        try:
            p[0]['place'] = [p[1]['place']]
            p[0]['type'] = p[1]['type']
        except:
            error('Referenceg', p)
    else:
        if p[3]['type'] != p[1]['type']:
            error('Type', p)
        else:
            try:
                p[0]['place'] = [p[1]['place']] + p[3]['place']
                p[0]['type'] = p[1]['type']
            except:
                error('Referenceh', p)


def p_stmts(p):
    """stmts 	: stmt stmts
                | stmt Marker"""
    p[0] = dict()
    p[0]['beginlist'] = merge(p[1].get('beginlist', []), p[2].get('beginlist', []))
    p[0]['endlist'] = merge(p[1].get('endlist', []), p[2].get('endlist', []))


def p_error(p):
    global haltExecution
    haltExecution = True
    try:
        print
        "Syntax Error near '" + str(p.value) + "' in line " + str(p.lineno - programLineOffset)
    except:
        try:
            print
            "Syntax Error in line " + str(p.lineno - programLineOffset)
        except:
            print
            "Syntax Error"
    sys.exit()


def error(errorType, p):
    global haltExecution
    haltExecution = True
    try:
        print
        (errorType + " Error near '" + str(p.value) + "' in line " + str(p.lineno - programLineOffset))
    except:
        try:
            print
            (errorType + " Error in line " + str(p.lineno - programLineOffset))
        except:
            print
            (errorType + " Error")
    sys.exit()


class G1Parser(object):
    def __init__(self, mlexer=None):
        if mlexer is None:
            mlexer = lexer.G1Lexer()
        self.mlexer = mlexer
        self.parser = yacc.yacc(start="file_input", debug=True)

    def parse(self, code):
        self.mlexer.input(code)
        result = self.parser.parse(lexer=self.mlexer, debug=True)
        return result


def initializeTF():
    scopeName = getCurrentScope()
    addIdentifier('True', 'BOOLEAN')
    addAttribute('True', scopeName, 1)
    addIdentifier('False', 'BOOLEAN')
    addAttribute('False', scopeName, 0)


if __name__ == "__main__":
    initializeTF()
    z = G1Parser()
    filename = sys.argv[1]
    # filename = "../test/for.py"
    sourcefile = open(filename)
    data = sourcefile.read()
    sys.stderr = open('dump', 'w')
    root = z.parse(data)
    sys.stderr.close()
