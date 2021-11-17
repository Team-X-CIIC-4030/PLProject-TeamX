
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'file_inputAMPER AND AS ASSERT AT BACKQUOTE BINARYNUMBER BREAK CIRCUMFLEX CLASS COLON COMMA CONTINUE DEDENT DEF DEL DOT ELIF ELSE ENDMARKER EQEQUAL EQUAL EXCEPT EXEC FINALLY FNUMBER FOR FROM GLOBAL GREATER GREATEREQUAL HEXADECIMALNUMBER IF IMPORT IN INDENT INUMBER IS LAMBDA LBRACE LEFTSHIFT LESS LESSEQUAL LPAREN LSQB MINEQUAL MINUS NAME NEWLINE NOT NOTEQUAL NUMBER OCTALNUMBER OR PASS PERCENT PERCENTEQUAL PLUS PLUSEQUAL PRINT RAISE RAWSTRING RBRACE RETURN RIGHTSHIFT RPAREN RSQB SEMI SLASH SLASHEQUAL SLASHSLASH SLASHSLASHEQUAL STAR STAREQUAL STARSTAR STARSTAREQUAL STRING TILDE TRIPLESTRING TRY UNICODESTRING VBAR WHILE WITH WS YIELDfile_input :\tsingle_stmt ENDMARKER\n    single_stmt\t:\tsingle_stmt NEWLINE\n                    |\n    single_stmt\t:\tsingle_stmt stmt\n    funcdef : DEF NAME MarkerScope parameters MarkerArg COLON suite\n    MarkerScope \t:\n    MarkerArg \t:\n    parameters \t: LPAREN RPAREN\n                    | LPAREN varargslist RPARENfunction_call \t: NAME LPAREN RPAREN\n                        | NAME LPAREN testlist RPAREN\n    varargslist \t: fpdef\n                    | fpdef EQUAL test\n    varargslist \t: fpdef COMMA varargslist\n                    | fpdef EQUAL test COMMA varargslist\n    fpdef \t: NAME\n                | LPAREN fplist RPAREN\n    fplist \t: fpdef\n                | fpdef COMMA fplist\n    stmt \t: simple_stmt\n                | compound_stmt\n    simple_stmt \t: small_stmts NEWLINE\n    small_stmts \t: small_stmt\n    small_stmt \t: flow_stmt Marker\n                    | expr_stmt Marker\n                    | print_stmt Marker\n    expr_stmt \t: test EQUAL test\n                    | test EQUAL function_call\n    print_stmt \t:\tPRINT\n                    |\tPRINT testlist\n    flow_stmt \t: break_stmt Marker\n                    | return_stmt Marker\n    flow_stmt \t: continue_stmt Marker\n    break_stmt \t: BREAK\n    continue_stmt \t: CONTINUE\n    return_stmt \t:\tRETURN\n                    |\tRETURN test\n    compound_stmt \t: if_stmt Marker\n                        | for_stmt Marker\n                        | while_stmt Marker\n                        | funcdef Marker\n                        | function_call Marker\n    if_stmt \t:\tIF test COLON MarkerIf suite\n                |\tIF test COLON MarkerIf suite ELSE COLON MarkerElse suite\n    while_stmt \t:\tWHILE Marker test COLON MarkerWhile suite\n    Marker \t\t:\n    MarkerWhile \t:\n    MarkerIf \t:\n    MarkerElse \t:\n    for_stmt \t:\tFOR atom IN test COLON MarkerFor suite\n    MarkerFor :\n    suite \t: simple_stmt\n                | NEWLINE INDENT stmts DEDENTtest \t: test_expr\n    test_expr \t: or_test\n    or_test \t: and_test\n                | and_test OR or_test\n    and_test \t: not_test\n                    | not_test AND and_test\n    not_test \t: NOT not_test\n                    | comparison\n    comparison \t: \texpr\n                    |\texpr comp_op expr\n    comp_op \t: LESS\n                | GREATER\n                | EQEQUAL\n                | GREATEREQUAL\n                | LESSEQUAL\n                | NOTEQUAL\n    expr \t: xor_expr\n                | xor_expr VBAR expr\n    xor_expr \t: and_expr\n                    | and_expr CIRCUMFLEX xor_expr\n    and_expr \t: shift_expr\n                    | shift_expr AMPER and_expr\n    shift_expr \t: arith_expr\n                    | arith_expr LEFTSHIFT shift_expr\n                    | arith_expr RIGHTSHIFT shift_expr\n    arith_expr \t:\tterm\n                    |\tterm PLUS arith_expr\n                    |\tterm MINUS arith_expr\n    term :\tfactor\n            |\tfactor STAR term\n            |\tfactor SLASH term\n            |\tfactor PERCENT term\n    factor \t: power\n                | PLUS factor\n                | MINUS factor\n    power \t: atom\n                | atom LSQB test RSQB\n    atom :\tNAME\n    atom :\tNUMBER\n    atom\t\t:\tSTRING\n                |\tTRIPLESTRING\n    atom :\tFNUMBER\n    atom :\tLSQB RSQB\n            | \tLSQB listmaker RSQB\n    atom\t:\tLPAREN RPAREN\n            | \tLPAREN testlist_comp RPAREN\n    listmaker \t: test\n                    | test COMMA listmaker\n    testlist_comp \t: test\n                        | test COMMA testlist_comp\n    testlist \t: test\n                    | test COMMA testlist\n    stmts \t: stmt stmts\n                | stmt Marker'
    
_lr_action_items = {'ENDMARKER':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,156,166,167,173,178,],[-3,3,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,-45,-50,-5,-53,-44,]),'NEWLINE':([0,2,4,5,6,7,8,9,10,11,12,13,14,18,21,23,24,25,26,27,28,29,30,31,32,33,34,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,54,55,56,57,58,59,61,68,71,72,73,74,75,76,77,78,79,82,97,98,102,105,106,107,112,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,132,134,135,138,140,142,143,145,146,155,156,157,163,166,167,172,173,178,],[-3,4,-2,-4,-20,-21,54,-46,-46,-46,-46,-46,-23,-89,-91,-46,-46,-46,-46,-46,-46,-29,-34,-36,-35,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-22,-38,-39,-40,-41,-42,-91,-98,-24,-25,-26,-31,-32,-33,-30,-104,-37,-60,-87,-88,-96,-48,-27,-28,-10,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,144,-90,-47,-11,-105,-43,-52,-51,144,144,-45,144,-49,-50,-5,144,-53,-44,]),'IF':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,154,156,165,166,167,173,178,],[-3,15,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,15,-45,15,-50,-5,-53,-44,]),'FOR':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,154,156,165,166,167,173,178,],[-3,17,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,17,-45,17,-50,-5,-53,-44,]),'WHILE':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,154,156,165,166,167,173,178,],[-3,19,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,19,-45,19,-50,-5,-53,-44,]),'DEF':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,154,156,165,166,167,173,178,],[-3,20,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,20,-45,20,-50,-5,-53,-44,]),'NAME':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,20,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,112,115,116,131,132,135,137,138,142,143,145,146,148,154,155,156,157,161,162,163,165,166,167,169,172,173,177,178,],[-3,21,-2,-4,-20,-21,-46,-46,-46,-46,-46,61,61,-46,66,61,61,61,61,61,61,61,-22,-38,-39,-40,-41,-42,21,61,61,61,61,61,61,-64,-65,-66,-67,-68,-69,61,61,61,61,61,61,61,61,61,61,-48,61,-10,61,61,61,61,-47,152,-11,-43,-52,-51,61,152,21,61,-45,61,61,152,-49,21,-50,-5,152,61,-53,152,-44,]),'PRINT':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,105,112,132,135,138,142,143,145,146,154,155,156,157,163,165,166,167,172,173,178,],[-3,29,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-48,-10,29,-47,-11,-43,-52,-51,29,29,29,-45,29,-49,29,-50,-5,29,-53,-44,]),'BREAK':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,105,112,132,135,138,142,143,145,146,154,155,156,157,163,165,166,167,172,173,178,],[-3,30,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-48,-10,30,-47,-11,-43,-52,-51,30,30,30,-45,30,-49,30,-50,-5,30,-53,-44,]),'RETURN':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,105,112,132,135,138,142,143,145,146,154,155,156,157,163,165,166,167,172,173,178,],[-3,31,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-48,-10,31,-47,-11,-43,-52,-51,31,31,31,-45,31,-49,31,-50,-5,31,-53,-44,]),'CONTINUE':([0,2,4,5,6,7,9,10,11,12,13,54,55,56,57,58,59,105,112,132,135,138,142,143,145,146,154,155,156,157,163,165,166,167,172,173,178,],[-3,32,-2,-4,-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-48,-10,32,-47,-11,-43,-52,-51,32,32,32,-45,32,-49,32,-50,-5,32,-53,-44,]),'NOT':([0,2,4,5,6,7,9,10,11,12,13,15,19,22,29,31,37,49,54,55,56,57,58,59,62,64,65,67,80,81,105,108,112,115,116,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,37,-2,-4,-20,-21,-46,-46,-46,-46,-46,37,-46,37,37,37,37,37,-22,-38,-39,-40,-41,-42,37,37,37,37,37,37,-48,37,-10,37,37,37,37,-47,-11,-43,-52,-51,37,37,37,-45,37,37,-49,37,-50,-5,37,-53,-44,]),'PLUS':([0,2,4,5,6,7,9,10,11,12,13,15,18,19,21,22,29,31,37,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,61,62,64,65,67,68,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,105,108,112,114,115,116,127,128,129,130,131,132,134,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,45,-2,-4,-20,-21,-46,-46,-46,-46,-46,45,-89,-46,-91,45,45,45,45,95,45,45,-82,-86,45,-92,-93,-94,-95,-22,-38,-39,-40,-41,-42,-91,45,45,45,45,-98,45,45,45,-64,-65,-66,-67,-68,-69,45,45,45,45,45,45,45,-87,-88,45,45,45,-96,-48,45,-10,-99,45,45,-83,-84,-85,-97,45,45,-90,-47,-11,-43,-52,-51,45,45,45,-45,45,45,-49,45,-50,-5,45,-53,-44,]),'MINUS':([0,2,4,5,6,7,9,10,11,12,13,15,18,19,21,22,29,31,37,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,61,62,64,65,67,68,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,105,108,112,114,115,116,127,128,129,130,131,132,134,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,46,-2,-4,-20,-21,-46,-46,-46,-46,-46,46,-89,-46,-91,46,46,46,46,96,46,46,-82,-86,46,-92,-93,-94,-95,-22,-38,-39,-40,-41,-42,-91,46,46,46,46,-98,46,46,46,-64,-65,-66,-67,-68,-69,46,46,46,46,46,46,46,-87,-88,46,46,46,-96,-48,46,-10,-99,46,46,-83,-84,-85,-97,46,46,-90,-47,-11,-43,-52,-51,46,46,46,-45,46,46,-49,46,-50,-5,46,-53,-44,]),'NUMBER':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,112,115,116,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,50,-2,-4,-20,-21,-46,-46,-46,-46,-46,50,50,-46,50,50,50,50,50,50,50,-22,-38,-39,-40,-41,-42,50,50,50,50,50,50,50,-64,-65,-66,-67,-68,-69,50,50,50,50,50,50,50,50,50,50,-48,50,-10,50,50,50,50,-47,-11,-43,-52,-51,50,50,50,-45,50,50,-49,50,-50,-5,50,-53,-44,]),'STRING':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,112,115,116,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,51,-2,-4,-20,-21,-46,-46,-46,-46,-46,51,51,-46,51,51,51,51,51,51,51,-22,-38,-39,-40,-41,-42,51,51,51,51,51,51,51,-64,-65,-66,-67,-68,-69,51,51,51,51,51,51,51,51,51,51,-48,51,-10,51,51,51,51,-47,-11,-43,-52,-51,51,51,51,-45,51,51,-49,51,-50,-5,51,-53,-44,]),'TRIPLESTRING':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,112,115,116,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,52,-2,-4,-20,-21,-46,-46,-46,-46,-46,52,52,-46,52,52,52,52,52,52,52,-22,-38,-39,-40,-41,-42,52,52,52,52,52,52,52,-64,-65,-66,-67,-68,-69,52,52,52,52,52,52,52,52,52,52,-48,52,-10,52,52,52,52,-47,-11,-43,-52,-51,52,52,52,-45,52,52,-49,52,-50,-5,52,-53,-44,]),'FNUMBER':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,112,115,116,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,53,-2,-4,-20,-21,-46,-46,-46,-46,-46,53,53,-46,53,53,53,53,53,53,53,-22,-38,-39,-40,-41,-42,53,53,53,53,53,53,53,-64,-65,-66,-67,-68,-69,53,53,53,53,53,53,53,53,53,53,-48,53,-10,53,53,53,53,-47,-11,-43,-52,-51,53,53,53,-45,53,53,-49,53,-50,-5,53,-53,-44,]),'LSQB':([0,2,4,5,6,7,9,10,11,12,13,15,17,18,19,21,22,29,31,37,45,46,49,50,51,52,53,54,55,56,57,58,59,61,62,64,65,67,68,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,102,105,108,112,114,115,116,130,131,132,135,138,142,143,145,146,154,155,156,157,161,163,165,166,167,172,173,178,],[-3,49,-2,-4,-20,-21,-46,-46,-46,-46,-46,49,49,64,-46,-91,49,49,49,49,49,49,49,-92,-93,-94,-95,-22,-38,-39,-40,-41,-42,-91,49,49,49,49,-98,49,49,49,-64,-65,-66,-67,-68,-69,49,49,49,49,49,49,49,49,49,49,-96,-48,49,-10,-99,49,49,-97,49,49,-47,-11,-43,-52,-51,49,49,49,-45,49,49,-49,49,-50,-5,49,-53,-44,]),'LPAREN':([0,2,4,5,6,7,9,10,11,12,13,15,17,19,21,22,29,31,37,45,46,49,54,55,56,57,58,59,62,64,65,66,67,80,81,83,84,85,86,87,88,89,90,91,92,93,94,95,96,99,100,101,105,108,111,112,115,116,131,132,135,137,138,142,143,145,146,148,154,155,156,157,161,162,163,165,166,167,169,172,173,177,178,],[-3,22,-2,-4,-20,-21,-46,-46,-46,-46,-46,22,22,-46,67,22,22,22,22,22,22,22,-22,-38,-39,-40,-41,-42,22,22,22,-6,22,22,22,22,-64,-65,-66,-67,-68,-69,22,22,22,22,22,22,22,22,22,22,-48,22,137,-10,22,22,22,22,-47,148,-11,-43,-52,-51,22,148,22,22,-45,22,22,148,-49,22,-50,-5,148,22,-53,148,-44,]),'$end':([1,3,],[0,-1,]),'DEDENT':([6,7,9,10,11,12,13,54,55,56,57,58,59,112,138,142,143,156,164,165,166,167,173,174,175,178,],[-20,-21,-46,-46,-46,-46,-46,-22,-38,-39,-40,-41,-42,-10,-11,-43,-52,-45,173,-46,-50,-5,-53,-106,-107,-44,]),'EQUAL':([16,18,21,33,34,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,61,68,82,97,98,102,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,134,151,152,168,],[62,-89,-91,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-60,-87,-88,-96,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,161,-16,-17,]),'STAR':([18,21,47,48,50,51,52,53,61,68,97,98,102,114,130,134,],[-89,-91,99,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-97,-90,]),'SLASH':([18,21,47,48,50,51,52,53,61,68,97,98,102,114,130,134,],[-89,-91,100,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-97,-90,]),'PERCENT':([18,21,47,48,50,51,52,53,61,68,97,98,102,114,130,134,],[-89,-91,101,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-97,-90,]),'LEFTSHIFT':([18,21,43,44,47,48,50,51,52,53,61,68,97,98,102,114,125,126,127,128,129,130,134,],[-89,-91,93,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-80,-81,-83,-84,-85,-97,-90,]),'RIGHTSHIFT':([18,21,43,44,47,48,50,51,52,53,61,68,97,98,102,114,125,126,127,128,129,130,134,],[-89,-91,94,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-80,-81,-83,-84,-85,-97,-90,]),'AMPER':([18,21,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,123,124,125,126,127,128,129,130,134,],[-89,-91,92,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'CIRCUMFLEX':([18,21,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,122,123,124,125,126,127,128,129,130,134,],[-89,-91,91,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'VBAR':([18,21,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,90,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'LESS':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,84,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'GREATER':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,85,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'EQEQUAL':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,86,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'GREATEREQUAL':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,87,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'LESSEQUAL':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,88,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'NOTEQUAL':([18,21,39,40,41,42,43,44,47,48,50,51,52,53,61,68,97,98,102,114,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,89,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-87,-88,-96,-99,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'AND':([18,21,36,38,39,40,41,42,43,44,47,48,50,51,52,53,61,68,82,97,98,102,114,119,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,81,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-60,-87,-88,-96,-99,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'OR':([18,21,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,61,68,82,97,98,102,114,118,119,120,121,122,123,124,125,126,127,128,129,130,134,],[-89,-91,80,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,-60,-87,-88,-96,-99,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,]),'COLON':([18,33,34,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,60,61,68,82,97,98,102,110,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,133,134,136,147,149,153,160,],[-89,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,105,-91,-98,-60,-87,-88,-96,135,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,145,-90,-7,157,-8,163,-9,]),'COMMA':([18,33,34,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,61,68,70,78,82,97,98,102,104,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,134,151,152,159,168,170,],[-89,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,-98,115,116,-60,-87,-88,-96,131,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,162,-16,169,-17,177,]),'RPAREN':([18,22,33,34,35,36,38,39,40,41,42,43,44,47,48,50,51,52,53,61,67,68,69,70,78,82,97,98,102,113,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,134,137,139,140,150,151,152,158,159,168,170,171,176,179,],[-89,68,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,-92,-93,-94,-95,-91,112,-98,114,-102,-104,-60,-87,-88,-96,138,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,149,-103,-105,160,-12,-16,168,-18,-17,-13,-14,-19,-15,]),'RSQB':([18,33,34,35,36,38,39,40,41,42,43,44,47,48,49,50,51,52,53,61,68,82,97,98,102,103,104,109,114,117,118,119,120,121,122,123,124,125,126,127,128,129,130,134,141,],[-89,-54,-55,-56,-58,-61,-62,-70,-72,-74,-76,-79,-82,-86,102,-92,-93,-94,-95,-91,-98,-60,-87,-88,-96,130,-100,134,-99,-57,-59,-63,-71,-73,-75,-77,-78,-80,-81,-83,-84,-85,-97,-90,-101,]),'IN':([50,51,52,53,61,63,68,102,114,130,],[-92,-93,-94,-95,-91,108,-98,-96,-99,-97,]),'ELSE':([54,142,143,173,],[-22,153,-52,-53,]),'INDENT':([144,],[154,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'file_input':([0,],[1,]),'single_stmt':([0,],[2,]),'stmt':([2,154,165,],[5,165,165,]),'simple_stmt':([2,132,146,154,155,157,165,172,],[6,143,143,6,143,143,6,143,]),'compound_stmt':([2,154,165,],[7,7,7,]),'small_stmts':([2,132,146,154,155,157,165,172,],[8,8,8,8,8,8,8,8,]),'if_stmt':([2,154,165,],[9,9,9,]),'for_stmt':([2,154,165,],[10,10,10,]),'while_stmt':([2,154,165,],[11,11,11,]),'funcdef':([2,154,165,],[12,12,12,]),'function_call':([2,62,154,165,],[13,107,13,13,]),'small_stmt':([2,132,146,154,155,157,165,172,],[14,14,14,14,14,14,14,14,]),'test':([2,15,22,29,31,49,62,64,65,67,108,115,116,131,132,146,154,155,157,161,165,172,],[16,60,70,78,79,104,106,109,110,78,133,70,78,104,16,16,16,16,16,170,16,16,]),'atom':([2,15,17,22,29,31,37,45,46,49,62,64,65,67,80,81,83,90,91,92,93,94,95,96,99,100,101,108,115,116,131,132,146,154,155,157,161,165,172,],[18,18,63,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,18,]),'flow_stmt':([2,132,146,154,155,157,165,172,],[23,23,23,23,23,23,23,23,]),'expr_stmt':([2,132,146,154,155,157,165,172,],[24,24,24,24,24,24,24,24,]),'print_stmt':([2,132,146,154,155,157,165,172,],[25,25,25,25,25,25,25,25,]),'break_stmt':([2,132,146,154,155,157,165,172,],[26,26,26,26,26,26,26,26,]),'return_stmt':([2,132,146,154,155,157,165,172,],[27,27,27,27,27,27,27,27,]),'continue_stmt':([2,132,146,154,155,157,165,172,],[28,28,28,28,28,28,28,28,]),'test_expr':([2,15,22,29,31,49,62,64,65,67,108,115,116,131,132,146,154,155,157,161,165,172,],[33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,33,]),'or_test':([2,15,22,29,31,49,62,64,65,67,80,108,115,116,131,132,146,154,155,157,161,165,172,],[34,34,34,34,34,34,34,34,34,34,117,34,34,34,34,34,34,34,34,34,34,34,34,]),'and_test':([2,15,22,29,31,49,62,64,65,67,80,81,108,115,116,131,132,146,154,155,157,161,165,172,],[35,35,35,35,35,35,35,35,35,35,35,118,35,35,35,35,35,35,35,35,35,35,35,35,]),'not_test':([2,15,22,29,31,37,49,62,64,65,67,80,81,108,115,116,131,132,146,154,155,157,161,165,172,],[36,36,36,36,36,82,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,36,]),'comparison':([2,15,22,29,31,37,49,62,64,65,67,80,81,108,115,116,131,132,146,154,155,157,161,165,172,],[38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,38,]),'expr':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,108,115,116,131,132,146,154,155,157,161,165,172,],[39,39,39,39,39,39,39,39,39,39,39,39,39,119,120,39,39,39,39,39,39,39,39,39,39,39,39,]),'xor_expr':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,91,108,115,116,131,132,146,154,155,157,161,165,172,],[40,40,40,40,40,40,40,40,40,40,40,40,40,40,40,121,40,40,40,40,40,40,40,40,40,40,40,40,]),'and_expr':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,91,92,108,115,116,131,132,146,154,155,157,161,165,172,],[41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,41,122,41,41,41,41,41,41,41,41,41,41,41,41,]),'shift_expr':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,91,92,93,94,108,115,116,131,132,146,154,155,157,161,165,172,],[42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,42,123,124,42,42,42,42,42,42,42,42,42,42,42,42,]),'arith_expr':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,91,92,93,94,95,96,108,115,116,131,132,146,154,155,157,161,165,172,],[43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,43,125,126,43,43,43,43,43,43,43,43,43,43,43,43,]),'term':([2,15,22,29,31,37,49,62,64,65,67,80,81,83,90,91,92,93,94,95,96,99,100,101,108,115,116,131,132,146,154,155,157,161,165,172,],[44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,44,127,128,129,44,44,44,44,44,44,44,44,44,44,44,44,]),'factor':([2,15,22,29,31,37,45,46,49,62,64,65,67,80,81,83,90,91,92,93,94,95,96,99,100,101,108,115,116,131,132,146,154,155,157,161,165,172,],[47,47,47,47,47,47,97,98,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,47,]),'power':([2,15,22,29,31,37,45,46,49,62,64,65,67,80,81,83,90,91,92,93,94,95,96,99,100,101,108,115,116,131,132,146,154,155,157,161,165,172,],[48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,48,]),'Marker':([9,10,11,12,13,19,23,24,25,26,27,28,165,],[55,56,57,58,59,65,71,72,73,74,75,76,175,]),'testlist_comp':([22,115,],[69,139,]),'testlist':([29,67,116,],[77,113,140,]),'comp_op':([39,],[83,]),'listmaker':([49,131,],[103,141,]),'MarkerScope':([66,],[111,]),'MarkerIf':([105,],[132,]),'parameters':([111,],[136,]),'suite':([132,146,155,157,172,],[142,156,166,167,178,]),'MarkerWhile':([135,],[146,]),'MarkerArg':([136,],[147,]),'varargslist':([137,162,177,],[150,171,179,]),'fpdef':([137,148,162,169,177,],[151,159,151,159,151,]),'MarkerFor':([145,],[155,]),'fplist':([148,169,],[158,176,]),'stmts':([154,165,],[164,174,]),'MarkerElse':([163,],[172,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> file_input","S'",1,None,None,None),
  ('file_input -> single_stmt ENDMARKER','file_input',2,'p_file_input','parser.py',16),
  ('single_stmt -> single_stmt NEWLINE','single_stmt',2,'p_single_stmt','parser.py',28),
  ('single_stmt -> <empty>','single_stmt',0,'p_single_stmt','parser.py',29),
  ('single_stmt -> single_stmt stmt','single_stmt',2,'p_single_stmt1','parser.py',38),
  ('funcdef -> DEF NAME MarkerScope parameters MarkerArg COLON suite','funcdef',7,'p_funcdef','parser.py',45),
  ('MarkerScope -> <empty>','MarkerScope',0,'p_MarkerScope','parser.py',57),
  ('MarkerArg -> <empty>','MarkerArg',0,'p_MarkerArg','parser.py',74),
  ('parameters -> LPAREN RPAREN','parameters',2,'p_parameters','parser.py',88),
  ('parameters -> LPAREN varargslist RPAREN','parameters',3,'p_parameters','parser.py',89),
  ('function_call -> NAME LPAREN RPAREN','function_call',3,'p_function_call','parser.py',97),
  ('function_call -> NAME LPAREN testlist RPAREN','function_call',4,'p_function_call','parser.py',98),
  ('varargslist -> fpdef','varargslist',1,'p_varargslist','parser.py',128),
  ('varargslist -> fpdef EQUAL test','varargslist',3,'p_varargslist','parser.py',129),
  ('varargslist -> fpdef COMMA varargslist','varargslist',3,'p_varargslistext','parser.py',138),
  ('varargslist -> fpdef EQUAL test COMMA varargslist','varargslist',5,'p_varargslistext','parser.py',139),
  ('fpdef -> NAME','fpdef',1,'p_fpdef','parser.py',149),
  ('fpdef -> LPAREN fplist RPAREN','fpdef',3,'p_fpdef','parser.py',150),
  ('fplist -> fpdef','fplist',1,'p_fplist','parser.py',160),
  ('fplist -> fpdef COMMA fplist','fplist',3,'p_fplist','parser.py',161),
  ('stmt -> simple_stmt','stmt',1,'p_stmt','parser.py',171),
  ('stmt -> compound_stmt','stmt',1,'p_stmt','parser.py',172),
  ('simple_stmt -> small_stmts NEWLINE','simple_stmt',2,'p_simple_stmt','parser.py',180),
  ('small_stmts -> small_stmt','small_stmts',1,'p_small_stmts','parser.py',186),
  ('small_stmt -> flow_stmt Marker','small_stmt',2,'p_small_stmt','parser.py',196),
  ('small_stmt -> expr_stmt Marker','small_stmt',2,'p_small_stmt','parser.py',197),
  ('small_stmt -> print_stmt Marker','small_stmt',2,'p_small_stmt','parser.py',198),
  ('expr_stmt -> test EQUAL test','expr_stmt',3,'p_expr_stmt','parser.py',213),
  ('expr_stmt -> test EQUAL function_call','expr_stmt',3,'p_expr_stmt','parser.py',214),
  ('print_stmt -> PRINT','print_stmt',1,'p_print_stmt','parser.py',337),
  ('print_stmt -> PRINT testlist','print_stmt',2,'p_print_stmt','parser.py',338),
  ('flow_stmt -> break_stmt Marker','flow_stmt',2,'p_flow_stmt','parser.py',358),
  ('flow_stmt -> return_stmt Marker','flow_stmt',2,'p_flow_stmt','parser.py',359),
  ('flow_stmt -> continue_stmt Marker','flow_stmt',2,'p_flow_stmt2','parser.py',366),
  ('break_stmt -> BREAK','break_stmt',1,'p_break_stmt','parser.py',374),
  ('continue_stmt -> CONTINUE','continue_stmt',1,'p_continue_stmt','parser.py',383),
  ('return_stmt -> RETURN','return_stmt',1,'p_return_stmt','parser.py',392),
  ('return_stmt -> RETURN test','return_stmt',2,'p_return_stmt','parser.py',393),
  ('compound_stmt -> if_stmt Marker','compound_stmt',2,'p_compound_stmt','parser.py',437),
  ('compound_stmt -> for_stmt Marker','compound_stmt',2,'p_compound_stmt','parser.py',438),
  ('compound_stmt -> while_stmt Marker','compound_stmt',2,'p_compound_stmt','parser.py',439),
  ('compound_stmt -> funcdef Marker','compound_stmt',2,'p_compound_stmt','parser.py',440),
  ('compound_stmt -> function_call Marker','compound_stmt',2,'p_compound_stmt','parser.py',441),
  ('if_stmt -> IF test COLON MarkerIf suite','if_stmt',5,'p_if_stmt','parser.py',451),
  ('if_stmt -> IF test COLON MarkerIf suite ELSE COLON MarkerElse suite','if_stmt',9,'p_if_stmt','parser.py',452),
  ('while_stmt -> WHILE Marker test COLON MarkerWhile suite','while_stmt',6,'p_while_stmt','parser.py',473),
  ('Marker -> <empty>','Marker',0,'p_Marker','parser.py',488),
  ('MarkerWhile -> <empty>','MarkerWhile',0,'p_MarkerWhile','parser.py',495),
  ('MarkerIf -> <empty>','MarkerIf',0,'p_MarkerIf','parser.py',503),
  ('MarkerElse -> <empty>','MarkerElse',0,'p_MarkerElse','parser.py',511),
  ('for_stmt -> FOR atom IN test COLON MarkerFor suite','for_stmt',7,'p_for_stmt','parser.py',522),
  ('MarkerFor -> <empty>','MarkerFor',0,'p_MarkerFor','parser.py',532),
  ('suite -> simple_stmt','suite',1,'p_suite','parser.py',580),
  ('suite -> NEWLINE INDENT stmts DEDENT','suite',4,'p_suite','parser.py',581),
  ('test -> test_expr','test',1,'p_array','parser.py',590),
  ('test_expr -> or_test','test_expr',1,'p_test','parser.py',613),
  ('or_test -> and_test','or_test',1,'p_or_test','parser.py',620),
  ('or_test -> and_test OR or_test','or_test',3,'p_or_test','parser.py',621),
  ('and_test -> not_test','and_test',1,'p_and_test','parser.py',640),
  ('and_test -> not_test AND and_test','and_test',3,'p_and_test','parser.py',641),
  ('not_test -> NOT not_test','not_test',2,'p_not_test','parser.py',659),
  ('not_test -> comparison','not_test',1,'p_not_test','parser.py',660),
  ('comparison -> expr','comparison',1,'p_comparision','parser.py',678),
  ('comparison -> expr comp_op expr','comparison',3,'p_comparision','parser.py',679),
  ('comp_op -> LESS','comp_op',1,'p_comp_op','parser.py',701),
  ('comp_op -> GREATER','comp_op',1,'p_comp_op','parser.py',702),
  ('comp_op -> EQEQUAL','comp_op',1,'p_comp_op','parser.py',703),
  ('comp_op -> GREATEREQUAL','comp_op',1,'p_comp_op','parser.py',704),
  ('comp_op -> LESSEQUAL','comp_op',1,'p_comp_op','parser.py',705),
  ('comp_op -> NOTEQUAL','comp_op',1,'p_comp_op','parser.py',706),
  ('expr -> xor_expr','expr',1,'p_expr','parser.py',713),
  ('expr -> xor_expr VBAR expr','expr',3,'p_expr','parser.py',714),
  ('xor_expr -> and_expr','xor_expr',1,'p_xor_expr','parser.py',732),
  ('xor_expr -> and_expr CIRCUMFLEX xor_expr','xor_expr',3,'p_xor_expr','parser.py',733),
  ('and_expr -> shift_expr','and_expr',1,'p_and_expr','parser.py',751),
  ('and_expr -> shift_expr AMPER and_expr','and_expr',3,'p_and_expr','parser.py',752),
  ('shift_expr -> arith_expr','shift_expr',1,'p_shift_expr','parser.py',770),
  ('shift_expr -> arith_expr LEFTSHIFT shift_expr','shift_expr',3,'p_shift_expr','parser.py',771),
  ('shift_expr -> arith_expr RIGHTSHIFT shift_expr','shift_expr',3,'p_shift_expr','parser.py',772),
  ('arith_expr -> term','arith_expr',1,'p_arith_expr','parser.py',790),
  ('arith_expr -> term PLUS arith_expr','arith_expr',3,'p_arith_expr','parser.py',791),
  ('arith_expr -> term MINUS arith_expr','arith_expr',3,'p_arith_expr','parser.py',792),
  ('term -> factor','term',1,'p_term','parser.py',810),
  ('term -> factor STAR term','term',3,'p_term','parser.py',811),
  ('term -> factor SLASH term','term',3,'p_term','parser.py',812),
  ('term -> factor PERCENT term','term',3,'p_term','parser.py',813),
  ('factor -> power','factor',1,'p_factor','parser.py',831),
  ('factor -> PLUS factor','factor',2,'p_factor','parser.py',832),
  ('factor -> MINUS factor','factor',2,'p_factor','parser.py',833),
  ('power -> atom','power',1,'p_power','parser.py',848),
  ('power -> atom LSQB test RSQB','power',4,'p_power','parser.py',849),
  ('atom -> NAME','atom',1,'p_atom1','parser.py',890),
  ('atom -> NUMBER','atom',1,'p_atom2','parser.py',911),
  ('atom -> STRING','atom',1,'p_atom3','parser.py',921),
  ('atom -> TRIPLESTRING','atom',1,'p_atom3','parser.py',922),
  ('atom -> FNUMBER','atom',1,'p_atom4','parser.py',930),
  ('atom -> LSQB RSQB','atom',2,'p_atom5','parser.py',938),
  ('atom -> LSQB listmaker RSQB','atom',3,'p_atom5','parser.py',939),
  ('atom -> LPAREN RPAREN','atom',2,'p_atom6','parser.py',959),
  ('atom -> LPAREN testlist_comp RPAREN','atom',3,'p_atom6','parser.py',960),
  ('listmaker -> test','listmaker',1,'p_listmaker','parser.py',970),
  ('listmaker -> test COMMA listmaker','listmaker',3,'p_listmaker','parser.py',971),
  ('testlist_comp -> test','testlist_comp',1,'p_testlist_comp','parser.py',993),
  ('testlist_comp -> test COMMA testlist_comp','testlist_comp',3,'p_testlist_comp','parser.py',994),
  ('testlist -> test','testlist',1,'p_testlist','parser.py',1005),
  ('testlist -> test COMMA testlist','testlist',3,'p_testlist','parser.py',1006),
  ('stmts -> stmt stmts','stmts',2,'p_stmts','parser.py',1023),
  ('stmts -> stmt Marker','stmts',2,'p_stmts','parser.py',1024),
]
