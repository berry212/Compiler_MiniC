grammar MiniC;

// 词法规则名总是以大写字母开头

// 语法规则名总是以小写字母开头

// 每个非终结符尽量多包含闭包、正闭包或可选符等的EBNF范式描述

// 若非终结符由多个产生式组成，则建议在每个产生式的尾部追加# 名称来区分，详细可查看非终结符statement的描述

// 语法规则描述：EBNF范式

// 源文件编译单元定义
compileUnit: (funcDef | varDecl)* EOF;

// 函数定义，目前不支持形参，也不支持返回void类型等
funcDef: (T_INT | T_VOID) T_ID T_L_PAREN formalParamList? T_R_PAREN block;

// 形参列表，支持多个形参
formalParamList: formalParam (T_COMMA formalParam)*;

// 单个形参定义 - 支持数组形参
formalParam: basicType T_ID arrayDeclarator?;

// 语句块看用作函数体，这里允许多个语句，并且不含任何语句
block: T_L_BRACE blockItemList? T_R_BRACE;

// 每个ItemList可包含至少一个Item
blockItemList: blockItem+;

// 每个Item可以是一个语句，或者变量声明语句
blockItem: statement | varDecl;

// 变量声明，目前不支持变量含有初值
varDecl: basicType varDef (T_COMMA varDef)* T_SEMICOLON;

// 基本类型
basicType: T_INT;

// 变量定义 - 支持数组定义
varDef: T_ID arrayDeclarator? (T_ASSIGN expr)?;

// 数组声明符 - 支持多维数组 - 需要初始化数组大小 - +代表1或多次 - 支持最高维度定义为空 eg. int a[][2]
arrayDeclarator:
	T_L_BRACKET expr? T_R_BRACKET (T_L_BRACKET expr T_R_BRACKET)*;

// 目前语句支持return和赋值语句 ?表示前面内容出现 1 或 0 次
statement:
	T_RETURN expr? T_SEMICOLON			# returnStatement
	| lVal T_ASSIGN expr T_SEMICOLON	# assignStatement
	| block								# blockStatement
	| ifStatement						# ifStmt
	| whileStatement					# whileStmt
	| forStatement						# forStmt
	| breakStatement					# breakStmt
	| continueStatement					# continueStmt
	| expr T_SEMICOLON					# expressionStatement
	| T_SEMICOLON						# nop;

forStatement:
	T_FOR T_L_PAREN forInit? T_SEMICOLON expr? T_SEMICOLON forUpdate? T_R_PAREN statement;

// for循环的初值部分，支持变量声明或表达式
forInit: varDeclNoSemi | expr;

varDeclNoSemi: basicType varDef (T_COMMA varDef)*;

// for循环的步进部分，支持多个表达式
forUpdate: expr (T_COMMA expr)*;

// 新增控制流语句规则
ifStatement:
	T_IF T_L_PAREN expr T_R_PAREN statement (T_ELSE statement)?;
whileStatement: T_WHILE T_L_PAREN expr T_R_PAREN statement;
breakStatement: T_BREAK T_SEMICOLON;
continueStatement: T_CONTINUE T_SEMICOLON;

// 分层设计，运算符优先级
expr: logicOrExp | lVal T_ASSIGN logicOrExp;

logicOrExp: logicAndExp (T_LOR logicAndExp)*;

logicAndExp: eqExp (T_LAND eqExp)*;

eqExp: relExp (eqOp relExp)*;
eqOp: T_EQ | T_NE;

relExp: addExp (relOp addExp)*;
relOp: T_GT | T_LT | T_GE | T_LE;

// 加减表达式
addExp: mulExp (addOp mulExp)*;

// 加减运算符
addOp: T_ADD | T_SUB;

// 乘除模表达式
mulExp: unaryExp (mulOp unaryExp)*;

// 乘除模运算符
mulOp: T_MUL | T_DIV | T_MOD;

// 一元表达式
unaryExp:
	primaryExp
	| T_ID T_L_PAREN realParamList? T_R_PAREN
	| unaryOp unaryExp
	| T_LNOT unaryExp // 添加逻辑非操作
	| T_INC lVal // 前置自增
	| T_DEC lVal // 前置自减
	| lVal T_INC // 后置自增
	| lVal T_DEC; // 后置自减

// 一元运算符
unaryOp: T_SUB;

// 基本表达式：括号表达式、整数、左值表达式
primaryExp: T_L_PAREN expr T_R_PAREN | T_DIGIT | lVal;

// 实参列表
realParamList: expr (T_COMMA expr)*;

// 左值表达式 - 支持数组访问
lVal: T_ID (T_L_BRACKET expr T_R_BRACKET)*;

// 用正规式来进行词法规则的描述

T_L_PAREN: '(';
T_R_PAREN: ')';
T_SEMICOLON: ';';
T_L_BRACE: '{';
T_R_BRACE: '}';
T_L_BRACKET: '[';
T_R_BRACKET: ']';

T_ASSIGN: '=';
T_COMMA: ',';

T_ADD: '+';
T_SUB: '-';
T_MUL: '*';
T_DIV: '/';
T_MOD: '%';

// 新增自增自减运算符
T_INC: '++';
T_DEC: '--';

// 要注意关键字同样也属于T_ID，因此必须放在T_ID的前面，否则会识别成T_ID
T_RETURN: 'return';
T_INT: 'int';
T_VOID: 'void';
T_IF: 'if';
T_ELSE: 'else';
T_WHILE: 'while';
T_FOR: 'for';
T_BREAK: 'break';
T_CONTINUE: 'continue';

T_ID: [a-zA-Z_][a-zA-Z0-9_]*;
// 支持十进制、八进制和十六进制
T_DIGIT:
	'0'
	| [1-9][0-9]*
	| '0' [0-7]+
	| '0' [xX][0-9a-fA-F]+;

// 新增关系运算符
T_GT: '>';
T_LT: '<';
T_GE: '>=';
T_LE: '<=';
T_EQ: '==';
T_NE: '!=';

// 新增逻辑运算符
T_LAND: '&&';
T_LOR: '||';
T_LNOT: '!';

/* 空白符丢弃 */
WS: [ \r\n\t]+ -> skip;

/* 多行注释丢弃 */
BLOCK_COMMENT: '/*' .*? '*/' -> skip;

/* 单行注释丢弃 */
LINE_COMMENT: '//' ~[\r\n]* -> skip;

/* 预处理指令跳过 - 忽略#include等预处理指令 */
PREPROCESSOR: '#' ~[\r\n]* -> skip;