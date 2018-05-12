import copy

# -------------------------------------------------------- #
#	DEFINE DATA TYPES                                      #
# -------------------------------------------------------- #
# PrimType is one of the below types
############################################
class INT():
    pass
class BOOL():
    pass
class STRING():
    pass
class NAME():
    pass
class UNIT():
    pass
class ERROR():
    pass
class EMPTY():
    pass
class RETURN():
    pass
############################################

class Int:
    def __init__(self, i):
        self.i = i
class Bool:
    def __init__(self, b):
        self.b = b
class String:
    def __init__(self, s):
        self.s = s
class Name:
    def __init__(self, n):
        self.n = n
class Error:
    pass
class Unit:
    pass
class Empty:
    pass
class Return:
    pass

# Data is one of
# - Int(int)
# - Bool(bool)
# - String(string)
# - Name(string)
# - Error()
# - Unit()
# - Empty()
# - Return()

class Obj:
    def __init__(self, d):
        self.d = d
class Obj2:
    def __init__(self, n1, n2):
        self.n1 = n1
        self.n2 = n2

# Object is one of
# - Obj(Data)
# - Obj2(Name, Name)

# ActionName is one of the below types
############################################
class PUSH:
    pass
class POP:
    pass
class ADD:
    pass
class SUB:
    pass
class MUL:
    pass
class DIV:
    pass
class REM:
    pass
class NEG:
    pass
class SWAP:
    pass
class CAT:
    pass
class AND:
    pass
class OR:
    pass
class NOT:
    pass
class EQUAL:
    pass
class LESS_THAN:
    pass
class BIND:
    pass
class IF:
    pass
class LET:
    pass
class END:
    pass
class FUN:
    pass
class IN_OUT_FUN:
    pass
class FUN_END:
    pass
class CALL:
    pass
class RETURN:
    pass
class QUIT:
    pass
############################################

class ANP:
    def __init__(self, string, actName):
        self.string = string
        self.actName = actName
# ActionNamePair is one of
# - ANP(String, ActionName)

class Cmd:
    def __init__(self, actName, obj):
        self.actName = actName
        self.obj = obj
# Command is one of
# - Cmd(ActionName, Object)

class Var:
    def __init__(self, n, d):
        self.n = n
        self.d = d
# Variable is one of
# - Var(Name, Data)

class Func:
    def __init__(self, n, arg, vList, fList, cList, b):
        self.n = n
        self.arg = arg
        self.vList = vList
        self.fList = fList
        self.cList = cList
        self.b = b
class NIL_FUNC:
    pass
# Function is one of
# - Func(Name, Name, [Variable], [Function], [Command], bool)
# - NIL_FUNC()

class Cell:
    def __init__(self, d):
        self.d = d
class Fell:
    def __init__(self, f):
        self.f = f
# StackCell is one of
# - Cell(Data)
# - Fell(Function)

# Stack is one of
# - [StackCell]

class Prog:
    def __init__(self, s, vList, fList, cList):
        self.s = s
        self.vList = vList
        self.fList = fList
        self.cList = cList
# Program is one of
# - Prog(Stack, [Variable], [Function], [Command])

class Body:
    def __init__(self, f):
        self.f = f
# ActionBody is one of
# - (Program Object -> Program)
class Act:
    def __init__(self, actName, actBody):
        self.actName = actName
        self.actBody = actBody
# Action is one of
# - Act(ActionName, ActionBody)

# -------------------------------------------------------- #
#	HELPER FUNCTIONS                                       #
# -------------------------------------------------------- #

# getVar : Name [Variable] -> StackCell
# returns data binded to n; returns n if no binding exists
def getVar(n, vList):
    for v in vList:
        if (v.n.n == n.n):
            return Cell(v.d)
    return Cell(n)

# getVal : StackCell [Variable] -> StackCell
# if sc is a binded variable, returns binded value
# otherwise returns d
def getVal(sc, vList):
    if (isinstance(sc, Cell) and isinstance(sc.d, Name)):
            return getVar(sc.d, vList)
    return sc

# getFunc : name * function list -> function 
# returns function with name n; returns NIL_FUNC if no bindng exists
def getFunc(n, fList):
    for f in fList:
        if (isinstance(f, Func) and f.n.n == n.n):
            return f
    return NIL_FUNC()

# removeLet : [Command] * int -> [Command]
# returns the command list after a let is complete where
# n is the number of end's we "owe" (covers case of let statements in let statements)
def removeLet(cList, n):
    temp = copy.deepcopy(cList)
    removed = False
    while(temp != [] and (not removed)):
        if (temp[0].actName == END):
            n -= 1
        elif (temp[0].actName == LET):
            n += 1
        if (n < 0):
            removed = True
        temp.pop(0)
    return temp

# removeFun : [Command] int -> [Command]
# returns the command list after a function is complete where
# n is the number of funEnd's we "owe" (covers case of function definitions inside of functions) *)
def removeFun(cList, n):
    temp = copy.deepcopy(cList)
    removed = False
    while(temp != [] and (not removed)):
        if (temp[0].actName == FUN_END):
            n -= 1
        elif (temp[0].actName == FUN or
              temp[0].actName == IN_OUT_FUN):
            n += 1
        if (n < 0):
            removed = True
        temp.pop(0)
    return temp

# getPreSpace / getPostSpace : [Char] -> [Char]
# returns char list before/after first space, respectively
def getPreSpace(s):
    cl = []
    for c in s:
        if c != " ":
            cl += c
        else:
            return cl
    return cl
def getPostSpace(s):
    postSpace = False
    cl = []
    for c in s:
        if postSpace:
            cl.append(c)
        elif c == " ":
            postSpace = True
    return cl

# -------------------------------------------------------- #
#	STACK FUNCTIONALITY                                    #
# -------------------------------------------------------- #

# push : StackCell * Stack -> Stack
# gives stack with new val pushed to top
def push(d, s):
    temp = copy.deepcopy(s)
    temp.append(d)
    return temp

# pop : Stack -> Stack
# gives stack with top val popped
def pop(s):
    temp = copy.deepcopy(s)
    if (temp == []):
        temp.append(Cell(Error()))
        return temp
    temp.pop()
    return temp

# top : Stack -> StackCell
# gives top element in stack
def top(s):
    if (s == []):
        return Cell(Empty())
    return s[-1]

# stackString : Stack -> String 
# converts a stack to a string
def stackString(s):
    out = ""
    for sc in reversed(s): # where sc is a StackCell
        if (isinstance(sc, Cell)):
            d = sc.d # where d is a Data
            if (isinstance(d, Int)):        # Int
                out += "{0}\n".format(d.i)
            elif (isinstance(d, Bool)):     # Bool
                if (d.b):
                    out += ":true:\n"
                else:
                    out += ":false:\n"
            elif (isinstance(d, String)):   # String
                out += "{0}\n".format(d.s)
            elif (isinstance(d, Name)):     # Name
                out += "{0}\n".format(d.n)
            elif (isinstance(d, Error)):    # Error
                out += ":error:\n"
            elif (isinstance(d, Unit)):     # Unit
                out += ":unit:\n"
            elif (isinstance(d, Return)):   # Return
                out += "Return\n"
            else:                           # Empty
                out += "\n"
        elif (isinstance(sc, Fell)):
            out += "<FUNCTION>\n"
    return out

# -------------------------------------------------------- #
#	ACTION BODY DEFINITIONS                                #
# -------------------------------------------------------- #
actionNameDictionary = [ANP("push", PUSH),
 				        ANP("pop", POP),
						ANP("add", ADD),
				    	ANP("sub", SUB),
						ANP("mul", MUL),
						ANP("div", DIV),
						ANP("rem", REM),
						ANP("neg", NEG),
						ANP("swap", SWAP),
						ANP("cat", CAT),
						ANP("and", AND),
						ANP("or", OR),
						ANP("not", NOT),
						ANP("equal", EQUAL),
						ANP("lessThan", LESS_THAN),
						ANP("bind", BIND),
						ANP("if", IF),
						ANP("let", LET),
						ANP("end", END),
						ANP("fun", FUN),
						ANP("inOutFun", IN_OUT_FUN),
						ANP("return", RETURN),
						ANP("funEnd", FUN_END),
						ANP("call", CALL)]

# push_ : Program Object -> Program
# pushes data to program's stack
def push_(p, obj):
    p.s = push(Cell(obj.d), p.s)
    return p

# pop_ : Program Object -> Program
# pops top stackCell from program's stack
def pop_(p, obj):
    p.s = pop(p.s)
    return p

# add_ : program * object -> program
# pops y then x & pushes x + y to stack
# expects two ints. otherwise push back both then push error
def add_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            p.s = push(Cell(Int(d2.d.i + d1.d.i)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# sub_ : program * object -> program
# pops y then x & pushes x - y to stack
# expects two ints. otherwise push back both then push error
def sub_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            p.s = push(Cell(Int(d2.d.i - d1.d.i)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# mul_ : program * object -> program
# pops y then x & pushes x * y to stack
# expects two ints. otherwise push back both then push error
def mul_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            p.s = push(Cell(Int(d2.d.i * d1.d.i)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# div_ : program * object -> program
# pops y then x & pushes x / y to stack
# expects two ints. otherwise push back both then push error
def div_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            if (d1.d.i != 0):
                p.s = push(Cell(Int(d2.d.i // d1.d.i)), s_)
                return p
    p.s = push(Cell(Error()), p.s)
    return p

# rem_ : program * object -> program
# pops y then x & pushes x % y to stack
# expects two ints. otherwise push back both then push error
def rem_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            if (d1.d.i != 0):
                p.s = push(Cell(Int(d2.d.i % d1.d.i)), s_)
                return p
    p.s = push(Cell(Error()), p.s)
    return p

# neg_ : program * object -> program
# pops i & pushes -i to stack
# expects an int. otherwise push back i then push error *)
def neg_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    s_ = pop(p.s)
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        p.s = push(Cell(Int(-d1.d.i)), s_)
        return p
    p.s = push(Cell(Error()), p.s)
    return p

# swap_ : program * object -> program
# pops y then x & pushes y then x
# expects two pieces of data. otherwise push back both then push error
def swap_(p, obj):
    d1 = top(p.s)
    d2 = top(pop(p.s))
    s_ = pop(pop(p.s))
    if ((isinstance(d1, Cell) and isinstance(d1.d, Empty)) or
        (isinstance(d2, Cell) and isinstance(d2.d, Empty))):
        p.s = push(Cell(Error()), p.s)
        return p
    p.s = push(d2, push(d1, s_))
    return p

# cat_ : program * object -> program
# pops y then x & pushes x + y
# expects two strings. otherwise push back both then push error
def cat_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, String)):
        if (isinstance(d2, Cell) and isinstance(d2.d, String)):
            p.s = push(Cell(String(d2.d.s + d1.d.s)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# and_ : program * object -> program
# pops y then x & pushes x and y
# expects two bools. otherwise push back both then push error
def and_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Bool)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Bool)):
            p.s = push(Cell(Bool(d2.d.b and d1.d.b)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# or_ : program * object -> program
# pops y then x & pushes x or y
# expects two bools. otherwise push back both then push error
def or_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Bool)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Bool)):
            p.s = push(Cell(Bool(d2.d.b or d1.d.b)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# not_ : program * object -> program
# pops i & pushes not i to stack
# expects a bool. otherwise push back i then push error
def not_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    s_ = pop(p.s)
    if (isinstance(d1, Cell) and isinstance(d1.d, Bool)):
        p.s = push(Cell(Bool(not d1.d.b)), s_)
        return p
    p.s = push(Cell(Error()), p.s)
    return p

# equal_ : program * object -> program
# pops y then x & pushes the bool (x == y) to stack
# expects two ints. otherwise push back both then push error
def equal_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            p.s = push(Cell(Bool(d2.d.i == d1.d.i)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# lessThan_ : program * object -> program
# pops y then x & pushes the bool (x < y) to stack
# expects two ints. otherwise push back both then push error
def lessThan_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and isinstance(d1.d, Int)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Int)):
            p.s = push(Cell(Bool(d2.d.i < d1.d.i)), s_)
            return p
    p.s = push(Cell(Error()), p.s)
    return p

# bind_ : program * object -> program
# pops y then x & creates a variable with name x binded to data y
# expects x to be a name & y to be data. If y is a name it must be binded
# otherwise push back both then push error
def bind_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = top(pop(p.s))
    s_ = pop(pop(p.s))
    if (isinstance(d1, Cell) and (isinstance(d1.d, Error) or
                                  isinstance(d1.d, Empty) or
                                  isinstance(d1.d, Name))):
        p.s = push(Cell(Error()), p.s)
        return p
    elif (isinstance(d1, Cell)):
        if (isinstance(d2, Cell) and isinstance(d2.d, Name)):
            p.s = push(Cell(Unit()), s_)
            p.vList.insert(0, Var(d2.d, d1.d))
            return p
        else:
            p.s = push(Cell(Error()), p.s)
            return p
    else:
        if (isinstance(d2, Cell) and isinstance(d2.d, Name)):
            p.s = push(Cell(Unit()), s_)
            p.fList.insert(0, Func(d2.d.n, d1.f.arg, d1.f.vList, d1.f.fList, d1.f.cList, d1.f.b))
            return p
        else:
            p.s = push(Cell(Error()), p.s)
            return p

# if_ : program * object -> program 
# pops z then y then x. pushes y to stack if x is true, otherwise push z 
# expects x to be a bool, y & z to be data. otherwise push back all three then push error
def if_(p, obj):
    d1 = getVal(top(p.s), p.vList)
    d2 = getVal(top(pop(p.s)), p.vList)
    d3 = getVal(top(pop(pop(p.s))), p.vList)
    s_ = pop(pop(pop(p.s)))
    if ((isinstance(d1, Cell) and isinstance(d1.d, Empty)) or
        (isinstance(d2, Cell) and isinstance(d2.d, Empty))):
        p.s = push(Cell(Error()), p.s)
        return p
    if (isinstance(d3, Cell) and isinstance(d3.d, Bool)):
        if (d3.d.b):
            p.s = push(d2, s_)
            return p
        else:
            p.s = push(d1, s_)
            return p
    else:
        p.s = push(Cell(Error()), p.s)
        return p

# let_ : program * object -> program
# runs new program until we hit "end" command
# in scope and pushes top of stack to outer program
# no side effects carry out of scope of let statement
def let_(p, obj):
    p_ = evalAll(Prog([], p.vList, p.fList, p.cList))
    if (p_.s == []):
        p.cList = removeLet(p.cList, 0)
        return p
    else:
        p.s = push(p_.s[-1], p.s)
        p.cList = removeLet(p.cList, 0)
        return p

# fun_ : program * object -> program
# creates a function whos scope goes until "funEnd"
# and push :unit: to stack
def fun_(p, obj):
    temp = copy.deepcopy(p)
    f = Func(obj.n1, obj.n2, temp.vList, temp.fList, temp.cList, False)
    p.s = push(Cell(Unit()), p.s)
    p.fList.insert(0, f)
    p.cList = removeFun(p.cList, 0)
    return p

# iOFun_ : program * object -> program
# creates an in-out-function whos scope goes until "funEnd"
# and push :unit: to stack
def iOFun_(p, obj):
    temp = copy.deepcopy(p)
    f = Func(obj.n1, obj.n2, temp.vList, temp.fList, temp.cList, True)
    p.s = push(Cell(Unit()), p.s)
    p.fList.insert(0, f)
    p.cList = removeFun(p.cList, 0)
    return p

# return_ : program * object -> program
# pushes Return to stack and ends program
def return_(p, obj):
    p.s = push(Cell(Return()), p.s)
    return p

# getReturnVal : program * stack * var list * function list -> program
# takes parent program and s_, vList_, fList_ from sub program and
# pushes return value from sub program to parent program *)
def getReturnVal(p, s_, vList_, fList_):
    if (s_ == []):
        return p
    sc = s_[-1]
    if (isinstance(sc, Cell) and isinstance(sc.d, Return)):
        d1 = getVal(top(pop(s_)), vList_)
        if (isinstance(d1, Cell) and isinstance(d1.d, Name)):
            d1_ = getFunc(d1.d, fList_)
            if (isinstance(d1_, NIL_FUNC)):
                p.s = push(d1, p.s)
                return p
            else:
                p.s = push(Fell(d1_), p.s)
                return p 
        else:
            p.s = push(d1, p.s)
            return p
    return p

# changeReference : bool * program * stackCell * name * var list * function list -> program
# takes parent program and s_, vList_, fList_ from sub program and
# pushes return value from sub program to parent program *)
def changeReference(b, pb, d1, arg, vList_, fList_):
    p = copy.deepcopy(pb)
    if (not b):
        return p
    if (isinstance(d1, Cell) and isinstance(d1.d, Name)):
        i = getVar(arg, vList_)
        if (isinstance(i, Cell) and isinstance(i.d, Name)):
            k = copy.deepcopy(getFunc(i.d, fList_))
            if (isinstance(k, NIL_FUNC)):
                return p
            f = Func(d1.d, k.arg, k.vList, k.fList, k.cList)
            p.fList.insert(0, f)
            return p
        if (isinstance(i, Cell)):
            p.vList.insert(0, Var(d1.d, i.d))
    return p

# call_ : program * object -> program 
# pops y then x off the stack and invokes function x of arguement y
# expects x to be a name binded to a function & y to be either a value
# or a name binded to a value or function. otherwise push both to stack then push error *)
def call_(pb, obj):
    p = copy.deepcopy(pb)
    d1 = top(p.s)
    d2 = top(pop(p.s))
    s_ = pop(pop(p.s))
    if (isinstance(d2, Cell) and isinstance(d2.d, Name)):
        f = getFunc(d2.d, p.fList)
    elif (isinstance(d2, Fell)):
        f = d2.f
    else:
        f = NIL_FUNC()
    d = getVal(d1, p.vList)
    if (isinstance(f, NIL_FUNC)):
        p.s = push(Cell(Error()), p.s)
        return p
    if (isinstance(d, Cell) and isinstance(d.d, Error)):
        p.s = push(Cell(Error()), p.s)
        return p
    if (isinstance(d, Cell) and isinstance(d.d, Name)):
        f2 = copy.deepcopy(getFunc(d.d, p.fList))
        if (isinstance(f2, NIL_FUNC)):
            p.s = push(Cell(Error()), p.s)
            return p
        newF = Func(f.arg, f2.arg, f2.vList, f2.fList, f2.cList, f2.b)
        fCop = copy.deepcopy(f)
        fCop.fList.insert(0, newF)
        fCop.fList.insert(0, fCop)
        p_ = evalAll(Prog([], f.vList, fCop.fList, f.cList))
        p.s = s_
        return changeReference(f.b, getReturnVal(p, p_.s, p_.vList, p_.fList), d1, f.arg, p_.vList, p_.fList)
    if (isinstance(d, Cell)):
        fCop = copy.deepcopy(f)
        fCop.vList.insert(0, Var(fCop.arg, d.d))
        fCop.fList.insert(0, fCop)
        p_ = evalAll(Prog([], fCop.vList, fCop.fList, f.cList))
        p.s = s_
        return changeReference(f.b, getReturnVal(p, p_.s, p_.vList, p_.fList), d1, f.arg, p_.vList, p_.fList)
    f2 = copy.deepcopy(d.f)
    newF = Func(f.arg, f2.arg, f2.vList, f2.fList, f2.cList, f2.b)
    fCop = copy.deepcopy(f)
    fCop.fList.insert(0, newF)
    fCop.fList.insert(0, fCop)
    p_ = evalAll(Prog([], f.vList, fCop.fList, f.cList))
    p.s = s_
    return changeReference(f.b, getReturnVal(p, p_.s, p_.vList, p_.fList), d1, f.arg, p_.vList, p_.fList)

# quit_ : Program Object -> Program
# removes all commands from a given program
def quit_(p, obj):
    p.cList = []
    return p

# -------------------------------------------------------- #
#	INTERPRETER FUNCTIONALITY                              #
# -------------------------------------------------------- #

actionDictionary = [Act(PUSH, Body(push_)),
                    Act(POP, Body(pop_)),
                    Act(ADD, Body(add_)),
                    Act(SUB, Body(sub_)),
                    Act(MUL, Body(mul_)),
                    Act(DIV, Body(div_)),
                    Act(REM, Body(rem_)),
                    Act(NEG, Body(neg_)),
                    Act(SWAP, Body(swap_)),
                    Act(CAT, Body(cat_)),
                    Act(AND, Body(and_)),
                    Act(OR, Body(or_)),
                    Act(NOT, Body(not_)),
                    Act(EQUAL, Body(equal_)),
                    Act(LESS_THAN, Body(lessThan_)),
                    Act(BIND, Body(bind_)),
                    Act(IF, Body(if_)),
                    Act(LET, Body(let_)),
                    Act(FUN, Body(fun_)),
                    Act(IN_OUT_FUN, Body(iOFun_)),
                    Act(RETURN, Body(return_)),
                    Act(CALL, Body(call_))]

# getActionBody : ActionName * [Action] -> ActionBody
# gives actionBody corresponding to actionName
def getActionBody(n, aList):
    for a in aList:
        if (a.actName == n):
            return a.actBody
    return Body(quit_)

# eval : Program Command -> Program
# gives program after applying given command
def eval(p, cmd):
    return (getActionBody(cmd.actName, actionDictionary).f)(p, cmd.obj)

# evalAll : Program -> Program
# gives program after applying all commands
def evalAll(p):
    p_ = copy.deepcopy(p)
    if (p_.cList == []):
        return p_
    cmd = p_.cList.pop(0)
    return evalAll(eval(p_, cmd))

# objType : [Char] -> PrimType
# gives type based on given first character
def objType(s):
    if (s == []):
        return EMPTY
    c = s[0]
    if (c == "0" or c == "1" or c == "2" or c == "3" or c == "4" or c == "5" or c == "6" or c == "7" or c == "8" or c == "9" or c == "-"):
        return INT
    elif (c == ":"):
        return BOOL
    elif (c == "\""):
        return STRING
    else:
        return NAME

# getObj : [Char] -> Object
# gives object based on char list
def getObj(s):
    sType = objType(s)
    if (sType == INT):          # INT
        try:
            n = int("".join(s))
        except ValueError:
            return Obj(Error())
        return Obj(Int(n))
    elif (sType == BOOL):       # BOOL
        bString = "".join(s)
        if (bString == ":true:"):
            return Obj(Bool(True))
        elif (bString == ":false:"):
            return Obj(Bool(False))
        elif (bString == ":unit:"):
            return Obj(Unit())
        else:
            return Obj(Error())
    elif (sType == STRING):     # STRING
        s.pop(0)
        s.pop()
        return Obj(String("".join(s)))
    elif (sType == NAME):       # NAME
        return Obj(Name("".join(s)))
    elif (sType == ERROR):      # ERROR
        return Obj(Error())
    elif (sType == UNIT):       # UNIT
        return Obj(Unit())
    elif (sType == EMPTY):      # EMPTY
        return Obj(Empty())

# getNames : [Char] -> Object
# gives obj of two names for a function
def getNames(s):
    n1 = "".join(getPreSpace(s))
    n2 = "".join(getPostSpace(s))
    return Obj2(Name(n1), Name(n2))
# getActionName : String * [ActionNamePair] -> ActionName
# gives actionName based on string s
def getActionName(s, anpList):
    for anp in anpList:
        if (anp.string == s):
            return anp.actName
    return QUIT

# toCommand : [Char] -> Command
# Parse string and gives command
def toCommand(s):
    actString = getPreSpace(s)
    objString = getPostSpace(s)
    action = getActionName("".join(actString), actionNameDictionary)
    if (action == FUN or action == IN_OUT_FUN):
        obj = getNames(objString)
    else:
        obj = getObj(objString)

    return Cmd(action, obj)

# toCommandList : [String] -> [Command]
# Parse each string and give a list of commands
def toCommandList(lines):
    cList = []
    for s in lines:
        l = list(s)
        c = l.pop()
        if c != '\n':
            l.append(c)
        cList.append(toCommand(l))
    return cList

# getFile: String -> [String]
# puts given file into a list of strings
# where each string is a line of the file
def getFile(inFile):
	s = []
	with open(inFile) as f:
		for line in f:
			s.append(line)
	f.close
	return s

# interpreter : String String -> Void
# evaluates inFile and places result in outFile
def interpreter(inFile, outFile):
    lines = getFile(inFile)
    cmds = toCommandList(lines)
    p = evalAll(Prog([], [], [], cmds))
    outString = stackString(p.s)
    f = open(outFile, "w")
    f.write(outString)
    f.close()

interpreter("interpreterPart3Tests/input/input1.txt", "output.txt")