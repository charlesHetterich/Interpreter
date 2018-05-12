(* -------------------------------------------------------- *
	DEFINE DATA TYPES
 * -------------------------------------------------------- *)
datatype name = Name of string
datatype primType = INT | BOOL | STRING | NAME | UNIT | ERROR | EMPTY | RETURN

datatype data = DInt of int
			  | DBool of bool
 			  | DString of string
			  | DName of name
			  | Error
			  | Unit
			  | Empty
			  | Return

datatype object = Obj of data | Obj2 of name * name

datatype actionName = PUSH | POP
					| ADD | SUB | MUL | DIV | REM | NEG
					| SWAP | CAT
					| AND | OR | NOT | EQUAL | LESS_THAN
					| BIND | IF | LET | FUN | IN_OUT_FUN | CALL
					| END | RETURN | FUN_END | QUIT
datatype actionNamePair = ANP of string * actionName

datatype command = Cmd of actionName * object
datatype cmdComponent = ACTION | OBJECT

datatype var = Var of name * data

datatype function = Func of name * name * var list * function list * command list * bool
				  | NIL_FUNC

datatype stackCell = Cell of data
				   | Fell of function
datatype stack = Stack of stackCell list

datatype program = Program of stack * var list * function list * command list

datatype actionBody = Body of program * object -> program
datatype action = Act of actionName * actionBody

(* -------------------------------------------------------- *
	HELPER FUNCTIONS
 * -------------------------------------------------------- *)

(* removeLast : 'a list -> 'a list 
 * removes the last element of a list if a last element is present *)
fun removeLast([])    = []
  | removeLast(h::[]) = []
  | removeLast(h::t)  = h::removeLast(t)

(* getVar : name * var list -> stackCell 
 * returns data binded to n; returns n if no binding exists *)
fun getVar(n, [])     		   = Cell(DName(n))
  | getVar(n, (Var(n2, d)::t)) =
	if n = n2 then Cell(d)
	else getVar(n, t)

(* getFunc : name * function list -> function 
 * returns function with name n; returns NIL_FUNC if no bindng exists *)
fun getFunc(n, []) 			   							 = NIL_FUNC
  | getFunc(n, Func(n2, arg, vlist, flist, cList, b)::t) =
	if n = n2 then Func(n2, arg, vlist, flist, cList, b)
	else getFunc(n, t)

(* getVal : stackCell * var list -> stackCell 
 * if d is a binded var, returns binded value 
 * otherwise returns d *)
fun getVal(Cell(DName(d)), vList) = getVar(d, vList)
  | getVal(d, vList)			  = d

(* removeLet : command list * int -> command list
 * returns the command list after a let is complete where
 * n is the number of end's we "owe" (covers case of let statements in let statements) *)
fun removeLet([], n) 			 = []
  | removeLet(Cmd(END, _)::t, 0) = t
  | removeLet(Cmd(END, _)::t, n) = removeLet(t, n - 1)
  | removeLet(Cmd(LET, _)::t, n) = removeLet(t, n + 1)
  | removeLet(h::t, n)			 = removeLet(t, n)

(* removeFun : command list * int -> command list
 * returns the command list after a function is complete where
 * n is the number of funEnd's we "owe" (covers case of function definitions inside of functions) *)
fun removeFun([], n) 					= []
  | removeFun(Cmd(FUN_END, _)::t, 0) 	= t
  | removeFun(Cmd(FUN_END, _)::t, n) 	= removeFun(t, n - 1)
  | removeFun(Cmd(FUN, _)::t, n) 		= removeFun(t, n + 1)
  | removeFun(Cmd(IN_OUT_FUN, _)::t, n) = removeFun(t, n + 1)
  | removeFun(h::t, n) 					= removeFun(t, n)

(* getPreSpace / getPostSpace : char list -> char list
 * returns char list before/after space, respectively *)
fun getPreSpace([])   = []
  | getPreSpace(h::t) =
	if h = #" " then []
	else h::getPreSpace(t)
fun getPostSpace([])   = []
  | getPostSpace(h::t) =
	if h = #" " then t
	else getPostSpace(t)

(* -------------------------------------------------------- *
	STACK FUNCTIONALITY
 * -------------------------------------------------------- *)

(* push : stackCell * stack -> stack
 * gives stack with new val pushed to top *)
fun push(v, Stack(l)) = Stack(v::l)

(* pop : stack -> stack
 * gives stack with top val popped *)
fun pop(Stack([])) = Stack([Cell(Error)])
  | pop(Stack(h::t)) = Stack(t)

(* top : stack -> stackcell
 * gives top element in stack *)
fun top(Stack([])) = Cell(Empty)
  | top(Stack(h::t)) = h

(* toString : stackCell -> string
 * converts stackCell to a string *)
fun toString(Cell(DInt(i)))  		 =
	let val (h::t) = explode(Int.toString(i)) in
		if i >= 0 then implode(h::t)
		else implode(#"-"::t)
	end
  | toString(Cell(DBool(i))) 	   = if i then ":true:" else ":false:"
  | toString(Cell(DString(i)))     = i
  | toString(Cell(DName(Name(i)))) = i
  | toString(Cell(Error))		   = ":error:"
  | toString(Cell(Unit))           = ":unit:"
  | toString(Cell(i))          	   = ""
  | toString(Fell(NIL_FUNC))	   = "<NIL FUNC>"
  | toString(Fell(i))	   		   = "<LAMBDA>"

(* stackString : stack -> string
 * converts a stack to a string *)
fun stackString(Stack([]))    = ""
  | stackString(Stack(h::t))  = toString(h)
							   ^"\n"
							   ^stackString(Stack(t));

(* -------------------------------------------------------- *
	ACTION BODY DEFINITIONS
 * -------------------------------------------------------- *)
 val actionNameDictionary = [ANP("push", PUSH),
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

(* push_ : program * object -> program
 * pushes data to a program's stack *)
fun push_(Program(s, vList, fList, cList), Obj(d)) =
	Program(push(Cell(d), s), vList, fList, cList)

(* pop_ : program * object -> program
 * pops top stackCell from program's stack *)
fun pop_(Program(s, vList, fList, cList), _) =
	Program(pop(s), vList, fList, cList)

(* add_ : program * object -> program
 * pops y then x & pushes x + y to stack
 * expects two ints. otherwise push back both then push error *)
fun add_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) => Program(push(Cell(DInt(x + y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	         => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* sub_ : program * object -> program
 * pops y then x & pushes x - y to stack
 * expects two ints. otherwise push back both then push error *)
fun sub_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) => Program(push(Cell(DInt(x - y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* mul_ : program * object -> program
 * pops y then x & pushes x * y to stack
 * expects two ints. otherwise push back both then push error *)
fun mul_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) => Program(push(Cell(DInt(x * y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	  	 	 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* div_ : program * object -> program
 * pops y then x & pushes x div y to stack
 * expects two ints & y != 0. otherwise push back both then push error *)
fun div_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) =>
				if y = 0 then Program(push(Cell(Error), s), vList, fList, cList)
				else Program(push(Cell(DInt(x div y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* rem_ : program * object -> program
 * pops y then x & pushes x mod y to stack
 * expects two ints & y != 0. otherwise push back both then push error *)
fun rem_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) =>
				if y = 0 then Program(push(Cell(Error), s), vList, fList, cList)
				else Program(push(Cell(DInt(x mod y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* neg_ : program * object -> program
 * pops i & pushes -i to stack
 * expects an int. otherwise push back i then push error *)
fun neg_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val s_ = pop(s)
	in
		case d1 of Cell(DInt(i)) => Program(push(Cell(DInt(~i)), s_), vList, fList, cList)
				 | i 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* swap_ : program * object -> program
 * pops y then x & pushes y then x
 * expects two pieces of data. otherwise push back both then push error *)
fun swap_(Program(s, vList, fList, cList), _) =
	let
		val d1 = top(s)
		val d2 = top(pop(s))
		val s_ = pop(pop(s))
	in
		case d1 of Cell(Empty) => Program(push(Cell(Error), s), vList, fList, cList)
				 | y     	   =>
				 	(case d2 of Cell(Empty) => Program(push(Cell(Error), s), vList, fList, cList)
					 		  | x     		=> Program(push(x, push(y, s_)), vList, fList, cList))
	end

(* cat_ : program * object -> program
 * pops y then x & pushes x^y
 * expects two strings. otherwise push back both then push error *)
fun cat_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DString(y)) => 
			(case d2 of Cell(DString(x)) => Program(push(Cell(DString(x^y)), s_), vList, fList, cList)
					  | x     	  	 	 => Program(push(Cell(Error), s), vList, fList, cList))
				 | y     	  		=> Program(push(Cell(Error), s), vList, fList, cList)
	end

(* and_ : program * object -> program
 * pops y then x & pushes x andalso y
 * expects two bools. otherwise push back both then push error *)
fun and_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DBool(y)) => 
			(case d2 of Cell(DBool(x)) => Program(push(Cell(DBool(x andalso y)), s_), vList, fList, cList)
					  | x     	 	   => Program(push(Cell(Error), s), vList, fList, cList))
				 | y     		  => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* or_ : program * object -> program
 * pops y then x & pushes x orelse y
 * expects two bools. otherwise push back both then push error *)
fun or_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DBool(y)) => 
			(case d2 of Cell(DBool(x)) => Program(push(Cell(DBool(x orelse y)), s_), vList, fList, cList)
					  | x     	 	   => Program(push(Cell(Error), s), vList, fList, cList))
				 | y     		  => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* or_ : program * object -> program
 * pops i  & pushes not i
 * expects a bool. otherwise push i back then push error *)
fun not_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val s_ = pop(s)
	in
		case d1 of Cell(DBool(i)) => Program(push(Cell(DBool(not i)), s_), vList, fList, cList)
				 | i        	  => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* equal_ : program * object -> program
 * pops y then x & pushes the bool (x = y) to stack
 * expects two ints. otherwise push back both then push error *)
fun equal_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) => Program(push(Cell(DBool(x = y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* lessThan_ : program * object -> program
 * pops y then x & pushes the bool (x < y) to stack
 * expects two ints. otherwise push back both then push error *)
fun lessThan_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val s_ = pop(pop(s))
	in
		case d1 of Cell(DInt(y)) => 
			(case d2 of Cell(DInt(x)) => Program(push(Cell(DBool(x < y)), s_), vList, fList, cList)
					  | x       	  => Program(push(Cell(Error), s), vList, fList, cList))
				 | y 	   		 => Program(push(Cell(Error), s), vList, fList, cList)
	end

(* bind_ : program * object -> program
 * pops y then x & creates a variable with name x binded to data y
 * expects x to be a name & y to be data. If y is a name it must be binded
 * otherwise push back both then push error *)
fun bind_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = top(pop(s))
		val s_ = pop(pop(s))
	in
		case d1 of (Cell(Error) | Cell(Empty)) 					 => Program(push(Cell(Error), s), vList, fList, cList)
				 | Cell(DName(y))  	   					 		 => Program(push(Cell(Error), s), vList, fList, cList)
		   		 | Cell(y) 			   		   					 =>
			(case d2 of Cell(DName(x)) => Program(push(Cell(Unit), s_), Var(x, y)::vList, fList, cList)
						| x        	   => Program(push(Cell(Error), s), vList, fList, cList))
				 | Fell(Func(_, arg, vList2, fList2, cList2, b)) =>
			(case d2 of Cell(DName(x)) => Program(push(Cell(Unit), s_), vList, Func(x, arg, vList2, fList2, cList2, b)::fList, cList)
						| x        	   => Program(push(Cell(Error), s), vList, fList, cList))
	end

(* if_ : program * object -> program 
 * pops z then y then x. pushes y to stack if x is true, otherwise push z 
 * expects x to be a bool, y & z to be data. otherwise push back all three then push error *)
fun if_(Program(s, vList, fList, cList), _) =
	let
		val d1 = getVal(top(s), vList)
		val d2 = getVal(top(pop(s)), vList)
		val d3 = getVal(top(pop(pop(s))), vList)
		val s_ = pop(pop(pop(s)))
	in
		case d1 of Cell(Empty) => Program(push(Cell(Error), s), vList, fList, cList)
				 | z     	   =>
			(case d2 of Cell(Empty) => Program(push(Cell(Error), s), vList, fList, cList)
					 	| y	  		=>
				(case d3 of Cell(DBool(x)) => Program(push(if x then y else z, s_), vList, fList, cList)
						  | x		 	   => Program(push(Cell(Error), s), vList, fList, cList)))
	end

(* fun_ : program * object -> program
 * creates a function whos scope goes until "funEnd"
 * and push :unit: to stack *)
fun fun_(Program(s, vList, fList, cList), Obj2(n, arg)) =
	let val f = Func(n, arg, vList, fList, cList, false) in
		Program(push(Cell(Unit), s), vList, f::fList, removeFun(cList, 0))
	end

(* iOFun_ : program * object -> program
 * creates an in-out-function whos scope goes until "funEnd"
 * and push :unit: to stack *)
fun iOFun_(Program(s, vList, fList, cList), Obj2(n, arg)) =
	let val f = Func(n, arg, vList, fList, cList, true) in
		Program(push(Cell(Unit), s), vList, f::fList, removeFun(cList, 0))
	end

(* return_ : program * object -> program
 * pushes Return to stack and ends program *)
fun return_(Program(s, vList, fList, cList), _) =
	Program(push(Cell(Return), s), vList, fList, [])

(* getReturnVal : program * stack * var list * function list -> program
 * takes parent program and s_, vList_, fList_ from sub program and
 * pushes return value from sub program to parent program *)
fun getReturnVal(Program(s, vList, fList, cList), s_, vList_, fList_) =
	case s_ of Stack([]) 			  => Program(s, vList, fList, cList)
			 | Stack(Cell(Return)::t) =>
		(let val d1 = getVal(top(pop(s_)), vList_) in
			case d1 of Cell(DName(i)) =>
				(let val d1_ = getFunc(i, fList_) in
					case d1_ of NIL_FUNC => Program(push(d1, s), vList, fList, cList)
							  | f		 => Program(push(Fell(f), s), vList, fList, cList)
				end)
					 | i			  => Program(push(i, s), vList, fList, cList)
		end)
			 | Stack(h::t) 			  => Program(s, vList, fList, cList)

(* changeReference : bool * program * stackCell * name * var list * function list -> program
 * takes parent program and s_, vList_, fList_ from sub program and
 * pushes return value from sub program to parent program *)
fun changeReference(false, Program(s, vList, fList, cList), d1, arg, vList_, fList_) = Program(s, vList, fList, cList)
  | changeReference(true, Program(s, vList, fList, cList), Cell(DName(d1)), arg, vList_, fList_) =
	let val i = getVar(arg, vList_) in
		case i of Cell(DName(j)) =>
			(let val k = getFunc(j, fList_) in
				case k of NIL_FUNC 								   => Program(s, vList, fList, cList)
						| Func(n, arg2, vList2, fList2, cList2, b) => 
					(let val f = Func(d1, arg2, vList2, fList2, cList2, b) in
						Program(s, vList, f::fList, cList)
					end)
			end)
				| Cell(j)		 => Program(s, Var(d1, j)::vList, fList, cList)
				| j				 => Program(s, vList, fList, cList)
	end
  | changeReference(true, Program(s, vList, fList, cList), d1, arg, vList_, fList_) = Program(s, vList, fList, cList)

(* quit_ : program * object -> program
 * removes all commands from a given program *)
fun quit_(Program(s, vList, fList, cList), _) =
	Program(s, vList, fList, [])

(* -------------------------------------------------------- *
	INTERPRETER FUNCTIONALITY
 * -------------------------------------------------------- *)

(* evalAll : program -> program
 * gives program after applying all commands *)
fun evalAll(Program(s, vList, fList, []))     = Program(s, vList, fList, [])
  | evalAll(Program(s, vList, fList, (h::t))) =
	let
		(* let_ : program * object -> program
		 * runs new program until we hit "end" command
		 * in scope and pushes top of stack to outer program
		 * no side effects carry out of scope of let statement *)
		fun let_(Program(s, vList, fList, cList), _) =
			let
				val Program(s_, vList_, fList_, cList_) = evalAll(Program(Stack([]), vList, fList, cList))
			in
				case s_ of Stack([])  => Program(s, vList, fList, removeLet(cList, 0))
						| Stack(h::t) => Program(push(h, s), vList, fList, removeLet(cList, 0))
			end

		(* call_ : program * object -> program 
		 * pops y then x off the stack and invokes function x of arguement y
		 * expects x to be a name binded to a function & y to be either a value
		 * or a name binded to a value or function. otherwise push both to stack then push error *)
		fun call_(Program(s, vList, fList, cList), _) =
			let
				val d1 = top(s)
				val d2 = top(pop(s))
				val s_ = pop(pop(s))

				val f = case d2 of Cell(DName(i)) => getFunc(i, fList)
								 | Fell(i)		  => i
								 | i	  		  => NIL_FUNC
				val d = getVal(d1, vList)
			in
				case f of NIL_FUNC 							      => Program(push(Cell(Error), s), vList, fList, cList)
				        | Func(n, arg, vList2, fList2, cList2, b) =>
					case d of Cell(Error) 								       => Program(push(Cell(Error), s), vList, fList, cList)
							| Cell(DName(i))								   =>
						(let val f2 = getFunc(i, fList) in
							case f2 of NIL_FUNC 							      => Program(push(Cell(Error), s), vList, fList, cList)
									 | Func(n2, arg2, vList3, fList3, cList3, b2) =>
								(let
									val newF = Func(arg, arg2, vList3, fList3, cList3, b2)
									val Program(s__, vList_, fList_, cList_) = evalAll(Program(Stack([]), vList2, newF::f::fList2, cList2))
								in
									changeReference(b, getReturnVal(Program(s_, vList, fList, cList), s__, vList_, fList_), d1, arg, vList_, fList_)
								end)
						end)
							| Cell(i)		 							       =>
						(let
							val Program(s__, vList_, fList_, cList_) = evalAll(Program(Stack([]), Var(arg, i)::vList2, f::fList2, cList2))
						in
							changeReference(b, getReturnVal(Program(s_, vList, fList, cList), s__, vList_, fList_), d1, arg, vList_, fList_)
						end)
							| Fell(Func(n2, arg2, vList3, fList3, cList3, b2)) =>
						(let
							val newF = Func(arg, arg2, vList3, fList3, cList3, b2)
							val Program(s__, vList_, fList_, cList_) = evalAll(Program(Stack([]), vList2, newF::f::fList2, cList2))
						in
							changeReference(b, getReturnVal(Program(s_, vList, fList, cList), s__, vList_, fList_), d1, arg, vList_, fList_)
						end)

			end

		val actionDictionary = [Act(PUSH, Body(push_)),
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

		(* getActionBody : actionName * action list -> actionBody
		 * gives actionBody coresponding to actionName *)
		fun getActionBody(n, []) 				  = quit_
		  | getActionBody(n, Act(n2, Body(a))::t) =
				if n = n2 then a
				else getActionBody(n, t)

		(* eval : program * command -> program
		 * gives program after applying given command *)
		fun eval(p, Cmd(a, obj)) = (getActionBody(a, actionDictionary))(p, obj)
	in
		evalAll(eval(Program(s, vList, fList, t), h))
	end

(* mag : char list -> Data 
 * gives magnitude of number from string or error if incorrect format *)
fun mag([])      = DInt(0)
  | mag(#"-"::t) = DInt(0)
  | mag(h::t)    =
	case h of (#"0" | #"1" | #"2" | #"3" | #"4" | #"5" | #"6" | #"7" | #"8" | #"9") =>
		(case (mag(t)) of Error   => Error
					    | DInt(n) => DInt((ord(h) - ord(#"0")) + (10 * n)))
	  	    | i => Error

(* toNumber : char list -> Data 
 * gives number from string or error if incorrect format *)
fun toNumber([])   = DInt(0)
fun toNumber(h::t) =
	(case (mag(rev(h::t))) of Error   => Error
					        | DInt(n) => if h = #"-" then DInt(~n) else DInt(n))

(* objType : char -> primType
 * gives type based on given first character *)
fun objType(#"0" | #"1" | #"2" | #"3" | #"4" | #"5" | #"6" | #"7" | #"8" | #"9" | #"-") = INT
  | objType(#":")  = BOOL
  | objType(#"\"") = STRING
  | objType(c)     = NAME

(* getObj : char list -> object
 * gives object based on char list *)
fun getObj([]) = Obj(Empty)
  | getObj(h::t) = 
	case objType(h) of INT    =>
		(case toNumber(h::t) of Error   => Obj(Error)
							  | DInt(n) => Obj(DInt(n)))
					 | BOOL   => 
		if implode((h::t)) = ":true:" then Obj(DBool(true))
		else if implode((h::t)) = ":false:" then Obj(DBool(false))
		else if implode((h::t)) = ":unit:" then Obj(Unit)
		else Obj(Error)
					 | STRING => Obj(DString(implode(removeLast(t))))
					 | NAME   => Obj(DName(Name(implode((h::t)))))
					 | ERROR  => Obj(Error)
					 | UNIT   => Obj(Unit)
					 | EMPTY  => Obj(Empty)

(* getNames : char list -> object 
 * gives obj of two names for a function *)
fun getNames([]) = Obj(Empty)
fun getNames(s)  =
	Obj2(Name(implode(getPreSpace(s))),
		 Name(implode(getPostSpace(s))))

(* getActionName : string * actionNamePair list -> actionName
 * gives actionName based on string s *)
fun getActionName(s, [])			  = QUIT
  | getActionName(s, (ANP(s2, n)::t)) =
	if s = s2 then n
		else getActionName(s, t)

(* toCommand : char list -> command
 * parse string and gives command *)
fun toCommand(s) =
	let
		val actionString = getPreSpace(s)
		val objString = getPostSpace(s)
		val action = getActionName(implode(actionString), actionNameDictionary)
		val obj = case action of (FUN | IN_OUT_FUN) => getNames(objString)
							   | a 				    => getObj(objString)
	in
		Cmd(action, obj)
	end

(* toCommandList : instream -> command list
 * parses file stream and gives list of commands *)
fun toCommandList(fstream) =
	let
		val l = TextIO.inputLine fstream
	in
		case l of NONE   => []
				| SOME i => toCommand(removeLast(explode(i)))::toCommandList(fstream)
	end

(* interpreter : string * string -> unit
 * evaluates inFile and places result in outFile *)
fun interpreter(inFile, outFile) =
	let
		val inStream = TextIO.openIn inFile
		val cmds = toCommandList(inStream)
		val Program(finalStack, _, _, _) = evalAll(Program(Stack([]), [], [], cmds))
		val outStream = TextIO.openOut outFile
	in
		TextIO.output(outStream, stackString(finalStack));
		TextIO.closeOut outStream
	end

(* RUN INTERPRETER *)
val x = interpreter("interpreterPart3Tests/input/input2.txt", "output.txt")