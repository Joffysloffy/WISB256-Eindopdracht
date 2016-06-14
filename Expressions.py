import math
from collections import namedtuple


# split a string into mathematical tokens
# returns a list of numbers, operators, parentheses and commas
# output will not contain spaces
def tokenize(string):
    splitchars = list("+-*/(),")

    # surround any splitchar by spaces
    tokenstring = []
    for c in string:
        if c in splitchars:
            tokenstring.append(" %s " % c)
        else:
            tokenstring.append(c)
    tokenstring = "".join(tokenstring)
    # split on spaces - this gives us our tokens
    tokens = tokenstring.split()

    # special casing for ** and negation:
    ans = []
    for t in tokens:
        if len(ans) > 0:
            # handle power **
            if t == ans[-1] == "*":
                ans[-1] = "**"
                continue

            # convert negation - to ~
            if t == "-":
                if ans[-1] in Expression.OPERATOR_SYMBOLS + ["(", ","]:
                    ans.append("~")
                    continue
            ans.append(t)
        elif t == "-":
            ans.append("~")
        else:
            ans.append(t)
    return ans


# check if a string represents a numeric value
def is_number(string):
    try:
        float(string)
        return True
    except ValueError:
        return False


# check if a string represents an integer value
def is_int(string):
    try:
        int(string)
        return True
    except ValueError:
        return False


class Expression:
    """A mathematical expression, represented as an expression tree"""

    """
    Any concrete subclass of Expression should have these methods:
     - __str__(): return a string representation of the Expression.
     - __eq__(other): tree-equality, check if other represents the same expression tree.
     - evaluate(variable_values: dict): evaluates the expression with values specified for the variables
    """

    # central list of properties of the operators
    OPERATOR_LIST = {"Addition": "+",
                     "Subtraction": "-",
                     "Multiplication": "*",
                     "Division": "/",
                     "Power": "**",
                     "Negation": "~"}
    OPERATOR_SYMBOLS = list(OPERATOR_LIST.values())
    UNARY_OPERATOR_SYMBOLS = ["~"]
    associativity = namedtuple("associativity", "left right")
    ASSOCIATIVITY = {"+": associativity(left=True, right=True),
                     "-": associativity(left=True, right=False),
                     "*": associativity(left=True, right=True),
                     "/": associativity(left=True, right=False),
                     "**": associativity(left=False, right=True),
                     "~": associativity(left=False, right=True)}
    PRECEDENCE = {"+": 0,
                  "-": 0,
                  "*": 1,
                  "/": 1,
                  "**": 2,
                  "~": 1}
    FUNCTION_END_TOKEN = "@"

    # TODO: when adding new methods that should be supported by all subclasses, add them to this list

    # operator overloading:
    # this allows us to perform 'arithmetic' with expressions, and obtain another expression
    def __add__(self, other):
        return AdditionNode(self, other)

    def __sub__(self, other):
        return SubtractionNode(self, other)

    def __mul__(self, other):
        return MultiplicationNode(self, other)

    def __truediv__(self, other):
        return DivisionNode(self, other)

    def __pow__(self, other):
        return PowerNode(self, other)

    def __invert__(self):
        return NegationNode(self)

    # the same as negation ~ so that overloaded negation with - be possible
    def __neg__(self):
        return NegationNode(self)

    # basic Shunting-yard algorithm
    @staticmethod
    def from_string(string):
        # split into tokens
        tokens = tokenize(string)
        # stack used by the Shunting-Yard algorithm
        stack = []
        # output of the algorithm: a list representing the formula in RPN
        # this will contain Constant's and '+'s
        output = []

        # list of operators
        oplist = Expression.OPERATOR_SYMBOLS
        fnlist = list(Function.BUILTIN_FUNCTIONS.keys())

        for index in range(len(tokens)):
            token = tokens[index]
            if is_number(token):
                # numbers go directly to the output
                if is_int(token):
                    output.append(Constant(int(token)))
                else:
                    output.append(Constant(float(token)))
            elif token in fnlist:
                stack.append(token)
                # to keep track of which arguments belong to the function
                output.append(Expression.FUNCTION_END_TOKEN)
            elif token == ",":
                # pop everything up to the left parenthesis to the output
                while not stack[-1] == "(":
                    output.append(stack.pop())
            elif token in Expression.UNARY_OPERATOR_SYMBOLS:
                # always pushed onto the stack
                stack.append(token)
            elif token in oplist:
                # pop operators from the stack to the output until the top is no longer an operator
                while len(stack) > 0 and stack[-1] in oplist:
                    if (Expression.ASSOCIATIVITY[token].left and Expression.PRECEDENCE[token] <= Expression.PRECEDENCE[stack[-1]]) or\
                       (Expression.ASSOCIATIVITY[token].right and Expression.PRECEDENCE[token] < Expression.PRECEDENCE[stack[-1]]):
                        output.append(stack.pop())
                    else:
                        break
                # push the new operator onto the stack
                stack.append(token)
            elif token == "(":
                # left parentheses go to the stack
                stack.append(token)
            elif token == ")":
                # right parenthesis: pop everything up to the last left parenthesis to the output
                while not stack[-1] == "(":
                    output.append(stack.pop())
                # pop the left parenthesis from the stack (but not to the output)
                stack.pop()
                # if the token before the left parenthesis is a function, pop it to the output
                if len(stack) > 0 and stack[-1] in fnlist:
                    output.append(stack.pop())
            elif index+1 < len(tokens) and tokens[index+1] == "(":
                # unknown token that is followed by a left parenthesis is presumed to be a function
                stack.append(token)
                # make sure the function is recognized later
                fnlist.append(token)
                output.append(Expression.FUNCTION_END_TOKEN)
            else:
                # unknown token is presumed to be a variable
                output.append(Variable(token))
            # else:
            #     # unknown token
            #     raise ValueError("Unknown token: %s" % token)

        # pop any tokens still on the stack to the output
        while len(stack) > 0:
            output.append(stack.pop())

        # convert RPN to an actual expression tree
        for t in output:
            if t in Expression.UNARY_OPERATOR_SYMBOLS:
                # first handle the unary operators
                x = stack.pop()
                stack.append(eval("%sx" % t))
            elif t in oplist:
                # let eval and operator overloading take care of figuring out what to do
                y = stack.pop()
                x = stack.pop()
                stack.append(eval("x %s y" % t))
            elif t in fnlist:
                arguments = []
                while stack[-1] != Expression.FUNCTION_END_TOKEN:
                    arguments.insert(0, stack.pop())
                # get rid of the FUNCTION_END_TOKEN
                stack.pop()
                try:
                    base = Function.BUILTIN_FUNCTIONS[t]
                except KeyError:
                    base = FunctionBase(t)
                stack.append(Function(base, *arguments))
            else:
                # a constant or FUNCTION_END_TOKEN, push it to the stack
                stack.append(t)
        # the resulting expression tree is what's left on the stack
        return stack[0]

    # evaluate the expression with values for variables and functions with a dictionary
    # keys are variables as strings with concrete values
    # keys are function names as strings with FunctionBase as values
    def evaluate(self, substitutions_unknowns={}):
        pass

    # substitute expressions for variables with a dictionary
    # keys are variables as strings with Expressions as values
    def substitute(self, substitutions_variables):
        pass

    def derivative(self, variable):
        pass

    def __contains__(self, item):
        if item == self:
            return True


class Constant(Expression):
    """Represents a constant value"""

    def __init__(self, value):
        self.value = value

    def __eq__(self, other):
        if isinstance(other, Constant):
            return self.value == other.value
        else:
            return False

    def __str__(self):
        return str(self.value)

    def evaluate(self, substitutions_unknowns={}):
        return self.value

    def substitute(self, substitutions_variables):
        return self

    def derivative(self, variable):
        return Constant(0)

    def __contains__(self, item):
        return self == item

    # allow conversion to numerical values
    def __int__(self):
        return int(self.value)

    def __float__(self):
        return float(self.value)


class Variable(Expression):
    """Represents a variable that may later be substituted for a value"""

    BUILTIN_CONSTANTS = {"pi": math.pi,
                         "π": math.pi,
                         "e": math.e}

    def __init__(self, symbol):
        self.symbol = symbol

    def __eq__(self, other):
        if isinstance(other, Variable):
            return self.symbol == other.symbol
        else:
            return False

    def __str__(self):
        return self.symbol

    def evaluate(self, substitutions_unknowns={}):
        try:
            return substitutions_unknowns[self.symbol]
        except KeyError:
            return Variable.BUILTIN_CONSTANTS[self.symbol]

    def substitute(self, substitutions_variables):
        try:
            return substitutions_variables[self.symbol]
        except KeyError:
            return Variable(self.symbol)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable == self:
            return Constant(1)
        else:
            return Constant(0)

    def __contains__(self, item):
        if isinstance(item, str):
            return item == self.symbol
        else:
            return item == self


class FunctionBase:

    # default variables for creating functions, iterable with square brackets, but not required
    class VARGetter(Variable):
        def __getitem__(self, item):
            return Variable("__VAR%d__" % item)
    VAR = VARGetter("__VAR0__")

    def __init__(self, symbol: str, executable=None, derivatives=None, variables=None):
        self.symbol = symbol
        self.executable = executable  # a Python function/method

        # of type Expression
        if derivatives is None:
            self._derivatives = []
        elif not isinstance(derivatives, list):
            self._derivatives = [derivatives]
        else:
            self._derivatives = derivatives

        if variables is None:
            self.variables = [FunctionBase.VAR[i] for i in range(len(self._derivatives))]
        else:
            self.variables = list(variables)  # the variables to be substituted in self.derivative, in order

    @property
    def derivatives(self):
        if self._derivatives == []:
            return [Function(FunctionBase(self.symbol + "'", self.executable))]
        else:
            return self._derivatives

    def has_derivative(self, index):
        try:
            self._derivatives[index]
            return True
        except IndexError:
            return False

    def __eq__(self, other):
        if isinstance(other, FunctionBase):
            return self.symbol == other.symbol and self.executable == other.executable
        else:
            return False


class Function(Expression):
    """Represents a function call, either pre-existing or later to be determined"""

    def __init__(self, base, *args):
        if isinstance(base, str):
            self.base = FunctionBase(base)
        else:
            self.base = base  # should be of type FunctionBase
        if len(args) == 0:
            self.arguments = FunctionBase.VAR,  # comma to make this a tuple
        else:
            self.arguments = args

    def __eq__(self, other):
        if isinstance(other, Function):
            return self.base == other.base and self.arguments == other.arguments
        else:
            return False

    def __str__(self):
        return "%s(%s)" % (self.base.symbol, ", ".join([str(a) for a in self.arguments]))

    def evaluate(self, substitutions_unknowns={}):
        try:
            f = substitutions_unknowns[self.base.symbol]
        except KeyError:
            f = Function.BUILTIN_FUNCTIONS[self.base.symbol].executable
        return f(*[a.evaluate(substitutions_unknowns) for a in self.arguments])

    def substitute(self, substitutions_variables):
        try:
            (variables, f) = substitutions_variables[self.base.symbol]
            f_var_subst = dict(zip(variables, [a.substitute(substitutions_variables) for a in self.arguments]))
            return f.substitute(f_var_subst)
        except KeyError:
            return Function(self.base, *[a.substitute(substitutions_variables) for a in self.arguments])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        diffs = []
        subst_vars_str = []
        for i in range(len(self.arguments)):
            if self.base.has_derivative(i):
                diffs.append(self.base.derivatives[i])
                subst_vars_str.append(str(self.base.variables[i]))
            else:
                try:
                    diffs.append(Function.BUILTIN_FUNCTIONS[self.base.symbol].derivatives[i])
                    subst_vars_str.append(str(Function.BUILTIN_FUNCTIONS[self.base.symbol].variables[i]))
                except KeyError:
                    diffs.append(Function("%s_%d" % (self.base.symbol, i+1), *self.arguments))
                    subst_vars_str.append(str(FunctionBase.VAR[i]))

        diff_components = []
        for i in range(len(self.arguments)):
            if variable in self.arguments[i]:
                diff_components.append(diffs[i].substitute(dict(zip(subst_vars_str, self.arguments))) * self.arguments[i].derivative(variable))

        if len(diff_components) > 0:
            result = diff_components[0]
            for i in range(1, len(diff_components)):
                result += diff_components[i]
            return result
        else:
            return Constant(0)

    def __contains__(self, item):
        if item == self:
            return True
        if isinstance(item, str):
            if item == self.base.symbol:
                return True
        for a in self.arguments:
            if item in a:
                return True
        return False


class OperatorNode(Expression):
    """The base for an operator in a node."""

    def __init__(self, op_symbol: str, is_left_associative: bool, is_right_associative: bool, precedence: int):
        self.is_left_associative = is_left_associative
        self.is_right_associative = is_right_associative
        self.precedence = precedence
        self.op_symbol = op_symbol


class UnaryNode(OperatorNode):
    """A node in the expression tree representing a prefix unary operator"""

    def __init__(self, operand: Expression, op_symbol):
        super().__init__(op_symbol, Expression.ASSOCIATIVITY[op_symbol].left, Expression.ASSOCIATIVITY[op_symbol].right, Expression.PRECEDENCE[op_symbol])
        self.operand = operand

    def __eq__(self, other):
        if type(self) == type(other):
            return self.operand == other.operand
        else:
            return False

    def __str__(self):
        vstring = str(self.operand)
        if isinstance(self.operand, OperatorNode):
            if self.operand.precedence < self.precedence:
                vstring = "(%s)" % vstring
        return self.op_symbol + vstring

    def evaluate(self, substitutions_unknowns={}):
        value = self.operand.evaluate(substitutions_unknowns)
        return eval("%svalue" % self.op_symbol)

    def substitute(self, substitutions_variables):
        value = self.operand.substitute(substitutions_variables)
        return eval("%svalue" % self.op_symbol)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        der = self.operand.derivative(variable)
        return eval("%sder" % self.op_symbol)

    def __contains__(self, item):
        if item == self:
            return True
        return item in self.operand


class NegationNode(UnaryNode):
    """Represents negation"""

    def __init__(self, operand):
        super().__init__(operand, Expression.OPERATOR_LIST["Negation"])

    def __str__(self):
        # use ‘-’ instead of ‘~’ when printing
        return "-" + super().__str__()[1:]

    def evaluate(self, substitutions_unknowns={}):
        return (Constant(0) - self.operand).evaluate(substitutions_unknowns)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        # use ‘-’ instead of ‘~’ for the derivative of the operand could be a float or int
        der = self.operand.derivative(variable)
        return eval("-der")


class BinaryNode(OperatorNode):
    """A node in the expression tree representing a binary operator."""

    def __init__(self, lhs: Expression, rhs: Expression, op_symbol):
        super().__init__(op_symbol, Expression.ASSOCIATIVITY[op_symbol].left, Expression.ASSOCIATIVITY[op_symbol].right, Expression.PRECEDENCE[op_symbol])
        self.lhs = lhs
        self.rhs = rhs

    def __eq__(self, other):
        if type(self) == type(other):
            return self.lhs == other.lhs and self.rhs == other.rhs
        else:
            return False

    def __str__(self):
        lstring = str(self.lhs)
        rstring = str(self.rhs)

        if isinstance(self.lhs, OperatorNode):
            if self.lhs.precedence < self.precedence or\
               (self.lhs.precedence == self.precedence and not self.is_left_associative):
                lstring = "(%s)" % lstring
        if isinstance(self.rhs, OperatorNode):
            if not isinstance(self.rhs, UnaryNode) and\
               (self.rhs.precedence < self.precedence or\
               (self.rhs.precedence == self.precedence and not self.is_right_associative)):
                rstring = "(%s)" % rstring
        return "%s %s %s" % (lstring, self.op_symbol, rstring)

    def evaluate(self, substitutions_unknowns={}):
        lvalue = self.lhs.evaluate(substitutions_unknowns)
        rvalue = self.rhs.evaluate(substitutions_unknowns)
        return eval("(lvalue) %s (rvalue)" % self.op_symbol)

    def substitute(self, substitutions_variables):
        lvalue = self.lhs.substitute(substitutions_variables)
        rvalue = self.rhs.substitute(substitutions_variables)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lvalue = self.lhs.derivative(variable)
        rvalue = self.rhs.derivative(variable)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def __contains__(self, item):
        if item == self:
            return True
        return item in self.lhs or item in self.rhs


class AdditionNode(BinaryNode):
    """Represents the addition operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Addition"])


class SubtractionNode(BinaryNode):
    """Represents the subtraction operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Subtraction"])


class MultiplicationNode(BinaryNode):
    """Represents the multiplication operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Multiplication"])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lderiv = self.lhs.derivative(variable)
        rderiv = self.rhs.derivative(variable)
        return lderiv * self.rhs + self.lhs * rderiv


class DivisionNode(BinaryNode):
    """Represents the division operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Division"])

    def derivative(self, variable: Variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lderiv = self.lhs.derivative(variable)
        rderiv = self.rhs.derivative(variable)
        return lderiv * self.rhs - self.lhs * rderiv / self.rhs ** Constant(2)


class PowerNode(BinaryNode):
    """Represents the power operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Power"])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self.rhs:
            return self.rhs * self.lhs ** (self.rhs - Constant(1)) * self.lhs.derivative(variable)

        if variable not in self.lhs:
            return self * Function("log", self.lhs) * self.rhs.derivative(variable)

        return self * (self.rhs.derivative(variable) * Function("log", self.lhs) + self.rhs * self.lhs.derivative(variable) / self.lhs)


Function.BUILTIN_FUNCTIONS = {"sin": FunctionBase("sin", math.sin, Function("cos")),
                              "cos": FunctionBase("cos", math.cos, -Function("sin")),
                              "tan": FunctionBase("tan", math.tan, Constant(1) + Function("tan")**Constant(2)),
                              "asin": FunctionBase("asin", math.asin, Constant(1) / Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2))),
                              "acos": FunctionBase("acos", math.acos, Constant(-1) / Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2))),
                              "atan": FunctionBase("atan", math.atan, Constant(1) / (FunctionBase.VAR**Constant(2) + Constant(1))),
                              "atan2": FunctionBase("atan2", math.atan2),  # atan2(y, x)
                              "sinh": FunctionBase("sinh", math.sinh, Function("cosh")),
                              "cosh": FunctionBase("cosh", math.cosh, Function("sinh")),
                              "tanh": FunctionBase("tanh", math.tanh, Constant(1) - Function("tanh")**Constant(2)),
                              "asinh": FunctionBase("asinh", math.asinh, Constant(1) / Function("sqrt", FunctionBase.VAR**Constant(2) + Constant(1))),
                              "acosh": FunctionBase("acosh", math.acosh, Constant(1) / Function("sqrt", FunctionBase.VAR**Constant(2) - Constant(1))),
                              "atanh": FunctionBase("atanh", math.atanh, Constant(1) / (Constant(1) - FunctionBase.VAR**Constant(2))),
                              "log": FunctionBase("log", math.log, [Constant(1) / (FunctionBase.VAR * Function("log", Variable("e"))), Constant(0)], [FunctionBase.VAR, Variable("e")]),  # log(x, base=e)
                              "lg": FunctionBase("lg", math.log2, Constant(1) / (FunctionBase.VAR * Function("log", Constant(2)))),
                              "exp": FunctionBase("exp", math.exp, Function("exp")),
                              "ceil": FunctionBase("ceil", math.ceil),
                              "floor": FunctionBase("floor", math.floor),
                              "factorial": FunctionBase("factorial", math.factorial),
                              "abs": FunctionBase("abs", math.fabs),
                              "sqrt": FunctionBase("sqrt", math.sqrt, Constant(1) / (Constant(2) * Function("sqrt")))
                              }



f = Expression.from_string("x*y")
expr = Expression.from_string("f(x+y, y)**2")
print(expr.substitute({"f": (["x", "y"], f)}))
print(expr.derivative("y"))
print(expr.derivative("y").substitute({"f": (["x", "y"], f)}))