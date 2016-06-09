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

    # special casing for **:
    ans = []
    for t in tokens:
        if len(ans) > 0 and t == ans[-1] == "*":
            ans[-1] = "**"
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
                     "Power": "**"}
    OPERATOR_SYMBOLS = list(OPERATOR_LIST.values())
    associativity = namedtuple("associativity", "left right")
    ASSOCIATIVITY = {"+": associativity(left=True, right=True),
                     "-": associativity(left=True, right=False),
                     "*": associativity(left=True, right=True),
                     "/": associativity(left=True, right=False),
                     "**": associativity(left=False, right=True)}
    PRECEDENCE = {"+": 0,
                  "-": 0,
                  "*": 1,
                  "/": 1,
                  "**": 2}
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
                while not stack[-1] == "(":
                    output.append(stack.pop())
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
            if t in oplist:
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
                stack.append(Function(t, *arguments))
            else:
                # a constant or FUNCTION_END_TOKEN, push it to the stack
                stack.append(t)
        # the resulting expression tree is what's left on the stack
        return stack[0]

    def evaluate(self, substitutions_unknowns=dict()):
        pass


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

    def evaluate(self, substitutions_unknowns=dict()):
        return self.value

    # allow conversion to numerical values
    def __int__(self):
        return int(self.value)

    def __float__(self):
        return float(self.value)


class Variable(Expression):
    """Represents a variable that may later be substituted for a value"""

    def __init__(self, symbol):
        self.symbol = symbol

    def __eq__(self, other):
        if isinstance(other, Variable):
            return self.symbol == other.symbol
        else:
            return False

    def __str__(self):
        return self.symbol

    def evaluate(self, substitutions_unknowns=dict()):
        return substitutions_unknowns[self.symbol]


class Function(Expression):
    """Represents a function call, either pre-existing or later to be determined"""

    BUILTIN_FUNCTIONS = {"sin": math.sin,  # sin(x)
                         "cos": math.cos,  # cos(x)
                         "tan": math.tan,  # tan(x)
                         "sec": lambda x: 1/math.cos(x),
                         "csc": lambda x: 1/math.sin(x),
                         "cot": lambda x: 1/math.tan(x),
                         "log": math.log  # log(x, base)
                         }

    def __init__(self, symbol, *args):
        self.symbol = symbol
        self.arguments = args

    def __eq__(self, other):
        if isinstance(other, Function):
            return self.symbol == other.symbol and self.arguments == other.arguments
        else:
            return False

    def __str__(self):
        return "%s(%s)" % (self.symbol, ", ".join([str(a) for a in self.arguments]))

    def evaluate(self, substitutions_unknowns=dict()):
        try:
            f = substitutions_unknowns[self.symbol]
        except KeyError:
            f = Function.BUILTIN_FUNCTIONS[self.symbol]
        return f(*[a.evaluate(substitutions_unknowns) for a in self.arguments])


class OperatorNode(Expression):
    """The base for an Operator in a node."""

    def __init__(self, op_symbol: str, is_left_associative: bool, is_right_associative: bool, precedence: int):
        self.is_left_associative = is_left_associative
        self.is_right_associative = is_right_associative
        self.precedence = precedence
        self.op_symbol = op_symbol


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

        # TODO: do we always need parentheses?
        if isinstance(self.lhs, BinaryNode):
            if self.lhs.precedence < self.precedence or\
               (self.lhs.precedence == self.precedence and not self.lhs.is_left_associative):
                lstring = "(%s)" % lstring
        if isinstance(self.rhs, BinaryNode):
            if self.rhs.precedence < self.precedence or\
               (self.rhs.precedence == self.precedence and not self.rhs.is_right_associative):
                rstring = "(%s)" % rstring
        return "%s %s %s" % (lstring, self.op_symbol, rstring)

    def evaluate(self, substitutions_unknowns=dict()):
        lvalue = self.lhs.evaluate(substitutions_unknowns)
        rvalue = self.rhs.evaluate(substitutions_unknowns)
        return eval((str(lvalue) + "%s" + str(rvalue)) % self.op_symbol)


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


class DivisionNode(BinaryNode):
    """Represents the division operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Division"])


class PowerNode(BinaryNode):
    """Represents the power operator"""

    def __init__(self, lhs, rhs):
        super().__init__(lhs, rhs, Expression.OPERATOR_LIST["Power"])


