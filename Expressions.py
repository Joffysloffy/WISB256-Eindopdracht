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
    COMMUTATIVITY = {"+": True,
                     "-": False,
                     "*": True,
                     "/": False,
                     "**": False}
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
        return ~self

    # basic Shunting-yard algorithm
    @staticmethod
    def from_string(string):
        # split into tokens
        tokens = tokenize(string)
        # stack used by the Shunting-Yard algorithm
        stack = []
        # output of the algorithm: a list representing the formula in RPN
        # this will contain instances of subclasses of Expression
        output = []

        # list of operators and functions
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

    # substitute expressions for variables and functions with a dictionary
    # keys of variables are strings with Expressions as values
    # keys of functions are strings with values either:
    #     a tuple with first an Expression and then a list of variables used
    #     just an Expression, provided it use FunctionBase.VAR as its variables
    def substitute(self, substitutions_unknowns, find_derivatives=True):
        return self

    def derivative(self, variable):
        pass

    # compute integral numerically on [lower_bound, upper_bound] w.r.t. variable.
    # substitutions_unknowns works the same as in evaluate()
    def numeric_integral(self, variable, lower_bound, upper_bound, step_count, substitutions_unknowns=None):
        if substitutions_unknowns is None:
            substitutions_unknowns = {}

        if isinstance(variable, Variable):
            variable = variable.symbol
        if variable in substitutions_unknowns:
            raise KeyError("Integration variable '%s' is substituted" % variable)

        step_size = (upper_bound - lower_bound) / step_count
        # make a partition of points for a middle Riemann sum
        partition = [lower_bound + step_size / 2 + step_size * i for i in range(step_count)]
        summation = 0
        for s in partition:
            substitutions_unknowns.update({variable: s})
            summation += step_size * self.evaluate(substitutions_unknowns)
        return summation

    def simplify(self):
        return self

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

    def derivative(self, variable):
        return Constant(0)

    def simplify(self):
        if self.value < 0:
            return -Constant(-self.value)
        else:
            return self

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

    def substitute(self, substitutions_unknowns, find_derivatives=True):
        try:
            return substitutions_unknowns[self.symbol]
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
    """Represents a standard function, with evaluation method and derivative"""

    # default variables for creating functions, iterable with square brackets, but not required
    class VARGetter(Variable):
        def __getitem__(self, key):
            if isinstance(key, slice):
                if key.stop is None:
                    raise IndexError("Upper bound for variables must be finite")
                else:
                    return [self[i] for i in range(*key.indices(key.stop))]
            else:
                return Variable("__VAR%d__" % key)
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
            self.variables = FunctionBase.VAR[0:len(self._derivatives)]
        else:
            self.variables = list(variables)  # the variables to be substituted in self.derivative, in order

    @property
    def derivatives(self):
        if self._derivatives == []:
            return [Function(FunctionBase(self.symbol + "_1", self.executable))]
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
            f = substitutions_unknowns[self.base.symbol].executable
        except KeyError:
            f = Function.BUILTIN_FUNCTIONS[self.base.symbol].executable
        return f(*[a.evaluate(substitutions_unknowns) for a in self.arguments])

    def substitute(self, substitutions_unknowns, find_derivatives=True):
        try:
            # allow for the use of the default variables VAR
            val = substitutions_unknowns[self.base.symbol]
            if isinstance(val, tuple):
                (f, variables) = val
            else:
                f = val
                variables = [str(FunctionBase.VAR[i]) for i in range(len(self.arguments))]
            f_var_subst = dict(zip(variables, [a.substitute(substitutions_unknowns) for a in self.arguments]))
            return f.substitute(f_var_subst)
        except KeyError:
            if find_derivatives:
                # attempt to find a provided function of which this one is some derivative
                fn_name = self.base.symbol
                var_index_stack = []
                while fn_name not in substitutions_unknowns:
                    i = fn_name.rfind("_")
                    if i > 0:
                        # get the index of the variable with respect to which the derivative was taken (e.g., f_i -> i-1)
                        try:
                            var_index_stack.append(int(fn_name[i+1:]) - 1)
                        except ValueError:
                            break
                        # get the name of the original function (e.g., f_i -> f)
                        fn_name = fn_name[:i]
                    else:
                        break
                else:  # the loop ended normally
                    val = substitutions_unknowns[fn_name]
                    if isinstance(val, tuple):
                        (f, variables) = val
                    else:
                        f = val
                        variables = [str(FunctionBase.VAR[i]) for i in range(len(self.arguments))]
                    # repeated take the correct derivative to get back to the unknown derivative
                    # also keep substituting in case there are nested functions of which we found newly unknown derivatives
                    while len(var_index_stack) > 0:
                        f = f.derivative(variables[var_index_stack.pop()]).substitute(substitutions_unknowns)
                    f_var_subst = dict(zip(variables, [a.substitute(substitutions_unknowns) for a in self.arguments]))
                    return f.substitute(f_var_subst)

        return Function(self.base, *[a.substitute(substitutions_unknowns) for a in self.arguments])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        # find the derivative w.r.t to each variable and create a dummy function if it does not exist
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

        # plug the arguments into the variables of each differentiated function and multiply with the derivative of the corresponding component
        diff_components = []
        for i in range(len(self.arguments)):
            if variable in self.arguments[i]:
                # here False in substitute(), because trying to find derivatives is redundant, as we know they are not there by construction
                diff_components.append(diffs[i].substitute(dict(zip(subst_vars_str, self.arguments)), False) * self.arguments[i].derivative(variable))

        # add everything together
        if len(diff_components) > 0:
            result = diff_components[0]
            for i in range(1, len(diff_components)):
                result += diff_components[i]
            return result
        else:
            return Constant(0)

    def simplify(self):
        return Function(self.base, *[a.simplify() for a in self.arguments])

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

    def substitute(self, substitutions_unknowns, find_derivatives=True):
        value = self.operand.substitute(substitutions_unknowns)
        return eval("%svalue" % self.op_symbol)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        der = self.operand.derivative(variable)
        return eval("%sder" % self.op_symbol)

    def simplify(self):
        operand = self.operand.simplify()
        return eval("%soperand" % self.op_symbol)

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

    def simplify(self):
        operand = self.operand.simplify()

        # undo double negation
        if isinstance(operand, NegationNode):
            return operand.operand

        # -0 is 0
        if operand == Constant(0):
            return Constant(0)

        return super().simplify()


class BinaryNode(OperatorNode):
    """A node in the expression tree representing a binary operator."""

    def __init__(self, lhs: Expression, rhs: Expression, op_symbol: str, is_commutative):
        super().__init__(op_symbol, Expression.ASSOCIATIVITY[op_symbol].left, Expression.ASSOCIATIVITY[op_symbol].right, Expression.PRECEDENCE[op_symbol])
        self.lhs = lhs
        self.rhs = rhs
        self.is_commutative = is_commutative

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

    def substitute(self, substitutions_unknowns, find_derivatives=True):
        lvalue = self.lhs.substitute(substitutions_unknowns)
        rvalue = self.rhs.substitute(substitutions_unknowns)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lvalue = self.lhs.derivative(variable)
        rvalue = self.rhs.derivative(variable)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # evaluate operations between two constants
        if isinstance(lhs, Constant) and isinstance(rhs, Constant):
            return Constant(eval("%s %s %s" % (lhs.value, self.op_symbol, rhs.value)))

        # attempt to simplify consecutive operations using commutativity by cycling the operands
        # e.g., if a*b*c does not simplify, try a*c*b and then a*(b*c)
        if self.is_commutative:
            # cycling in case of lhs
            if type(self) == type(lhs):
                operandslr = eval("lhs.lhs %s rhs" % self.op_symbol)
                operandslr_simpl = operandslr.simplify()
                if operandslr == operandslr_simpl:
                    operandsrr = eval("lhs.rhs %s rhs" % self.op_symbol)
                    operandsrr_simpl = operandsrr.simplify()
                    if not operandsrr == operandsrr_simpl:
                        return eval("lhs.lhs %s operandsrr_simpl" % self.op_symbol)
                else:
                    return eval("operandslr_simpl %s lhs.rhs" % self.op_symbol)
            # cycling in case of rhs reduces to the case of lhs
            if type(self) == type(rhs):
                operandsll = eval("lhs %s rhs.lhs" % self.op_symbol)
                operandsll_simpl = operandsll.simplify()
                if operandsll == operandsll_simpl:
                    operandslr = eval("lhs %s rhs.rhs" % self.op_symbol)
                    operandslr_simpl = operandslr.simplify()
                    if not operandslr == operandslr_simpl:
                        return eval("operandslr_simpl %s rhs.lhs" % self.op_symbol)
                else:
                    return eval("operandsll_simpl %s rhs.rhs" % self.op_symbol)

        # if all else fails, return the operation between the simplified sides
        return eval("lhs %s rhs" % self.op_symbol)

    @staticmethod
    # distributive laws, e.g., a*b+a*c = a*(b+c)
    # e.g., node_lower is AdditionNode and node_higher is MultiplicationNode
    # left and right represent left- and right-distributivity
    # returns None if no simplification was made
    def simplify_distributively(lhs, rhs, node_lower, node_higher, left=True, right=True):
        # if lhs is the higher precedence node
        if isinstance(lhs, node_higher):
            # if also rhs is higher precedence node
            if isinstance(rhs, node_higher):
                if left:
                    if rhs.lhs == lhs.lhs:
                        return node_higher(rhs.lhs, node_lower(lhs.rhs, rhs.rhs)).simplify()
                    if lhs.is_commutative:
                        if rhs.lhs == lhs.rhs:
                            return node_higher(rhs.lhs, node_lower(lhs.lhs, rhs.rhs)).simplify()
                if right:
                    if lhs.is_commutative:
                        if rhs.rhs == lhs.lhs:
                            return node_higher(node_lower(lhs.rhs, rhs.lhs), rhs.rhs).simplify()
                    if rhs.rhs == lhs.rhs:
                        return node_higher(node_lower(lhs.lhs, rhs.lhs), rhs.rhs).simplify()
            else:
                if lhs.lhs == rhs and left:
                    return node_higher(rhs, node_lower(lhs.rhs, Constant(1))).simplify()
                elif lhs.rhs == rhs and right:
                    return node_higher(node_lower(lhs.lhs, Constant(1)), rhs).simplify()
        # if just rhs is higher precedence node
        if isinstance(rhs, node_higher):
            if rhs.lhs == lhs and left:
                return node_higher(lhs, node_lower(Constant(1), rhs.rhs)).simplify()
            elif rhs.rhs == lhs and right:
                return node_higher(node_lower(Constant(1), rhs.lhs), lhs).simplify()

    def __contains__(self, item):
        if item == self:
            return True
        return item in self.lhs or item in self.rhs


class AdditionNode(BinaryNode):
    """Represents the addition operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Addition"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # adding a negative is subtraction
        if isinstance(rhs, NegationNode):
            return (lhs - rhs.operand).simplify()

        # 0 is a unit w.r.t +
        if lhs == Constant(0):
            return rhs
        if rhs == Constant(0):
            return lhs

        # adding two of the same elements is multiplication by 2
        if lhs == rhs:
            return (Constant(2) * lhs).simplify()

        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, AdditionNode, MultiplicationNode)
        if distr_simpl is not None:
            return distr_simpl

        if isinstance(lhs, DivisionNode) and isinstance(rhs, DivisionNode):
            if lhs.rhs == rhs.rhs:
                return ((lhs.lhs + rhs.lhs) / lhs.rhs).simplify()

        return super().simplify()


class SubtractionNode(BinaryNode):
    """Represents the subtraction operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Subtraction"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # subtracting a negative is addition
        if isinstance(rhs, NegationNode):
            return (lhs + rhs.operand).simplify()

        # subtracting from 0 is negation
        if lhs == Constant(0):
            return (-rhs).simplify()

        # subtracting 0 does nothing
        if rhs == Constant(0):
            return lhs

        # subtracting the same elements is 0
        if rhs == lhs:
            return Constant(0)

        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, SubtractionNode, MultiplicationNode)
        if distr_simpl is not None:
            return distr_simpl

        if isinstance(lhs, DivisionNode) and isinstance(rhs, DivisionNode):
            if lhs.rhs == rhs.rhs:
                return ((lhs.lhs - rhs.lhs) / lhs.rhs).simplify()

        return super().simplify()


class MultiplicationNode(BinaryNode):
    """Represents the multiplication operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Multiplication"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lderiv = self.lhs.derivative(variable)
        rderiv = self.rhs.derivative(variable)
        return lderiv * self.rhs + self.lhs * rderiv

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # multiplication with 0 is 0
        if lhs == Constant(0) or rhs == Constant(0):
            return Constant(0)

        # 1 is a unit w.r.t. *
        if lhs == Constant(1):
            return rhs
        if rhs == Constant(1):
            return lhs

        # take negation outside of multiplication to simplify with other operators
        if isinstance(lhs, NegationNode):
            if isinstance(rhs, NegationNode):
                # multiplying two negatives yields positives
                return (lhs.operand * rhs.operand).simplify()
            else:
                return (-(lhs.operand * rhs)).simplify()
        elif isinstance(rhs, NegationNode):
            return (-(lhs * rhs.operand)).simplify()

        # multiplication with a fraction should be multiplied with the numerator for further simplification (e.g., 2*1/3 -> 2/3)
        if isinstance(lhs, DivisionNode):
            return ((lhs.lhs * rhs) / lhs.rhs).simplify()
        if isinstance(rhs, DivisionNode):
            return ((lhs * rhs.lhs) / rhs.rhs).simplify()

        # multiplying the same thing is squaring
        if lhs == rhs:
            return (lhs ** Constant(2)).simplify()

        # product of two powers adds the powers
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, AdditionNode, PowerNode, True, False)
        if distr_simpl is not None:
            return distr_simpl

        return super().simplify()


class DivisionNode(BinaryNode):
    """Represents the division operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Division"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def derivative(self, variable: Variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lderiv = self.lhs.derivative(variable)
        rderiv = self.rhs.derivative(variable)
        return (lderiv * self.rhs - self.lhs * rderiv) / self.rhs ** Constant(2)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # dividing by 0 is impossible
        if rhs == Constant(0):
            raise ZeroDivisionError()

        # 0 divided by anything non-zero is 0
        if lhs == Constant(0):
            return Constant(0)

        # dividing by 1 does nothing
        if rhs == Constant(1):
            return lhs

        # do not evaluate division by two integers (as in super().simplify()) but simplify the fraction
        if isinstance(lhs, Constant) and isinstance(rhs, Constant):
            if isinstance(lhs.value, int) and isinstance(rhs.value, int):
                gcd = math.gcd(lhs.value, rhs.value)
                if gcd > 1:
                    return (Constant(lhs.value // gcd) / Constant(rhs.value // gcd)).simplify()
                else:
                    return lhs / rhs

        # take negation outside of a fraction to simplify with other operators
        if isinstance(lhs, NegationNode):
            if isinstance(rhs, NegationNode):
                # divide two negatives yields positives
                return (lhs.operand / rhs.operand).simplify()
            else:
                return (-(lhs.operand / rhs)).simplify()
        elif isinstance(rhs, NegationNode):
            return (-(lhs / rhs.operand)).simplify()

        # dividing by a fraction is multiplication with the reciprocal
        if isinstance(rhs, DivisionNode):
            return (lhs * rhs.rhs / rhs.lhs).simplify()

        # dividing a fraction is multiplication with its denominator
        if isinstance(lhs, DivisionNode):
            return lhs.lhs / (lhs.rhs * rhs).simplify()

        # dividing by a negative power is multiplying with a positive one
        if isinstance(rhs, PowerNode):
            if isinstance(rhs.rhs, NegationNode):
                return (lhs * (rhs.lhs ** rhs.rhs.operand)).simplify()

        # division of two powers adds the powers
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, SubtractionNode, PowerNode, True, False)
        if distr_simpl is not None:
            return distr_simpl

        return super().simplify()


class PowerNode(BinaryNode):
    """Represents the power operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Power"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self.rhs:
            return self.rhs * self.lhs ** (self.rhs - Constant(1)) * self.lhs.derivative(variable)

        if variable not in self.lhs:
            return self * Function("log", self.lhs) * self.rhs.derivative(variable)

        return self * (self.rhs.derivative(variable) * Function("log", self.lhs) + self.rhs * self.lhs.derivative(variable) / self.lhs)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # raising to the 0-th power is 1 (Python evaluates 0**0 as 1)
        if rhs == Constant(0):
            return Constant(1)

        # raising 0 or 1 to any power remains 0 or 1 respectively; raising to the power 1 does nothing
        if lhs == Constant(0) or lhs == Constant(1) or rhs == Constant(1):
            return lhs

        #squaring is the inverse of taking the square root
        if rhs == Constant(2):
            if isinstance(lhs, Function):
                if lhs.base.symbol == "sqrt":
                    return lhs.arguments[0].simplify()

        # a power of a power is the product of the powers (with some adjustments for powers of negatives)
        if isinstance(lhs, PowerNode):
            power = (lhs.rhs * rhs).simplify()
            base = lhs.lhs
            if isinstance(base, NegationNode):
                if isinstance(lhs.rhs, Constant):
                    if float(lhs.rhs).is_integer():
                        if int(lhs.rhs.value) % 2 == 0:
                            base = base.operand
                        return (base ** power).simplify()
            else:
                return (base ** power).simplify()

        return super().simplify()


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
                              "sqrt": FunctionBase("sqrt", math.sqrt, Constant(1) / (Constant(2) * Function("sqrt"))),
                              }


