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

    # evaluate the expression with values for variables and functions with a dictionary
    # keys are variables as strings with concrete values
    # keys are function names as strings with FunctionBase as values
    def evaluate(self, substitutions_unknowns={}):
        raise NotImplementedError("evaluation for the following expression was not possible: %s" % self)

    # substitute expressions for variables and functions with a dictionary
    # keys of variables are strings with Expressions as values
    # keys of functions are strings with values either:
    #     a tuple with first an Expression and then a list of variables used
    #     just an Expression, provided it use FunctionBase.VAR as its variables
    def substitute(self, substitutions_unknowns, find_derivatives=True):
        return self

    # variable can either be a string or of type Variable
    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self:
            return Constant(0)

        raise NotImplementedError("the following expression could not be differentiated: %s w.r.t. %s" % (self, variable))

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self:
            return self * variable

        raise NotImplementedError("the following expression could not be integrated: %s w.r.t. %s" % (self, variable))

    def simplify(self):
        return self

    def __contains__(self, item):
        return item == self

    # remaining methods

    # compute integral numerically on [lower_bound, upper_bound] w.r.t. variable.
    # variable can be a string or of type Variable
    # substitutions_unknowns works the same as in evaluate()
    def numeric_integral(self, variable, lower_bound, upper_bound, step_count, substitutions_unknowns=None):
        if substitutions_unknowns is None:
            substitutions_unknowns = {}

        if isinstance(variable, Variable):
            variable = variable.symbol
        if variable in substitutions_unknowns:
            raise KeyError("integration variable '%s' is substituted" % variable)

        step_size = (upper_bound - lower_bound) / step_count
        # make a partition of points for a middle Riemann sum
        partition = [lower_bound + step_size / 2 + step_size * i for i in range(step_count)]
        summation = 0
        for s in partition:
            substitutions_unknowns.update({variable: s})
            summation += step_size * self.evaluate(substitutions_unknowns)
        return summation

    # checks if self is of the form a*variable + b for non-zero a
    def is_linear(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        der = self.derivative(variable).simplify()
        return (not der == Constant(0)) and variable not in der

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
                # pop everything up to the left parenthesis to the output, but leave that on the stack
                try:
                    while not stack[-1] == "(":
                        output.append(stack.pop())
                except IndexError as e:
                    raise IndexError("misplaced function separator or mismatched parentheses") from e
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
                try:
                    while not stack[-1] == "(":
                        output.append(stack.pop())
                    # pop the left parenthesis from the stack (but not to the output)
                    stack.pop()
                except IndexError as e:
                    raise IndexError("mismatched parentheses") from e
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
                try:
                    x = stack.pop()
                except IndexError as e:
                    raise IndexError("missing argument for '%s' operator" % t) from e
                stack.append(eval("%sx" % t))
            elif t in oplist:
                # let eval and operator overloading take care of figuring out what to do
                try:
                    y = stack.pop()
                    x = stack.pop()
                except IndexError as e:
                    raise IndexError("missing argument(s) for '%s' operator" % t) from e
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

    def simplify(self):
        # set negative constants to negated positive ones for further simplifications
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
            try:
                return Variable.BUILTIN_CONSTANTS[self.symbol]
            except KeyError as e:
                raise KeyError("variable '%s' was unspecified" % self.symbol) from e

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

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable == self:
            return self ** Constant(2) / Constant(2)
        else:
            return self * variable

    def __contains__(self, item):
        if isinstance(item, str):
            return item == self.symbol
        else:
            return item == self


class FunctionBase:
    """Represents a standard function, with evaluation method and derivatives"""

    # default variables for creating functions, optionally iterable with square brackets
    class VARGetter(Variable):
        def __getitem__(self, key):
            if isinstance(key, slice):
                if key.stop is None:
                    raise IndexError("upper bound for VAR must be finite")
                else:
                    return [self[i] for i in range(*key.indices(key.stop))]
            else:
                return Variable("__VAR%d__" % key)
    VAR = VARGetter("__VAR0__")

    def __init__(self, symbol: str, executable=None, derivatives=None, integrals=None, variables=None):
        self.symbol = symbol
        self.executable = executable  # a Python function/method

        # of type Expression
        if derivatives is None:
            self._derivatives = []
        elif not isinstance(derivatives, list):
            self._derivatives = [derivatives]
        else:
            self._derivatives = derivatives

        # of type Expression
        if integrals is None:
            self._integrals = []
        elif not isinstance(integrals, list):
            self._integrals = [integrals]
        else:
            self._integrals = integrals

        if variables is None:
            self.variables = FunctionBase.VAR[0:len(self._derivatives)]
        else:
            self.variables = list(variables)  # the variables to be substituted in self.derivative, in order

    # TODO: implement integral

    @property
    def derivatives(self):
        if self._derivatives == []:
            return [Function(FunctionBase(self.symbol + "_1"))]
        else:
            return self._derivatives

    def has_derivative(self, index=0):
        try:
            self._derivatives[index]
            return True
        except IndexError:
            return False

    @property
    def integrals(self):
        if self._integrals == []:
            return [Function(FunctionBase(self.symbol + "^1"))]
        else:
            return self._integrals

    def has_integral(self, index=0):
        try:
            self._integrals[index]
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

    # base is of type FunctionBase, or just a string
    # *args represent the arguments of the function, which should be Expressions
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
            return self.base.symbol == other.base.symbol and self.arguments == other.arguments
        else:
            return False

    def __str__(self):
        return "%s(%s)" % (self.base.symbol, ", ".join([str(a) for a in self.arguments]))

    def evaluate(self, substitutions_unknowns={}):
        try:
            f = substitutions_unknowns[self.base.symbol].executable
        except KeyError:
            try:
                f = Function.BUILTIN_FUNCTIONS[self.base.symbol].executable
            except KeyError as e:
                raise KeyError("function '%s' was unspecified" % self.base.symbol) from e
        return f(*[a.evaluate(substitutions_unknowns) for a in self.arguments])

    # TODO: add support for undefined integrals
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
                        # get the index of the variable with respect to which the derivative was taken (e.g., f_j -> j-1)
                        try:
                            var_index_stack.append(int(fn_name[i+1:]) - 1)
                        except ValueError:
                            break
                        # get the name of the original function (e.g., f_j -> f)
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
                    # repeatedly take the correct derivative to get back to the unknown derivative, where we started
                    # also keep substituting in case there are nested functions of which we found newly unknown derivatives
                    while len(var_index_stack) > 0:
                        f = f.derivative(variables[var_index_stack.pop()]).substitute(substitutions_unknowns)
                    f_var_subst = dict(zip(variables, [a.substitute(substitutions_unknowns) for a in self.arguments]))
                    return f.substitute(f_var_subst)

        # the function itself was not to be substituted, so pass the substitution on to its arguments
        return Function(self.base, *[a.substitute(substitutions_unknowns) for a in self.arguments])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        # find the derivative w.r.t. each variable and create a dummy function if it does not exist
        diffs = []  # derivatives w.r.t. the variable of each component
        subst_vars_str = []  # the variables for which the arguments are substituted
        dummy_indices = []
        for i in range(len(self.arguments)):
            if self.base.has_derivative(i):
                diffs.append(self.base.derivatives[i])
                subst_vars_str.append(str(self.base.variables[i]))
            else:
                try:
                    diffs.append(Function.BUILTIN_FUNCTIONS[self.base.symbol].derivatives[i])
                    subst_vars_str.append(str(Function.BUILTIN_FUNCTIONS[self.base.symbol].variables[i]))
                except (KeyError, IndexError):
                    # if the derivative could not be found, create a dummy function
                    diffs.append(Function("%s_%d" % (self.base.symbol, i+1), *self.arguments))
                    subst_vars_str.append(str(FunctionBase.VAR[i]))
                    dummy_indices.append(i)

        # plug the arguments into the variables of each differentiated function and multiply with the derivative of the corresponding component (chain rule)
        diff_components = []
        for i in range(len(self.arguments)):
            if variable in self.arguments[i]:
                if i in dummy_indices:
                    # dummy functions already have the arguments plugged in
                    diff_components.append(diffs[i] * self.arguments[i].derivative(variable))
                else:
                    # here False in substitute(), because trying to find derivatives is redundant, as we know they are not there by construction (the dummy functions)
                    diff_components.append(diffs[i].substitute(dict(zip(subst_vars_str, self.arguments)), False) * self.arguments[i].derivative(variable))

        # add everything together (chain rule)
        if len(diff_components) > 0:
            result = diff_components[0]
            for i in range(1, len(diff_components)):
                result += diff_components[i]
            return result
        else:
            # the variable appeared in none of the components, so the derivative is 0
            return Constant(0)

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)
        args_with_var = []
        index = -1
        for i in range(len(self.arguments)):
            if variable in self.arguments[i]:
                args_with_var.append(self.arguments[i])
                index = i
        # we can generally only integrate if one argument contains the variable
        if len(args_with_var) == 1:
            arg = args_with_var[0]
            subst_var_str = []
            is_dummy = False
            if arg.is_linear(variable):
                integral = None
                if self.base.has_integral(index):
                    integral = self.base.integrals[index]
                    subst_var_str = [str(v) for v in self.base.variables]
                else:
                    try:
                        integral = Function.BUILTIN_FUNCTIONS[self.base.symbol].integrals[index]
                        subst_var_str = [str(v) for v in FunctionBase.VAR[:len(self.arguments)]]
                    except (KeyError, IndexError):
                        # create a dummy function f^(index+1)
                        integral = Function("%s^%d" % (self.base.symbol, index+1), *self.arguments)
                        is_dummy = True

                # substitute the variables and multiply with the derivative of the argument containing variable
                if is_dummy:
                    return integral / arg.derivative(variable)
                else:
                    return integral.substitute(dict(zip(subst_var_str, self.arguments))) / arg.derivative(variable)

        return super().integral(variable)

    # TODO: simplify derivatives and integrals (e.g., f_1^1 -> f)
    def simplify(self):
        arguments = [a.simplify() for a in self.arguments]

        # log(b, b) = 1
        if self.base.symbol == "log":
            if (len(arguments) == 1 and arguments[0] == Variable("e")) or\
               (len(arguments) > 1 and arguments[0] == arguments[1]):
                return Constant(1)

            # log(exp(x)) = x
            if len(arguments) == 1 or arguments[1] == Variable("e"):
                if isinstance(arguments[0], Function):
                    if arguments[0].base.symbol == "exp":
                        return arguments[0].arguments[0].simplify()

        # exp(log(x)) = x
        if self.base.symbol == "exp":
            if isinstance(arguments[0], Function):
                if arguments[0].base.symbol == "log" and arguments[0].arguments[1] == Variable("e"):
                    return arguments[0].arguments[0].simplify()

        # lg(2) = 1
        if self.base.symbol == "lg":
            if self.arguments[0] == Constant(2):
                return Constant(1)

        # trigonometric functions composed with their inverse counterparts
        if self.base.symbol in ["sin", "cos", "tan", "sinh", "cosh", "tanh"]:
            if isinstance(arguments[0], Function):
                if "a" + self.base.symbol == arguments[0].base.symbol:
                    return arguments[0].arguments[0].simplify()
        if self.base.symbol in ["asin", "acos", "atan", "asinh", "acosh", "atanh"]:
            if isinstance(arguments[0], Function):
                if self.base.symbol[1:] == arguments[0].base.symbol:
                    return arguments[0].arguments[0].simplify()

        return Function(self.base, *arguments)

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

    def __init__(self, operand: Expression, op_symbol: str):
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

    def simplify(self, operand=None):
        if operand is None:
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

        return -self.operand.derivative(variable)

    def integral(self, variable):
        return -self.operand.integral(variable)

    def simplify(self):
        operand = self.operand.simplify()

        # undo double negation
        if isinstance(operand, NegationNode):
            return operand.operand

        # -0 is 0
        if operand == Constant(0):
            return Constant(0)

        return super().simplify(operand)


class BinaryNode(OperatorNode):
    """A node in the expression tree representing a binary operator."""

    def __init__(self, lhs: Expression, rhs: Expression, op_symbol: str, is_commutative):
        super().__init__(op_symbol, Expression.ASSOCIATIVITY[op_symbol].left, Expression.ASSOCIATIVITY[op_symbol].right, Expression.PRECEDENCE[op_symbol])
        self.lhs = lhs
        self.rhs = rhs
        self.is_commutative = is_commutative

        if isinstance(self.lhs, int) or isinstance(self.rhs, int):
            print("one side int:", self)

    def __eq__(self, other):
        if type(self) == type(other):
            if self.is_commutative:
                return (self.lhs == other.lhs and self.rhs == other.rhs) or\
                       (self.lhs == other.rhs and self.rhs == other.lhs)
            else:
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
        # parentheses are necessary in case l/r-value be a negative int/float for powers (then, e.g., (-2)**2 would be evaluated as -2**2 instead)
        return eval("(lvalue) %s (rvalue)" % self.op_symbol)

    def substitute(self, substitutions_unknowns, find_derivatives=True):
        lvalue = self.lhs.substitute(substitutions_unknowns)
        rvalue = self.rhs.substitute(substitutions_unknowns)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def derivative(self, variable):
        lvalue = self.lhs.derivative(variable)
        rvalue = self.rhs.derivative(variable)
        return eval("lvalue %s rvalue" % self.op_symbol)

    def simplify(self, lhs=None, rhs=None):
        if lhs is None:
            lhs = self.lhs.simplify()
        if rhs is None:
            rhs = self.rhs.simplify()

        # evaluate operations between two constants
        if isinstance(lhs, Constant) and isinstance(rhs, Constant):
            return Constant(eval("%s %s %s" % (lhs.value, self.op_symbol, rhs.value))).simplify()

        # attempt to simplify two consecutive same-precedence operators by reorganizing the expression tree
        # e.g., if a*b/c does not simplify, try (a/c)*b and then a*(b/c)
        if isinstance(lhs, BinaryNode):
            if lhs.precedence == self.precedence:
                if self.is_commutative:
                    operandslr = eval("lhs.lhs %s rhs" % self.op_symbol)
                    operandslr_simpl = operandslr.simplify()
                    if operandslr == operandslr_simpl:
                        operandsrr = eval("rhs %s lhs.rhs" % lhs.op_symbol)
                        operandsrr_simpl = operandsrr.simplify()
                        if not operandsrr == operandsrr_simpl:
                            return eval("lhs.lhs %s operandsrr_simpl" % self.op_symbol).simplify()
                    else:
                        return eval("operandslr_simpl %s lhs.rhs" % lhs.op_symbol).simplify()
                else:
                    operandslr = eval("lhs.lhs %s rhs" % self.op_symbol)
                    operandslr_simpl = operandslr.simplify()
                    if operandslr == operandslr_simpl:
                        if lhs.is_commutative:
                            operandsrr = eval("lhs.rhs %s rhs" % self.op_symbol)
                            operandsrr_simpl = operandsrr.simplify()
                            if not operandsrr == operandsrr_simpl:
                                return eval("lhs.lhs %s operandsrr_simpl" % lhs.op_symbol).simplify()
                    else:
                        return eval("operandslr_simpl %s lhs.rhs" % lhs.op_symbol).simplify()
        if isinstance(rhs, BinaryNode):
            if rhs.precedence == self.precedence:
                if self.is_commutative:
                    operandsll = eval("lhs %s rhs.lhs" % self.op_symbol)
                    operandsll_simpl = operandsll.simplify()
                    if operandsll == operandsll_simpl:
                        operandslr = eval("lhs %s rhs.rhs" % rhs.op_symbol)
                        operandslr_simpl = operandslr.simplify()
                        if not operandslr == operandslr_simpl:
                            return eval("operandslr_simpl %s rhs.lhs" % self.op_symbol).simplify()
                    else:
                        return eval("operandsll_simpl %s rhs.rhs" % rhs.op_symbol).simplify()

        # if all else fails, return the operation between the (simplified) sides

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

    def integral(self, variable):
        return self.lhs.integral(variable) + self.rhs.integral(variable)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # adding a negative is subtraction
        if isinstance(rhs, NegationNode):
            return (lhs - rhs.operand).simplify()
        if isinstance(lhs, NegationNode):
            return (rhs - lhs.operand).simplify()

        # 0 is a unit w.r.t +
        if lhs == Constant(0):
            return rhs
        if rhs == Constant(0):
            return lhs

        # adding two of the same elements is multiplication by 2
        if lhs == rhs:
            return (Constant(2) * lhs).simplify()

        # simplify according to distributivity with multiplication
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, AdditionNode, MultiplicationNode)
        if distr_simpl is not None:
            return distr_simpl

        # simplify fractions with the same denominator
        if isinstance(lhs, DivisionNode) and isinstance(rhs, DivisionNode):
            if lhs.rhs == rhs.rhs:
                return ((lhs.lhs + rhs.lhs) / lhs.rhs).simplify()

        return super().simplify(lhs, rhs)


class SubtractionNode(BinaryNode):
    """Represents the subtraction operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Subtraction"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def integral(self, variable):
        return self.lhs.integral(variable) - self.rhs.integral(variable)

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

        # handle the special cases x-a-b, a-(x±b) and a-(b±x) when a+b simplifies
        if isinstance(lhs, SubtractionNode):
            operandsrr = lhs.rhs + rhs
            operandsrr_simpl = operandsrr.simplify()
            if not operandsrr == operandsrr_simpl:
                return (lhs.lhs - operandsrr_simpl).simplify()
        if isinstance(rhs, SubtractionNode):
            return (lhs - rhs.lhs + rhs.rhs).simplify()
        if isinstance(rhs, AdditionNode):
            return (lhs - rhs.lhs - rhs.rhs).simplify()

        # simplify according to distributivity with multiplication
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, SubtractionNode, MultiplicationNode)
        if distr_simpl is not None:
            return distr_simpl

        # simplify fractions with the same denominator
        if isinstance(lhs, DivisionNode) and isinstance(rhs, DivisionNode):
            if lhs.rhs == rhs.rhs:
                return ((lhs.lhs - rhs.lhs) / lhs.rhs).simplify()

        return super().simplify(lhs, rhs)


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

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self.lhs:
            return self.lhs * self.rhs.integral(variable)
        elif variable not in self.rhs:
            return self.lhs.integral(variable) * self.rhs

        return super().integral(variable)

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

        # product of two exponentiations yields addition of the exponents
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, AdditionNode, PowerNode, True, False)
        if distr_simpl is not None:
            return distr_simpl

        return super().simplify(lhs, rhs)


class DivisionNode(BinaryNode):
    """Represents the division operator"""

    def __init__(self, lhs, rhs):
        op_symbol = Expression.OPERATOR_LIST["Division"]
        super().__init__(lhs, rhs, op_symbol, Expression.COMMUTATIVITY[op_symbol])

    def derivative(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        lderiv = self.lhs.derivative(variable)
        rderiv = self.rhs.derivative(variable)
        return (lderiv * self.rhs - self.lhs * rderiv) / self.rhs ** Constant(2)

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self.rhs:
            return self.lhs.integral(variable) * self.rhs

        if variable not in self.lhs:
            if self.rhs == variable:
                return self.lhs * Function("log", Function("abs", variable))

            if isinstance(self.rhs, PowerNode):
                return self.lhs * (self.rhs.lhs ** -self.rhs.rhs).integral(variable)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # dividing by 0 is impossible
        if rhs == Constant(0):
            raise ZeroDivisionError("right-hand side of the following expression is zero: %s" % self)

        # 0 divided by anything non-zero is 0
        if lhs == Constant(0):
            return Constant(0)

        # dividing by 1 does nothing
        if rhs == Constant(1):
            return lhs

        # dividing the same things yields 1
        if lhs == rhs:
            return Constant(1)

        # do not evaluate division by two integers (as in super().simplify()), but simplify the fraction,
        # lest we get long floats such as 0.3333333333333333 instead of 1/3
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
                # dividing two negatives yields positives
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

        # division of two exponentiations yields subtraction of the exponents
        distr_simpl = BinaryNode.simplify_distributively(lhs, rhs, SubtractionNode, PowerNode, True, False)
        if distr_simpl is not None:
            return distr_simpl

        return super().simplify(lhs, rhs)


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

    def integral(self, variable):
        if isinstance(variable, str):
            variable = Variable(variable)

        if variable not in self.rhs:
            if self.lhs.is_linear(variable):
                if self.rhs == -Constant(1):
                    return Function("log", Function("abs", self.lhs)) / self.lhs.derivative(variable)
                else:
                    return self.lhs ** (self.rhs + Constant(1)) / (self.rhs + Constant(1)) / self.lhs.derivative(variable)

        if variable not in self.lhs:
            if self.rhs.is_linear(variable):
                return self / Function("log", self.lhs) / self.rhs.derivative(variable)

        super().integral(variable)

    def simplify(self):
        lhs = self.lhs.simplify()
        rhs = self.rhs.simplify()

        # raising to the 0-th power is 1 (Python evaluates 0**0 as 1)
        if rhs == Constant(0):
            return Constant(1)

        # raising 0 or 1 to any power remains 0 or 1, respectively; raising to the power 1 does nothing
        if lhs == Constant(0) or lhs == Constant(1) or rhs == Constant(1):
            return lhs

        # squaring is the inverse of taking the square root
        if rhs == Constant(2):
            if isinstance(lhs, Function):
                if lhs.base.symbol == "sqrt":
                    return lhs.arguments[0].simplify()

        # negative powers is division by the positive power
        if isinstance(rhs, NegationNode):
            return (Constant(1) / lhs ** rhs.operand).simplify()

        # simplify negated bases if the exponent is an integer
        if isinstance(lhs, NegationNode):
            if isinstance(rhs, Constant):
                if float(rhs).is_integer():
                    if int(rhs.value) % 2 == 0:
                        return (lhs.operand ** rhs).simplify()
                    else:
                        return (-(lhs.operand ** rhs)).simplify()

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

        return super().simplify(lhs, rhs)


Function.BUILTIN_FUNCTIONS = {"sin": FunctionBase("sin", math.sin, Function("cos"),
                                                                   -Function("cos")),
                              "cos": FunctionBase("cos", math.cos, -Function("sin"),
                                                                   Function("sin")),
                              "tan": FunctionBase("tan", math.tan, Constant(1) + Function("tan")**Constant(2),
                                                                   -Function("log", Function("cos"))),
                              "asin": FunctionBase("asin", math.asin, Constant(1) / Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2)),
                                                                      Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2)) + FunctionBase.VAR * Function("asin")),
                              "acos": FunctionBase("acos", math.acos, -Constant(1) / Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2)),
                                                                      FunctionBase.VAR * Function("acos") - Function("sqrt", Constant(1) - FunctionBase.VAR**Constant(2))),
                              "atan": FunctionBase("atan", math.atan, Constant(1) / (FunctionBase.VAR**Constant(2) + Constant(1)),
                                                                      FunctionBase.VAR * Function("atan") - Function("log", FunctionBase.VAR**Constant(2) + Constant(1)) / Constant(2)),
                              "atan2": FunctionBase("atan2", math.atan2),  # atan2(y, x)
                              "sinh": FunctionBase("sinh", math.sinh, Function("cosh"),
                                                                      Function("cosh")),
                              "cosh": FunctionBase("cosh", math.cosh, Function("sinh"),
                                                                      Function("sinh")),
                              "tanh": FunctionBase("tanh", math.tanh, Constant(1) - Function("tanh")**Constant(2),
                                                                      Function("log", Function("cosh"))),
                              "asinh": FunctionBase("asinh", math.asinh, Constant(1) / Function("sqrt", FunctionBase.VAR**Constant(2) + Constant(1)),
                                                                         FunctionBase.VAR * Function("asinh") - Function("sqrt", FunctionBase.VAR**Constant(2) + Constant(1))),
                              "acosh": FunctionBase("acosh", math.acosh, Constant(1) / Function("sqrt", FunctionBase.VAR**Constant(2) - Constant(1)),
                                                                         FunctionBase.VAR * Function("acosh") - Function("sqrt", FunctionBase.VAR**Constant(2) - Constant(1))),
                              "atanh": FunctionBase("atanh", math.atanh, Constant(1) / (Constant(1) - FunctionBase.VAR**Constant(2)),
                                                                         FunctionBase.VAR * Function("atanh") + Function("log", Constant(1) - FunctionBase.VAR**Constant(2)) / Constant(2)),
                              "log": FunctionBase("log", math.log, [Constant(1) / (FunctionBase.VAR * Function("log", Variable("e"))), Constant(0)],
                                                                   [FunctionBase.VAR * (Function("log") - Constant(1)) / Function("log", Variable("e")), Constant(0)], [FunctionBase.VAR, Variable("e")]),  # log(x, base=e)
                              "lg": FunctionBase("lg", math.log2, Constant(1) / (FunctionBase.VAR * Function("log", Constant(2))),
                                                                  FunctionBase.VAR * (Function("log") - Constant(1)) / Function("log", Constant(2))),
                              "exp": FunctionBase("exp", math.exp, Function("exp"),
                                                                   Function("exp")),
                              "ceil": FunctionBase("ceil", math.ceil),
                              "floor": FunctionBase("floor", math.floor),
                              "factorial": FunctionBase("factorial", math.factorial),
                              "abs": FunctionBase("abs", math.fabs),
                              "sqrt": FunctionBase("sqrt", math.sqrt, Constant(1) / (Constant(2) * Function("sqrt")),
                                                                      Constant(2) / Constant(3) * FunctionBase.VAR * Function("sqrt")),
                              }


# f = Expression.from_string("{0}**2+{1}".format(*FunctionBase.VAR[:2]))
# print(Expression.from_string("f(a*x+b, c*y+d)").derivative("x").simplify().integral("x").simplify())
string = "sqrt(x)"
print(Expression.from_string(string))
print(Expression.from_string(string).integral("x").simplify())
print(Expression.from_string(string).integral("x").simplify().derivative("x").simplify())