# Lamcalcj

Lamcalcj is a collection of lambda calculus libraries including:

- Basic lambda AST (lamcalcj-ast)
- Utilities (lamcalcj-utils)
- Parser Combinators (lamcalcj-parser)
- Compiler (lamcalcj-compiler)
- Pretty printer (lamcalcj-pretty-print)
- Beta reducer and eta converter (lamcalcj-reducer)

With following features supported:

- Using trampoline to optimize all recursive code path to avoid stack overflow.
- Using parser combinators and customizable syntax for customizing, extending compiler and/or pretty printer.
- Multiple reduction strategies including: head only, evaluation only and their combinations or just using normal order reduction.
- Max step limitation can be given to reducer to avoid lambda terms those have no normal form.
- Other utilities like alpha conversion for renaming, searching free variables, checking whether two terms are alpha equivalent, etc.

# Examples

- GUI Interface: [https://github.com/yuxuanchiadm/lambdacore](https://github.com/yuxuanchiadm/lambdacore)