-define(ctime(Pid, Label, Expr), ctime(Pid, Label, fun() -> Expr end)).