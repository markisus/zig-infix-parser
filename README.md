Create an expression object at comptime.
```zig
const E = CompileExpression("x*(x+1)*y-3*y");
```

Now execute that expression at runtime by binding variables with an anonyous struct.  
```zig
const result : f64 = E.eval(.{ .x=1.234, .y=3.456 });
```
