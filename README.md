# rox

Tree walk interpreter for the [Lox language](http://www.craftinginterpreters.com/the-lox-language.html) in Rust, following the book [Crafting Interpreters](http://craftinginterpreters.com/). Work in progress.

## Features

- [x] Evaluate expressions (upon numbers, strings and booleans)
- [x] Variable declaration and assignment
- [x] Scopes
- [x] Control Flow (if statements, while and foor loops)
- [x] Functions
- [x] Closures
- [x] Classes
- [x] Inheritance

## Examples

### Fibonacci numbers (naive implementation)

```c
fun fib(n) {
    if (n < 2) {
	return n;
    }
    return fib(n - 1) + fib(n - 2);
}

var result = fib(10);
print(result);
```

### Closures

```c
fun makeCounter() {
    var i = 0;
    fun count() {
        i = i + 1;
        print i;
    }
    return count;
}

var counter = makeCounter();
counter(); // 1
counter(); // 2
```

### OOP and inheritance

```c
class Doughnut {
  cook() {
    print "Fry until golden brown.";
  }
}

class BostonCream < Doughnut {}

BostonCream().cook();
```
