# PokéLang (initial interpreter)

PokéLang is a tiny experimental interpreter whose syntax is Java-like but uses Pokémon-themed types and names.

Types:
- `Pikachu` -> integer
- `Charizard` -> char
- `Bulbasaur` -> boolean
- `Squirtle` -> float
- `Mew` -> string

Printing
- Use `Pokedex.println(expr);` to print values (similar to `System.out.println`).

Features included in this initial implementation
- Tokenizer, parser, and interpreter (single-file) in `pokelang/interpreter.py`
- Variable declarations, assignments, arithmetic, boolean operators
- String and char literals
- Simple function call support (Pokedex.println)

Usage
1. Put your PokéLang program in a `.poke` (or any) file, for example `examples/hello.poke`.
2. Run:
   ```
      python pokelang/interpreter.py examples/hello.poke
         ```

         Example
         ```
         Pikachu a = 2;
         Pikachu b = 3;
         Pokedex.println(a + b); // prints 5

         Mew s = "Hello, world!";
         Pokedex.println(s);
         ```

         Extending
         - Add more built-in functions (e.g., Pokedex.readLine).
         - Add control flow (if, while, for) and block scoping.
         - Add arrays/objects and a small standard library.

         Notes
         This is an educational, minimal interpreter. It is intentionally small to make experimentation easy.