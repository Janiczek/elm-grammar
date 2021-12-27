# Janiczek/elm-grammar

## Context free grammar parsers

# TODO

* the whole documentation
* mention we can't deal with left recursion
  * mention a failed experiment with instaparse's GLL in the `gll` branch

```
breakfast -> protein ( <" with "> breakfast <" on the side"> )?
breakfast -> bread

protein   -> ("really" <" ">)+ "crispy" <" "> "bacon"
protein   -> "sausage"
protein   -> cooked <" "> "eggs" 

cooked    -> "scrambled" | "poached" | "fried"

bread     -> "toast" | "biscuits" | "English muffin" 
```

* TODO the arithmetic expression example?
* TODO the Lox language example?

* TODO: when running
```
<foo> -> "bar"
foo -> "baz"
```
on "bar", don't show the "foo" tag, but when running it on "baz", show the "foo" tag.

* TODO: mention that it's enough to only hide one tag to hide all its occurences
* TODO: mention that the starting tag cannot be hidden

### Implemented:

```
- Literal:       s  -> "a"
- Tag:           s  -> a
- Concatenation: s  -> a b
- Alternation:   s  -> a | b
- Grouping:      s  -> (a)
- Hiding:        s  -> <a>
- Optional:      s  -> a?
- Zero or more:  s  -> a*
- One or more:   s  -> a+
- Lookahead:     s  -> &a
- Comment:       s  -> (* hello *) "world"
- Hiding tags:  <s> -> "abc"
```

### TODO:

```
- Regex:         s  -> /"[^"]*"/
```
