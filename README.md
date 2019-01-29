### JCC - COMP 442
The application is divided into a compiler, which acts as the driver and a tokenizer, which removes
comments and tokenizes an input string. The tokenizer returns a sequence of token objects which are
identified by their case class names and a value parameter. This assignment was written in Scala to
leverage the powerful pattern matching mechanics of the langue. Scala allows the extraction of regular
expression groups into variables with simple match/case statements which makes parsing tokens much easier
than in traditional languages. Scala unit testing is also very easy using the scalatest library. English
like spec files were created to validate the functioning of the tokenizer. The automata object stores key
tags that represent automata states as well as a list of reserved words that are matched once an ID token
is located. The AtoCCConverter takes a sequence of tokens as input and converts it to a string of AtoCC
formatted values that are then outputted to a file by the compiler. Right now the program is setup to
read from the resource folder where 3 program files are stored however, additional program files can
easily be added and analyzed by dropping a new program with program#.txt naming convention and
incrementing the programFiles variable.