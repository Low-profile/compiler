# final project

This repo is for the final project of the course functional programming studio.
The project is to implement a compiler for a subset of c language in scala pure functionally.

The code is wrote from scratch, the only library used is from here [scala library](https://github.com/mrksr/llvmil) to emit llvm IR code,
however,the library is not implemented pure functional.
And I modified few lines of code to fit my project.

I used [here](https://www2.cs.arizona.edu/~debray/Teaching/CSc453/DOCS/cminusminusspec.html) for the language specification.

Implemented the parser combinator from scratch, referenced from [here](https://fsharpforfunandprofit.com/series/understanding-parser-combinators.html),
which is a monadic parser combinator tutorial in F#.



## Usage
```bash
sbt assembly

#will generate a jar file at the location showed in prompt

java -jar jarfile sourceFileName outputFileName

#will generate the llvm IR code at the outputFileName, if outputFileName is not given, the default name will be test.ll
```

## testcase
This project doesn't support the fullset of the c language.

The syntax showed testcase folder are supported ones, which is just a subset of the language.

## runtime

The function declaration showed at the head of the file can be omitted now, it is designed as a small runtime library written in C
