# Preface

This series of articles introduces the implementation of a Lua interpreter from scratch in the Rust language.

The Rust language has a distinctive personality and is also [widely popular](https://survey.stackoverflow.co/2022/?utm_source=so-owned&utm_medium=announcement-banner&utm_campaign=dev-survey-2022&utm_content=results#section-most-loved-dreaded-and-wanted-programming-scripting-and-markup-languages), however the learning curve is steep. After I finished reading ["Rust Programming Language"](https://kaisery.github.io/trpl-zh-cn/) and wrote some practice codes, I deeply felt that I had to go through a larger project practice to understand and master.

[Implementing a Lua Interpreter](http://lua-users.org/wiki/LuaImplementations) is very suitable as this exercise project. Because of its moderate scale, it is enough to cover most of the basic features of Rust without being difficult to reach; [clear goal](https://www.lua.org/manual/5.4/), no need to spend energy discussing requirements; in addition, Lua language It is also an excellently designed and widely used language. Implementing a Lua interpreter can not only practice Rust language skills, but also gain an in-depth understanding of Lua language.

This series of articles documents the learning and exploration process during this project. Similar to other projects from scratch [Build your own X](https://build-your-own-x.vercel.app/), this project also has a clear big goal, an unknown exploration process and a continuous sense of accomplishment , but with some differences:

- Most of the authors of other projects have been immersed in related fields for many years, and my work is not in the direction of programming language or compilation principles. I don't have complete theoretical knowledge for implementing an interpreter, and I just cross the river by feeling the stones. But think of the good in everything, which also provides a real beginner's perspective.

- Most of the other projects are for the purpose of learning or teaching, simplifying the complexity and realizing a prototype with only the most basic functions. And my goal is to implement a production-level Lua interpreter, pursuing stability, completeness, and performance.

In addition, since the original intention of the project is to learn the Rust language, there will also be some learning notes and usage experience of the Rust language in the article.

## content

The content is organized as follows. Chapter 1 implements a minimal interpreter that can only parse `print "hello, world!"` statements. Although simple, it includes the complete process of the interpreter and builds the basic framework. Subsequent chapters will start with the Lua language features, and gradually add functions to this minimal interpreter.

Chapter 2 introduces the most basic concepts of types and variables in programming languages. Chapter 3 introduces several features of the Rust language with the goal of perfecting the string type. Chapter 4 implements the table structure in Lua and introduces the key ExpDesc concept in syntax analysis. Chapter 5 is about tedious numerical calculations.

In Chapter 6 Control Structures, things start to get interesting, jumping back and forth between bytecodes based on judgment conditions. Chapter 7 introduces logical and relational operations, combined with the control structures of the previous chapter through specific optimizations.

Chapter 8 introduces functions. The basic concept and implementation of functions are relatively simple, but variable parameters and multiple return values require careful management of the stack. The closure introduced in Chapter 9 is a powerful feature in the Lua language, and the key here is Upvalue and its escape.

Each chapter starts from the functional characteristics of Lua, first discusses how to design, and then introduces the specific implementation. It is not only necessary to clarify "how to do it", but more importantly, "why to do it", and try to avoid writing code and reading notes. However, in order to achieve complete Lua features, there must be some articles that are boring, especially the first few chapters. Readers can browse the relatively interesting [Definition of String Type](./ch03-01.string_type.md) and [Escape of Upvalue](./ch09-02.escape_and_closure.md) to judge this series Whether the article is to taste.

Each chapter has a complete [runnable code](https://github.com/WuBingzheng/build-lua-in-rust/tree/main/listing), and the code of each chapter is based on the previous chapter The final code that guarantees that the entire project is continuous. The code changes corresponding to each section are concentrated in two or three commits, and the change history can be viewed through git. In the first chapters, after introducing the design principles, the code will be explained line by line; in the end, only the key parts of the code will be explained; the last two chapters will basically not talk about the code.

At present, these chapters only complete the core part of the Lua interpreter, and are still far from a complete interpreter. The [To be continued](./TO_BE_CONTINUED.md) section lists a partial list of unfinished features.

The basic syntax of Lua and Rust will not be introduced in this article, we expect readers to have a basic understanding of both languages. Among them, the more familiar with the Lua language, the better. There are no high requirements for the Rust language, as long as you have read "Rust Programming Language" and understand the basic grammar. After all, the original intention of this project is to learn Rust. In addition, when it comes to implementing a language interpreter, it will remind people of the difficult compilation principle, but in fact, because the Lua language is very simple, and there is the official Lua interpreter code as a reference, this project does not require much theory Knowledge is mainly based on engineering practice.

Due to my limited technical ability in compiling principles, Lua language, Rust language, etc., there must be many mistakes in projects and articles; in addition, my language expression ability is very poor, and there will be many words or sentences in the article that do not understand Readers are welcome to come to the project's [github homepage](https://github.com/WuBingzheng/build-lua-in-rust) to submit issue feedback.