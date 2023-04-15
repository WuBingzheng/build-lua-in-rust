# Preface

> This series was [written in Chinese](https://wubingzheng.github.io/build-lua-in-rust/zh/) originally. This English version is mainly translated by [Google Translate](https://translate.google.com/). So please forgive me for the terrible writing.

This series of articles introduces the implementation of a Lua interpreter from scratch in the Rust language.

The Rust language has a distinctive personality and is also [widely popular](https://survey.stackoverflow.co/2022/?utm_source=so-owned&utm_medium=announcement-banner&utm_campaign=dev-survey-2022&utm_content=results#section-most-loved-dreaded-and-wanted-programming-scripting-and-markup-languages), however the learning curve is steep. After I finished reading ["Rust Programming Language"](https://doc.rust-lang.org/stable/book/title-page.html) and wrote some practice codes, I deeply felt that I had to go through a larger project practice to understand and master.

[Implementing a Lua Interpreter](http://lua-users.org/wiki/LuaImplementations) is very suitable as this exercise project. Because of its moderate scale, it is enough to cover most of the basic features of Rust without being difficult to reach; [clear goal](https://www.lua.org/manual/5.4/), no need to spend energy discussing requirements; in addition, Lua language It is also an excellently designed and widely used language. Implementing a Lua interpreter can not only practice Rust language skills, but also gain an in-depth understanding of Lua language.

This series of articles documents the learning and exploration process during this project. Similar to other from scratch [Build your own X](https://build-your-own-x.vercel.app/) projects, this project also has a clear big goal, an unknown exploration process and a continuous sense of accomplishment, but with some differences:

- Most of the authors of other projects have been immersed in related fields for many years, but my job is not in the direction of programming language or compilation principles. I don't have complete theoretical knowledge for implementing an interpreter, and I just cross the river by feeling the stones. But think of the good in everything, which also provides a real beginner's perspective.

- Most of the other projects are for the purpose of learning or teaching, simplifying the complexity and realizing a prototype with only the most basic functions. But my goal is to implement a production-level Lua interpreter, pursuing stability, completeness, and performance.

In addition, since the original intention of the project is to learn the Rust language, there will also be some learning notes and usage experience of the Rust language in the article.

## Content

The content is organized as follows. Chapter 1 implements a minimal interpreter that can only parse `print "hello, world!"` statements. Although simple, it includes the complete process of the interpreter and builds the basic framework. Subsequent chapters will gradually add Lua features to this minimal interpreter.

Chapter 2 introduces the most basic concepts of types and variables in programming languages. Chapter 3 introduces several features of the Rust language with the goal of perfecting the string type. Chapter 4 implements the table structure in Lua and introduces the key ExpDesc concept in syntax analysis. Chapter 5 is about tedious arithmetic calculations.

In Chapter 6 Control Structures, things start to get interesting, jumping back and forth between bytecodes based on judgment conditions. Chapter 7 introduces logical and relational operations, combined with the control structures of the previous chapter through specific optimizations.

Chapter 8 introduces functions. The basic concept and implementation of functions are relatively simple, but variable parameters and multiple return values require careful management of the stack. The closure introduced in Chapter 9 is a powerful feature in the Lua language, and the key here is Upvalue and its escape.

Every feature is designed on demand, but not completed in one step like a prophet. Take the conditional jump instruction as example, at the beginning, in order to support the [`if` statement](./ch06-01.if.md), we add `Test(u8, u16)` bytecode, which means if the value of the first associated parameter is *false*, then jump *forward* to the distance represented by the second associated parameter; then in order to support the [`while` statement](./ch06-03.while_break.md) and need to jump *backward*, we change the second associated parameter from `u16` to `i16` type, and use a negative number to represent backward jump; then in order to support [logical operations](./ch07-01.logical_in_condition.md), which may jump if *true* or *false* both, we add `TestAndJump` and `TestOrJump` two bytecodes to replace `Test`. As a result, according to our own learning and development path, we produced a set of bytecodes slightly different from the official version of Lua.

Each chapter starts with Lua's functional features, discussing how to design and then introducing specific implementations. It's not only important to explain "how to do it," but also to explain "why to do it". However, to achieve complete Lua features, some articles may be boring, especially in the first few chapters. Readers can browse the relatively interesting [Definition of String Type](./ch03-01.string_type.md) and [Escape of Upvalue](./ch09-02.escape_and_closure.md), to judge whether this series of articles is to your taste.

Each chapter has a complete [runnable code](https://github.com/WuBingzheng/build-lua-in-rust/tree/main/listing), and the code for each chapter is based on the final code of the previous chapter, ensuring that the entire project is continuous. In the beginning chapters, after introducing the design principles, the code will be explained line by line; later on, only the key parts of the code will be explained; and in the last two chapters will basically not talk about the code.

At present, these chapters only complete the core part of the Lua interpreter, and are still far from a complete interpreter. The [To be continued](./TO_BE_CONTINUED.md) section lists a partial list of unfinished features.

The basic syntax of Lua and Rust will not be explained in this article. We expect readers to have a basic understanding of both languages. The more familiar with the Lua language, the better. There are no high requirements for the Rust language, as long as you have read "Rust Programming Language" and understand the basic grammar. After all, the original intention of this project is to learn Rust. In addition, when it comes to implementing a language interpreter, it will remind people of the difficult compilation principle. However, in reality, since Lua is a very simple language and there is Lua's official implementation of the interpreter code as a reference, this project requires little theoretical knowledge and is mainly focused on practical engineering.

Due to my limited technical ability in compiling principles, Lua language, Rust language, etc., there must be mistakes in projects and articles. In addition, this English version articles are automatically translated from the [Chinese version](https://wubingzheng.github.io/build-lua-in-rust/zh/) mostly, so there may be many not appropriate or not fluent sentences. You are welcome to come to the project's [github homepage](https://github.com/WuBingzheng/build-lua-in-rust) to submit issue feedback.
