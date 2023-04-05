# references

- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/), which is also the requirements document for this project.

- "Lua Programming (4th Edition)", the official Lua tutorial. Although it is based on the Lua 5.3 version, it has little impact due to the [not many changes] (http://www.lua.org/manual/5.4/readme.html#changes) of the 5.4 version.

- "Lua Design and Implementation", it feels like a source code reading note of Lua's official implementation. It directly talks about the details of the code implementation, and it is very difficult to read when you first get started.

- "Implementing Lua by Yourself", which is very similar to this series of articles, also implements a Lua interpreter from scratch. But this book is based on the bytecode definition in the official implementation of Lua as the starting point. First implement the virtual machine to execute the bytecode, and then implement the compiler to generate the bytecode. And our series of articles is based on the Lua language manual, designing and implementing the compilation process, virtual machine, bytecode definition, etc.

- [Why is there no continue statement?](https://www.luafaq.org/#T1.26), an explanation of why there is no continue statement in Lua.

- [《Rust Programming Language》](https://kaisery.github.io/trpl-zh-cn/), the official Rust tutorial.

- [Official Rust Documentation](https://doc.rust-lang.org/), mainly refer to the standard library part.

- [Designing a GC in Rust](https://manishearth.github.io/blog/2015/09/01/designing-a-gc-in-rust/), introduces the design idea of implementing GC in Rust.

- [gc-crate](https://crates.io/crates/gc), an implementation based on the above design ideas.

- [A Tour of Safe Tracing GC Designs in Rust](https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/) , introducing a GC design implemented in Rust. I just remember one thing: implementing GC in Rust is hard.

- [Implementing a safe garbage collector in Rust](https://coredumped.dev/2022/04/11/implementing-a-safe-garbage-collector-in-rust/), another project that uses Rust to implement GC.

- [When Zig is safer and faster than Rust](https://zackoverflow.dev/writing/unsafe-rust-vs-zig/), starting from Roc language using Zig instead of Rust to implement the GC part, to illustrate the use It is difficult to implement certain functions in unsafe Rust.

- [Luster](https://github.com/kyren/luster), a Lua interpreter implemented in Rust, also uses GC instead of RC, but the project is not completed.

- [The Story of Tail Call Optimizations in Rust](https://dev.to/seanchen1991/the-story-of-tail-call-optimizations-in-rust-35hf), a discussion of tail call support in the Rust language.

- [Lua bindings: lua, hlua or rlua?](https://www.reddit.com/r/rust/comments/8coe49/lua_bindings_lua_hlua_or_rlua/), there are three existing Lua crates on Reddit: lua, hlua and A simple comparison of rlua.

- [A Survey of Rust Embeddable Scripting Languages](https://www.boringcactus.com/2020/09/16/survey-of-rust-embeddable-scripting-languages.html), for several that can be used in Rust A comparison of the usage of different scripting languages (including Lua).

- [Implement TryFrom for float to integer types](https://github.com/rust-lang/rust/pull/47857),

- [Floating Point Arcade](https://gist.github.com/CrockAgile/09065649ae5a52629599ebc5645922d6), an introduction to converting integer random numbers to floating point numbers.

- [The Rust Performance Book](https://nnethercote.github.io/perf-book/title-page.html).