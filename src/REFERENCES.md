# 参考文献

- [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/)，也是这个项目的需求文档。

- 《Lua程序设计（第4版）》，Lua官方教程。虽然是基于Lua 5.3版本，但是由于5.4版本的[变化并不多](http://www.lua.org/manual/5.4/readme.html#changes)，所以影响不大。

- 《Lua设计与实现》，感觉像是一份Lua官方实现的源码阅读笔记，直接讲代码实现细节，刚上手看时很吃力。

- 《自己动手实现Lua》，跟本系列文章很像，也是从零实现一个Lua解释器。但是这本书是以Lua官方实现里的字节码定义为出发点，先实现虚拟机去执行字节码，然后再实现编译器去生成字节码。而我们这系列文章是以Lua语言手册为出发点，设计并实现编译过程、虚拟机、字节码定义等。

- [Why is there no continue statement?](https://www.luafaq.org/#T1.26)，对Lua中为什么没有continue语句的解释。

- [《Rust程序设计语言》](https://kaisery.github.io/trpl-zh-cn/)，Rust官方教程。

- [Rust官方文档](https://doc.rust-lang.org/)，主要是参考其中的标准库部分。

- [Designing a GC in Rust](https://manishearth.github.io/blog/2015/09/01/designing-a-gc-in-rust/)，介绍用Rust实现GC的设计思路。

- [gc-crate](https://crates.io/crates/gc)，基于上述设计思路的一个实现。

- [A Tour of Safe Tracing GC Designs in Rust](https://manishearth.github.io/blog/2021/04/05/a-tour-of-safe-tracing-gc-designs-in-rust/)，介绍一个用Rust实现的GC设计。我只记得其中一点：用Rust实现GC是很难的。

- [Implementing a safe garbage collector in Rust](https://coredumped.dev/2022/04/11/implementing-a-safe-garbage-collector-in-rust/)，另外一个用Rust实现GC的项目。

- [When Zig is safer and faster than Rust](https://zackoverflow.dev/writing/unsafe-rust-vs-zig/)，以Roc语言使用Zig而非Rust来实现GC部分为出发点，来说明用unsafe Rust来实现某些功能是很困难的。

- [Luster](https://github.com/kyren/luster)，用Rust实现的Lua解释器，也是用的GC而非RC，但项目没完成。

- [The Story of Tail Call Optimizations in Rust](https://dev.to/seanchen1991/the-story-of-tail-call-optimizations-in-rust-35hf)，对Rust语言支持尾调用的讨论。

- [Lua bindings: lua, hlua or rlua?](https://www.reddit.com/r/rust/comments/8coe49/lua_bindings_lua_hlua_or_rlua/)，Reddit上对现有的3个Lua crate：lua、hlua和rlua的简单对比。

- [A Survey of Rust Embeddable Scripting Languages](https://www.boringcactus.com/2020/09/16/survey-of-rust-embeddable-scripting-languages.html)，对几个可以在Rust中使用的脚本语言（包括Lua）在使用方式上的对比。

- [Implement TryFrom for float to integer types](https://github.com/rust-lang/rust/pull/47857)。

- [Floating Point Arcade](https://gist.github.com/CrockAgile/09065649ae5a52629599ebc5645922d6)，把整型随机数转换为浮点数的介绍。

- [The Rust Performance Book](https://nnethercote.github.io/perf-book/title-page.html)。
