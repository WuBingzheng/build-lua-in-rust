#[derive(Debug)]
pub enum ByteCode {
    // global variable
    GetGlobal(u8, u8),
    SetGlobal(u8, u8),
    SetGlobalConst(u8, u8), // TODO u8?

    // local variable
    LoadConst(u8, u16),
    LoadNil(u8, u8),
    LoadBool(u8, bool),
    LoadInt(u8, i16),
    Move(u8, u8),

    // table
    NewTable(u8, u8, u8),
    SetTable(u8, u8, u8),
    SetField(u8, u8, u8),
    SetInt(u8, u8, u8),
    SetTableConst(u8, u8, u8),
    SetFieldConst(u8, u8, u8),
    SetIntConst(u8, u8, u8),
    SetList(u8, u8),
    GetTable(u8, u8, u8),
    GetField(u8, u8, u8),
    GetInt(u8, u8, u8),

    // function call
    Call(u8, u8),

    // unops
    Neg(u8, u8),
    Not(u8, u8),
    BitNot(u8, u8),
    Len(u8, u8),

    // binops
    Add(u8, u8, u8),
    AddConst(u8, u8, u8),
    AddInt(u8, u8, u8),
    Sub(u8, u8, u8),
    SubInt(u8, u8, u8),
    SubConst(u8, u8, u8),
    Mul(u8, u8, u8),
    MulInt(u8, u8, u8),
    MulConst(u8, u8, u8),
    Mod(u8, u8, u8),
    ModInt(u8, u8, u8),
    ModConst(u8, u8, u8),
    Div(u8, u8, u8),
    DivInt(u8, u8, u8),
    DivConst(u8, u8, u8),
    Idiv(u8, u8, u8),
    IdivInt(u8, u8, u8),
    IdivConst(u8, u8, u8),
    Pow(u8, u8, u8),
    PowInt(u8, u8, u8),
    PowConst(u8, u8, u8),
    BitAnd(u8, u8, u8),
    BitAndInt(u8, u8, u8),
    BitAndConst(u8, u8, u8),
    BitXor(u8, u8, u8),
    BitXorInt(u8, u8, u8),
    BitXorConst(u8, u8, u8),
    BitOr(u8, u8, u8),
    BitOrInt(u8, u8, u8),
    BitOrConst(u8, u8, u8),
    ShiftL(u8, u8, u8),
    ShiftLInt(u8, u8, u8),
    ShiftLConst(u8, u8, u8),
    ShiftR(u8, u8, u8),
    ShiftRInt(u8, u8, u8),
    ShiftRConst(u8, u8, u8),

    Concat(u8, u8, u8),
    ConcatInt(u8, u8, u8),
    ConcatConst(u8, u8, u8),
}
