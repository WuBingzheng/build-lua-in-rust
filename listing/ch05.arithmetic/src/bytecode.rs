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

    // unops
    Neg(u8, u8),
    Not(u8, u8),
    BitNot(u8, u8),
    Len(u8, u8),

    // call function
    Call(u8, u8),
}
