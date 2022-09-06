#[derive(Debug)]
pub enum ByteCode {
    GetGlobal(u8, u8),
    SetGlobal(u8, u8),
    SetGlobalConst(u8, u8), // TODO u8?
    LoadConst(u8, u16),
    LoadNil(u8, u8),
    LoadBool(u8, bool),
    LoadInt(u8, i16),
    Move(u8, u8),
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
    Call(u8, u8),
}
