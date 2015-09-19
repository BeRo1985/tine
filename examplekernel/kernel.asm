.include("pecoff.inc")

.cpu(all)

.target(pe64){
  imagebase = 0x100000
  codebase = 0x1000
  subsystem = IMAGE_SUBSYSTEM_EFI_APPLICATION
  characteristics = IMAGE_FILE_CHARACTERISTICS_DLL
}

.section(".text", IMAGE_SCN_CNT_CODE | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_EXECUTE | IMAGE_SCN_ALIGN_16BYTES){
  .use64
  .entrypoint
  _main:
    
    // Align stack
    sub rsp, 0x100
    mov rcx, 0xf
    not rcx  
    and rsp, rcx  
    
    // Clear screen
    xor rax, rax
    mov rcx, 0x2000
    mov rdi, 0xb8000
    rep stosw    
    
    // Print text onto screen
    
    mov rsi, offset TextMessage  // Read from our text message string 
    mov rdi, 0xb8000             // Write to 0xb800:0x0000 as segmented real mode address or 0xb8000 as linear address
    mov ah, 0x07                 // Light grey on black
  PrintLoop:
    lodsb  
    test al, al
    jz PrintLoopDone
    stosw
    jmp PrintLoop
  PrintLoopDone:    
    
  Hang:
    hlt
    jmp Hang
    
}

.section(".data", IMAGE_SCN_CNT_INITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES){
  TextMessage: 
    db "Hello 64-bit world!\0"
}

.section(".bss", IMAGE_SCN_CNT_UNINITIALIZED_DATA | IMAGE_SCN_MEM_READ | IMAGE_SCN_MEM_WRITE | IMAGE_SCN_ALIGN_16BYTES){
  DummyData: 
    dd 0
}
