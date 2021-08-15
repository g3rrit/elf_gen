
let elf_file_hdr = {|
                # All numbers (except in names) are in base sixteen (hexadecimal)
                # 00 <- number of bytes listed so far
7F 45 4C 46     # 04 e_ident[EI_MAG]: ELF magic number
01              # 05 e_ident[EI_CLASS]: 1: 32-bit, 2: 64-bit
   01           # 06 e_ident[EI_DATA]: 1: little-endian, 2: big-endian
      01        # 07 e_ident[EI_VERSION]: ELF header version; must be 1
         00     # 08 e_ident[EI_OSABI]: Target OS ABI; should be 0

00              # 09 e_ident[EI_ABIVERSION]: ABI version; 0 is ok for Linux
   00 00 00     # 0C e_ident[EI_PAD]: unused, should be 0
00 00 00 00     # 10

02 00           # 12 e_type: object file type; 2: executable
      03 00     # 14 e_machine: instruction set architecture; 3: x86, 3E: amd64
01 00 00 00     # 18 e_version: ELF identification version; must be 1

54 80 04 08     # 1C e_entry: memory address of entry point (where process starts)
34 00 00 00     # 20 e_phoff: file offset where program headers begin

00 00 00 00     # 24 e_shoff: file offset where section headers begin
00 00 00 00     # 28 e_flags: 0 for x86

34 00           # 2A e_ehsize: size of this header (34: 32-bit, 40: 64-bit)
      20 00     # 2C e_phentsize: size of each program header (20: 32-bit, 38: 64-bit)
01 00           # 2E e_phnum: #program headers
      28 00     # 30 e_shentsize: size of each section header (28: 32-bit, 40: 64-bit)

00 00           # 32 e_shnum: #section headers
      00 00     # 34 e_shstrndx: index of section header containing section names
|}

let elf_program_hdr = {|
01 00 00 00     # 38 p_type: segment type; 1: loadable

54 00 00 00     # 3C p_offset: file offset where segment begins
54 80 04 08     # 40 p_vaddr: virtual address of segment in memory (x86: 08048054)
    
00 00 00 00     # 44 p_paddr: physical address of segment, unspecified by 386 supplement
0C 00 00 00     # 48 p_filesz: size in bytes of the segment in the file image ############

0C 00 00 00     # 4C p_memsz: size in bytes of the segment in memory; p_filesz <= p_memsz
05 00 00 00     # 50 p_flags: segment-dependent flags (1: X, 2: W, 4: R)

00 10 00 00     # 54 p_align: 1000 for x86
|}

let elf_program_seg = {|
B8 01 00 00 00  # 59 eax <- 1 (exit)
BB 03 00 00 00  # 5E ebx <- 3 (param)
CD 80           # 60 syscall >> int 80
|}

let hex_to_bytes (s : string) : Bytes.t =
    Str.global_replace (Str.regexp " \\|\n\\|#.*$") "" s |>
    fun s -> Hex.to_bytes (`Hex s)

let write_to_file (path : string) (data : bytes) =
    let data_len = Bytes.length data in
    let fd = Unix.openfile path [ Unix.O_WRONLY; Unix.O_CREAT ] 0o777 in
    let write_len = Unix.write fd data 0 data_len in
    if write_len != data_len 
    then Printf.printf "Unable to write to file [%s]\n" path
    else Printf.printf "Wrote [%d] bytes to file [%s]\n" write_len path
    ; Unix.close fd

let () =
    Printf.printf "+-------------+\n|Elf Generator|\n+-------------+\n"
    ; let pgr = String.concat "" [ elf_file_hdr; elf_program_hdr; elf_program_seg ] in
      hex_to_bytes pgr |> write_to_file "out"

