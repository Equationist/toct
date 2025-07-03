(* Mach-O Object File Writer for ARM64 *)
(* Implements the Mach-O file format for macOS/Darwin *)

open Machine

(* Define some basic ARM64 registers for use in dummy code generation *)
let x29 = make_gpr 29 8  (* Frame pointer *)
let x30 = make_gpr 30 8  (* Link register *)
let sp = make_gpr 31 8   (* Stack pointer *)

(* Mach-O Constants *)
module MachO = struct
  (* Magic numbers *)
  let mh_magic_64 = 0xfeedfacf  (* 64-bit Mach-O *)
  let mh_cigam_64 = 0xcffaedfe  (* Swapped magic *)
  
  (* CPU types *)
  let cpu_type_arm64 = 0x0100000c
  let cpu_subtype_arm64_all = 0x00000000
  
  (* File types *)
  let mh_object = 0x1  (* Relocatable object file *)
  
  (* Header flags *)
  let mh_noundefs = 0x1
  let mh_subsections_via_symbols = 0x2000
  
  (* Load commands *)
  let lc_segment_64 = 0x19
  let lc_symtab = 0x2
  let lc_dysymtab = 0xb
  
  (* Section types *)
  let s_regular = 0x0
  let s_zerofill = 0x1
  let s_attr_pure_instructions = 0x80000000
  let s_attr_some_instructions = 0x00000400
end

(* Mach-O data structures *)
type mach_header_64 = {
  magic: int32;      (* Magic number *)
  cputype: int32;    (* CPU type *)
  cpusubtype: int32; (* CPU subtype *)
  filetype: int32;   (* File type *)
  ncmds: int32;      (* Number of load commands *)
  sizeofcmds: int32; (* Size of load commands *)
  flags: int32;      (* Flags *)
  reserved: int32;   (* Reserved (0) *)
}

type segment_command_64 = {
  cmd: int32;       (* LC_SEGMENT_64 *)
  cmdsize: int32;   (* Size of this command *)
  segname: string;  (* Segment name (16 bytes) *)
  vmaddr: int64;    (* VM address *)
  vmsize: int64;    (* VM size *)
  fileoff: int64;   (* File offset *)
  filesize: int64;  (* File size *)
  maxprot: int32;   (* Maximum VM protection *)
  initprot: int32;  (* Initial VM protection *)
  nsects: int32;    (* Number of sections *)
  flags: int32;     (* Flags *)
}

type section_64 = {
  sectname: string;  (* Section name (16 bytes) *)
  segname: string;   (* Segment name (16 bytes) *)
  addr: int64;       (* Memory address *)
  size: int64;       (* Size in bytes *)
  offset: int32;     (* File offset *)
  align: int32;      (* Alignment (power of 2) *)
  reloff: int32;     (* Relocation offset *)
  nreloc: int32;     (* Number of relocations *)
  flags: int32;      (* Section type and attributes *)
  reserved1: int32;  (* Reserved *)
  reserved2: int32;  (* Reserved *)
  reserved3: int32;  (* Reserved *)
}

type nlist_64 = {
  n_strx: int32;     (* String table index *)
  n_type: int;       (* Type flag (byte) *)
  n_sect: int;       (* Section number (byte) *)
  n_desc: int;       (* See stab.h (short) *)
  n_value: int64;    (* Value of symbol *)
}

type relocation_info = {
  r_address: int32;   (* Offset in section *)
  r_symbolnum: int32; (* Symbol index if external *)
  r_pcrel: bool;      (* PC-relative? *)
  r_length: int;      (* 0=byte, 1=word, 2=long, 3=quad *)
  r_extern: bool;     (* External symbol? *)
  r_type: int;        (* Relocation type *)
}

(* Binary writing utilities *)
module Writer = struct
  type t = {
    mutable buffer: Buffer.t;
    mutable offset: int;
  }
  
  let create () = {
    buffer = Buffer.create 4096;
    offset = 0;
  }
  
  let write_byte w b =
    Buffer.add_char w.buffer (Char.chr (b land 0xff));
    w.offset <- w.offset + 1
  
  let write_int16 w i =
    write_byte w (i land 0xff);
    write_byte w ((i lsr 8) land 0xff)
  
  let write_int32 w i =
    let i = Int32.to_int i in
    write_byte w (i land 0xff);
    write_byte w ((i lsr 8) land 0xff);
    write_byte w ((i lsr 16) land 0xff);
    write_byte w ((i lsr 24) land 0xff)
  
  let write_int64 w i =
    let lo = Int64.to_int32 i in
    let hi = Int64.to_int32 (Int64.shift_right_logical i 32) in
    write_int32 w lo;
    write_int32 w hi
  
  let write_string w s len =
    let slen = String.length s in
    Buffer.add_string w.buffer (String.sub s 0 (min slen len));
    for _ = slen to len - 1 do
      write_byte w 0
    done;
    w.offset <- w.offset + len
  
  let align w alignment =
    let mask = alignment - 1 in
    while (w.offset land mask) <> 0 do
      write_byte w 0
    done
  
  let contents w = Buffer.contents w.buffer
end

(* ARM64 instruction encoding *)
module ARM64Encode = struct
  (* Encode a 32-bit ARM64 instruction *)
  let encode_instruction (op: machine_op) : int32 option =
    match op with
    | MOV (dst, src) when dst.reg_class = GPR && src.reg_class = GPR ->
      (* MOV is encoded as ORR dst, xzr, src *)
      let sf = if dst.reg_size = 8 then 1 else 0 in
      let opc = 0b01 in
      let n = 0 in
      Some (Int32.logor
        (Int32.shift_left (Int32.of_int (sf lsl 31 lor opc lsl 29 lor 0b01010 lsl 24 lor n lsl 22)) 0)
        (Int32.of_int (src.reg_index lsl 16 lor 0b11111 lsl 5 lor dst.reg_index)))
    
    | ADD (dst, src1, src2) when dst.reg_class = GPR ->
      (* ADD instruction: sf 0 0 01011 shift 0 Rm imm6 Rn Rd *)
      let sf = if dst.reg_size = 8 then 1 else 0 in
      Some (Int32.logor
        (Int32.shift_left (Int32.of_int (sf lsl 31 lor 0b0001011 lsl 24)) 0)
        (Int32.of_int (src2.reg_index lsl 16 lor src1.reg_index lsl 5 lor dst.reg_index)))
    
    | SUB (dst, src1, src2) when dst.reg_class = GPR ->
      (* SUB instruction *)
      let sf = if dst.reg_size = 8 then 1 else 0 in
      Some (Int32.logor
        (Int32.shift_left (Int32.of_int (sf lsl 31 lor 0b1001011 lsl 24)) 0)
        (Int32.of_int (src2.reg_index lsl 16 lor src1.reg_index lsl 5 lor dst.reg_index)))
    
    | RET ->
      (* RET = RET X30 *)
      Some 0xd65f03c0l
    
    | JMP label ->
      (* B instruction - offset will be fixed up later *)
      Some 0x14000000l  (* Placeholder *)
    
    | CALL (_, Some name) ->
      (* BL instruction - offset will be fixed up later *)
      Some 0x94000000l  (* Placeholder *)
    
    | _ -> None  (* Not implemented *)
  
  (* Encode a sequence of machine instructions *)
  let encode_instructions instrs =
    List.filter_map (fun instr ->
      match encode_instruction instr.op with
      | Some encoded -> Some (instr, encoded)
      | None -> 
        Printf.eprintf "Warning: Cannot encode instruction: %s\n"
          (Option.value instr.comment ~default:"<unknown>");
        None
    ) instrs
end

(* Symbol and relocation management *)
type symbol = {
  name: string;
  section: int;  (* Section number, 0 for undefined *)
  value: int64;  (* Symbol value/offset *)
  is_global: bool;
}

type object_file = {
  text_section: (machine_instr * int32) list;  (* Instructions with encoding *)
  data_section: bytes;
  symbols: symbol list;
  relocations: relocation_info list;
}

(* Write Mach-O header *)
let write_header w ncmds sizeofcmds =
  let header = {
    magic = Int32.of_int MachO.mh_magic_64;
    cputype = Int32.of_int MachO.cpu_type_arm64;
    cpusubtype = Int32.of_int MachO.cpu_subtype_arm64_all;
    filetype = Int32.of_int MachO.mh_object;
    ncmds = Int32.of_int ncmds;
    sizeofcmds = Int32.of_int sizeofcmds;
    flags = Int32.of_int (MachO.mh_noundefs lor MachO.mh_subsections_via_symbols);
    reserved = 0l;
  } in
  Writer.write_int32 w header.magic;
  Writer.write_int32 w header.cputype;
  Writer.write_int32 w header.cpusubtype;
  Writer.write_int32 w header.filetype;
  Writer.write_int32 w header.ncmds;
  Writer.write_int32 w header.sizeofcmds;
  Writer.write_int32 w header.flags;
  Writer.write_int32 w header.reserved

(* Write segment and section *)
let write_text_segment w text_offset text_size =
  (* Segment command *)
  let seg_cmd = {
    cmd = Int32.of_int MachO.lc_segment_64;
    cmdsize = Int32.of_int (72 + 80);  (* segment + 1 section *)
    segname = "__TEXT";
    vmaddr = 0L;
    vmsize = Int64.of_int text_size;
    fileoff = Int64.of_int text_offset;
    filesize = Int64.of_int text_size;
    maxprot = 7l;  (* RWX *)
    initprot = 5l;  (* R-X *)
    nsects = 1l;
    flags = 0l;
  } in
  
  Writer.write_int32 w seg_cmd.cmd;
  Writer.write_int32 w seg_cmd.cmdsize;
  Writer.write_string w seg_cmd.segname 16;
  Writer.write_int64 w seg_cmd.vmaddr;
  Writer.write_int64 w seg_cmd.vmsize;
  Writer.write_int64 w seg_cmd.fileoff;
  Writer.write_int64 w seg_cmd.filesize;
  Writer.write_int32 w seg_cmd.maxprot;
  Writer.write_int32 w seg_cmd.initprot;
  Writer.write_int32 w seg_cmd.nsects;
  Writer.write_int32 w seg_cmd.flags;
  
  (* Text section *)
  let sect = {
    sectname = "__text";
    segname = "__TEXT";
    addr = 0L;
    size = Int64.of_int text_size;
    offset = Int32.of_int text_offset;
    align = 2l;  (* 2^2 = 4 byte alignment *)
    reloff = 0l;
    nreloc = 0l;
    flags = Int32.of_int (MachO.s_regular lor MachO.s_attr_pure_instructions);
    reserved1 = 0l;
    reserved2 = 0l;
    reserved3 = 0l;
  } in
  
  Writer.write_string w sect.sectname 16;
  Writer.write_string w sect.segname 16;
  Writer.write_int64 w sect.addr;
  Writer.write_int64 w sect.size;
  Writer.write_int32 w sect.offset;
  Writer.write_int32 w sect.align;
  Writer.write_int32 w sect.reloff;
  Writer.write_int32 w sect.nreloc;
  Writer.write_int32 w sect.flags;
  Writer.write_int32 w sect.reserved1;
  Writer.write_int32 w sect.reserved2;
  Writer.write_int32 w sect.reserved3

(* Generate object file from module *)
let generate_object_file (m: Compilerkit_pir.Module_ir.pir_module) : object_file =
  (* For now, just handle functions *)
  let text_instrs = ref [] in
  let symbols = ref [] in
  
  List.iter (function
    | Compilerkit_pir.Module_ir.FuncDecl func ->
      (* Add function symbol *)
      symbols := {
        name = func.name;
        section = 1;  (* Text section *)
        value = Int64.of_int (List.length !text_instrs * 4);
        is_global = true;
      } :: !symbols;
      
      (* Generate dummy instructions for now *)
      let dummy_instrs = [
        { label = Some func.name; op = MOV (x29, sp); comment = Some "save fp" };
        { label = None; op = MOV (sp, x29); comment = Some "restore sp" };
        { label = None; op = RET; comment = Some "return" };
      ] in
      
      let encoded = ARM64Encode.encode_instructions dummy_instrs in
      text_instrs := !text_instrs @ encoded
      
    | _ -> ()
  ) m.items;
  
  {
    text_section = !text_instrs;
    data_section = Bytes.empty;
    symbols = List.rev !symbols;
    relocations = [];
  }

(* Write complete Mach-O file *)
let write_object_file filename obj =
  let w = Writer.create () in
  
  (* Calculate sizes *)
  let header_size = 32 in
  let seg_cmd_size = 72 + 80 in  (* segment + 1 section *)
  let text_size = List.length obj.text_section * 4 in
  let text_offset = header_size + seg_cmd_size in
  
  (* Write header *)
  write_header w 1 seg_cmd_size;
  
  (* Write segment *)
  write_text_segment w text_offset text_size;
  
  (* Write text section content *)
  List.iter (fun (_, encoded) ->
    Writer.write_int32 w encoded
  ) obj.text_section;
  
  (* Write to file *)
  let oc = open_out_bin filename in
  output_string oc (Writer.contents w);
  close_out oc

(* Main entry point *)
let write_macho_file filename m =
  let obj = generate_object_file m in
  write_object_file filename obj;
  Printf.printf "Wrote Mach-O object file: %s\n" filename