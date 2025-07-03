(** C89/90 Abstract Syntax Tree *)

(** Binary operators *)
type binop =
  (* Arithmetic *)
  | Add | Sub | Mul | Div | Mod
  (* Comparison *)
  | Lt | Gt | Le | Ge | Eq | Ne
  (* Bitwise *)
  | BitAnd | BitOr | BitXor | Shl | Shr
  (* Logical *)
  | LogAnd | LogOr
  (* Assignment *)
  | Assign | AddAssign | SubAssign | MulAssign | DivAssign | ModAssign
  | AndAssign | OrAssign | XorAssign | ShlAssign | ShrAssign

(** Unary operators *)
type unop =
  | Plus | Minus | Not | BitNot
  | PreInc | PreDec | PostInc | PostDec
  | Deref | AddrOf

(** Type qualifiers *)
type type_qualifier =
  | Const
  | Volatile

(** Storage class specifiers *)
type storage_class =
  | Auto
  | Register
  | Static
  | Extern
  | Typedef

(** Basic type specifiers *)
type type_spec =
  | Void
  | Char
  | Short
  | Int
  | Long
  | Float
  | Double
  | Signed
  | Unsigned
  | StructType of string option * struct_decl list option  (* tag, declarations *)
  | UnionType of string option * struct_decl list option
  | EnumType of string option * enum_item list option
  | TypeName of string  (* For typedef'd types *)

and struct_decl = {
  spec_quals: (type_spec list * type_qualifier list);
  declarators: struct_declarator list;
}

and struct_declarator =
  | StructDecl of declarator
  | BitField of declarator option * expr

and enum_item = string * expr option  (* name, value *)

(** Type information *)
and ctype = {
  specs: type_spec list;
  quals: type_qualifier list;
  decl: declarator;
}

(** Declarators *)
and declarator =
  | DirectDecl of direct_declarator
  | PointerDecl of type_qualifier list * declarator

and direct_declarator =
  | Ident of string
  | ParenDecl of declarator
  | ArrayDecl of direct_declarator * expr option  (* size *)
  | FuncDecl of direct_declarator * param_list option

and param_list =
  | ParamList of param_decl list * bool  (* params, has_ellipsis *)

and param_decl = {
  param_specs: type_spec list;
  param_quals: type_qualifier list;
  param_decl: declarator option;
}

(** Expressions *)
and expr =
  | IntLit of int64 * string  (* value, suffix *)
  | FloatLit of float * string
  | CharLit of char
  | StringLit of string
  | Ident of string
  | BinOp of binop * expr * expr
  | UnOp of unop * expr
  | TernOp of expr * expr * expr  (* cond ? true_expr : false_expr *)
  | FuncCall of expr * expr list
  | ArrayRef of expr * expr  (* array[index] *)
  | Member of expr * string  (* struct.member *)
  | PtrMember of expr * string  (* ptr->member *)
  | SizeofExpr of expr
  | SizeofType of ctype
  | Cast of ctype * expr
  | Comma of expr list

(** Statements *)
type stmt =
  | ExprStmt of expr option  (* expression; or just ; *)
  | CompoundStmt of block_item list
  | IfStmt of expr * stmt * stmt option
  | SwitchStmt of expr * stmt
  | WhileStmt of expr * stmt
  | DoWhileStmt of stmt * expr
  | ForStmt of expr option * expr option * expr option * stmt
  | GotoStmt of string
  | ContinueStmt
  | BreakStmt
  | ReturnStmt of expr option
  | LabeledStmt of string * stmt
  | CaseStmt of expr * stmt
  | DefaultStmt of stmt

(** Block items (declarations or statements) *)
and block_item =
  | Decl of declaration
  | Stmt of stmt

(** Declarations *)
and declaration = {
  storage: storage_class list;
  specs: type_spec list;
  quals: type_qualifier list;
  init_decls: init_declarator list;
}

and init_declarator = {
  decl: declarator;
  init: initializer_ option;
}

and initializer_ =
  | ExprInit of expr
  | ListInit of init_item list

and init_item =
  | SimpleInit of initializer_
  | DesignatedInit of designator list * initializer_

and designator =
  | ArrayDesignator of expr
  | MemberDesignator of string

(** Function definitions *)
type func_def = {
  storage: storage_class list;
  specs: type_spec list;
  quals: type_qualifier list;
  declarator: declarator;
  old_style_params: string list;  (* K&R style *)
  body: stmt;  (* Must be CompoundStmt *)
}

(** External declarations *)
type external_decl =
  | FuncDef of func_def
  | Decl of declaration

(** Translation unit (a C file) *)
type translation_unit = external_decl list

(** Location information for AST nodes *)
type 'a located = {
  value: 'a;
  loc: Lexer.location;
}