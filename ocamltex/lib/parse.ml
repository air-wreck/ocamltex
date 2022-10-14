open! Core

module Box = struct
  type t =
    { height : float
    ; width : float
    ; depth : float
    }
end

module Glue = struct
  type t =
    { space : float
    ; stretch : float
    ; shrink : float
    }
end

(* See TeXBook, p. 140 *)
module MathStyle = struct
  type style =
    | Display
    | Text
    | Script
    | Scriptscript

  type t = { style : style ; cramped : bool }
end

(* See TeXBook, pp. 156--157 *)
module rec MathAtom : sig
  type kind =
    | Ord
    | Op
    | Bin
    | Rel
    | Open
    | Close
    | Punct
    | Inner
    | Over
    | Under
    | Acc
    | Rad
    | Vcent

  type field =
    | Empty
    | Symbol of { family : string ; pos : int }
    | Box of Box.t
    | List of MathList.t

  type t =
    { nucleus : field ; superscript : field ; subscript : field ; kind : kind }
end = MathAtom
and MathList : sig
  type boundary = Left | Right
  type vertical = Mark | Insert | Vadjust

  type elem =
    | Atom of MathAtom.t
    | HorizontalMaterial
    | VerticalMaterial of vertical
    | Glue
    | Kern
    | StyleChange
    | Fraction
    | Boundary of boundary
    | Choice

  type t = elem list
end = MathList

module Catcode = struct
  type t =
    [ `Escape      (* 0 *)
    | `BeginGroup  (* 1 *)
    | `EndGroup    (* 2 *)
    | `MathShift   (* 3 *)
    | `Align       (* 4 *)
    | `EOL         (* 5 *)
    | `Parameter   (* 6 *)
    | `Superscript (* 7 *)
    | `Subscript   (* 8 *)
    | `Ignored     (* 9 *)
    | `Space       (* 10 *)
    | `Letter      (* 11 *)
    | `Other       (* 12 *)
    | `Active      (* 13 *)
    | `Comment     (* 14 *)
    | `Invalid     (* 15 *)
    ]

  (* Note that technically, there are some restrictions on how these can
   * be changed. For example, the character code of a category 10 symbol
   * is always 32. *)
  let of_char = function
    | '\\'   -> `Escape
    | '{'    -> `BeginGroup
    | '}'    -> `EndGroup
    | '$'    -> `MathShift
    | '&'    -> `Align
    | '\n'   -> `EOL
    | '#'    -> `Parameter
    | '^'    -> `Superscript
    | '_'    -> `Subscript
    | '\x00' -> `Ignored
    | ' '    -> `Space
    | '~'    -> `Active
    | '%'    -> `Comment
    | '\x7f' -> `Invalid
    | c -> if Char.is_alpha c then `Letter else `Other

  let to_char = function
    | `Space  -> ' '
    | `Return -> '\n'
    | `Invalid -> '\x7f'
end

module Token = struct
  type t =
    | Char of { c : char ; code : Catcode.t }
    | ControlSequence of string

  let char c = Char { c ; code = Catcode.of_char c }
  let cs name = ControlSequence (List.rev name |> String.of_char_list)
  let par = ControlSequence "par"
end

module Lexer = struct
  type state =
    | BeginningLine  (* N *)
    | MiddleLine     (* M *)
    | SkippingBlanks (* S *)
    | ReadingCSName of char list
    | LineEnded

  (* Strip all space characters at the right end of the input line, then
   * insert a return at the end of the line. *)
  let strip_line line =
    String.rev line
    |> String.fold ~init:['\n'] ~f:(fun acc c ->
        match acc, c with
        | ['\n'], ' ' -> acc
        | _ -> c :: acc)
    |> String.of_char_list

  (* TODO: handle case when ReadingCSName when line done *)
  (* TODO: handle superscript codes, e.g. ^^A, p. 46 *)
  let handle_char (state, tokens) (c, next) =
    match state, Catcode.of_char c, Catcode.of_char next with
    | LineEnded, _, _ -> (LineEnded, tokens)
    | ReadingCSName cs, `Letter, `Letter -> (ReadingCSName (c :: cs), tokens)
    | ReadingCSName cs, `Letter, _ -> (MiddleLine, Token.cs (c :: cs) :: tokens)
    | ReadingCSName _, _, _ -> (MiddleLine, Token.cs [c] :: tokens)
    | BeginningLine, `EOL, _ -> (LineEnded, Token.par :: tokens)
    | MiddleLine, `EOL, _ -> (LineEnded, Token.char ' ' :: tokens)
    | SkippingBlanks, `EOL, _ -> (LineEnded, tokens)
    | _, `Escape, _ -> (ReadingCSName [], tokens)
    | _, `BeginGroup, _
    | _, `EndGroup, _
    | _, `MathShift, _
    | _, `Align, _
    | _, `Parameter, _
    | _, `Superscript, _
    | _, `Subscript, _
    | _, `Letter, _
    | _, `Other, _
    | _, `Active, _ -> (MiddleLine, Token.char c :: tokens)
    | _, `Ignored, _ -> (state, tokens)
    | BeginningLine, `Space, _ -> (BeginningLine, tokens)
    | SkippingBlanks, `Space, _ -> (SkippingBlanks, tokens)
    | MiddleLine, `Space, _ -> (SkippingBlanks, Token.char ' ' :: tokens)
    | _, `Comment, _ -> (LineEnded, tokens)
    | _, `Invalid, _ -> print_endline "invalid character"; (state, tokens)

  (* TODO: expandafter, noexpand, csname, string, number, romannumeral, etc. *)
  let tokenize text =
    String.split_lines text
    |> List.map ~f:String.to_list
    |> List.map ~f:(function
      | [] -> failwith "Lines are always nonempty"
      | (_ :: tail) as line ->
          (match List.zip line (tail @ [Catcode.to_char `Invalid]) with
           | Unequal_lengths -> failwith "Lists are same length"
           | Ok line -> line))
    |> List.map ~f:(List.fold ~init:(BeginningLine, []) ~f:handle_char)
    |> List.map ~f:snd
    |> List.concat_map ~f:List.rev
end

(*
 * a + b
 *
 * \frac{a}{b}
 *
 * \Delta
 *
 * \left( \right)
 *
 * \int_0^\infty f(x)\,dx
 *
 *)
