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
    | `EOL -> '\n'
    | `Invalid -> '\x7f'
end

module Token = struct
  type t =
    | Char of { c : char ; code : Catcode.t }
    | ControlSequence of string

  let of_char c = Char { c ; code = Catcode.of_char c }
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
   * insert an EOL at the end of the line. *)
  let strip_line line =
    String.rev line
    |> String.fold ~init:['\n'] ~f:(fun acc c ->
        match acc, c with
        | ['\n'], ' ' -> acc
        | _ -> c :: acc)

  (* TODO: handle superscript codes, e.g. ^^A, p. 46 *)
  let rec handle_char (state, tokens) c =
    match state, Catcode.of_char c with
    | LineEnded, _ -> (LineEnded, tokens)
    | ReadingCSName cs, `Letter -> (ReadingCSName (c :: cs), tokens)
    | ReadingCSName cs, `EOL -> (LineEnded, Token.cs cs :: tokens)
    | ReadingCSName [], `Space -> (SkippingBlanks, Token.cs [c] :: tokens)
    | ReadingCSName [], _ -> (MiddleLine, Token.cs [c] :: tokens)
    | ReadingCSName cs, _ -> handle_char (SkippingBlanks, Token.cs cs :: tokens) c
    | BeginningLine, `EOL -> (LineEnded, Token.par :: tokens)
    | MiddleLine, `EOL -> (LineEnded, Token.of_char ' ' :: tokens)
    | SkippingBlanks, `EOL -> (LineEnded, tokens)
    | _, `Escape -> (ReadingCSName [], tokens)
    | _, `BeginGroup
    | _, `EndGroup
    | _, `MathShift
    | _, `Align
    | _, `Parameter
    | _, `Superscript
    | _, `Subscript
    | _, `Letter
    | _, `Other
    | _, `Active -> (MiddleLine, Token.of_char c :: tokens)
    | _, `Ignored -> (state, tokens)
    | BeginningLine, `Space -> (BeginningLine, tokens)
    | SkippingBlanks, `Space -> (SkippingBlanks, tokens)
    | MiddleLine, `Space -> (SkippingBlanks, Token.of_char ' ' :: tokens)
    | _, `Comment -> (LineEnded, tokens)
    | _, `Invalid -> print_endline "invalid character"; (state, tokens)

  let tokenize_line line =
    let
      (_, tokens) = List.fold line ~init:(BeginningLine, []) ~f:handle_char
    in
    List.rev tokens

  (* TODO: expandafter, noexpand, csname, string, number, romannumeral, etc. *)
  let tokenize text =
    String.split_lines text
    |> List.map ~f:strip_line
    |> List.concat_map ~f:tokenize_line
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
