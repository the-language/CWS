;    Copyright (C) 2018  Zaoqi
;
;    This program is free software: you can redistribute it and/or modify
;    it under the terms of the GNU Affero General Public License as published
;    by the Free Software Foundation, either version 3 of the License, or
;    (at your option) any later version.
;
;    This program is distributed in the hope that it will be useful,
;    but WITHOUT ANY WARRANTY; without even the implied warranty of
;    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;    GNU Affero General Public License for more details.
;
;    You should have received a copy of the GNU Affero General Public License
;    along with this program.  If not, see <http://www.gnu.org/licenses/>.
#lang typed/racket #:with-refinements
{define-syntax-rule {record x ...} {struct x ... #:transparent}}
{define-syntax define-data
  {syntax-rules ()
    [(_ (t c ...) (ons f ...) ...)
     {begin
       {record (c ...) ons (f ...)} ...
       {define-type (t c ...) (U (ons c ...) ...)}}]
    [(_ t (ons f ...) ...)
     {begin
       {record ons (f ...)} ...
       {define-type t (U ons ...)}}]}}
{: string-add-between (-> (Listof String) String String)}
{define (string-add-between xs a) (apply string-append (add-between xs a))}
{define-type (Maybe a) (U a False)}

{define-type Id Symbol}
{define-type Left
  (U
   Value2 ;bug
   Dot
   UnRef
   )}
{define-type Const
  (U
   ValueRational
   ValueInteger
   ValueChar
   ValueString
   ValueStruct ;bug
   ValueVector ;bug
   )}
{define-data Value
  (ValueRational [Rational : Exact-Rational])
  (ValueInteger [Integer : Integer])
  (ValueChar [Char : Char])
  (ValueString [String : String])
  (ValueVoid)
  (Value2 [void : Value] [Value : Value])
  (Call/cc [Id : Id] [Value : Value])
  (Apply [Value : Value] [List : (Listof Value)])
  (Dot [Value : Value] [Id : Id])
  (UnRef [Value : Value])
  (& [Value : Value])
  (xfx [op : (U '+ '- '* '/ '% '& '\| '&& '|| '<< '>>)] [x : Value] [y : Value])
  (fx [op : (U 'sizeof '~ '!)] [Value : Value])
  (Let [Id : Id] [Type : Type] [Value : (Maybe Value)] [result : Value])
  (ValueStruct [Id : Id] [List : (Listof (Pairof Id Value))])
  (ValueVector [List : (Listof Value)])
  }
{define-data GlobalLine
  (DefineStruct [Id : Id] [List : (Listof (Pairof Type Id))])
  (DefineUnion [Id : Id] [List : (Listof (Pairof Type Id))])
  (DefineType [Id : Id] [Type : Type])
  (Define [Id : Id] [Type : Type] [Value : (Maybe Const)])
  (DefineFunction [Id : Id] [args : (Listof (Pairof Type Id))] [result : Type] [Value : Value])
  }
{define-data Type
  (TypeId [Id : Id])
  (Arrow [List : (Listof Type)] [Type : Type])
  (Int) ;ssize_t
  (Nat) ;size_t
  (Ref [Type : Type])
  (TypeAny)
  (TypeVoid)
  (TypeNat8)
  (TypeNat16)
  (TypeNat32)
  (TypeNat64)
  (TypeInt8)
  (TypeInt16)
  (TypeInt32)
  (TypeInt64)
  (TypeFloat)
  (TypeDouble)}

{define alphabet (list->set (string->list "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM"))}
{define alphabetdi (list->set (string->list "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM1234567890"))}
{: Id->String (-> Id String)}
{define (Id->String id)
  {let* ([s (symbol->string id)] [cs (string->list s)] [a (car cs)] [d (cdr cs)])
    (if (and (set-member? alphabet a) (andmap {ann {λ [x : Char] (set-member? alphabetdi x)} (-> Char Boolean)} d))
        s
        (string-append
         "LFC_"
         (apply string-append (map {ann {λ [c : Char] {ann (string-append (number->string (char->integer c)) "_") String}} (-> Char String)} cs)) 
         (list->string (filter {ann {λ [x : Char] (set-member? alphabetdi x)} (-> Char Boolean)} cs))))}}
