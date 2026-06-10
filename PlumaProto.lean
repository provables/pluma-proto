-- This module serves as the root of the `OeisLtProto` library.
-- Import modules here that should be built as part of the library.
import Lean
import SQLite

open Lean Elab Command

structure PlumaContext where
  env : Environment
  ctx : Core.Context
  state : Core.State
  db : SQLite

inductive PlumaError where
  | JSONDecodeError (e : String)
  | UserError (e : String)
  | IOError (e : IO.Error)

instance : Repr PlumaError where
  reprPrec
    | .JSONDecodeError e, _ => f!"JSONDecodeError: {e}"
    | .UserError e, _ => f!"UserError: {e}"
    | .IOError e, _ => f!"IOError: {e}"

abbrev PlumaM := ReaderT PlumaContext (EIO PlumaError)

instance : MonadLift IO PlumaM where
  monadLift o := IO.toEIO PlumaError.IOError o

-- def toIO {α : Type}
--       (x : CommandElabM α) (state : GenSeqContext) (throwOnError : Bool := true) : IO α := do
--     Prod.fst <$> (Core.CoreM.toIO · state.ctx state.state) (liftCommandElabM x throwOnError)

instance : MonadLift CommandElabM PlumaM where
  monadLift o := do
    Prod.fst <$> Core.CoreM.toIO
      (liftCommandElabM o (throwOnError := false))
      (← read).ctx (← read).state

def f : PlumaM Nat := do
  IO.println ""
  throw <| PlumaError.UserError ""
  return 1
  --throw <| IO.Error.userError ""

-- Implement monad lift from IO -> EIO Myerror so the user can use IO inside OEISM (and
-- make OEISM in EIO MyError)

abbrev PluginFunction := Σ input : Type, Σ _ : FromJson input, Σ output : Type, Σ _ : ToJson output,
  input → PlumaM output

structure Plugin : Type where
  cmd : String
  function : Json → PlumaM Json

instance : CoeFun Plugin (fun _ => Json → PlumaM Json) where
  coe p := p.function

def mkPlugin {a b : Type} [FromJson a] [ToJson b] (cmd : String) (f : a → PlumaM b)
    : Plugin :=
  let g (x : Json) := do
    let .ok (obj : a) := FromJson.fromJson? x
      | throw <| .JSONDecodeError s!"JSON input cannot be converted to type of plugin function"
    return ToJson.toJson (← f obj)
  ⟨cmd, g⟩

-- Client provides a value `plugin : Plugin`.

/--
Attribute to derive FromJson, ToJson, and coercions for a type.
-/
syntax (name := plumaData) "plumaData" : attr

@[inherit_doc plumaData]
initialize Lean.registerBuiltinAttribute {
  name := `plumaData
  descr := "attribute for Pluma"
  add := fun declName stx kind => do
    let x ← liftCommandElabM do
      if not (← Lean.Elab.Deriving.FromToJson.mkToJsonInstanceHandler #[declName]) then
        throwError s!"failed to derive ToJson instance for {declName}"
      if not (← Lean.Elab.Deriving.FromToJson.mkFromJsonInstanceHandler #[declName]) then
        throwError s!"failed to derive FromJson instance for {declName}"
      elabCommand (← `(command|
        instance : Coe $(mkIdent declName) Lean.Json where
          coe := Lean.ToJson.toJson
      ))
}
