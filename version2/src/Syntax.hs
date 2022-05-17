{- PiForall language, OPLSS -}

-- | The abstract syntax of the simple dependently typed language
-- See comment at the top of 'Parser' for the concrete syntax of this language
module Syntax where

import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Text.ParserCombinators.Parsec.Pos (SourcePos, initialPos, newPos)
import Unbound.Generics.LocallyNameless qualified as Unbound

-----------------------------------------

-- * Names

-----------------------------------------

-- | For variable names, we use the Unbound library to
-- automatically generate free variable, substitution,
-- and alpha-equality function.
type TName = Unbound.Name Term

-- | module names
type MName = String



-----------------------------------------

-- * Core language

-----------------------------------------

-- | Combined syntax for types and terms
-- (type synonym for documentation)
type Type = Term

-- | basic language
data Term
  = -- | type of types  `Type`
    Type
  | -- | variables  `x`
    Var TName
  | -- | abstraction  `\x. a`
    Lam (Unbound.Bind (TName , Unbound.Embed Annot) Term)
  | -- | application `a b`
    App Term Arg
  | -- | function type   `(x : A) -> B`
    Pi (Unbound.Bind (TName , Unbound.Embed Term) Term)
  | -- | Annotated terms `( x : A )`
    Ann Term Term
  | -- | parenthesized term, useful for printing
    Paren Term
  | -- | marked source position, for error messages
    Pos SourcePos Term
  | -- | an axiom 'TRUSTME', inhabits all types
    TrustMe Annot
  | -- | a directive to the type checker to print out the current context (like a typed hole)
    PrintMe Annot
  | -- | let expression, introduces a new (potentially recursive) definition in the ctx
    -- | `let x = a in b`
    Let (Unbound.Bind (TName, Unbound.Embed Term) Term)
  | -- | The type with a single inhabitant `Unit`
    TyUnit
  | -- | The inhabitant, written `()`
    LitUnit
  | -- | The type with two inhabitants (homework) `Bool`
    TyBool
  | -- | True and False
    LitBool Bool
  | -- | If expression for eliminating booleans
    If Term Term Term Annot
  | -- | sigma type `{ x : A | B }`   -- homework
    Sigma (Unbound.Bind (TName, Unbound.Embed Term) Term)
  | -- | introduction for sigmas `( a , b )`
    Prod Term Term Annot
  | -- | elimination form  `let (x,y) = a in b`
    LetPair Term (Unbound.Bind (TName, TName) Term) Annot

     | -- | Equality type  `a = b`
    TyEq Term Term
  | -- | Proof of equality `refl`
    Refl Annot
  | -- | equality elimination  `subst a by pf`
    Subst Term Term Annot
  | -- | witness to an equality contradiction
    Contra Term Annot
   

   
  deriving (Show, Generic, Typeable, Unbound.Alpha)

-- | An argument to a function
data Arg = Arg { unArg :: Term}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

-- | An 'Annot' is optional type information
newtype Annot = Annot (Maybe Term)
  deriving (Show, Generic, Typeable)
  deriving anyclass (Unbound.Subst Term)





-----------------------------------------

-- * Modules and declarations

-----------------------------------------

-- | A Module has a name, a list of imports, a list of declarations,
--   and a set of constructor names (which affect parsing).
data Module = Module
  { moduleName :: MName,
    moduleImports :: [ModuleImport],
    moduleEntries :: [Decl] 
  }
  deriving (Show, Generic, Typeable)

newtype ModuleImport = ModuleImport MName
  deriving (Show, Eq, Generic, Typeable)

data Sig = S {sigName :: TName , sigType :: Type}
  deriving (Show, Generic, Typeable, Unbound.Alpha, Unbound.Subst Term)

mkSig :: TName -> Type -> Sig
mkSig n t = S n  t

-- | Declarations are the components of modules
data Decl
  = -- | Declaration for the type of a term
    Sig Sig
  | -- | The definition of a particular name, must
    -- already have a type declaration in scope
    Def TName Term
  | -- | A potentially (recursive) definition of
    -- a particular name, must be declared
    RecDef TName Term 
  deriving (Show, Generic, Typeable)



-- * Auxiliary functions on syntax



-- | Default name for '_' occurring in patterns
wildcardName :: TName
wildcardName = Unbound.string2Name "_"

-- | empty Annotation
noAnn :: Annot
noAnn = Annot Nothing

-- | Partial inverse of Pos
unPos :: Term -> Maybe SourcePos
unPos (Pos p _) = Just p
unPos _ = Nothing

-- | Tries to find a Pos anywhere inside a term
unPosDeep :: Term -> Maybe SourcePos
unPosDeep = unPos -- something (mkQ Nothing unPos) -- TODO: Generic version of this

-- | Tries to find a Pos inside a term, otherwise just gives up.
unPosFlaky :: Term -> SourcePos
unPosFlaky t = fromMaybe (newPos "unknown location" 0 0) (unPosDeep t)



-----------------

-- We use the unbound-generics library to mark the binding occurrences of
-- variables in the syntax. That allows us to automatically derive
-- functions for alpha-equivalence, free variables and substitution
-- using generic programming. 


-- * Substitution

-- The Subst class derives capture-avoiding substitution
-- It has two parameters because the sort of thing we are substituting
-- for may not be the same as what we are substituting into.

-- class Subst b a where
--    subst  :: Name b -> b -> a -> a       -- single substitution
--    substs :: [(Name b, b)] -> a -> a     -- multiple substitution

instance Unbound.Subst Term Term where
  isvar (Var x) = Just (Unbound.SubstName x)
  isvar _ = Nothing

------------------

-- * Alpha equivalence and free variables

-- Among other things, the Alpha class enables the following
-- functions:
--    -- Compare terms for alpha equivalence
--    aeq :: Alpha a => a -> a -> Bool
--    -- Calculate the free variables of a term
--    fv  :: Alpha a => a -> [Unbound.Name a]

------------------

instance Unbound.Alpha Annot where
  -- override default behavior so that type annotations are ignored
  -- when comparing for alpha-equivalence
  aeq' _ _ _ = True
  fvAny' _ _ = pure
  open _ _ = id
  close _ _ = id
  isPat _ = mempty
  isTerm _ = mempty
  nthPatFind _ = mempty
  namePatFind _ = mempty
  swaps' _ _ = id
  freshen' _ x = return (x, mempty)
  lfreshen' _ x cont = cont x mempty
  acompare' _ _ _ = EQ

-----------------

-- * Source Positions

-- We want to ignore source positions during comparison
instance Unbound.Alpha SourcePos where
  aeq' _ _ _ = True
  fvAny' _ _ = pure
  open _ _ = id
  close _ _ = id
  isPat _ = mempty
  isTerm _ = mempty
  nthPatFind _ = mempty
  namePatFind _ = mempty
  swaps' _ _ = id
  freshen' _ x = return (x, mempty)
  lfreshen' _ x cont = cont x mempty
  acompare' _ _ _ = EQ

-- Substitutions ignore source positions
instance Unbound.Subst b SourcePos where subst _ _ = id; substs _ = id

-- Internally generated source positions
internalPos :: SourcePos
internalPos = initialPos "internal"