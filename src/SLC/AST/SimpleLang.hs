module SLC.AST.SimpleLang where

import SLC.AST.Shared

data TypeDecl = RecordDecl Identifier [RecordMember]

data RecordMember = RecordMember Identifier TypeName

data Module = Module Name [Import] [TypeDecl]

data TopLevelDecl = TopLevelType TypeDecl

