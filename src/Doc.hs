module Doc
  ( Doc
  , Layout(..)
  , (<+>), (<|>)
  , text, line, hardline, space, empty
  , nest, nestAbsolute, align, column
  , setLayout, layoutChoice
  , fill, stack, hardStack, sep
  , pretty, prettyWith
  ) where

data Layout = SingleLine | MultiLine
  deriving (Eq, Show)

data Doc
  = Empty
  | Text String
  | Concat Doc Doc
  | Line
  | Nest Int Doc
  | Align Doc
  | Column (Int -> Doc)
  | NestAbsolute Int Doc
  | LayoutChoice Doc Doc
  | SetLayout Layout Doc
  | HardLine

-- Concatenates two documents with nothing in between.
instance Semigroup Doc where
  x <> y = Concat x y

-- `empty` is the identity for `<>`.
instance Monoid Doc where
  mempty = Empty

infixr 6 <+>

-- Concatenates `x` and `y` separated by a single space.
(<+>) :: Doc -> Doc -> Doc
x <+> y = x <> space <> y

infixr 5 <|>

-- Concatenates `x` and `y` separated by a soft line break.
(<|>) :: Doc -> Doc -> Doc
x <|> y = x <> line <> y

text :: String -> Doc
text str = Text str

line :: Doc
line = Line

hardline :: Doc
hardline = HardLine

space :: Doc
space = Text " "

empty :: Doc
empty = Empty

nest :: Int -> Doc -> Doc
nest level doc = Nest level doc

nestAbsolute :: Int -> Doc -> Doc
nestAbsolute level doc = NestAbsolute level doc

align :: Doc -> Doc
align doc = Align doc

column :: (Int -> Doc) -> Doc
column f = Column f

setLayout :: Layout -> Doc -> Doc
setLayout layout doc = SetLayout layout doc

layoutChoice :: Doc -> Doc -> Doc
layoutChoice singleLine multiLine = LayoutChoice singleLine multiLine

-- Renders `doc` and, in MultiLine mode, pads it with spaces
fill :: Int -> Doc -> Doc
fill width doc =
  layoutChoice
    doc
    (column (\k1 -> doc <> column (\k2 ->
      let used = k2 - k1
      in if used >= width then empty else text (replicate (width - used) ' '))))

-- Folds `docs` with `<|>`.
stack :: [Doc] -> Doc
stack docs = folddoc (<|>) docs

-- Folds `docs` with `hardline`.
hardStack :: [Doc] -> Doc
hardStack docs = folddoc (\a b -> a <> hardline <> b) docs

-- Folds `docs` separated by `separator`.
sep :: Doc -> [Doc] -> Doc
sep separator docs = folddoc (\a b -> a <> separator <> b) docs

-- Right fold over `docs` with `f`.
folddoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
folddoc _ []       = Empty
folddoc _ [x]      = x
folddoc f (x : xs) = f x (folddoc f xs)


-- Renders `doc` using the default layout mode MultiLine.
pretty :: Doc -> String
pretty doc = render "" 0 [(0, MultiLine, doc)]

-- Renders `doc` under the given top-level layout mode.
prettyWith :: Layout -> Doc -> String
prettyWith layout doc = render "" 0 [(0, layout, doc)]

trimTrailing :: String -> String
trimTrailing sb = reverse (dropWhile (== ' ') (reverse sb))


render :: String -> Int -> [(Int, Layout, Doc)] -> String
render sb _ [] = sb

render sb k ((_, _, Empty) : z) =
  render sb k z

render sb k ((i, l, Concat x y) : z) =
  render sb k ((i, l, x) : (i, l, y) : z)

render sb k ((_, _, Text s) : z) =
  render (sb ++ s) (k + length s) z

render sb k ((i, l, Line) : z) =
  case l of
    SingleLine ->
      render (sb ++ " ") (k + 1) z
    MultiLine  ->
      render (trimTrailing sb ++ "\n" ++ replicate i ' ') i z

render sb _ ((i, _, HardLine) : z) =
  render (trimTrailing sb ++ "\n" ++ replicate i ' ') i z

render sb k ((i, l, Nest j x) : z) =
  case l of
    SingleLine -> render sb k ((i,     l, x) : z)
    MultiLine  -> render sb k ((i + j, l, x) : z)

render sb k ((i, l, Align x) : z) =
  case l of
    SingleLine -> render sb k ((i, l, x) : z)
    MultiLine  -> render sb k ((k, l, x) : z)

render sb k ((i, l, Column f) : z) =
  render sb k ((i, l, f k) : z)

render sb k ((i, l, LayoutChoice s m) : z) =
  case l of
    SingleLine -> render sb k ((i, l, s) : z)
    MultiLine  -> render sb k ((i, l, m) : z)

render sb k ((i, _, SetLayout newLayout x) : z) =
  render sb k ((i, newLayout, x) : z)

render sb k ((_, l, NestAbsolute j x) : z) =
  render sb k ((j, l, x) : z)