module Main where

import Doc

ifExpr :: Doc -> Doc -> Doc -> Doc
ifExpr c t e =
  layoutChoice
    -- SingleLine:  if c then t else e
    (text "if" <+> c 
      <+> text "then" 
      <> line <> t <+> text "else" 
      <> line <> e <> line <> text "end")
    -- MultiLine:
    --   if c
    --   then t
    --   else e
    ( text "if" <+> c
        <> line
        <> nest 2 (text "then" <+> t)
        <> line
        <> text "else" <+> e
    )

--
--   SingleLine:  [1, 2, 3]
--
--   MultiLine:
--     [
--       1,
--       2,
--       3
--     ]
list :: [Doc] -> Doc
list items =
  layoutChoice
    (text "[" <> sep (text ", ") items <> text "]")
    ( text "["
        <> nest 2 (hardline <> sep (text "," <> hardline) items)
        <> hardline
        <> text "]"
    )

something :: Doc
something =   
  layoutChoice
    -- SingleLine
    (text "something" <> line <> text "else")
    -- MultiLine
    (text "something" <> hardline <> text "else")

exampleNestAlign :: Doc
exampleNestAlign =
  layoutChoice
    -- SingleLine
    (text "Example:" <+> text "aligned" <+> text "and nested")
    -- MultiLine
    (text "Example: "
        <> hardline
        <> align (text "aligned" <+> text "text")
        <> hardline
        <> nestAbsolute 4 (text "nested" <+> text "text")
    )

enumDecl :: String -> [(String, [String])] -> Doc
enumDecl name cases =
  let caseDoc (cname, tys) =
        text "case" <+> text cname
          <> text "(" <> sep (text ", ") (map text tys) <> text ")"
  in layoutChoice
       -- SingleLine
       ( text "enum" <+> text name <+> text "{"
           <+> sep (text ", ") (map caseDoc cases)
           <+> text "}"
       )
       -- MultiLine
       ( text "enum" <+> text name <+> text "{"
           <> nest 4 (hardline <> sep (text "," <> hardline) (map caseDoc cases))
           <> hardline
           <> text "}"
       )

matchExpr :: Doc -> [(String, String)] -> Doc
matchExpr expr cases =
  let maxLen = maximum (map (length . fst) cases)
      pad p  = p ++ replicate (maxLen - length p) ' '
      aligned (p, e) = text "case" <+> text (pad p) <+> text "=>" <+> text e
      flat    (p, e) = text "case" <+> text p       <+> text "=>" <+> text e
  in layoutChoice
       -- SingleLine
       ( text "match" <+> expr <+> text "{"
           <+> sep (text " ") (map flat cases)
           <+> text "}"
       )
       -- MultiLine
       ( text "match" <+> expr <+> text "{")
           <> nest 4 (hardline <> sep hardline (map aligned cases))
           <> hardline
           <> text "}"

shapeEnum :: Doc
shapeEnum =
  enumDecl "Shape"
    [ ("Circle",    ["Int32"])
    , ("Square",    ["Int32"])
    , ("Rectangle", ["Int32", "Int32"])
    ]

defArea :: Doc
defArea =
  text "def area(s: Shape): Int32 ="
    <+> matchExpr (text "s")
          [ ("Circle(r)",       "3 * (r * r)")
          , ("Square(w)",       "w * w")
          , ("Rectangle(h, w)", "h * w")
          ]

shapeProgram :: Doc
shapeProgram = shapeEnum <> hardline <> defArea

section :: String -> String -> IO ()
section title body = do
  putStrLn (replicate 50 '-')
  putStrLn ("-- " ++ title)
  putStrLn (replicate 50 '-')
  putStrLn body
  putStrLn ""

main :: IO ()
main = do
  let nums = map (text . show) [1, 2, 3 :: Int]
      ifD  = ifExpr (text "x > 0") (text "x") (text "0 - x")

  section "ifExpr, single-line layout"
    (prettyWith SingleLine ifD)

  section "ifExpr, multi-line layout"
    (prettyWith MultiLine ifD)

  section "list, forced single-line via setLayout"
    (pretty (setLayout SingleLine (list nums)))

  section "list, multi-line (default)"
    (pretty (list nums))

  section "shape program, multi-line layout"
    (prettyWith MultiLine shapeProgram)