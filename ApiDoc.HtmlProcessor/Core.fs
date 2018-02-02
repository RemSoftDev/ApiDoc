module Core

type Tree<'Content> =
    | Leaf of 'Content
    | Branch of Tree<'Content> list
