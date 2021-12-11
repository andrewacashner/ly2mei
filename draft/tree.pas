{ `BinaryTree`

  Basic binary tree implementation

  Andrew Cashner, 2021/11/03
}

{$mode objfpc}{$H+}{$J-}

program BinaryTree;

uses SysUtils;

type
  TPNode = ^TNode;
  TNode = record
    FData: String;
    FPLeft, FPRight: TPNode;
  end;

function NewNode(Data: String): TPNode;
var
  Branch: TPNode;
begin
  new(Branch);
  Branch^.FData := Data;
  Branch^.FPLeft := nil;
  Branch^.FPRight := nil;
  result := Branch;
end;

function AddBranch(Tree, Left, Right: TPNode): TPNode;
begin
  assert(Tree <> nil);
  Tree^.FPLeft := Left;
  Tree^.FPRight := Right;
  result := Tree;
end;

function TreeToStringPreorder(Tree: TPNode): String;
var
  OutputStr: String = '';
begin
  if Tree <> nil then
  begin
    if Length(OutputStr) > 0 then
      OutputStr := OutputStr + ' '; 

    OutputStr := OutputStr + Tree^.FData + ' '
                  + TreeToStringPreorder(Tree^.FPLeft)
                  + TreeToStringPreorder(Tree^.FPRight);
  end;
  result := OutputStr;
end;

procedure FreeTree(Tree: TPNode);
begin
  if Tree <> nil then
  begin
    FreeTree(Tree^.FPLeft);
    FreeTree(Tree^.FPRight);
    dispose(Tree);
  end;
end;

{ MAIN }
var
  BTree: TPNode = nil;
begin
  { Binary tree:
    1
      1.1
        1.1.1
          1.1.1.1
          1.1.1.2
        1.1.2
          1.1.2.1
      1.2
        1.2.1
          1.2.1.1
        1.2.2
          1.2.2.1
          1.2.2.2
    2
      2.1
        2.1.1
        2.1.2
      2.2
        2.2.1
          2.2.1.1
          2.2.1.2
        2.2.2
          2.2.2.1
  }
  BTree := AddBranch(NewNode('root'),
                  AddBranch(NewNode('1'), 
                    AddBranch(NewNode('1.1'), 
                      AddBranch(NewNode('1.1.1'), NewNode('1.1.1.1'), NewNode('1.1.1.2')),
                      AddBranch(NewNode('1.1.2'), NewNode('1.1.2.1'), nil)),
                    AddBranch(NewNode('1.2'),
                      AddBranch(NewNode('1.2.1'), NewNode('1.2.1.1'), nil),
                      AddBranch(NewNode('1.2.2'), NewNode('1.2.2.1'), NewNode('1.2.2.2')))),
                  AddBranch(NewNode('2'),
                    AddBranch(NewNode('2.1'), NewNode('2.1.1'), NewNode('2.1.2')),
                    AddBranch(NewNode('2.2'),
                      AddBranch(NewNode('2.2.1'), NewNode('2.2.1.1'), NewNode('2.2.1.2')),
                      AddBranch(NewNode('2.2.2'), NewNode('2.2.2.1'), nil))));
  WriteLn(TreeToStringPreorder(BTree));
  FreeTree(BTree);

  { Left-child, right-sibling tree 
    1
      1.1
        1.1.1
          1.1.1.1
          1.1.1.2
        1.1.2
          1.1.2.1
      1.2
        1.2.1
          1.2.1.1
        1.2.2
          1.2.2.1
          1.2.2.2
      1.3
        1.3.1
          1.3.1.1
          1.3.1.1
      1.4
    2
      2.1
        2.1.1
        2.1.2
      2.2
        2.2.1
          2.2.1.1
          2.2.1.2
        2.2.2
          2.2.2.1
    3
      3.1
        3.1.1
      3.2
        3.2.1
        3.2.2
    4
      4.1
  }
  BTree := nil;
  BTree := AddBranch(NewNode('1'),
            AddBranch(NewNode('1.1'), 
              AddBranch(NewNode('1.1.1'), 
                AddBranch(NewNode('1.1.1.1'), nil, NewNode('1.1.1.2')),
                AddBranch(NewNode('1.1.2'), NewNode('1.1.2.1'), nil)),
              AddBranch(NewNode('1.2'),
                AddBranch(NewNode('1.2.1'), 
                  NewNode('1.2.1.1'),
                  AddBranch(NewNode('1.2.2'), 
                    AddBranch(NewNode('1.2.2.1'), nil, NewNode('1.2.2.2')), 
                    nil)),
                AddBranch(NewNode('1.3'),
                  AddBranch(NewNode('1.3.1'),
                    AddBranch(NewNode('1.3.1.1'), nil, NewNode('1.3.1.2')),
                    nil), 
                  NewNode('1.4')))),
            AddBranch(NewNode('2'),
              AddBranch(NewNode('2.1'),
                AddBranch(NewNode('2.1.1'), nil, NewNode('2.1.2')),
                AddBranch(NewNode('2.2'),
                  AddBranch(NewNode('2.2.1'),
                    AddBranch(NewNode('2.2.1.1'), nil, NewNode('2.2.1.2')),
                    AddBranch(NewNode('2.2.2'), NewNode('2.2.2.1'), nil)),
                  nil)),
              AddBranch(NewNode('3'), 
                AddBranch(NewNode('3.1'), 
                  NewNode('3.1.1'), 
                  AddBranch(NewNode('3.2'), 
                    AddBranch(NewNode('3.2.1'), nil, NewNode('3.2.2')),
                    nil)),
                AddBranch(NewNode('4'), NewNode('4.1'), nil))));

  WriteLn(TreeToStringPreorder(BTree));
  FreeTree(BTree);
end.

{ OR: binary tree as nested list
    [ 1
      [ 1.1
        [ 1.1.1
          [ 1.1.1.1
          , 1.1.1.2
          ]
        , 1.1.2
          [ 1.1.2.1 ]
        ]
      , 1.2
        [ 1.2.1
          [ 1.2.1.1 ]
        , 1.2.2
          [ 1.2.2.1
          , 1.2.2.2
          ]
        ]
      , 1.3
        [ 1.3.1
          [ 1.3.1.1
          , 1.3.1.1
          ]
        , 1.4
        ]
      ]
    , 2
      [ 2.1
        [ 2.1.1
        , 2.1.2
        ]
      , 2.2
        [ 2.2.1
          [ 2.2.1.1
          , 2.2.1.2
          ]
        , 2.2.2
          [ 2.2.2.1 ]
        ]
      ]
    , 3
      [ 3.1
        [ 3.1.1 ]
      , 3.2
        [ 3.2.1
        , 3.2.2
        ]
      ]
    , 4
      [ 4.1 ]
    ]
}
