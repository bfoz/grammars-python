import pytest

import grammars.python

from recursive_descent import Node, RecursiveDescent

def _and_test(name):
    return Node(grammars.python.and_test,
        items=[
            Node(grammars.python.not_test,
                items=[
                    [],
                    Node(grammars.python.comparison,
                        items=[
                            Node(grammars.python.expr,
                                items=[
                                    Node(grammars.python.xor_expr,
                                        items=[
                                            Node(grammars.python.and_expr,
                                                items=[
                                                    Node(grammars.python.shift_expr,
                                                        items=[
                                                            Node(grammars.python.arith_expr,
                                                                items=[
                                                                    Node(grammars.python.term,
                                                                        items=[
                                                                            Node(grammars.python.factor,
                                                                                items=[
                                                                                    [],
                                                                                    Node(grammars.python.power,
                                                                                        items=[
                                                                                            Node(grammars.python.atom_expr,
                                                                                                items=[
                                                                                                    [],
                                                                                                    Node(grammars.python.atom, Node(grammars.python.NAME, name)),
                                                                                                    []
                                                                                                ]
                                                                                            ),
                                                                                            []
                                                                                        ]
                                                                                    )
                                                                                ]
                                                                            ),
                                                                            []
                                                                        ]
                                                                    ),
                                                                    []
                                                                ]
                                                            ),
                                                            []
                                                        ]
                                                    ),
                                                    []
                                                ]
                                            ),
                                            []
                                        ]
                                    ),
                                    []
                                ]
                            ),
                            []
                        ]
                    )
                ]
            ),
            []
        ]
    )

def test_arglist():
    parser = RecursiveDescent(grammars.python.arglist)
    tree = parser.parse("'abc', 'def'")
    assert tree == Node(
        rule=grammars.python.arglist,
        items=[
            Node(
                rule=grammars.python.argument,
                items=Node(
                    rule=grammars.python.argument[0],
                    items=[
                        Node(
                            rule=grammars.python.test,
                            items=Node(
                                rule=grammars.python.test[0],
                                items=[
                                    Node(
                                        rule=grammars.python.or_test,
                                        items=[
                                            Node(
                                                rule=grammars.python.and_test,
                                                items=[
                                                    Node(
                                                        rule=grammars.python.not_test,
                                                        items=[
                                                            [],
                                                            Node(
                                                                rule=grammars.python.comparison,
                                                                items=[
                                                                    Node(
                                                                        rule=grammars.python.expression,
                                                                        items=[
                                                                            Node(
                                                                                rule=grammars.python.xor_expr,
                                                                                items=[
                                                                                    Node(
                                                                                        rule=grammars.python.and_expr,
                                                                                        items=[
                                                                                            Node(
                                                                                                rule=grammars.python.shift_expr,
                                                                                                items=[
                                                                                                    Node(
                                                                                                        rule=grammars.python.arith_expr,
                                                                                                        items=[
                                                                                                            Node(
                                                                                                                rule=grammars.python.term,
                                                                                                                items=[
                                                                                                                    Node(
                                                                                                                        rule=grammars.python.factor,
                                                                                                                        items=[
                                                                                                                            [],
                                                                                                                            Node(
                                                                                                                                rule=grammars.python.power,
                                                                                                                                items=[
                                                                                                                                    Node(
                                                                                                                                        rule=grammars.python.atom_expr,
                                                                                                                                        items=[
                                                                                                                                            [],
                                                                                                                                            Node(
                                                                                                                                                rule=grammars.python.atom,
                                                                                                                                                items=Node(
                                                                                                                                                    rule=grammars.python.stringliteral,
                                                                                                                                                    items=[
                                                                                                                                                        [],
                                                                                                                                                        Node(
                                                                                                                                                            rule=grammars.python.stringliteral[1],
                                                                                                                                                            items=Node(
                                                                                                                                                                rule=grammars.python.shortstring,
                                                                                                                                                                items=Node(
                                                                                                                                                                    rule=grammars.python.shortstring[0],
                                                                                                                                                                    items=[
                                                                                                                                                                        Node(rule="'", items="'"),
                                                                                                                                                                        Node(rule=grammars.python.shortstringitem, items='abc'),
                                                                                                                                                                        Node(rule="'", items="'")
                                                                                                                                                                    ]
                                                                                                                                                                )
                                                                                                                                                            )
                                                                                                                                                        )
                                                                                                                                                    ]
                                                                                                                                                )
                                                                                                                                            ),
                                                                                                                                            []
                                                                                                                                        ]
                                                                                                                                    ),
                                                                                                                                    []
                                                                                                                                ]
                                                                                                                            )
                                                                                                                        ]
                                                                                                                    ),
                                                                                                                    []
                                                                                                                ]
                                                                                                            ),
                                                                                                            []
                                                                                                        ]
                                                                                                    ),
                                                                                                    []
                                                                                                ]
                                                                                            ),
                                                                                            []
                                                                                        ]
                                                                                    ),
                                                                                    []
                                                                                ]
                                                                            ),
                                                                            []
                                                                        ]
                                                                    ),
                                                                    []
                                                                ]
                                                            )
                                                        ]
                                                    ),
                                                    []
                                                ]
                                            ),
                                            []
                                        ]
                                    ),
                                    []
                                ]
                            )
                        ),
                        []
                    ]
                )
            ),
            [
                Node(
                    rule=grammars.python.arglist[1].element,
                    items=[
                        Node(rule=',', items=','),
                        Node(
                            rule=grammars.python.argument,
                            items=Node(
                                rule=grammars.python.argument[0],
                                items=[
                                    Node(
                                        rule=grammars.python.test,
                                        items=Node(
                                            rule=grammars.python.test[0],
                                            items=[
                                                Node(
                                                    rule=grammars.python.or_test,
                                                    items=[
                                                        Node(
                                                            rule=grammars.python.and_test,
                                                            items=[
                                                                Node(
                                                                    rule=grammars.python.not_test,
                                                                    items=[
                                                                        [],
                                                                        Node(
                                                                            rule=grammars.python.comparison,
                                                                            items=[
                                                                                Node(
                                                                                    rule=grammars.python.expression,
                                                                                    items=[
                                                                                        Node(
                                                                                            rule=grammars.python.xor_expr,
                                                                                            items=[
                                                                                                Node(
                                                                                                    rule=grammars.python.and_expr,
                                                                                                    items=[
                                                                                                        Node(
                                                                                                            rule=grammars.python.shift_expr,
                                                                                                            items=[
                                                                                                                Node(
                                                                                                                    rule=grammars.python.arith_expr,
                                                                                                                    items=[
                                                                                                                        Node(
                                                                                                                            rule=grammars.python.term,
                                                                                                                            items=[
                                                                                                                                Node(
                                                                                                                                    rule=grammars.python.factor,
                                                                                                                                    items=[
                                                                                                                                        [],
                                                                                                                                        Node(
                                                                                                                                            rule=grammars.python.power,
                                                                                                                                            items=[
                                                                                                                                                Node(
                                                                                                                                                    rule=grammars.python.atom_expr,
                                                                                                                                                    items=[
                                                                                                                                                        [],
                                                                                                                                                        Node(
                                                                                                                                                            rule=grammars.python.atom,
                                                                                                                                                            items=Node(
                                                                                                                                                                rule=grammars.python.stringliteral,
                                                                                                                                                                items=[
                                                                                                                                                                    [],
                                                                                                                                                                    Node(
                                                                                                                                                                        rule=grammars.python.stringliteral[1],
                                                                                                                                                                        items=Node(
                                                                                                                                                                            rule=grammars.python.shortstring,
                                                                                                                                                                            items=Node(
                                                                                                                                                                                rule=grammars.python.shortstring[0],
                                                                                                                                                                                items=[
                                                                                                                                                                                    Node(rule="'", items="'"),
                                                                                                                                                                                    Node(rule=grammars.python.shortstringitem, items='def'),
                                                                                                                                                                                    Node(rule="'", items="'")
                                                                                                                                                                                ]
                                                                                                                                                                            )
                                                                                                                                                                        )
                                                                                                                                                                    )
                                                                                                                                                                ]
                                                                                                                                                            )
                                                                                                                                                        ),
                                                                                                                                                        []
                                                                                                                                                    ]
                                                                                                                                                ),
                                                                                                                                                []
                                                                                                                                            ]
                                                                                                                                        )
                                                                                                                                    ]
                                                                                                                                ),
                                                                                                                                []
                                                                                                                            ]
                                                                                                                        ),
                                                                                                                        []
                                                                                                                    ]
                                                                                                                ),
                                                                                                                []
                                                                                                            ]
                                                                                                        ),
                                                                                                        []
                                                                                                    ]
                                                                                                ),
                                                                                                []
                                                                                            ]
                                                                                        ),
                                                                                        []
                                                                                    ]
                                                                                ),
                                                                                []
                                                                            ]
                                                                        )
                                                                    ]
                                                                ),
                                                                []
                                                            ]
                                                        ),
                                                        []
                                                    ]
                                                ),
                                                []
                                            ]
                                        )
                                    ),
                                    []
                                ]
                            )
                        )
                    ]
                )
            ],
            []
        ]
    )

def test_or_test():
    parser = RecursiveDescent(grammars.python.or_test)
    tree = parser.parse('a or b')
    assert tree == Node(grammars.python.or_test,
        items=[
            _and_test('a'),
            [
                Node(
                    rule=grammars.python.or_test[1].element,
                    items=[
                        Node('or', 'or'),
                        _and_test('b')
                    ]
                )
            ]
        ]
    )

def test_test():
    parser = RecursiveDescent(grammars.python.test)
    tree = parser.parse('a or b')
    assert tree == Node(
        grammars.python.test,
        items=Node(
            grammars.python.test[0],
            items=[
                Node(
                    grammars.python.or_test,
                    items=[
                        _and_test('a'),
                        [
                            Node(
                                rule=grammars.python.or_test[1].element,
                                items=[
                                    Node('or', 'or'),
                                    _and_test('b')
                                ]
                            )
                        ]
                    ]
                ),
                []
            ]
        )
    )

def test_expression():
    """ expr: xor_expr ('|' xor_expr)* """
    parser = RecursiveDescent(grammars.python.expression)
    tree = parser.parse('a | b')
    assert tree == Node(
        rule=grammars.python.expression,
        items=[
            Node(
                rule=grammars.python.xor_expr,
                items=[
                    Node(
                        rule=grammars.python.and_expr,
                        items=[
                            Node(
                                rule=grammars.python.shift_expr,
                                items=[
                                    Node(
                                        rule=grammars.python.arith_expr,
                                        items=[
                                            Node(
                                                rule=grammars.python.term,
                                                items=[
                                                    Node(
                                                        rule=grammars.python.factor,
                                                        items=[
                                                            [],
                                                            Node(
                                                                rule=grammars.python.power,
                                                                items=[
                                                                    Node(
                                                                        rule=grammars.python.atom_expr,
                                                                        items=[
                                                                            [],
                                                                            Node(
                                                                                rule=grammars.python.atom,
                                                                                items=Node(rule=grammars.python.NAME, items='a')
                                                                            ),
                                                                            []
                                                                        ]
                                                                    ),
                                                                    []
                                                                ]
                                                            )
                                                        ]
                                                    ),
                                                    []
                                                ]
                                            ),
                                            []
                                        ]
                                    ),
                                    []
                                ]
                            ),
                            []
                        ]
                    ),
                    []
                ]
            ),
            [
                Node(
                    rule=grammars.python.expression[1].element,
                    items=[
                        Node(rule='|', items='|'),
                        Node(
                            rule=grammars.python.xor_expr,
                            items=[
                                Node(
                                    rule=grammars.python.and_expr,
                                    items=[
                                        Node(
                                            rule=grammars.python.shift_expr,
                                            items=[
                                                Node(
                                                    rule=grammars.python.arith_expr,
                                                    items=[
                                                        Node(
                                                            rule=grammars.python.term,
                                                            items=[
                                                                Node(
                                                                    rule=grammars.python.factor,
                                                                    items=[
                                                                        [],
                                                                        Node(
                                                                            rule=grammars.python.power,
                                                                            items=[
                                                                                Node(
                                                                                    rule=grammars.python.atom_expr,
                                                                                    items=[
                                                                                        [],
                                                                                        Node(
                                                                                            rule=grammars.python.atom,
                                                                                            items=Node(rule=grammars.python.NAME, items='b')
                                                                                        ),
                                                                                        []
                                                                                    ]
                                                                                ),
                                                                                []
                                                                            ]
                                                                        )
                                                                    ]
                                                                ),
                                                                []
                                                            ]
                                                        ),
                                                        []
                                                    ]
                                                ),
                                                []
                                            ]
                                        ),
                                        []
                                    ]
                                ),
                                []
                            ]
                        )
                    ]
                )
            ]
        ]
    )

def test_star_expr():
    """ star_expr: '*' expr """
    parser = RecursiveDescent(grammars.python.star_expr)
    tree = parser.parse('*a')
    assert tree == Node(
        rule=grammars.python.star_expr,
        items=[
            Node(rule='*', items='*'),
            Node(
                rule=grammars.python.expression,
                items=[
                    Node(
                        rule=grammars.python.xor_expr,
                        items=[
                            Node(
                                rule=grammars.python.and_expr,
                                items=[
                                    Node(
                                        rule=grammars.python.shift_expr,
                                        items=[
                                            Node(
                                                rule=grammars.python.arith_expr,
                                                items=[
                                                    Node(
                                                        rule=grammars.python.term,
                                                        items=[
                                                            Node(
                                                                rule=grammars.python.factor,
                                                                items=[
                                                                    [],
                                                                    Node(
                                                                        rule=grammars.python.power,
                                                                        items=[
                                                                            Node(
                                                                                rule=grammars.python.atom_expr,
                                                                                items=[
                                                                                    [],
                                                                                    Node(
                                                                                        rule=grammars.python.atom,
                                                                                        items=Node(rule=grammars.python.NAME, items='a')
                                                                                    ),
                                                                                    []
                                                                                ]
                                                                            ),
                                                                            []
                                                                        ]
                                                                    )
                                                                ]
                                                            ),
                                                            []
                                                        ]
                                                    ),
                                                    []
                                                ]
                                            ),
                                            []
                                        ]
                                    ),
                                    []
                                ]
                            ),
                            []
                        ]
                    ),
                    []
                ]
            )
        ]
    )

def test_import_stmt():
    parser = RecursiveDescent(grammars.python.import_stmt)
    tree = parser.parse('from foo.subfoo import Alpha, Bravo, Charlie,')
    assert tree == Node(
        rule=grammars.python.import_stmt,
        items=Node(
            rule=grammars.python.import_from,
            items=[
                Node(rule='from', items='from'),
                Node(
                    rule=grammars.python.import_from[1],
                    items=Node(
                        rule=grammars.python.import_from[1][0],
                        items=[
                            [],
                            Node(
                                rule=grammars.python.dotted_name,
                                items=[
                                    Node(rule=grammars.python.NAME, items='foo'),
                                    [
                                        Node(
                                            rule=grammars.python.dotted_name[1].element,
                                            items=[
                                                Node(rule='.', items='.'),
                                                Node(rule=grammars.python.NAME, items='subfoo')
                                            ]
                                        )
                                    ]
                                ]
                            )
                        ]
                    )
                ),
                Node(rule='import', items='import'),
                Node(
                    rule=grammars.python.import_from[3],
                    items=Node(
                        rule=grammars.python.import_as_names,
                        items=[
                            Node(
                                rule=grammars.python.import_as_name,
                                items=[
                                    Node(rule=grammars.python.NAME, items='Alpha'),
                                    []
                                ]
                            ),
                            [
                                Node(
                                    rule=grammars.python.import_as_names[1].element,
                                    items=[
                                        Node(rule=',', items=','),
                                        Node(
                                            rule=grammars.python.import_as_name,
                                            items=[
                                                Node(rule=grammars.python.NAME, items='Bravo'),
                                                []
                                            ]
                                        )
                                    ]
                                ),
                                Node(
                                    rule=grammars.python.import_as_names[1].element,
                                    items=[
                                        Node(rule=',', items=','),
                                        Node(
                                            rule=grammars.python.import_as_name,
                                            items=[
                                                Node(rule=grammars.python.NAME, items='Charlie'),
                                                []
                                            ]
                                        )
                                    ]
                                )
                            ],
                            Node(rule=',', items=',')
                        ]
                    )
                )
            ]
        )
    )
