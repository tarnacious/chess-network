var failure = function(input) { 
    return [];
};

var item = function(input) {
    if (input.length === 0) {
        return [];
    } else {
        return [[input[0],input.slice(1)]];
    }
};

var ret = function(v) {
    return function(input) {
        return [[v, input]];
    };
};

var parse = function(p, input) {
    if (typeof(p) == 'object') {
        console.log(p);
    }
    return p(input);
};

var or = function(p1, p2) {
    return function(input) {
        var first = parse(p1, input);
        if (first.length === 0) {
            return parse(p2, input);
        } else {
            return first;
        }
    };
};

var sat = function(cont) {
    return function(input) {
        var head = parse(item, input);
        if (head.length > 0) {
            if (cont(head[0][0])) {
                return head;
            } else {
                return [];
            }
        } else {
            return [];

        }
    };
};

var _do = function(p1, p2) {
    return function(input) {
        var first = parse(p1, input);
        if (first.length > 0) {
            var value = first[0][0];
            var remain = first[0][1];
            return parse(p2(value),remain);
        } else {
            return [];
        }
    };
};

var _char = function(p) {
    return sat(function(x) { return x == p; });
};

var _string = function(str) {
    if (str.length === 0) return ret([]);
    var head = str[0];
    var tail = str.slice(1);
    return _do(_char(head), function(h) {
                return _do(_string(tail), function(t) {
                    return ret(h + t);
                });
    });
}; 

var take = function(p, n) {
    if (n === 0) return ret([]);
    return _do(p, function(t) {
        return _do(take(p, n-1),function(r) {
            return ret([t].concat(r));
        });
    });
};

var many = function(p) {
    return or(many1(p), ret([]));
};

var many1 = function(p) {
    return _do(p, function(q) {
        return _do(many(p), function(z) {
            return ret([q].concat(z));
        });
    });
};

var space = many(or(_char(" "),_char("\n"))); 

var turn = _do(space, function(_) {
    return _do(or(_string("Black"),_string("White")), function(t) {
        return ret(t);
    });
});

var pieceType = or(_char("K"), 
              or(_char("Q"),
                or(_char("B"), 
                  or(_char("N"), 
                    or(_char("R"), _char("P"))))));

var colour = or(_char("B"), _char("W"));

var empty = _do(_char('-'), function(e1) { 
    return _do(_char('-'), function(e1) {
        return ret("==");
    });
});

var piece = _do(colour, function(c) {    
    return _do(pieceType, function(p) {
        return ret(c.toLowerCase()+p);
    });
});

var square = _do(space, function(_) {
    return _do(or(piece, empty), function(s) {
        return ret(s);
    });
});

var row = take(square, 8);
var board = take(row, 8);

var colParser = or(_char('1'),
             or(_char('2'),
              or(_char('3'),
               or(_char('4'),
                or(_char('5'), 
                 or(_char('6'), 
                  or(_char('7'), _char('8'))))))));

var rowParser = or(_char('a'),
             or(_char('b'),
              or(_char('c'),
               or(_char('d'),
                or(_char('e'), 
                 or(_char('f'), 
                  or(_char('g'), _char('h'))))))));


var posParser = _do(rowParser, function(row) {
    return _do(colParser, function(col) {
        return ret(row + col);
    });
});

var moveParser = _do(space, function(_) {
    return _do(piece, function(p) {
        return _do(space, function(_) {
            return _do(posParser, function(source) {
                return _do(_char(':'), function(_) {
                    return _do(posParser, function(target) {
                        return ret({ piece: p, source: source, target: target });
                    });
                });
            });
        });
    });
});

                  
var state = _do(turn, function(t) {
    return _do(board, function(b) {
       return _do(many(moveParser), function(m) {
          return ret({'turn': t, 'board': b, 'moves': m});
        });
    });
});

var parseState = function(s) {
    var result = parse(state, s);
    if (result.length === 0) {
        return null; 
    } else {
        return result[0][0];
    }
};

exports.parse = parseState;
