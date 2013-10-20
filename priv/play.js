var WebSocket = require('faye-websocket'),
    Parser = require('./parser.js');

var Connection = function() {
};

Connection.prototype.sendMessage = function(message) {
    var command = { "command": "message", 
                    "message": message };
    this.send(command);
};

Connection.prototype.sendMove = function(move) {
    var command = { "command": "move", 
                    "move": move }
    this.send(command);
};

Connection.prototype.send = function(command) {
    var json = JSON.stringify(command);
    console.log("sending", json);
    this.socket.send(json);

};

Connection.prototype.onOpen = function() {
    console.log("socket open");
};

Connection.prototype.onData = function(json) {
    var data = JSON.parse(json);
    if (data.type == 'message') {
        this.onMessage(data.message);
    }
    if (data.type == 'state') {
        this.onState(data.state);
    }
}

Connection.prototype.onMessage = function(message) {
    console.log("socket message", message);
}

Connection.prototype.onState = function(state) {
    console.log("socket state", state);
}

Connection.prototype.createWebSocket = function(room) {
    var _this = this;
    this.socket = new WebSocket.Client('ws://localhost:5000/' + room);
    this.socket.onopen = function() {
        _this.onOpen();
    };
    this.socket.onmessage = function (evt) {
        _this.onData(evt.data);
    };
    this.socket.onclose = function() {
        console.log("socket closed");
    };
};




var Player = function(room, color) {
    this.room = room;
    this.color = color;
    this.createWebSocket(room);
}

Player.prototype = new Connection();
Player.prototype.constructor = Player;

Player.prototype.makeMove =  function(game) {
    var moves = game.moves.length;
    var random = Math.random();
    var index = Math.floor(random * moves);
    var move = game.moves[index];
    console.log(random, index, moves);
    var command = [move.piece.toUpperCase(), " ", move.source, ":", move.target].join("");
    console.log(command);
    this.sendMove(command);
};

Player.prototype.onState = function(state) {
    console.log("got state: ", state);
    var game = Parser.parse(state);
    if (game.moves.length == 0) {
        console.log("no moves");
        return;
    }
    if (game.turn === this.color) {
        this.makeMove(game);
    };
};
    

var player1 = new Player("myroom", "White");
var player2 = new Player("myroom", "Black");
