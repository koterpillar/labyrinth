$(document).ready(function () {
    var gameId = null;
    var gameLength = 0;

    function privateMove(move) {
        return /^(choose|reorder)/.test(move);
    }

    function refreshGames(noTimer) {
        $.getJSON('/list', function (result) {
            var list = $('#games');
            list.html('');
            $.each(result, function (game, details) {
                var desc = game + " - " +
                    details.width + "x" + details.height + ", " +
                    details.players + " players";
                if (details.gameEnded) {
                    desc += ", finished";
                }
                var link = $('<a href="/">' + desc + '</a>');
                link.click(function () {
                    gameId = game;
                    return false;
                });
                list.append(link);
                list.append('<br />');
            });
        });
        if (!noTimer) {
            setTimeout(refreshGames, 5000);
        }
    }

    $('#add_game').submit(function () {
        var form = $(this);
        $.post('/add', form.serializeArray(), function (result) {
            refreshGames(true);
        });
        return false;
    });

    $('#back_to_list').click(function () {
        gameId = null;
        refreshGame(true);
        return false;
    });

    function refreshGame(noTimer) {
        if (gameId) {
            var firstSwitch = !$('#game').is(':visible');
            $('#game').show();
            $('#games_container').hide();
            $.getJSON('/' + gameId + '/log', function (result) {
                var log = result.log;
                if (firstSwitch || log.length > gameLength) {
                    var str = "";
                    for (var i = 0; i < log.length; i++) {
                        var move = log[i];
                        str += move.player + " > " + move.move + "<br />";
                        str += move.result + "<br />";
                    }
                    if (result.game.gameEnded) {
                        str += "<pre>" + result.game.map + "</pre>";
                    }
                    $('#history').html(str);
                    scrollDown();
                    gameLength = log.length;
                }
                var player_input = $('#make_move_player');
                if (player_input.val() === "" ||
                    $('#make_move_hotseat').is(':checked')) {
                    player_input.val(result.game.currentTurn);
                }
                player_input.attr('max', result.game.players - 1);
            });
        } else {
            $('#game').hide();
            $('#games_container').show();
        }
        if (!noTimer) {
            setTimeout(refreshGame, 1000);
        }
    }

    function scrollDown() {
        var game_content = $('#history');
        if (game_content.is(':visible')) {
            var totalHeight  = window.innerHeight;
            var headerHeight = $('.header:visible').outerHeight(true);
            var footerHeight = $('.footer:visible').outerHeight(true);
            var resultHeight = totalHeight - headerHeight - footerHeight;
            game_content.outerHeight(resultHeight);
            game_content.scrollTop(game_content.prop('scrollHeight'));
        }
    }

    $(window).resize(scrollDown);
    $(window).trigger('resize');

    function addLine(line) {
        $('#history').append(line + '<br />');
    }

    function randomPercent() {
        var val = Math.random() * 100;
        return Math.round(val) + '%';
    }

    var obscureMoveDiv = null;
    function obscureMove() {
        var input = $('#make_move_move');
        var move = input.val();
        if (privateMove(input.val()) && $('#make_move_hotseat').is(':checked')) {
            if (obscureMoveDiv == null) {
                obscureMoveDiv = $('<div />');
                obscureMoveDiv.addClass('obscureMove');
                $('#make_move').after(obscureMoveDiv);
                obscureMoveDiv.offset(input.offset());
                obscureMoveDiv.width(input.outerWidth());
                obscureMoveDiv.height(input.outerHeight());
            }
            obscureMoveDiv.css('background-position',
                randomPercent() + ' ' + randomPercent());
        } else {
            if (obscureMoveDiv != null) {
                obscureMoveDiv.remove();
                obscureMoveDiv = null;
            }
        }
    }

    $('#make_move_move').keydown(obscureMove);
    $('#make_move_move').keyup(obscureMove);
    $('#make_move_move').change(obscureMove);
    $('#make_move_hotseat').change(obscureMove);

    $('#make_move').submit(function () {
        var form = $(this);
        var data = form.serialize();
        var move = $('#make_move_move').val();
        if (privateMove(move)) {
            move = '***';
        }
        addLine($('#make_move_player').val() + " > " + move);
        scrollDown();
        $.post('/' + gameId + '/move', data, function (result) {
            addLine(result);
            $('#make_move_move').val('');
            obscureMove();
            $('#make_move_move').focus();
            scrollDown();
        });
        return false;
    });

    refreshGames();
    refreshGame();
});
