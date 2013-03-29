$(document).ready(function () {
    var gameId = null;
    var gameLength = 0;

    function refreshGames(noTimer) {
        $.getJSON('/list', function (result) {
            var list = $('#games');
            list.html('');
            $.each(result, function (i, game) {
                var link = $('<a href="/">' + game + '</a>');
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
            $('#game').show();
            $('#games_container').hide();
            $.getJSON('/' + gameId + '/log', function (result) {
                var str = "";
                for (var i = 0; i < result.length; i++) {
                    var move = result[i];
                    str += move.player + " > " + move.move + "<br />";
                    str += move.result + "<br />";
                }
                $('#history').html(str);
                if (result.length > gameLength) {
                    scrollDown();
                }
                gameLength = result.length;
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
        $(window).scrollTop($('#history').height());
    }

    function addLine(line) {
        $('#history').append(line + '<br />');
    }

    $('#make_move').submit(function () {
        var form = $(this);
        var data = form.serialize();
        addLine($('#make_move_player').val() + " > " + $('#make_move_move').val());
        scrollDown();
        $.post('/' + gameId + '/move', data, function (result) {
            addLine(result);
            $('#move').val('');
            $('#move').focus();
            scrollDown();
        });
        return false;
    });

    refreshGames();
    refreshGame();
});
