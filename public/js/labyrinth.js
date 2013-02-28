$(document).ready(function () {
    var gameId = null;
    var gameLength = 0;

    function refreshGames(noTimer) {
        $.get('/list', function (result) {
            var list = $('#games');
            list.html('');
            $.each(result.split(", "), function (i, game) {
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
            $.get('/' + gameId + '/log', function (result) {
                result = result.replace(/\n/g, '<br />');
                result += '<br />';
                $('#history').html(result);
                if (result.length > gameLength) {
                    $(window).scrollTop($('#history').height());
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

    function addLine(line) {
        $('#history').append(line + '<br />');
    }

    $('#make_move').submit(function () {
        var form = $(this);
        var data = form.serialize();
        addLine("player " + $('#player').val() + ": " + $('#move').val());
        $.post('/' + gameId + '/move', data, function (result) {
            addLine(result);
            $('#move').val('');
            $('#move').focus();
        });
        return false;
    });

    refreshGames();
    refreshGame();
});
