$(document).ready(function () {
    function addLine (line) {
        $('#history').append(line + '<br />');
    }

    $('#submit').click(function () {
        var move = $('#move').val();
        addLine(move);
        $.post('/move', { move: move }, function (result) {
            addLine(result);
        });
    });
});
