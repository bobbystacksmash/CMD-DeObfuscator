
function HANDLER_generic (identcmd) {
    console.log("-- generic handler --");
}


function HANDLER_set (identcmd, context) {

}


function HANDLER_cmd (identcmd, context) {

    // First we need to create a fresh context.
    let newctx = {
        command:   "cmd",
        variables: {},
        options:   {}
    };
    context.unshift(newctx);
}


module.exports = function get_command(identcmd) {
    let cmd = identcmd.cmd.toLowerCase();

    const commands = {
        cmd:      HANDLER_cmd,
        regsvr32: HANDLER_generic,
        set:      HANDLER_set
    };

    if (commands.hasOwnProperty(cmd)) {
        return commands[cmd];
    }

    return commands.default;
}
