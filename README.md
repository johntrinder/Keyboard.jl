Keyboard

A simple suite of Keyboard utilities for use in the Julia REPL.

Functions exported are:

readsingleline, readkey, queryrepeat, queryyesno, helpkeyboard, readradiomenu, readmultimenu, setmargin

Read the docstrings for each function in the "../src/Keyboard.jl" file and also the code in the "../test/testkeyboard.jl" file. In VSCode open the "Keyboard" folder and install it as a package.

In VSCode run (Ctrl+Alt+Enter) testkeyboard.jl and jump to the REPL. From there run function testall() to see it in action. Since this senses Ctrl+C to jump back to the main menu, I suggest opening the REPL in an external terminal (Ctrl+Shift+C).

Examples of arg 'data' for functions readradiomenu, readmultimenu

        1a) The descriptors are mandatory; keys will be auto-generated:
            ["aaa", ..., "eee"]
        1b) The descriptors are mandatory; keys will be auto-generated:
            [
            ["aaa"],
            ...,
            ["eee"],
            ]       
        2) Keys + descriptors:
            [
            ['z',"aaa"],
            ...,
            ['u',"eee"],
            ]             
        3) Keys + descriptors + functions (zero-arg):
            [
            ['z',"aaa", fun_1],
            ...,
            ['u',"eee", fun_5],
            ]
        4) Descriptors + functions (zero-arg); keys will be auto-generated:
            [
            ["aaa", fun_1],
            ...,
            ["eee", fun_5],
            ]
        Note that
            (1) dummy()=nothing  may be used as a dummy function.
            (2) If functions are specified in a list, all items must contain one.

# 3 types of return from readradiomenu:

    1) a Char = Char(3) - Ctrl+C was pressed.
    2) a Nothing - the ESC key was pressed.
    3) a NamedTuple{(:keychar, :index), Tuple{Char, Int64}} - a key character was selected; 
                                :index is the position in the list of keys/descriptors.

# 3 types of return from readmultimenu:

    1) a Char = Char(3), Ctrl+c was pressed.
    2) a Nothing - ESC key was pressed.
    3) a Union{Tuple{Vararg{T}}, Tuple{}} where T <:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}
        This is either a tuple of T's above, or an empty tuple.