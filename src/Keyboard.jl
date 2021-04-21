module Keyboard

export readsingleline, readkey, queryrepeat, queryyesno, helpkeyboard, 
        readradiomenu, readmultimenu, setmargin

import REPL
using Printf
using InteractiveUtils

# cf https://stackoverflow.com/questions/60954235/how-can-i-test-whether-stdin-has-input-available-in-julia 

"""
Global var setting the left offset in the REPL for printing the list of keys/descriptors.

To modify, call setmargin().
"""
MARGIN = 1

ALLSPECIALKEYS_LUT = Dict{UInt64, Symbol}(
    0x1B5B5B41 => :FN1,
    0x1B5B5B42 => :FN2,
    0x1B5B5B43 => :FN3,
    0x1B5B5B44 => :FN4,
    0x1B5B5B45 => :FN5,
    0x1B5B31377E => :FN6,
    0x1B5B31387E => :FN7,
    0x1B5B31397E => :FN8,
    0x1B5B32307E => :FN9,
    0x1B5B32317E => :FN10,
    0x1B5B32337E => :FN11, # = SHIFT_FN1
    0x1B5B32347E => :FN12, # = SHIFT_FN2
    # 0x1B5B32337E => :SHIFT_FN1,
    # 0x1B5B32347E => :SHIFT_FN2,
    # 0x1B5B32357E => :SHIFT_FN3,
    # 0x1B5B32367E => :SHIFT_FN4,
    # 0x1B5B32387E => :SHIFT_FN5,
    # 0x1B5B32397E => :SHIFT_FN6,
    # 0x1B5B33317E => :SHIFT_FN7,
    # 0x1B5B33327E => :SHIFT_FN8,
    # 0x1B5B33337E => :SHIFT_FN9,
    # 0x1B5B33347E => :SHIFT_FN10,
    # 0x1B5B323324 => :SHIFT_FN11,
    # 0x1B5B323424 => :SHIFT_FN12,
    0x1B5B31315E => :CTRL_FN1,
    0x1B5B31325E => :CTRL_FN2,
    0x1B5B31335E => :CTRL_FN3,
    0x1B5B31345E => :CTRL_FN4,
    0x1B5B31355E => :CTRL_FN5,
    0x1B5B31375E => :CTRL_FN6,
    0x1B5B31385E => :CTRL_FN7,
    0x1B5B31395E => :CTRL_FN8,
    0x1B5B32305E => :CTRL_FN9,
    0x1B5B32315E => :CTRL_FN10,
    0x1B5B32335E => :CTRL_FN11,
    0x1B5B32345E => :CTRL_FN12,
    0x1B1B5B5B41 => :ALT_FN1,
    0x1B1B5B5B42 => :ALT_FN2,
    0x1B1B5B5B43 => :ALT_FN3,
    0x1B1B5B5B44 => :ALT_FN4,
    0x1B1B5B5B45 => :ALT_FN5,
    0x1B1B5B31377E => :ALT_FN6,
    0x1B1B5B31387E => :ALT_FN7,
    0x1B1B5B31397E => :ALT_FN8,
    0x1B1B5B32307E => :ALT_FN9,
    0x1B1B5B32317E => :ALT_FN10,
    0x1B1B5B32337E => :ALT_FN11,
    0x1B1B5B32347E => :ALT_FN12,
    0x1B5B337E => :DEL,
    0x1B5B317E => :HOME,
    0x1B5B347E => :END,
    0x1B5B357E => :PAGEUP,
    0x1B5B367E => :PAGEDOWN,
    0x1B5B333B327E => :SHIFT_DEL,
    0x1B5B313B327E => :SHIFT_HOME,
    0x1B5B343B327E => :SHIFT_END,
    0x1B5B353B327E => :SHIFT_PAGEUP,
    0x1B5B363B327E => :SHIFT_PAGEDOWN,
    0x1B5B333B357E => :CTRL_DEL,
    0x1B5B313B357E => :CTRL_HOME,
    0x1B5B343B357E => :CTRL_END,
    0x1B5B353B357E => :CTRL_PAGEUP,
    0x1B5B363B357E => :CTRL_PAGEDOWN,
    0x1B1B5B337E => :ALT_DEL,
    0x1B1B5B317E => :ALT_HOME,
    0x1B1B5B347E => :ALT_END,
    0x1B1B5B357E => :ALT_PAGEUP,
    0x1B1B5B367E => :ALT_PAGEDOWN,
    0x1B5B41 => :CURSORUP,
    0x1B5B42 => :CURSORDOWN,
    0x1B5B44 => :CURSORLEFT,
    0x1B5B43 => :CURSORRIGHT,
    0x1B5B313B3541 => :CTRL_CURSORUP,
    0x1B5B313B3542 => :CTRL_CURSORDOWN,
    0x1B5B313B3544 => :CTRL_CURSORLEFT,
    0x1B5B313B3543 => :CTRL_CURSORRIGHT,
    0x1B1B5B41 => :ALT_CURSORUP,
    0x1B1B5B42 => :ALT_CURSORDOWN,
    0x1B1B5B44 => :ALT_CURSORLEFT,
    0x1B1B5B43 => :ALT_CURSORRIGHT
    )

CTRLCHARS_LUT =  Dict(zip(collect(Char(1):Char(31)), collect("ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_")))

"""
            _vector_sans_type_tostring(vec::AbstractVector)::String

Return a String detailing the arg 'vec''s values with nesting (if any), without the Types moiety.
"""
function _vector_sans_type_tostring(vec::AbstractVector)::String
    io = IOBuffer()

    function recurse(v::AbstractVector)
        print(io, "[")
        for (i, elt) in enumerate(v)
            i > 1 && print(io, ", ")
            if elt isa AbstractVector
                recurse(elt)
            else
                if elt isa Symbol 
                    elt = String(":" * String(elt))
                elseif elt isa Char
                    elt = "'" * elt * "'"
                elseif elt isa String
                    elt = "\"" * elt * "\""
                end
                print(io, elt)
            end
        end
        print(io, "]")
    end
    recurse(vec)

    return String(take!(io))
end

"""
            _splitspecialkeycombo(s::Symbol) ::
                    NamedTuple{(:metakey, :specialkey), Tuple{Union{Symbol, Nothing}, Symbol}}

Split the supplied Symbol, arg 's' according to the underscore seperator '_'. 
The left-hand moiety will be a metakey name; the right a specialkey name. If no seperator is present, assume 's' 
is a specialkey.

# Examples of arg 's':

 :CTRL_HOME, :HOME

 Returns (metakey=:CTRL, specialkey=:HOME) and (metakey=nothing, specialkey=:HOME)
"""
function _splitspecialkeycombo(s::Symbol) ::
        NamedTuple{(:metakey, :specialkey), Tuple{Union{Symbol, Nothing}, Symbol}}
    ans1 = nothing
    ans2 = nothing
    a = split(String(s), "_")
    if length(a) == 1
        ans2 = Symbol(a[1])
    else
        ans1 = Symbol(a[1])
        ans2 = Symbol(a[2])
    end

    return (metakey = ans1, specialkey = ans2)
end

"""
            _getallkeynames():: 
                    Tuple{NamedTuple{(:ctrl, :shift, :alt), Tuple{Symbol, Symbol, Symbol}}, NTuple{21,Symbol}}
# Return
All extracted meta- and special-keys as symbols. 

(The meta-keys are in the NamedTuple, eg alt=:ALT, and the special-keys are in the NTuple, eg :ESC).
"""
function _getallkeynames() :: 
        Tuple{NamedTuple{(:ctrl, :shift, :alt), Tuple{Symbol, Symbol, Symbol}}, NTuple{21,Symbol}}
    metas = Set{Symbol}()
    specials = Set{Symbol}()
    for v in values(ALLSPECIALKEYS_LUT)
        a = _splitspecialkeycombo(v)
        push!(specials, a.specialkey)
        if a.metakey !== nothing
            push!(metas, a.metakey)
        end
    end
    if length(metas) != 3
        error("Metakey-name error in ALLSPECIALKEYS_TABLE")
    end
    if length(specials) != 21
        error("Special-name error in ALLSPECIALKEYS_TABLE")
    end
    metakeys = collect(metas)
    sort!(metakeys)
    ans1 = (ctrl = metakeys[2], shift = metakeys[3], alt = metakeys[1])
    specialsnames = collect(specials)
    sort!(specialsnames)
    
    return ans1, tuple(specialsnames ...)
end

"""
            _isprintable(c::Char) :: Bool = c > ' ' && isprint(c)
Here, printable means that the arg 'c' > space and isprint(c) evaluates to true.
"""
_isprintable(c::Char) :: Bool = c > ' ' && isprint(c)
"""
            _isprintable(s::AbstractString) :: Bool 
Test arg 's' consists of _isprintable() characters (space is also regarded as non-printable).
"""
function _isprintable(s::AbstractString) :: Bool 
    isempty(s) && return false
    for c in s
        !_isprintable(c) && return false
    end
    return true;
end

METAKEYNAMES_TABLE, SPECIALKEYNAMES_TABLE = _getallkeynames()

CTRLSPECIALKEYNAMES_LUT = Dict{Char, Symbol}(
    '[' => :ESC,
    'H' => :BACK,
    'I' => :TAB,
    'M' => :ENTER,
)

"""
            readkey(; prompt::String = "Press any key..")

If arg 'prompt' != "" then the function evaluates println(prompt) before waiting for a key-press.

        Returns a NamedTuple{(:metakey, :keychar, :specialkey),
            Tuple{Union{Symbol, Nothing}, Union{Char, Nothing}, Union{Symbol, Nothing}}}
"""
function readkey(; prompt::String="Press any key..") ::
    NamedTuple{(:metakey, :keychar, :specialkey),
        Tuple{Union{Symbol, Nothing}, Union{Char, Nothing}, Union{Symbol, Nothing}}}
    term = REPL.Terminals.TTYTerminal("xterm",stdin,stdout,stderr)
    REPL.Terminals.raw!(term,true)
    Base.start_reading(stdin)

    prompt != "" && println(prompt)
    read(stdin, bytesavailable(stdin)) # clear stdin

    while true
        keychar::Union{Char, Nothing} = nothing
        metakey::Union{Symbol, Nothing} = nothing
        specialkey::Union{Symbol, Nothing} = nothing
        
        sleep(0.01)
        ba = bytesavailable(stdin)
        ba == 0 && continue

        data = read(stdin, ba)
        if ba == 1
            keychar = data[1]
            if iscntrl(keychar)
                keychar = CTRLCHARS_LUT[keychar]
                metakey = METAKEYNAMES_TABLE.ctrl
                if haskey(CTRLSPECIALKEYNAMES_LUT, keychar) 
                    specialkey = CTRLSPECIALKEYNAMES_LUT[keychar]
                end
            end
        elseif ba == 2
            keychar = data[2]
            metakey = METAKEYNAMES_TABLE.alt
        else
            n::UInt64 = data[1]
            for i in 2:ba
                n <<= 8
                n += data[i]
            end
            if haskey(ALLSPECIALKEYS_LUT, n)
                combo = ALLSPECIALKEYS_LUT[n]
                metakey, specialkey = _splitspecialkeycombo(combo)
                keychar = nothing
            else
                continue # key not in LUTs
            end
        end
        metakey === keychar === specialkey === nothing && continue
        ans = (metakey = metakey, keychar = keychar, specialkey = specialkey)

        return ans
    end
end

"""
Compose and return a user-friendly string describing special key choices to be printed after the list of choices upon return.
# Examples
        "[ESC=cancel, Enter=y, ^C=quit]" :- escape = quitCtrlC = true; defaultKey isa Char.
        "[ESC=cancel, ^C=quit, TAB=all, BACK=none, ENTER=commit]" :- escape = quitCtrlC = ismultisel = true.
"""
function _getescenter(escape::Bool, defaultKey::Union{Char, Nothing}, quitCtrlC::Bool, ismultisel::Bool)::String
    s = ""
    function escenterquit()
        escape && (s = "[ESC=cancel")
        if defaultKey === nothing && !quitCtrlC
            if isempty(s)
                return s
            else
                return s * ']'
            end
        end
        s = isempty(s) ? "[" : s
        if defaultKey isa Char 
            if escape 
                s *= ", Enter=$defaultKey"
            else
                s *= "Enter=$defaultKey"
            end
        end
        !quitCtrlC && return s * ']'
        if length(s) > 2
            s *= ", ^C=quit]" 
        else
            s = "[^C=quit]"
        end
        return s
    end

    s = escenterquit()
    !ismultisel && return s

    s = strip(s) # just in case
    s = isempty(s) ? "[" : s[1:end-1] * ", "
    s *= "TAB=all, BACK=none, ENTER=commit]"
    return s
end

"""
            _getkeyboardchar(
                keys::String, # a string of Chars with which to validate a key-press.
                defaultKey::Union{Char, Nothing}, # if this is a Char, detect the Enter key press.
                case::Bool, # whether to apply case-sensitivity to detection in arg 'keys'.
                escape::Bool, # whether to detect Esc key-press.
                quitCtrlC::Bool, # whether to detect Ctrl+C key-press.
                ismultisel::Bool # true = detect Tab (fill), Back (unfill) and Enter (commit) key-presses.
                )::Tuple{Union{Char, Symbol}, Int64}

Wait for a key-press. Validate using the args supplied.
# Return a Tuple{Union{Char, Symbol}, Int64} upon valid key-press:
        1) if key-press = Ctrl+C, return (Char(3), index = -1).
        2) if key-press is in arg 'keys' then return (keychar, index_into_keys).
        3) if key-press is a special key then return (specialkey, index = 0).
"""
function _getkeyboardchar(
    keys::String, # a list of Chars with which to validate a key-press
    defaultKey::Union{Char, Nothing}, # if this is a Char, detect the Enter key press
    case::Bool, # whether to apply case-sensitivity to detection in arg 'keys'
    escape::Bool, # whether to detect Esc key-press
    quitCtrlC::Bool, # whether to detect Ctrl+C key-press
    ismultisel::Bool # true = detect Tab (fill), Back (unfill) and Enter (commit) key-presses
    ) :: Tuple{Union{Char, Symbol}, Int64}

    ix = nothing
    u = nothing
    while true
        res = readkey(prompt="")
        quitCtrlC && res.metakey === METAKEYNAMES_TABLE.ctrl && res.keychar == 'C' && 
                (u=Char(3); ix=-1; break)
        escape && res.specialkey === :ESC && 
                (u=:ESC; ix=0; break)
        defaultKey !== nothing && res.specialkey === :ENTER && 
                (u=defaultKey; ix=findfirst(u, keys); break)
        sk = res.specialkey
        ismultisel && sk in (:TAB, :ENTER, :BACK) &&
                (u = sk; ix=0; break)
        (res.keychar === nothing || res.metakey !== nothing) && continue 
        u = res.keychar
        !case && (u = lowercase(u))
        ix = findfirst(u, keys)
        ix !== nothing && break
    end
    return u, ix
end

"""
        readsingleline(
           prompt::String, 
           keys::String, # a list of keyboard characters one of which the user may select (not case-sensitive).
           ;
           escape::Bool = true, 
           defaultKey::Union{Char, Nothing} = nothing, # or a keyboard character present in the arg 'keys' (not case-sensitive).
           quitCtrlC::Bool = false
           )::Union{Char, Nothing}

Prints the arg 'prompt' followed by waiting for a valid key-press.

It returns EITHER a user-selected char (always in lower case) present in the arg 'keys' OR nothing (the user pressed ESC). 

If the arg 'quitCtrlC' = true THEN returns Char(3) if Ctrl+C is pressed.

NB. This function is not case-sensitive.
"""
function readsingleline(
    prompt::String, 
    keys::String, 
    ;
    escape::Bool = true, 
    defaultKey::Union{Char, Nothing} = nothing,
    quitCtrlC::Bool = false
    )::Union{Char, Nothing}
    
    keychars = Vector{Char}("(")
    prompt = strip(prompt)
    isempty(prompt) && error("The arg 'prompt' has no text.")
    keys = lowercase(join(split(keys)))
    isempty(keys) && error("The arg 'keys' is empty.")
    !_isprintable(keys) && error("The arg 'keys' (\"$keys\") must consist of printable characters.")
    if defaultKey isa Char
        defaultKey = lowercase(defaultKey) 
        !(defaultKey in keys) && error("The arg 'defaultKey'('$defaultKey') is not in the arg 'keys'(\"$keys\").")
    end
    for chr in keys
        push!(keychars, chr, '|')
    end
    pop!(keychars); push!(keychars, ')')
    prompt *= join(keychars) * _getescenter(escape, defaultKey, quitCtrlC, false)

    println(prompt)

    chr, _ = _getkeyboardchar(keys, defaultKey, false, escape, quitCtrlC, false)
    chr === :ESC && return nothing
    return chr
end

"""
        _readmultiline(
            prompt::String,  # a string to print first of all, describing the menu.
            data,  # a Vector of Vectors with which to extract keys (optional), descriptors and functions (optional).
            ;
            escape::Bool=true, 
            case::Bool=false, # true=case-sensitive for the extracted keys.
            ismultisel::Bool, # true=multiple keys selection; else radio type selection
            quitCtrlC::Bool=false
            )::Union{T, Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} 
                    where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

# Argument 'data'

            Examples:
    
            1) The descriptors are mandatory; keys will be auto-generated:
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
            4) Descriptors + functions (zero-arg):
                [
                ["aaa", fun_1],
                ...,
                ["eee", fun_5],
                ]
            Note that
                (1) dummy()=nothing  may be used as a dummy function.
                (2) If functions are specified in a list, all items must contain one.
        
# 4 types of return:

        1) a Char = Char(3), Ctrl+c was pressed.
        2) a Nothing - ESC key was pressed.
        3) a NamedTuple{(:keychar, :index), Tuple{Char, Int64}} - a key character was selected; 
            :index is the position in the list of keys (ismultisel=false).
        4) a Union{Tuple{Vararg{T}}, Tuple{}} where T <:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}
            This is either a tuple of T's above, or an empty list; (ismultisel=true).
"""
function _readmultiline(
    prompt::String, 
    data, 
    ;
    escape::Bool=true, 
    case::Bool=false,
    ismultisel::Bool,
    quitCtrlC::Bool=false
    )::Union{T, Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

    datauniform = nothing
    try; datauniform = Vector{Vector{Any}}(deepcopy(data))
    catch; error("The passed 'data' must be of the form [[...],[...], etc]")
    end

    sel = ("$(' '^MARGIN){", "} ") # prefix the keychar, and postfix for a user-selected key
    unsel = ("$(' '^MARGIN) ", "  ") # ditto for a non-user-selected key

    # ASSIGN vars typeel_1 AND typeel_3 (CORRESPONDING TO THE TYPES OF THE 3 COLUMNS IN datauniform)
    # AND ASSIGN nothing TO UNASIGNED COLUMNS IN datauniform:
    typeel_1 = Nothing; typeel_3 = Nothing
    firstitem = data[1]
    lenfirstitem = length(firstitem)

    if lenfirstitem == 0
        error("The first item is empty in the passed arg 'data'")
    elseif lenfirstitem == 1
        push!.(datauniform, nothing)
        pushfirst!.(datauniform, nothing)
    elseif lenfirstitem == 2
        fun = pushfirst!
        if firstitem[2] isa String 
            fun = push!
            typeel_1 = Char
        else
            typeel_3 = Function
        end
        fun.(datauniform, nothing)
    elseif lenfirstitem == 3
        typeel_1 = Char
        typeel_3 = Function
    else
        error("The number of elements can not exceed 3 in the item: $(_vector_sans_type_tostring(firstitem))")
    end

    typeel_1 === Nothing && (case = false) # auto-generated keys are all lower case anyway

    # EXTRACT ALL ELEMENTS FROM datauniform INTO 3 Vectors:
    keychars = Vector{typeel_1}()
    descriptors = Vector{String}()
    functions = Vector{typeel_3}()
    for i in 1:length(datauniform)
        item = datauniform[i]
        dataitem = data[i]
        length(dataitem) != lenfirstitem && 
            error("There must be $lenfirstitem element(s) in $(_vector_sans_type_tostring(dataitem))")
        err = nothing
        try
            err = "The first element must be a Char in "
            ele = item[1] isa typeel_1 ? item[1] : "dummy" # int and floats are converted to Char in push!
            ele isa Char && !_isprintable(ele) &&
                (err = "The key character must be printable in "; error())
            push!(keychars, ele)
            err = "Either a String is missing or it is in the wrong position in "
            push!(descriptors, item[2])
            err = "The last element must be a Function in "
            push!(functions, item[3])
        catch
            error(err * _vector_sans_type_tostring(dataitem))
        end
    end

    # CHECK FOR DUPLICATES IN keychars AND CONVERT IT TO A CASE-ADJUSTED String keystring:
    keystring = nothing
    if typeel_1 === Char
        keystring = join(keychars)
        !case && (keystring = lowercase(keystring))
        length(Set(collect(keystring))) < length(keystring) && 
            error("There are duplicates in the extracted keys: \"$(join(keychars))\"")
    else
        keystring = join('1':'9') * join('a':'z')
        length(data) > length(keystring) && 
            error("Can't auto-generate all of the key characters - the arg 'data' is too big.")
        keystring = keystring[1:length(data)]
    end

    # OK, NO DETECTED ERRORS IN PASSED 'data'; PRINT THE LIST FOLLOWED BY THE PROMPT:
    println(prompt)
    for i in 1:length(data)
        println("$(unsel[1])$(keystring[i])$(unsel[2])$(descriptors[i])")
    end
    println(_getescenter(escape, nothing, quitCtrlC, ismultisel))

    # DETECT KEY-PRESSES AND ASSIMILATE THEM:
    lenkeys = length(keystring)
    selkeys = Vector{Bool}(undef, lenkeys)
    fill!(selkeys, false)
    while true
        cors, index = _getkeyboardchar(keystring, nothing, case, escape, quitCtrlC, ismultisel)
        cors === Char(3) && return cors
        cors === :ESC && return nothing
        if !ismultisel
            typeel_3 === Function && cors isa Char && cors !== Char(3) && functions[index]() # eval the function
            return (keychar=cors, index=index)
        end
        if cors === :ENTER
            v = Vector{NamedTuple{(:keychar, :index), Tuple{Char, Int64}}}()
            for i in 1:lenkeys
                selkeys[i] && push!(v, (keychar=keystring[i], index=i))
                typeel_3 === Function && selkeys[i] && functions[i]() # eval a function
            end
            return Tuple(v)
        elseif cors === :TAB
            fill!(selkeys, true)
        elseif cors === :BACK
            fill!(selkeys, false)
        else
            selkeys[index] = !selkeys[index] # keychar pressed - toggle selection
        end
        print("\x1b[?25l\x1b[$(lenkeys + 1)A") # move cursor up n lines and hide it
        for i in 1:lenkeys
            p = selkeys[i] ? sel : unsel
            println("$(p[1])$(keystring[i])$(p[2])")                
        end
        println("\x1b[?25h") # extra 1 for the keys info line at the bottom; show cursor
    end
end

"""
            readradiomenu(    
                    prompt::String, # a string to print first of all, describing the menu.
                    data::Vector{String}, # a single Vector of Strings
                    ;
                    escape::Bool=true, 
                    case::Bool=false, # true=case-sensitive for the extracted keys.
                    quitCtrlC::Bool=false
                    )::Union{T, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

This overloaded function differs in that the arg 'data' must be a Vector of Strings.
"""
function readradiomenu(    
        prompt::String, # a string to print first of all, describing the menu.
        data::Vector{String}, # a single array of Strings
        ;
        escape::Bool=true, 
        case::Bool=false, # true=case-sensitive for the extracted keys.
        quitCtrlC::Bool=false
        )::Union{T, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

    d = [[x] for x in data]
    readradiomenu(    
        prompt, 
        d, 
        ;
        escape = escape, 
        case = case,
        quitCtrlC = quitCtrlC
        )
end

"""
            readradiomenu(    
                prompt::String, # a string to print first of all, describing the menu.
                data, # a Vector of Vectors with which to extract keys (optional), descriptors and functions (optional).
                ;
                escape::Bool=true, 
                case::Bool=false, # true=case-sensitive for the extracted keys.
                quitCtrlC::Bool=false
                )::Union{T, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

Prints a prompt followed by a formatted list of keys and descriptors; it then waits for a valid single keypress, 
upon which the function will return with the results. If function names were supplied, the corresponding zero-arg
function will be evaluated.

Evaluate setmargin(..) to set the left offset into the list printout (global).

# Argument 'data'

        Examples:

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

# 3 types of return:

    1) a Char = Char(3) - Ctrl+C was pressed.
    2) a Nothing - the ESC key was pressed.
    3) a NamedTuple{(:keychar, :index), Tuple{Char, Int64}} - a key character was selected; 
                                :index is the position in the list of keys/descriptors.
"""
readradiomenu(    
    prompt::String, 
    data, 
    ;
    escape::Bool=true, 
    case::Bool=false,
    quitCtrlC::Bool=false
    )::Union{T, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}} = 
        _readmultiline(prompt, data, escape=escape, case=case, ismultisel=false, quitCtrlC=quitCtrlC)


"""
            readmultimenu(    
                prompt::String, # a string to print first of all, describing the menu.
                data::Vector{String}, # a single Vector of Strings
                ;
                escape::Bool=true, 
                case::Bool=false, # true=case-sensitive for the extracted keys.
                quitCtrlC::Bool=false
                )::Union{Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} 
                        where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

This overloaded function differs in that the arg 'data' must be a Vector of Strings.
"""        
function readmultimenu(    
    prompt::String, # a string to print first of all, describing the menu.
    data::Vector{String}, # a single array of Strings
    ;
    escape::Bool=true, 
    case::Bool=false, # true=case-sensitive for the extracted keys.
    quitCtrlC::Bool=false
    )::Union{Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

    d = [[x] for x in data]
    readmultimenu(    
        prompt, 
        d, 
        ;
        escape = escape, 
        case = case,
        quitCtrlC = quitCtrlC
        )
end

"""
            readmultimenu(    
                prompt::String,  # a string to print first of all, describing the menu.
                data,  # a Vector of Vectors with which to extract keys (optional), descriptors and functions (optional).
                ;
                escape::Bool=true, 
                case::Bool=false, # true=case-sensitive for the extracted keys.
                quitCtrlC::Bool=false
                )::Union{Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} 
                    where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}

Prints a prompt followed by a formatted list of keys and descriptors; 
it then waits for a succession of valid single keypresses, upon which each selected key from the list 
will be 'un|checked' (toggled). If function names were supplied, the corresponding zero-arg functions will 
be evaluated sequentially. Pressing the Enter key will commit the changes.

Pressing TAB will check all of the keys; whilst pressing BACK will uncheck them.

Evaluate setmargin(..) to set the left offset into the list's printout (global).

# Argument 'data'

            Examples:
    
            1a) The descriptors are mandatory; keys will be auto-generated:
                [ "aaa", ..., "eee"]
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

# 3 types of return:

        1) a Char = Char(3), Ctrl+c was pressed.
        2) a Nothing - ESC key was pressed.
        3) a Union{Tuple{Vararg{T}}, Tuple{}} where T <:NamedTuple{(:keychar, :index), Tuple{Char, Int64}}
            This is either a tuple of T's above, or an empty tuple.
"""
readmultimenu(    
    prompt::String, 
    data, 
    ;
    escape::Bool=true, 
    case::Bool=false,
    quitCtrlC::Bool=false
    )::Union{Union{Tuple{Vararg{T}}, Tuple{}}, Nothing, Char} where T>:NamedTuple{(:keychar, :index), Tuple{Char, Int64}} = 
        _readmultiline(prompt, data, escape=escape, case=case, ismultisel=true, quitCtrlC=quitCtrlC)


"""
                queryrepeat(prompt::String="Repeat?") :: Bool

Print the prompt and wait for a valid key press; upon which the function will return a Bool.

IF return = true, the 'y' key or Enter key was pressed ELSE the 'n' key or ESC key was pressed.
"""
function queryrepeat(prompt::String="Repeat?") :: Bool
    c = readsingleline(prompt, "YN", escape=true, defaultKey='Y')
    (c === nothing || c === 'n') && return false
    return true
end

"""
        queryyesno(prompt::String)::Bool

Print the prompt and wait for a valid key press; upon which the function will return a Bool.

IF return = true, the 'y' key or Enter key was pressed ELSE the 'n' key or ESC key was pressed.
"""
queryyesno(prompt::String)::Bool = queryrepeat(prompt)

"""
            helpkeyboard()

Print all of the special key names extracted (including the metakeys).
"""
function helpkeyboard()
    f(x) = ":$(String(x))"
    L = f.(SPECIALKEYNAMES_TABLE) # convert tuple of symbols to tuple of strings
    L2 = [s for s in L]
    sort!(L2)
    functionkeys = [s for s in L2 if lowercase(s[2]) == 'f']
    specialkeys = [s for s in L2 if lowercase(s[2]) != 'f']
    metakeys = f.(Tuple(METAKEYNAMES_TABLE))
    ctrlspecialkeys = f.(values(CTRLSPECIALKEYNAMES_LUT))
    step = 6
    function printit(title, list)
        println(title)
        for i in 0:step:length(list) - 1
            for j in 1:step
                ix = i + j
                ix > length(list) && break
                print("\t$(list[ix])")
            end
            println()
        end
    end
    printit("\nFunction-key symbols:", functionkeys)
    printit("\nSpecial-key symbols:", specialkeys)
    printit("\nMeta-key symbols:", metakeys)
    printit("\nControl-special-key symbols:", ctrlspecialkeys)
end

"""
            setmargin(margin::Real)

Sets the private global MARGIN which is used to set the left offset of the printed list of keys/descriptors.
"""
function setmargin(margin::Real)
    x::Int64 = floor(Int64, margin)
    global MARGIN = min(max(x, 0), 30)    
end

end # Keyboard
