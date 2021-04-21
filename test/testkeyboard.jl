using Printf

using Keyboard

setmargin(6)

function test_readkey()
    println()
    println("Press any key combination (^C to exit).")
    println("METAKEY, KEYCHAR, SPECIALKEY")
    while true
        r = readkey(prompt = "")
        r.metakey === Keyboard.METAKEYNAMES_TABLE.ctrl && r.keychar == 'C' && break
        show(r.metakey); print(", "); show(r.keychar); print(", "); show(r.specialkey)
        println()
    end
end

function test_readsinglelinekey()
    p = "Do you wish to go riding?"
    k = "YN"
    shouldnt = "SHOULDN'T BE HERE"
    println()
    report(err) = println("ERROR TRAPPED: ", sprint(showerror, err))

    try
        readsingleline(p, "       ")
        println(shouldnt)
    catch e
        report(e)
    end
    try
        readsingleline(p, "YN\u7f")
        println(shouldnt)
    catch e
        report(e)
    end
    try
        readsingleline("       ", k)
        println(shouldnt)
    catch e
        report(e)
    end
    try
        readsingleline(p, k, defaultKey='Z')
        println(shouldnt)
    catch e
    report(e)
    end
    
    println()

    function assimresult(c::Union{Char, Nothing})::Bool
        println("result: '$c'\n")
        res = queryrepeat()
        println()
        return res
    end

    ctrl_c = Char(3)
    while true
        s = readsingleline(p, k, escape=true, defaultKey='Y', quitCtrlC=true)
        s == ctrl_c && return
    !assimresult(s) && break
    end
    while true
        s = readsingleline(p, k, escape=false, defaultKey='Y', quitCtrlC=true)
        s == ctrl_c && return
    !assimresult(s) && break
    end
    while true
        s = readsingleline(p, k, escape=true, quitCtrlC=true)
        s == ctrl_c && return
    !assimresult(s) && break
    end
    while true
        s = readsingleline(p, k, escape=false, quitCtrlC=true)
        s == ctrl_c && return
    !assimresult(s) && break
    end
end

function test_readmultiline_errors()
    shouldnt = "SHOULDN'T BE HERE "
    report(err) = println("ERROR TRAPPED: ", sprint(showerror, err))
    # report2(err) = println("FALSE ERROR - SHOULDN'T BE HERE: ", sprint(showerror, err))

    L = (
        "\nMiscellaneous:\n",

        [[' ', "aaa"]], 
        [['\x13', "bbb"]], 
        [[1, :a],4,[:b, 4]],        

        "\nIncorrect Lengths:\n",
        [[]],
        [["xxx"],[]],
        [['c',"one",report],['d',"two"]], 
        [["four",report],['g',"five",test_readkey]],
        [["six"],['h',"seven" ]],
        [['c',"message", report, 3]],

        "\nErroneous Elements:\n",
        [['e'],['f',"three"]],
        [["eight"],["nine"],[3]],
        [['i', "ten"],[4, "eleven"]],
        [['j', "twelve"],['k', 5]],
        [['l', "thirteen", report],['m', "fourteen", 6]],
        [['n', "fifteen", 7],['o', "sixteen", report]],
        [["seventeen", 7],["eighteen", report]],
        [["twenty", 'r'],['q', "nineteen"]],
        [[8, :abc],['s', "twentyone"]],

        "\nDuplicate key characters:\n",
        [['a', "one"],['A', "two"]],
        )
    for e in L
        if e isa String; println(e)
        else
            try
                readmultimenu("", e)
                println(shouldnt * _vector_sans_type_tostring(e))
            catch e
                report(e)
            end
        end
    end
    # try
    #     L = [['b', "twentytwo"], ['B', "twentythree"]] # ?duplicate keys
    #     readmultilinekey(data=L, case=true)
    # catch e
    #     report2(e) # this shouldn't occur - false error
    # end
end

function _test_readmultiline(;ismultisel::Bool)
    println()
    function doit(prompt, data; case::Bool=false)::Bool
        pre = nothing
        if ismultisel; pre = "Select one or more options. ";
        else; pre = "Select an option. "
        end
        prompt = pre * prompt
        while true
            r = nothing
            !ismultisel && (r = readradiomenu(prompt, data, case=case, quitCtrlC=true))
            ismultisel && (r = readmultimenu(prompt, data, case=case, quitCtrlC=true))
            r === Char(3) && return false
            s = r === nothing ? "ESC key pressed" : 
                (ismultisel ? "keys pressed=$r" : "keypress='$(r.keychar)', index=$(r.index)")
            println(s)        
            println()
            !queryrepeat() && (println(); break)
            println()
        end
        return true
    end
    L = [
        "aaa",
        "bbb",
        "ccc",
        "ddd",
        "eee",
        ]
    p = "Just Strings supplied, all in a single Vector (keys auto-generated, no functions)"
    !doit(p, L) && return

    L = [
        ["aaa"],
        ["bbb"],
        ["ccc"],
        ["ddd"],
        ["eee"],
        ]
    p = "Just Strings supplied, each in a Vector of it's own (keys auto-generated, no functions)"
    !doit(p, L) && return

    L = [
        ['z',"aaa"],
        ['y',"bbb"],
        ['x',"ccc"],
        ['v',"ddd"],
        ['u',"eee"],
        ]
    p = "Strings and keys supplied (no functions)"
    !doit(p, L) && return
    
    sp = "Function '%s' was evaluated\n"
    fun_1() = @eval @printf($sp, "fun_1")
    fun_2() = @eval @printf($sp, "fun_2")
    fun_3() = @eval @printf($sp, "fun_3")
    fun_4() = @eval @printf($sp, "fun_4")
    fun_5() = @eval @printf($sp, "fun_5")

    L = [
        ['z',"aaa", fun_1],
        ['y',"bbb", fun_2],
        ['x',"ccc", fun_3],
        ['v',"ddd", fun_4],
        ['u',"eee", fun_5],
        ]
    p = "Strings, keys and functions supplied"
    !doit(p, L) && return
    
    L = [
        ["aaa", fun_1],
        ["bbb", fun_2],
        ["ccc", fun_3],
        ["ddd", fun_4],
        ["eee", fun_5],
        ]
    p = "Strings and functions supplied (keys are auto-generated)"
    !doit(p, L) && return
    
    L = [
        ['a', "aaa", fun_1],
        ['A', "bbb", fun_2],
        ['b', "ccc", fun_3],
        ['B', "ddd", fun_4],
        ['c', "eee", fun_5],
        ]
    p = "Strings, keys and functions supplied (keys are case-sensitive)"
    !doit(p, L, case=true) && return

    return
end
test_readradiomenu() = _test_readmultiline(ismultisel=false)
test_readmultimenu() = _test_readmultiline(ismultisel=true)

function testall()
    L = [
        ["test_readmultiline_errors()", test_readmultiline_errors],
        ["test_readmultimenu()", test_readmultimenu],
        ["test_readradiomenu()", test_readradiomenu],
        ["test_readsinglelinekey()", test_readsinglelinekey],
        ["test_readkey()", test_readkey],
        ]
    p = "Select one of the following tests (which will be evaluated):"
    while true
        println()
        res = readradiomenu(p, L)
        res === nothing && return
        readkey()
    end
end

