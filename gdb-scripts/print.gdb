define pw
    dont-repeat
    if $argc == 0
        print w_str(w)
    end
    if $argc == 1
        print w_str($arg0)
    end
end
document pw
    Print w, args = [0,1]
end

define pws
    dont-repeat
    call printf("-------------------------------\n")
    if $argc == 0
        call ws_print(ws, ws_recurse_none)
    end
    if $argc == 1
        call ws_print($arg0, ws_recurse_none)
    end
end
document pws
    Print ws, args = [0,1,2]
end

define pwsv
    dont-repeat
    call printf("-------------------------------\n")
    if $argc == 0
        call ws_print(ws, ws_recurse_ws)
    end
    if $argc == 1
        call ws_print($arg0, ws_recurse_ws)
    end
end
document pwsv
    Print ws, args = [0,1,2]
end

define pwsvv
    dont-repeat
    call printf("-------------------------------\n")
    if $argc == 0
        call ws_print(ws, ws_recurse_most)
    end
    if $argc == 1
        call ws_print($arg0, ws_recurse_most)
    end
end
document pwsvv
    Print ws, args = [0,1,2]
end
