local function smart_esc()
    local fullscreen = mp.get_property_bool("fullscreen")
    if fullscreen then
        mp.set_property("fullscreen", "no")
    else
        mp.command("quit")
    end
end

mp.add_key_binding("ESC", "smart-esc", smart_esc)
