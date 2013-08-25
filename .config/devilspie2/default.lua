-- Devilspie2 configuration

local app = get_application_name()

if app == "emacs" or app == "Vim" or app == "scite" then
   set_window_geometry(683, 0, 683, 711)
   maximize_vertically()
   
elseif app == "VLC media player" then
   set_window_geometry(0, 0, 683, 711)
   maximize_vertically()
   
end
