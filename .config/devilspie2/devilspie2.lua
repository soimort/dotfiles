-- Devilspie2 configuration (customized for Hope, dual monitor setting)


--print(get_application_name())
--print(get_process_name())
--print(get_monitor_geometry(get_monitor_index()))
--print(get_window_geometry())


if get_process_name() == "mate-terminal" then
   unmaximize()
   set_window_geometry(1080, 27, 960, 1023)  -- wmctrl -i -r <WIN> -e 0,1080,27,960,1023
   maximize_vertically()
   undecorate_window()

elseif get_process_name() == "eog" or get_process_name() == "gnome-system-mo" then
   unmaximize()
   set_window_geometry(2040, 27, 960, 1023)
   maximize_vertically()
   undecorate_window()

elseif get_application_name() == "Task Manager - Chromium" then
   unmaximize()
   set_window_geometry(2040, 27, 960, 1023)
   maximize_vertically()
   undecorate_window()

--elseif get_process_name() == "chromium" then
--   unmaximize()
--   set_window_geometry(1080, 27, 1920, 1023)
--   maximize_vertically()
--   undecorate_window()

end

-- Full-screen #2: 0.0 0.0 1080.0 1890.0
