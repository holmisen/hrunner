import Graphics.UI.Gtk


main = do
   initGUI
   
   d <- dialogNew
   dialogAddButton d "OK" ResponseOk
--   dialogRun d
   widgetShowAll d
   mainGUI
