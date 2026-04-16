from apis import ApisProvider

import gdb

objfile: gdb.Objfile = gdb.current_objfile()
objfile.pretty_printers.append(ApisProvider("apis"))
