(load (compile-file "code.lisp"))
(lw:deliver nil "my_lib" 0 :dll-exports '("toLispTime"))
