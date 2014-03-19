   program main
     external myfunc, myconstraint
     double precision lb(2)
     integer*8 opt
     double precision dd
     double precision x(2), minf
     integer ires
     include 'nlopt.f'

     call nlo_create(opt, NLOPT_LD_SLSQP, 2)
     call nlo_get_lower_bounds(ires, opt, lb)
     lb(2) = 0.0
     call nlo_set_lower_bounds(ires, opt, lb)
     dd = 2.
     call nlo_set_min_objective(ires, opt, myfunc, dd)

     call nlo_add_equality_constraint(ires, opt,&
          myconstraint, 0, 1.D-8)

     call nlo_set_xtol_rel(ires, opt, 1.D-4)

     x(1) = 3.0
     x(2) = 5.0
     call nlo_optimize(ires, opt, x, minf)
     if (ires.lt.0) then
        write(*,*) 'nlopt failed!'
        write(*,*) 'found min at ', x(1), x(2)
        write(*,*) 'min val = ', minf
        write(*,*) 'return value = ', ires
     else
        write(*,*) 'found min at ', x(1), x(2)
        write(*,*) 'min val = ', minf
        write(*,*) 'return value = ', ires
     endif

     call nlo_destroy(opt)
     stop
   end program main

   subroutine myfunc(val, n, x, grad, need_gradient, f_data)
     double precision val, x(n), grad(n), f_data, a
     integer n, need_gradient

     if (need_gradient.ne.0) then
        grad(1) = 0.0
        grad(2) = 0.5 / dsqrt(x(2))
     endif
     val = dsqrt(x(2))
   end subroutine myfunc

   subroutine myconstraint(val, n, x, grad, need_gradient)
     integer need_gradient
     double precision val, x(n), grad(n)

     if (need_gradient.ne.0) then
        grad(1) = 2.0*(x(1)-1.0)
        grad(2) = -1.0
     endif
     val = (x(1)-1.0)**2 - x(2) + 1.0
   end subroutine myconstraint

   subroutine mytest(a,b)
     double precision a,b
     a = a + b
   end subroutine mytest
